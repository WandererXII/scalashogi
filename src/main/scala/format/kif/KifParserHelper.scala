package shogi
package format
package kif

import cats.data.Validated
import cats.data.Validated.{ invalid, valid }
import cats.implicits._

import shogi.variant._

object KifParserHelper {

  def parseSituation(
      str: String,
      handicap: Option[String],
      moves: List[String]
  ): Validated[String, Situation] = {
    val lines = augmentString(str).linesIterator.toList.map(_.trim.replace("：", ":").replace("　", " "))
    val ranks = lines.view
      .filter(l => (l lift 0 contains '|') && (l.size <= 100))
      .map(
        _.replace(".", "・")
          .replace(" ", "")
          .drop(1)
          .takeWhile(_ != '|')
      )
      .toList

    val variant = detectVariant(ranks, handicap, moves) | Standard

    if (ranks.isEmpty)
      handicap
        .filterNot(h => KifUtils.defaultHandicaps.exists(_._2.exists(_ == h)))
        .fold(valid(Situation(variant)): Validated[String, Situation])(parseHandicap(_, variant))
    else if (ranks.size == variant.numberOfRanks)
      for {
        pieces <- parseBoard(ranks, variant)
        board        = Board(pieces)
        senteHandStr = lines.find(l => l.startsWith("先手の持駒:") || l.startsWith("下手の持駒:"))
        goteHandStr  = lines.find(l => l.startsWith("後手の持駒:") || l.startsWith("上手の持駒:"))
        hands <- parseHands(senteHandStr, goteHandStr, variant)
        color = Color.fromSente(!lines.exists(l => l.startsWith("後手番") || l.startsWith("上手番")))
      } yield Situation(board, hands, color, variant)
    else
      invalid(
        s"Cannot parse board setup (wrong number of ranks provided ${ranks.size}/${variant.numberOfRanks})"
      )
  }

  // We need to somehow figure out if it's chushogi
  // looking for something like this - `△6五龍馬 （←4三）`
  // but not strictly - focus on the two kanji representation
  private val chushogiFullKanjiRoles = KifUtils.toKifChushogi map { case (_, v) => v.head } mkString "|"
  private val chushogiKifMoveRegex =
    raw"""^(${KifParser.colorsS})?(${KifParser.positionS})(${chushogiFullKanjiRoles})\s*(${KifParser.parsS})?←?(${KifParser.positionS})(${KifParser.parsS})?""".r.unanchored
  private def detectVariant(
      ranks: List[String],
      handicap: Option[String],
      moves: List[String]
  ): Option[Variant] = {
    if (
      ranks.size == 5 ||
      handicap.exists(h => KifUtils.defaultHandicaps.get(Minishogi).fold(false)(_.exists(_ == h)))
    ) Minishogi.some
    else if (ranks.size == 12 || moves.exists(m => chushogiKifMoveRegex.matches(m)))
      Chushogi.some
    else None
  }

  private def parseBoard(ranks: List[String], variant: Variant): Validated[String, PieceMap] = {
    @scala.annotation.tailrec
    def makePiecesList(
        pieces: List[(Pos, Piece)],
        chars: List[Char],
        pieceSoFar: String,
        x: Int,
        y: Int
    ): Validated[String, List[(Pos, Piece)]] =
      chars match {
        case Nil         => valid(pieces)
        case '・' :: rest => makePiecesList(pieces, rest, "", x - 1, y)
        case (c @ ('v' | 'V' | '成' | '+')) :: rest =>
          makePiecesList(pieces, rest, pieceSoFar + c.toString, x, y)
        case p :: rest =>
          (for {
            pos <- Pos.at(x, y) toValid s"Too many files in board setup on rank $y"
            pieceStr = pieceSoFar + p
            piece <- KifUtils.toPieceBoard(
              pieceStr,
              variant
            ) toValid s"Unknown piece in board setup: $pieceStr"
            _ <- Validated.cond(
              variant.allRoles contains piece.role,
              (),
              s"${piece.role} is not valid in $variant variant"
            )
          } yield pos -> piece :: pieces) match {
            case cats.data.Validated.Valid(ps) => makePiecesList(ps, rest, "", x - 1, y)
            case e                             => e
          }
      }
    ranks.zipWithIndex.foldLeft[Validated[String, List[(Pos, Piece)]]](valid(Nil)) { case (acc, cur) =>
      for {
        pieces     <- acc
        nextPieces <- makePiecesList(Nil, cur._1.toList, "", variant.numberOfFiles - 1, cur._2)
      } yield pieces ::: nextPieces
    } map (_.toMap)
  }

  private def parseHands(
      sente: Option[String],
      gote: Option[String],
      variant: Variant
  ): Validated[String, Hands] = {

    def parseHand(str: String): Validated[String, Hand] = {
      def parseHandPiece(str: String, hand: Hand): Validated[String, Hand] =
        for {
          roleStr <- str.headOption toValid "Cannot parse hand"
          num = KifUtils kanjiToInt str.tail
          rolesBase <- KifUtils
            .anyToRole(roleStr.toString, variant)
            .map(_.toList) toValid s"Unknown piece in hand: $roleStr"
          role <- variant.handRoles.find(
            rolesBase contains _
          ) toValid s"Cannot place ${rolesBase mkString ","} in hand in $variant variant"
        } yield hand.store(role, num)
      val values = (str.split(":").lastOption | "").trim
      if (values == "なし" || values == "") valid(Hand.empty)
      else
        values.split(" ").foldLeft[Validated[String, Hand]](valid(Hand.empty)) { case (acc, cur) =>
          acc andThen (parseHandPiece(cur, _))
        }
    }

    if (sente.isDefined || gote.isDefined)
      for {
        senteHand <- sente.fold[Validated[String, Hand]](valid(Hand.empty))(parseHand _)
        goteHand  <- gote.fold[Validated[String, Hand]](valid(Hand.empty))(parseHand _)
      } yield Hands(senteHand, goteHand)
    else valid(Hands(variant))
  }

  // supporting only handicaps for standard shogi
  private def parseHandicap(str: String, variant: Variant): Validated[String, Situation] =
    for {
      hPosition <- StartingPosition.handicaps.positions.find(
        _.japanese == str
      ) toValid s"Unknown handicap: $str"
      situation <- hPosition.sfen.toSituation(variant) toValid s"Cannot parse handicap: $str"
    } yield situation

  def createResult(termination: Option[Tag], color: Color): Option[Tag] = {
    termination.map(_.value.toLowerCase) match {
      case Some("投了") | Some("反則負け") | Some("切れ負け") | Some("time-up") =>
        Tag(_.Result, color.fold("0-1", "1-0")).some
      case Some("入玉勝ち") | Some("詰み") | Some("反則勝ち") => Tag(_.Result, color.fold("1-0", "0-1")).some
      case Some("持将棋") | Some("千日手") | Some("引き分け") | Some("引分け") => Tag(_.Result, "1/2-1/2").some
      case _                                                      => None
    }
  }

}
