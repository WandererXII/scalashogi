package shogi
package format
package kif

import cats.data.Validated
import cats.data.Validated.invalid
import cats.data.Validated.valid
import cats.implicits._

import shogi.variant._

object KifParserHelper {

  def parsePosition(
      str: String,
      handicapString: Option[String],
      steps: List[String],
  ): Validated[String, Position] = {
    val lines =
      augmentString(str).linesIterator.map(_.trim.replace("：", ":").replace("　", " ")).toList
    val ranks = lines
      .withFilter(l => (l lift 0 contains '|') && (l.sizeIs <= 100))
      .map(
        _.replace(".", "・")
          .replace(" ", "")
          .drop(1)
          .takeWhile(_ != '|'),
      )

    val variant = detectVariant(ranks, handicapString, steps) | Standard

    if (ranks.isEmpty)
      handicapString
        .filterNot(isDefaultHandicapOf(_, variant))
        .fold(valid(Position(variant)): Validated[String, Position])(parseHandicap(_, variant))
    else if (ranks.sizeIs == variant.numberOfRanks)
      for {
        pieces <- parseBoard(ranks, variant)
        board        = Board(pieces)
        senteHandStr = lines.find(l => l.startsWith("先手の持駒:") || l.startsWith("下手の持駒:"))
        goteHandStr  = lines.find(l => l.startsWith("後手の持駒:") || l.startsWith("上手の持駒:"))
        hands <- parseHands(senteHandStr, goteHandStr, variant)
        color = Color.fromSente(!lines.exists(l => l.startsWith("後手番") || l.startsWith("上手番")))
      } yield Position(board, hands, color, variant)
    else
      invalid(
        s"Cannot parse board setup (wrong number of ranks provided ${ranks.size}/${variant.numberOfRanks})",
      )
  }

  private val stepsPlayedRegex = raw"""^${Kif.stepsPlayedHeader}(\d+)""".r.unanchored

  def parseStartStep(str: String): Option[Int] =
    augmentString(str).linesIterator
      .collectFirst { case stepsPlayedRegex(steps) => steps }
      .flatMap(_.toIntOption)
      .map(steps => (steps min 9998) + 1)

  // We need to somehow figure out if it's chushogi
  // looking for something like this - `△6五龍馬 （←4三）`
  // shared Standard piece names only identify chushogi when an origin arrow is present.
  private val chushogiFullKanjiRoles = KifUtils.toKifChushogi map { case (_, v) =>
    v.head
  } mkString "|"
  private val chushogiKifMoveRegex =
    raw"""^(${KifParser.colorsS})?(${KifParser.squareS})(${chushogiFullKanjiRoles})\s*(${KifParser.parsS})?(?<arrow>←)?(${KifParser.squareS})(${KifParser.parsS})?""".r.unanchored
  private def isDefaultHandicapOf(handicap: String, variant: Variant): Boolean =
    KifUtils.defaultHandicaps.get(variant).fold(false)(_.exists(_ == handicap.toLowerCase))
  private def detectVariant(
      ranks: List[String],
      handicapString: Option[String],
      steps: List[String],
  ): Option[Variant] = {
    if (handicapString.exists(isDefaultHandicapOf(_, Kyotoshogi))) Kyotoshogi.some
    else if (
      ranks.sizeIs == 5 ||
      handicapString.exists(isDefaultHandicapOf(_, Minishogi))
    ) Minishogi.some
    else if (
      ranks.sizeIs == 12 ||
      handicapString
        .exists(h => isDefaultHandicapOf(h, Chushogi) && !isDefaultHandicapOf(h, Standard)) ||
      (ranks.isEmpty && steps.exists(s =>
        chushogiKifMoveRegex
          .findFirstMatchIn(s)
          .exists(m => m.group("arrow") != null || KifUtils.toRole(m.group(3), Standard).isEmpty),
      ))
    )
      Chushogi.some
    else if (handicapString.exists(isDefaultHandicapOf(_, Annanshogi)))
      Annanshogi.some
    else if (handicapString.exists(isDefaultHandicapOf(_, Checkshogi)))
      Checkshogi.some
    else if (handicapString.exists(isDefaultHandicapOf(_, Dobutsu)))
      Dobutsu.some
    else None
  }

  private def parseBoard(ranks: List[String], variant: Variant): Validated[String, PieceMap] = {
    @scala.annotation.tailrec
    def makePiecesList(
        pieces: List[(Square, Piece)],
        chars: List[Char],
        pieceSoFar: String,
        x: Int,
        y: Int,
    ): Validated[String, List[(Square, Piece)]] =
      chars match {
        case Nil                                   => valid(pieces)
        case '・' :: rest                           => makePiecesList(pieces, rest, "", x - 1, y)
        case (c @ ('v' | 'V' | '成' | '+')) :: rest =>
          makePiecesList(pieces, rest, pieceSoFar + c.toString, x, y)
        case p :: rest =>
          (for {
            square <- Square.at(x, y) toValid s"Too many files in board setup on rank $y"
            pieceStr = pieceSoFar + p
            piece <- KifUtils.toPieceBoard(
              pieceStr,
              variant,
            ) toValid s"Unknown piece in board setup: $pieceStr"
            _ <- Validated.cond(
              variant.allRoles contains piece.role,
              (),
              s"${piece.role} is not valid in $variant variant",
            )
          } yield square -> piece :: pieces) match {
            case cats.data.Validated.Valid(ps) => makePiecesList(ps, rest, "", x - 1, y)
            case e                             => e
          }
      }
    ranks.zipWithIndex.foldLeft[Validated[String, List[(Square, Piece)]]](valid(Nil)) {
      case (acc, cur) =>
        for {
          pieces     <- acc
          nextPieces <- makePiecesList(Nil, cur._1.toList, "", variant.numberOfFiles - 1, cur._2)
        } yield pieces ::: nextPieces
    } map (_.toMap)
  }

  private def parseHands(
      sente: Option[String],
      gote: Option[String],
      variant: Variant,
  ): Validated[String, Hands] = {

    def parseHand(str: String): Validated[String, Hand] = {
      def parseHandPiece(str: String, hand: Hand): Validated[String, Hand] =
        for {
          roleStr <- (if (variant == Dobutsu)
                        str.inits.find(prefix => KifUtils.anyToRole(prefix, variant).isDefined)
                      else str.headOption.map(_.toString)) toValid s"Unknown piece in hand: $str"
          num = KifUtils kanjiToInt str.drop(roleStr.length)
          rolesBase <- KifUtils
            .anyToRole(roleStr, variant)
            .map(_.toList) toValid s"Unknown piece in hand: $roleStr"
          role <- variant.handRoles.find(
            rolesBase contains _,
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

  private def parseHandicap(str: String, variant: Variant): Validated[String, Position] =
    for {
      handicap <- Handicap.allByVariant
        .get(variant)
        .flatMap(
          _.find(
            _.japanese == str,
          ),
        ) toValid s"Unknown handicap: $str"
      position <- handicap.sfen.toPosition(variant) toValid s"Cannot parse handicap: $str"
    } yield position

  def createResult(termination: Option[Tag], color: Color, handicap: Boolean): Option[Tag] = {
    termination.map(_.value.toLowerCase) match {
      case Some("投了") | Some("詰み") | Some("反則負け") | Some("切れ負け") | Some("time-up") |
          Some("トライ勝ち") | Some("王手勝ち") =>
        Tag(_.Result, Color.showResult(Some(!color))).some
      case Some("入玉勝ち") | Some("反則勝ち") | Some("トライ負け") =>
        Tag(_.Result, Color.showResult(Some(color))).some
      case Some("持将棋") | Some("千日手") | Some("引き分け") | Some("引分け") =>
        Tag(_.Result, Color.showResult(None)).some
      // Forfeits are from sente's view, or uwate's in handicap games
      case Some("不戦勝") => Tag(_.Result, Color.showResult(Some(Color.fromSente(!handicap)))).some
      case Some("不戦敗") => Tag(_.Result, Color.showResult(Some(Color.fromSente(handicap)))).some
      case _ => None
    }
  }

}
