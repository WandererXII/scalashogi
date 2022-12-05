package shogi
package format
package kif

import cats.syntax.option._

import shogi.variant._
import shogi.format.usi.Usi
import shogi.format.forsyth.Sfen

final case class Kif(
    tags: Tags,
    moves: List[NotationMove],
    initial: Initial = Initial.empty
) extends Notation {

  def withMoves(moves: List[NotationMove]) =
    copy(moves = moves)

  def withTags(tags: Tags) =
    copy(tags = tags)

  def renderMovesAndVariations(moveline: List[NotationMove]): String = {
    val mainline = moveline
      .foldLeft[(List[String], Option[Pos])]((Nil, None)) { case ((acc, lastDest), cur) =>
        (
          Kif.renderNotationMove(cur, lastDest, tags.variant | Standard) :: acc,
          cur.usiWithRole.usi.positions.lastOption
        )
      }
      ._1
      .reverse mkString "\n"

    val variations = moveline.reverse.foldLeft("")((acc, cur) => {
      acc + cur.variations.map(v => s"\n\n変化：${cur.moveNumber}手\n${renderMovesAndVariations(v)}").mkString("")
    })

    s"$mainline$variations"
  }

  def render: String = {
    val initStr =
      if (initial.comments.nonEmpty) initial.comments.map(Kif.fixComment _).mkString("* ", "\n* ", "\n")
      else ""
    val header      = Kif renderHeader tags
    val movesHeader = "\n手数----指手---------消費時間--\n"
    val movesStr    = renderMovesAndVariations(moves)
    s"$header$movesHeader$initStr$movesStr"
  }.trim

  override def toString = render
}

object Kif {

  def renderNotationMove(cur: NotationMove, lastDest: Option[Pos], variant: Variant): String = {
    val resultStr   = cur.result.fold("")(r => s"\n${moveNumberOffset(cur.moveNumber + 1)}$offset$r")
    val timeStr     = clockString(cur) | ""
    val glyphsNames = cur.glyphs.toList.map(_.name)
    val commentsStr = (glyphsNames ::: cur.comments).map { text => s"\n* ${fixComment(text)}" }.mkString("")
    cur.usiWithRole.usi match {
      case Usi.Move(orig, dest, prom, Some(midStep)) => {
        val m1 = Usi.WithRole(Usi.Move(orig, midStep, false, None), cur.usiWithRole.role)
        val m2 = Usi.WithRole(Usi.Move(midStep, dest, prom, None), cur.usiWithRole.role)
        val s1 = s"${s"${moveNumberOffset(cur.moveNumber)}一歩目"} ${renderMove(m1, lastDest, variant)}"
        val s2 =
          s"${s"${moveNumberOffset(cur.moveNumber)}二歩目"} ${renderMove(m2, None, variant)}$timeStr$commentsStr$resultStr"
        List(s1, s2) mkString "\n"
      }
      case _ =>
        s"${moveNumberOffset(cur.moveNumber)}$offset${renderMove(cur.usiWithRole, lastDest, variant)}$timeStr$commentsStr$resultStr"
    }
  }

  private def renderDest(dest: Pos, lastDest: Option[Pos], variant: Variant): String = {
    val useLastDest = lastDest.fold(false)(_ == dest)
    variant match {
      case Chushogi => if (useLastDest) "仝" else dest.kanjiKey
      case _        => if (useLastDest) "同　" else dest.kanjiFullWidthKey
    }
  }

  private def renderOrig(orig: Pos, variant: Variant): String =
    variant match {
      case Chushogi => s" （←${orig.kanjiKey}）"
      case _        => s"(${orig.hexKey})"
    }

  def renderMove(usiWithRole: Usi.WithRole, lastDest: Option[Pos], variant: Variant): String =
    usiWithRole.usi match {
      case Usi.Drop(role, pos) =>
        val roleStr = KifUtils.toKif(role, variant).map(_.head) | ""
        s"${pos.kanjiFullWidthKey}${roleStr}打"
      case Usi.Move(orig, dest, prom, _) => {
        val promStr = if (prom) "成" else ""
        val roleStr = KifUtils.toKif(usiWithRole.role, variant).map(_.head) | ""
        s"${renderDest(dest, lastDest, variant)}$roleStr$promStr${renderOrig(orig, variant)}"
      }
    }

  def renderHeader(tags: Tags): String =
    headerTags
      .map { tag =>
        // we need these even empty
        if (tag == Tag.Sente || tag == Tag.Gote) {
          val playerName = tags(tag.name) | ""
          val playerTag = {
            if (!tags.sfen.exists(StartingPosition isHandicap _)) tag.kifName
            else if (tag == Tag.Sente) "下手"
            else "上手"
          }
          s"$playerTag：${if (playerName == "?") "" else playerName}"
        } else if (tag == Tag.Handicap) {
          renderSetup(tags.sfen, tags.variant | Standard)
        } else {
          tags(tag.name).fold("")(tagValue => {
            if (tagValue != "?" && tagValue != "") s"${tag.kifName}：$tagValue"
            else ""
          })
        }
      }
      .filter(_.nonEmpty)
      .mkString("\n")

  def renderSetup(sfen: Option[Sfen], variant: Variant): String =
    sfen
      .filterNot(f => variant.initialSfen.truncate == f.truncate)
      .fold {
        val handicapName = KifUtils.defaultHandicaps.get(variant).map(_.head) | ""
        s"${Tag.Handicap.kifName}：$handicapName"
      } { sf =>
        getHandicapName(sf).fold(sf.toSituation(variant).fold("")(renderSituation _))(hc =>
          s"${Tag.Handicap.kifName}：$hc"
        )
      }

  def renderSituation(sit: Situation): String = {
    val kifBoard = new scala.collection.mutable.StringBuilder(256)
    val nbRanks  = sit.variant.numberOfRanks - 1
    val nbFiles  = sit.variant.numberOfFiles - 1
    val space    = if (sit.variant.chushogi) 3 else 2
    val padder   = if (sit.variant.chushogi) "  ・" else " ・"
    for (y <- 0 to nbRanks) {
      kifBoard append "|"
      for (x <- nbFiles to 0 by -1) {
        sit.board(x, y).flatMap(p => KifUtils.toKifBoard(p, sit.variant)) match {
          case None => kifBoard append padder
          case Some(kif) =>
            kifBoard append String
              .format("%1$" + space + "s", kif)
              .mkString("")
        }
      }
      kifBoard append s"|${KifUtils.intToKanji(y + 1)}"
      if (y < nbRanks) kifBoard append '\n'
    }
    List(
      if (sit.variant.supportsDrops) s"後手の持駒：${renderHand(sit.hands(Gote), sit.variant)}" else "",
      fileNums(sit.variant),
      s"+${"-" * ((nbFiles + 1) * (space + 1))}+",
      kifBoard.toString,
      s"+${"-" * ((nbFiles + 1) * (space + 1))}+",
      if (sit.variant.supportsDrops) s"先手の持駒：${renderHand(sit.hands(Sente), sit.variant)}" else "",
      if (sit.color.gote) "後手番" else ""
    ).filter(_.nonEmpty).mkString("\n")
  }

  private def fileNums(variant: Variant): String =
    variant match {
      case Standard  => "  ９ ８ ７ ６ ５ ４ ３ ２ １"
      case Minishogi => "  ５ ４ ３ ２ １"
      case Chushogi  => " １２ １１ １０ ９  ８  ７  ６  ５  ４  ３  ２  １"
    }

  private def renderHand(hand: Hand, variant: Variant): String = {
    if (hand.isEmpty) "なし"
    else
      Standard.handRoles
        .map { r =>
          val cnt = hand(r)
          val kif = KifUtils.toKif(r, variant).map(_.head) | ""
          if (cnt == 1) kif
          else if (cnt > 1) kif + KifUtils.intToKanji(cnt)
          else ""
        }
        .filter(_.nonEmpty)
        .mkString("　")
  }

  def createTerminationMove(status: Status, winnerTurn: Boolean): Option[String] = {
    import Status._
    status match {
      case Aborted | NoStart     => "中断".some
      case Timeout | Outoftime   => "切れ負け".some
      case Resign if !winnerTurn => "投了".some
      case PerpetualCheck        => "反則勝ち".some
      case Mate if winnerTurn    => "反則勝ち".some // pawn checkmate
      case Mate | Stalemate      => "詰み".some
      case Draw                  => "千日手".some
      case Impasse27             => "入玉勝ち".some
      case _                     => None
    }
  }

  // tags we render in header
  private val headerTags = Tag.tsumeTypes ++ List(
    Tag.Start,
    Tag.End,
    Tag.Event,
    Tag.Site,
    Tag.TimeControl,
    Tag.Handicap,
    Tag.Sente,
    Tag.SenteTeam,
    Tag.Gote,
    Tag.GoteTeam,
    Tag.Opening
  )

  private def getHandicapName(sfen: Sfen): Option[String] =
    StartingPosition.handicaps.positions.find(_.sfen.truncate == sfen.truncate).map(t => t.japanese)

  private def clockString(cur: NotationMove): Option[String] =
    cur.secondsSpent.map(spent =>
      s"${offset}(${formatKifSpent(spent)}/${cur.secondsTotal.fold("")(total => formatKifTotal(total))})"
    )

  private val offset = "   "

  private val noDoubleLineBreakRegex = "(\r?\n){2,}".r

  private def moveNumberOffset(moveNumber: Int) =
    f"$moveNumber%4d"

  private def fixComment(txt: String) =
    noDoubleLineBreakRegex.replaceAllIn(txt, "\n").replace("\n", "\n* ")

  private def formatKifSpent(t: Int) =
    ms.print(
      org.joda.time.Duration.standardSeconds(t).toPeriod
    )

  private def formatKifTotal(t: Int) =
    hms.print(
      org.joda.time.Duration.standardSeconds(t).toPeriod
    )

  private[this] val ms = new org.joda.time.format.PeriodFormatterBuilder().printZeroAlways
    .minimumPrintedDigits(2)
    .appendMinutes
    .appendSeparator(":")
    .minimumPrintedDigits(2)
    .appendSeconds
    .toFormatter

  private[this] val hms = new org.joda.time.format.PeriodFormatterBuilder().printZeroAlways
    .minimumPrintedDigits(2)
    .appendHours
    .appendSeparator(":")
    .minimumPrintedDigits(2)
    .appendMinutes
    .appendSeparator(":")
    .minimumPrintedDigits(2)
    .appendSeconds
    .toFormatter
}
