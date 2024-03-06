package shogi
package format
package csa

import cats.syntax.option._

import shogi.variant.Standard
import shogi.format.usi.Usi
import shogi.format.forsyth.Sfen

final case class Csa(
    steps: List[NotationStep],
    initialSfen: Option[Sfen],
    initial: Initial = Initial.empty,
    tags: Tags = Tags.empty
) extends Notation {

  def variant = Standard

  def withSteps(steps: List[NotationStep]) =
    copy(steps = steps)

  def withTags(tags: Tags) =
    copy(tags = tags)

  private def renderMainline(steps: List[NotationStep], turn: Color): String =
    steps
      .foldLeft[(List[String], Color)]((Nil, turn)) { case ((acc, curTurn), cur) =>
        (Csa.renderNotationStep(cur, curTurn.some) :: acc, !curTurn)
      }
      ._1
      .reverse mkString "\n"

  def render: String = {
    val initStr =
      if (initial.comments.nonEmpty)
        initial.comments.map(Csa.fixComment _).mkString("")
      else ""
    val header               = Csa renderHeader tags
    val initialSfenOrDefault = initialSfen | variant.initialSfen
    val setup =
      initialSfenOrDefault.toSituation(variant).fold("")(Csa renderSituation _)
    val startColor: Color = initialSfenOrDefault.color | Sente
    val stepsStr          = renderMainline(steps, startColor)
    List[String](
      header,
      setup,
      initStr,
      stepsStr
    ).filter(_.nonEmpty).mkString("\n")
  }.trim

  override def toString = render
}

object Csa {

  def renderNotationStep(cur: NotationStep, turn: Option[Color]) = {
    val csaStep     = renderCsaStep(cur.usiWithRole, turn)
    val timeStr     = clockString(cur) | ""
    val commentsStr = cur.comments.map { text => s"\n'${fixComment(text)}" }.mkString("")
    val resultStr   = cur.result.fold("")(t => s"\n$t")
    s"$csaStep$timeStr$commentsStr$resultStr"
  }

  def renderCsaStep(usiWithRole: Usi.WithRole, turn: Option[Color]) =
    usiWithRole.usi match {
      case Usi.Drop(role, pos) =>
        s"${turn.fold("")(_.fold("+", "-"))}00${CsaUtils.makeCsaPos(pos)}${CsaUtils.toCsa(role) | ""}"
      case Usi.Move(orig, dest, prom, _) => {
        val finalRole = Standard.promote(usiWithRole.role).filter(_ => prom) | usiWithRole.role
        s"${turn.fold("")(_.fold("+", "-"))}${CsaUtils.makeCsaPos(orig)}${CsaUtils
            .makeCsaPos(dest)}${CsaUtils.toCsa(finalRole) | ""}"
      }
    }

  def renderHeader(tags: Tags): String =
    csaHeaderTags
      .map { ct =>
        if (ct == Tag.Sente || ct == Tag.Gote) {
          tags(ct.name).fold("")(tagValue =>
            if (isValidTagValue(tagValue))
              s"N${ct.csaName}${tagValue.replace(",", ";")}"
            else
              ""
          )
        } else {
          tags(ct.name).fold("")(tagValue => {
            if (isValidTagValue(tagValue)) s"$$${ct.csaName}:${tagValue.replace(",", ";")}"
            else ""
          })
        }
      }
      .filter(_.nonEmpty)
      .mkString("\n")

  // we want only ascii tags
  private def isValidTagValue(str: String): Boolean =
    str.nonEmpty && str != "?" && str.forall(c => c >= 32 && c < 127)

  def renderSituation(sit: Situation): String = {
    val csaBoard = new scala.collection.mutable.StringBuilder(256)
    for (y <- 0 to 8) {
      csaBoard append ("P" + (y + 1))
      for (x <- 8 to 0 by -1) {
        sit.board(x, y).flatMap(CsaUtils toCsa _) match {
          case None => csaBoard append " * "
          case Some(csa) =>
            csaBoard append s"$csa"
        }
      }
      if (y < 8) csaBoard append '\n'
    }
    List[String](
      csaBoard.toString,
      renderHand(sit.hands(Sente), "P+"),
      renderHand(sit.hands(Gote), "P-"),
      if (sit.color.gote) "-" else "+"
    ).filter(_.nonEmpty).mkString("\n")
  }

  private def renderHand(hand: Hand, prefix: String): String = {
    if (hand.isEmpty) ""
    else
      Standard.handRoles
        .map { r =>
          val cnt = hand(r)
          s"00${CsaUtils.toCsa(r) | ""}".repeat(math.min(cnt, 81))
        }
        .filter(_.nonEmpty)
        .mkString(prefix, "", "")
  }

  def createTerminationStep(
      status: Status,
      winnerTurn: Boolean,
      winnerColor: Option[Color]
  ): Option[String] = {
    import Status._
    status match {
      case Aborted | NoStart                                 => "%CHUDAN".some
      case Timeout | Outoftime                               => "%TIME_UP".some
      case Resign if !winnerTurn                             => "%TORYO".some
      case PerpetualCheck if winnerColor.contains(Sente)     => "%-ILLEGAL_ACTION".some
      case PerpetualCheck                                    => "%+ILLEGAL_ACTION".some
      case Mate if winnerTurn && winnerColor.contains(Sente) => "%-ILLEGAL_ACTION".some // pawn checkmate
      case Mate if winnerTurn                                => "%+ILLEGAL_ACTION".some // pawn checkmate
      case Mate | Stalemate                                  => "%TSUMI".some
      case Repetition                                        => "%SENNICHITE".some
      case Impasse27                                         => "%KACHI".some
      case _                                                 => None
    }
  }

  // tags we render in header
  private val csaHeaderTags = List[TagType](
    Tag.Sente,
    Tag.Gote,
    Tag.Event,
    Tag.Site,
    Tag.Start,
    Tag.End,
    Tag.TimeControl,
    Tag.SenteTeam,
    Tag.GoteTeam,
    Tag.Opening
  )

  private def clockString(cur: NotationStep): Option[String] =
    cur.secondsSpent.map(spent => s",T$spent")

  private val noDoubleLineBreakRegex = "(\r?\n){2,}".r

  private def fixComment(txt: String) =
    noDoubleLineBreakRegex.replaceAllIn(txt, "\n").replace("\n", "\n'")

}
