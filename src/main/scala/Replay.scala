package shogi

import cats.data.NonEmptyList
import cats.data.Validated
import cats.data.Validated.Invalid
import cats.data.Validated.Valid
import cats.implicits._

import shogi.format.ParsedNotation
import shogi.format.ParsedSteps
import shogi.format.forsyth.Sfen
import shogi.format.usi.Usi

object Replay {

  // Both outcomes expose the last valid game, including the setup if no move was accepted.
  sealed trait Result {
    def state: Situation
    def valid: Validated[String, Situation]
  }

  object Result {
    final case class Complete(state: Situation) extends Result {
      def valid = Validated.valid(state)
    }
    final case class Incomplete(state: Situation, failures: String) extends Result {
      def valid = Validated.invalid(failures)
    }
  }

  def fromParsedNotation(parsed: ParsedNotation, op: ParsedSteps => ParsedSteps): Result =
    playSteps(
      parsed.initialSfen,
      parsed.variant,
      op(parsed.parsedSteps).value,
    ) { (situation, step) =>
      situation(step)
    }

  def fromUsi(
      usis: Seq[Usi],
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant,
  ): Result =
    playSteps(initialSfen, variant, usis) { (situation, usi) =>
      situation(usi)
    }

  // Chronological snapshots, setup first; retain the accepted prefix and first error.
  def statesWhileValid(
      usis: Seq[Usi],
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant,
  ): (NonEmptyList[Situation], Option[String]) = {

    @scala.annotation.tailrec
    def mk(
        states: NonEmptyList[Situation],
        usis: List[Usi],
    ): (NonEmptyList[Situation], Option[String]) =
      usis match {
        case Nil         => (states, None)
        case usi :: rest =>
          states.head(usi) match {
            case Valid(newSituation) => mk(newSituation :: states, rest)
            case Invalid(err)        => (states, err.some)
          }
      }

    mk(NonEmptyList.one(Situation(initialSfen, variant)), usis.toList) match {
      case (states, err) => (states.reverse, err)
    }
  }

  // Use for trusted usis
  // doesn't verify whether the usis are valid
  def usiWithRoleWhilePossible(
      usis: Seq[Usi],
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant,
  ): List[Usi.WithRole] = {

    @scala.annotation.tailrec
    def mk(
        roleMap: Map[Pos, Role],
        usis: List[Usi],
        roles: List[Usi.WithRole],
    ): List[Usi.WithRole] =
      usis match {
        case Nil         => roles
        case usi :: rest =>
          usi match {
            case Usi.Move(orig, dest, prom, midStep) =>
              roleMap.get(orig) match {
                case Some(role) => {
                  val maybePromoted = variant.promote(role).filter(_ => prom) | role
                  val toRemove      = List[Option[Pos]](Some(orig), midStep).flatten
                  mk(
                    roleMap -- toRemove + (dest -> maybePromoted),
                    rest,
                    Usi.WithRole(usi, role) :: roles,
                  )
                }
                case None => roles
              }
            case Usi.Drop(role, pos) =>
              mk(roleMap + (pos -> role), rest, Usi.WithRole(usi, role) :: roles)
          }
      }

    val init = Situation(initialSfen, variant)
    mk(init.board.pieces.map { case (k, v) => k -> v.role }, usis.toList, Nil).reverse
  }

  private def playSteps[A](
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant,
      steps: Seq[A],
  )(
      step: (Situation, A) => Validated[String, Situation],
  ): Result = {
    @scala.annotation.tailrec
    def loop(situation: Situation, remaining: List[A]): Result =
      remaining match {
        case Nil           => Result.Complete(situation)
        case input :: rest =>
          step(situation, input) match {
            case Invalid(err) => Result.Incomplete(situation, err)
            case Valid(next)  => loop(next, rest)
          }
      }

    loop(Situation(initialSfen, variant), steps.toList)
  }

}
