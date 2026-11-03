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
    def position: Position
    def valid: Validated[String, Position]
  }

  object Result {
    final case class Complete(position: Position) extends Result {
      def valid = Validated.valid(position)
    }
    final case class Incomplete(position: Position, failures: String) extends Result {
      def valid = Validated.invalid(failures)
    }
  }

  def fromParsedNotation(parsed: ParsedNotation, op: ParsedSteps => ParsedSteps): Result =
    playSteps(
      parsed.initialSfen,
      parsed.variant,
      op(parsed.parsedSteps).value,
    ) { (position, step) =>
      position(step)
    }

  def fromUsi(
      usis: Seq[Usi],
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant,
  ): Result =
    playSteps(initialSfen, variant, usis) { (position, usi) =>
      position(usi)
    }

  // Chronological snapshots, setup first; retain the accepted prefix and first error.
  def positionsWhileValid(
      usis: Seq[Usi],
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant,
  ): (NonEmptyList[Position], Option[String]) = {

    @scala.annotation.tailrec
    def mk(
        positions: NonEmptyList[Position],
        usis: List[Usi],
    ): (NonEmptyList[Position], Option[String]) =
      usis match {
        case Nil         => (positions, None)
        case usi :: rest =>
          positions.head(usi) match {
            case Valid(newPosition) => mk(newPosition :: positions, rest)
            case Invalid(err)       => (positions, err.some)
          }
      }

    mk(NonEmptyList.one(Position(initialSfen, variant)), usis.toList) match {
      case (positions, err) => (positions.reverse, err)
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

    val init = Position(initialSfen, variant)
    mk(init.board.pieces.map { case (k, v) => k -> v.role }, usis.toList, Nil).reverse
  }

  private def playSteps[A](
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant,
      steps: Seq[A],
  )(
      step: (Position, A) => Validated[String, Position],
  ): Result = {
    @scala.annotation.tailrec
    def loop(position: Position, remaining: List[A]): Result =
      remaining match {
        case Nil           => Result.Complete(position)
        case input :: rest =>
          step(position, input) match {
            case Invalid(err) => Result.Incomplete(position, err)
            case Valid(next)  => loop(next, rest)
          }
      }

    loop(Position(initialSfen, variant), steps.toList)
  }

}
