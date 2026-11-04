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

  // Complete and Incomplete expose the last valid position, including the setup if no move was accepted.
  // InvalidSfen has no position to expose, as no setup could be read.
  sealed trait Result {
    def valid: Validated[String, Position]
  }

  object Result {
    final case class Complete(position: Position) extends Result {
      def valid = Validated.valid(position)
    }
    final case class Incomplete(position: Position, failure: String) extends Result {
      def valid = Validated.invalid(failure)
    }
    final case class InvalidSfen(failure: String) extends Result {
      def valid = Validated.invalid(failure)
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
  // An unparsable SFEN yields no snapshots, only the error.
  def positionsWhileValid(
      usis: Seq[Usi],
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant,
  ): (List[Position], Option[String]) = {

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

    setup(initialSfen, variant) match {
      case Valid(position) =>
        mk(NonEmptyList.one(position), usis.toList) match {
          case (positions, err) => (positions.reverse.toList, err)
        }
      case Invalid(err) => (Nil, err.some)
    }
  }

  // Use for trusted usis
  // doesn't verify whether the usis are valid
  // empty if the SFEN is unparsable, there is no board to read roles from
  def usiWithRoleWhilePossible(
      usis: Seq[Usi],
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant,
  ): List[Usi.WithRole] = {

    @scala.annotation.tailrec
    def mk(
        roleMap: Map[Square, Role],
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
                  val toRemove      = List[Option[Square]](Some(orig), midStep).flatten
                  mk(
                    roleMap -- toRemove + (dest -> maybePromoted),
                    rest,
                    Usi.WithRole(usi, role) :: roles,
                  )
                }
                case None => roles
              }
            case Usi.Drop(role, square) =>
              mk(roleMap + (square -> role), rest, Usi.WithRole(usi, role) :: roles)
          }
      }

    setup(initialSfen, variant).fold(
      _ => Nil,
      init => mk(init.board.pieces.map { case (k, v) => k -> v.role }, usis.toList, Nil).reverse,
    )
  }

  private def setup(
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant,
  ): Validated[String, Position] =
    initialSfen.fold(Validated.valid[String, Position](Position(variant))) { sfen =>
      Position.fromInitialSfen(sfen, variant) toValid s"Invalid SFEN for ${variant.name}: $sfen"
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

    setup(initialSfen, variant) match {
      case Valid(position) => loop(position, steps.toList)
      case Invalid(err)    => Result.InvalidSfen(err)
    }
  }

}
