package shogi

import cats.data.NonEmptyList
import cats.data.Validated
import cats.data.Validated.Invalid
import cats.data.Validated.Valid
import cats.data.Validated.valid
import cats.implicits._

import shogi.format.ParsedNotation
import shogi.format.ParsedSteps
import shogi.format.forsyth.Sfen
import shogi.format.usi.Usi

object Replay {

  // Both outcomes expose the last valid game, including the setup if no move was accepted.
  sealed trait Result {
    def state: State
    def valid: Validated[String, State]
  }

  object Result {
    final case class Complete(state: State) extends Result {
      def valid = Validated.valid(state)
    }
    final case class Incomplete(state: State, failures: String) extends Result {
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

  def statesWhileValid(
      usis: Seq[Usi],
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant,
  ): (NonEmptyList[State], Option[String]) = {

    @scala.annotation.tailrec
    def mk(states: NonEmptyList[State], usis: List[Usi]): (NonEmptyList[State], Option[String]) =
      usis match {
        case Nil         => (states, None)
        case usi :: rest =>
          states.head(usi) match {
            case Valid(newState) => mk(newState :: states, rest)
            case Invalid(err)    => (states, err.some)
          }
      }

    mk(NonEmptyList.one(State(initialSfen, variant)), usis.toList) match {
      case (states, err) => (states.reverse, err)
    }
  }

  def situations(
      usis: Seq[Usi],
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant,
  ): Validated[String, NonEmptyList[Situation]] = {
    val init = initialSfenToSituation(initialSfen, variant)
    usis.foldLeft[Validated[String, NonEmptyList[Situation]]](valid(NonEmptyList.one(init))) {
      case (acc, usi) =>
        acc andThen { sits =>
          sits.head(usi) andThen { sit =>
            valid(sit :: sits)
          }
        }
    } map (_.reverse)
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

    val init = initialSfenToSituation(initialSfen, variant)
    mk(init.board.pieces.map { case (k, v) => k -> v.role }, usis.toList, Nil).reverse
  }

  private def initialSfenToSituation(
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant,
  ): Situation =
    initialSfen.flatMap(_.toSituation(variant)) | Situation(variant)

  private def playSteps[A](
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant,
      steps: Seq[A],
  )(
      step: (Situation, A) => Validated[String, Situation],
  ): Result = {
    val setup = initialSfen.filterNot(_.initialOf(variant)).flatMap(_.toSituationPlus(variant))
    val initialSituation = setup.fold(Situation(variant))(_.situation)
    val startedAtPly     = setup.fold(0)(_.plies)
    val startedAtStep    = setup.fold(1)(_.stepNumber)

    def finish(
        situation: Situation,
        usis: Vector[Usi],
        error: Option[String],
    ) = {
      val state = State(
        situation = situation,
        usis = usis,
        plies = startedAtPly + usis.size,
        startedAtPly = startedAtPly,
        startedAtStep = startedAtStep,
      )
      error.fold[Result](Result.Complete(state))(Result.Incomplete(state, _))
    }

    @scala.annotation.tailrec
    def loop(
        situation: Situation,
        usis: Vector[Usi],
        remaining: List[A],
    ): Result =
      remaining match {
        case Nil           => finish(situation, usis, None)
        case input :: rest =>
          step(situation, input) match {
            case Invalid(err) => finish(situation, usis, Some(err))
            case Valid(next)  =>
              loop(
                next,
                next.history.lastUsi.fold(usis)(usis :+ _),
                rest,
              )
          }
      }

    loop(
      initialSituation,
      Vector.empty,
      steps.toList,
    )
  }

}
