package shogi

import cats.data.NonEmptyList
import cats.data.Validated
import cats.data.Validated.Invalid
import cats.data.Validated.Valid
import cats.data.Validated.valid
import cats.implicits._

import shogi.format.ParsedNotation
import shogi.format.ParsedSteps
import shogi.format.Tags
import shogi.format.forsyth.Sfen
import shogi.format.usi.Usi

object Replay {

  // Both outcomes expose the last valid game, including the setup if no move was accepted.
  sealed trait Result {
    def game: Game
    def valid: Validated[String, Game]
  }

  object Result {
    final case class Complete(game: Game) extends Result {
      def valid = Validated.valid(game)
    }
    final case class Incomplete(game: Game, failures: String) extends Result {
      def valid = Validated.invalid(failures)
    }
  }

  def fromParsedNotation(parsed: ParsedNotation, op: ParsedSteps => ParsedSteps): Result =
    playSteps(
      parsed.initialSfen,
      parsed.variant,
      parsed.tags,
      op(parsed.parsedSteps).value,
    ) { (situation, step) =>
      situation(step).map(_ -> step.metas.timeSpent)
    }

  def fromUsi(
      usis: Seq[Usi],
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant,
      tags: Tags = Tags.empty,
  ): Result =
    playSteps(initialSfen, variant, tags, usis) { (situation, usi) =>
      situation(usi).map(_ -> None)
    }

  def gamesWhileValid(
      usis: Seq[Usi],
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant,
  ): (NonEmptyList[Game], Option[String]) = {

    @scala.annotation.tailrec
    def mk(games: NonEmptyList[Game], usis: List[Usi]): (NonEmptyList[Game], Option[String]) =
      usis match {
        case Nil         => (games, None)
        case usi :: rest =>
          games.head(usi) match {
            case Valid(newGame) => mk(newGame :: games, rest)
            case Invalid(err)   => (games, err.some)
          }
      }

    mk(NonEmptyList.one(Game(initialSfen, variant)), usis.toList) match {
      case (games, err) => (games.reverse, err)
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
      tags: Tags,
      steps: Seq[A],
  )(
      step: (Situation, A) => Validated[String, (Situation, Option[Centis])],
  ): Result = {
    val setup = initialSfen.filterNot(_.initialOf(variant)).flatMap(_.toSituationPlus(variant))
    val initialSituation = setup.fold(Situation(variant))(_.situation)
    val startedAtPly     = setup.fold(0)(_.plies)
    val startedAtStep    = setup.fold(1)(_.stepNumber)
    val clockConfig      = tags.clockConfig

    def finish(
        situation: Situation,
        players: Option[Color.Map[ClockPlayer]],
        usis: Vector[Usi],
        error: Option[String],
    ) = {
      val game = Game(
        situation = situation,
        usis = usis,
        clock = clockConfig.zip(players).map { case (config, ps) =>
          Clock(config = config, color = situation.color, players = ps)
        },
        plies = startedAtPly + usis.size,
        startedAtPly = startedAtPly,
        startedAtStep = startedAtStep,
      )
      error.fold[Result](Result.Complete(game))(Result.Incomplete(game, _))
    }

    @scala.annotation.tailrec
    def loop(
        situation: Situation,
        players: Option[Color.Map[ClockPlayer]],
        usis: Vector[Usi],
        remaining: List[A],
    ): Result =
      remaining match {
        case Nil           => finish(situation, players, usis, None)
        case input :: rest =>
          step(situation, input) match {
            case Invalid(err)             => finish(situation, players, usis, Some(err))
            case Valid((next, timeSpent)) =>
              loop(
                next,
                players.map { ps =>
                  timeSpent.fold(ps) { spent =>
                    ps.update(situation.color, takeRecordedTime(_, spent, next.status.isEmpty))
                  }
                },
                next.history.lastUsi.fold(usis)(usis :+ _),
                rest,
              )
          }
      }

    loop(
      initialSituation,
      clockConfig.map { config =>
        val player = ClockPlayer.withConfig(config)
        Color.Map(player, player)
      },
      Vector.empty,
      steps.toList,
    )
  }

  private def takeRecordedTime(
      player: ClockPlayer,
      spent: Centis,
      gameActive: Boolean,
  ): ClockPlayer = {
    val stepTime   = spent.nonNeg
    val remaining  = player.remaining - stepTime
    val periodSpan =
      if (!remaining.isPositive && player.byoyomi.isPositive)
        math.min((-remaining.centis / player.byoyomi.centis) + 1, player.periodsLeft)
      else 0
    val active = gameActive && stepTime < player.remaining + player.periodsLeft * player.byoyomi
    val reloadByoyomi =
      active && player.byoyomi.isPositive && (player.spentPeriods > 0 || periodSpan > 0)
    val next =
      if (reloadByoyomi) player.setRemaining(remaining atLeast player.byoyomi)
      else
        player.takeTime(
          stepTime - (if (active) player.increment else Centis(0)) - player.byoyomi * periodSpan,
        )
    next.spendPeriods(periodSpan).copy(lastTime = stepTime)
  }

}
