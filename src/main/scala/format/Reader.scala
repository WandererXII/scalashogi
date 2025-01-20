package shogi
package format

import cats.data.Validated

import shogi.format.forsyth.Sfen
import shogi.format.usi.Usi

object Reader {

  sealed trait Result {
    def valid: Validated[String, Replay]
  }

  object Result {
    final case class Complete(replay: Replay) extends Result {
      def valid = Validated.valid(replay)
    }
    final case class Incomplete(replay: Replay, failures: String) extends Result {
      def valid = Validated.invalid(failures)
    }
  }

  def fromParsedNotation(parsed: ParsedNotation, op: ParsedSteps => ParsedSteps): Result =
    makeReplayFromParsedSteps(
      makeGame(parsed.initialSfen, parsed.variant, parsed.tags),
      op(parsed.parsedSteps),
    )

  def fromUsi(
      usis: Seq[Usi],
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant,
      tags: Tags,
  ): Result =
    makeReplayFromUsi(makeGame(initialSfen, variant, tags), usis)

  private def makeReplayFromUsi(game: Game, usis: Seq[Usi]): Result =
    usis.foldLeft[Result](Result.Complete(Replay(game))) {
      case (Result.Complete(replay), usi) =>
        replay
          .state(usi)
          .fold(
            err => Result.Incomplete(replay, err),
            game => Result.Complete(replay(game)),
          )
      case (r: Result.Incomplete, _) => r
    }

  private def makeReplayFromParsedSteps(game: Game, parsedSteps: ParsedSteps): Result =
    parsedSteps.value.foldLeft[Result](Result.Complete(Replay(game))) {
      case (Result.Complete(replay), parsedStep) =>
        replay
          .state(parsedStep)
          .fold(
            err => Result.Incomplete(replay, err),
            game => Result.Complete(replay(game)),
          )
      case (r: Result.Incomplete, _) => r
    }

  private def makeGame(initialSfen: Option[Sfen], variant: shogi.variant.Variant, tags: Tags) =
    Game(
      initialSfen,
      variant,
    ).copy(
      clock = tags.clockConfig map Clock.apply,
    )
}
