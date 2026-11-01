package shogi

import cats.data.Validated

import shogi.format.ParsedStep
import shogi.format.forsyth.Sfen
import shogi.format.usi.Usi

final case class Game(
    situation: Situation,
    usis: Vector[Usi] = Vector.empty,
    plies: Int = 0,
    startedAtPly: Int = 0,
    startedAtStep: Int = 1,
) {

  private def applySituation(sit: Situation): Game =
    copy(
      situation = sit,
      plies = plies + 1,
      usis = sit.history.lastUsi.fold(usis)(usis :+ _),
    )

  def apply(usi: Usi): Validated[String, Game] =
    situation(usi).map(applySituation(_))

  def apply(parsedStep: ParsedStep): Validated[String, Game] =
    situation(parsedStep).map(applySituation(_))

  def board = situation.board

  def hands = situation.hands

  def color = situation.color

  def history = situation.history

  def variant = situation.variant

  // It starts at 1, and is incremented after Gote's move/drop.
  def fullTurnNumber: Int = 1 + plies / 2

  def playedPlies: Int = plies - startedAtPly

  def stepNumber: Int = startedAtStep + playedPlies

  def withBoard(b: Board) = copy(situation = situation.copy(board = b))

  def withHands(hs: Hands) = copy(situation = situation.copy(hands = hs))

  def withColor(c: Color) = copy(situation = situation.copy(color = c))

  def withHistory(h: History) = copy(situation = situation.copy(history = h))

  def withPlies(p: Int) = copy(plies = p)

  def toSfen: Sfen = Sfen(this)
}

object Game {
  def apply(situation: Situation): Game =
    new Game(situation)

  def apply(variant: shogi.variant.Variant): Game =
    Game(Situation(variant))

  def apply(initialSfen: Option[Sfen], variant: shogi.variant.Variant): Game =
    initialSfen
      .filterNot(_.initialOf(variant))
      .flatMap {
        _.toSituationPlus(variant)
      }
      .fold(apply(variant)) { parsed =>
        apply(parsed.situation).copy(
          plies = parsed.plies,
          startedAtPly = parsed.plies,
          startedAtStep = parsed.stepNumber,
        )
      }
}
