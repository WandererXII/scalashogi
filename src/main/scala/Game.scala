package shogi

import cats.data.Validated

import shogi.format.forsyth.Sfen
import shogi.format.ParsedMove
import shogi.format.usi.Usi

case class Game(
    situation: Situation,
    usiMoves: Vector[Usi] = Vector.empty,
    clock: Option[Clock] = None,
    plies: Int = 0,
    startedAtPly: Int = 0,
    startedAtMove: Int = 1
) {

  private def applySituation(sit: Situation, metrics: MoveMetrics = MoveMetrics.empty): Clock.WithCompensatedLag[Game] = {
    val newClock = applyClock(metrics, sit.status.isEmpty)
    Clock.WithCompensatedLag(
      copy(
        situation = sit,
        plies = plies + 1,
        usiMoves = sit.history.lastMove.fold(usiMoves)(usiMoves :+ _),
        clock = newClock.map(_.value)
      ),
      newClock.flatMap(_.compensated)
    )
  }

  private def applyClock(
      metrics: MoveMetrics,
      gameActive: => Boolean
  ): Option[Clock.WithCompensatedLag[Clock]] =
    clock.map { prev =>
      {
        val c1 = metrics.frameLag.fold(prev)(prev.withFrameLag)
        val c2 = c1.step(metrics, gameActive)
        if (plies - startedAtPly == 1) c2.map(_.start) else c2
      }
    }

  def apply(usi: Usi, metrics: MoveMetrics): Validated[String, Clock.WithCompensatedLag[Game]] =
    situation(usi).map(applySituation(_, metrics))

  def apply(usi: Usi): Validated[String, Game] =
    situation(usi).map(applySituation(_).value)

  def apply(parsedMove: ParsedMove, metrics: MoveMetrics): Validated[String, Clock.WithCompensatedLag[Game]] =
    situation(parsedMove).map(applySituation(_, metrics))

  def apply(parsedMove: ParsedMove): Validated[String, Game] =
    situation(parsedMove).map(applySituation(_).value)

  def board = situation.board

  def hands = situation.hands

  def color = situation.color

  def history = situation.history

  def variant = situation.variant

  // It starts at 1, and is incremented after Gote's move.
  def fullTurnNumber: Int = 1 + plies / 2

  def playedPlies: Int = plies - startedAtPly

  def moveNumber: Int = startedAtMove + playedPlies

  def withBoard(b: Board) = copy(situation = situation.copy(board = b))

  def withHands(hs: Hands) = copy(situation = situation.copy(hands = hs))

  def withColor(c: Color) = copy(situation = situation.copy(color = c))

  def withHistory(h: History) = copy(situation = situation.copy(history = h))

  def withClock(c: Option[Clock]) = copy(clock = c)

  def withPlies(p: Int) = copy(plies = p)

  def toSfen: Sfen = Sfen(this)
}

object Game {
  def apply(situation: Situation): Game =
    new Game(situation)

  def apply(variant: shogi.variant.Variant): Game =
    Game(Situation(variant))

  def apply(variantOption: Option[shogi.variant.Variant], sfen: Option[Sfen]): Game = {
    val variant = variantOption | shogi.variant.Standard
    val g       = apply(variant)
    sfen
      .filterNot(_.initialOf(variant))
      .flatMap {
        _.toSituationPlus(variant)
      }
      .fold(g) { parsed =>
        g.copy(
          situation = Situation(
            board = parsed.situation.board,
            hands = parsed.situation.hands,
            color = parsed.situation.color,
            history = History.empty,
            variant = g.variant
          ),
          plies = parsed.plies,
          startedAtPly = parsed.plies,
          startedAtMove = parsed.moveNumber
        )
      }
  }
}
