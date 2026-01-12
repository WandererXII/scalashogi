package shogi

import shogi.format.usi.Usi

object Perft {

  def drops(sit: Situation): List[Usi.Drop] =
    sit.dropActors.values.toList
      .flatMap(_.toUsis)

  def moves(sit: Situation): List[Usi.Move] =
    sit.moveActors.values.toList
      .flatMap(_.toUsis)

  def perft(game: Game, depth: Int, log: Boolean = false): Int =
    if (depth > 0) {
      if (game.situation.end) 0
      else {
        val mds: List[Usi] = moves(game.situation) ::: drops(game.situation)
        mds.foldLeft(0) { (p, u) =>
          val newGame = game(u).toOption.get
          val cnt     = perft(newGame, depth - 1)
          if (log) println(s"${u.usi.padTo(5, ' ')} - $cnt")
          p + cnt
        }
      }
    } else 1

}
