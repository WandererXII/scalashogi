package shogi

import format.usi.Usi

object Perft {

  def drops(sit: Situation): List[Usi] =
    sit
      .dropActorsOf(sit.color)
      .flatMap(_.toUsis)

  def moves(sit: Situation): List[Usi] =
    sit
      .moveActorsOf(sit.color)
      .flatMap(_.toUsis)

  def perft(game: Game, depth: Int, log: Boolean = false): Int =
    if (depth > 0) {
      val mds: List[Usi] = moves(game.situation) ::: drops(game.situation)
      mds.foldLeft(0) { (p, u) =>
        val cnt = perft(game(u).toOption.get, depth - 1)
        if (log) println(s"${u.usi.padTo(5, ' ')} - $cnt")
        p + cnt
      }
    } else 1

}
