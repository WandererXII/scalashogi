package shogi

import shogi.format.usi.Usi

object Perft {

  def drops(sit: Situation): List[Usi.Drop] =
    sit.dropActors.values.toList
      .flatMap(_.toUsis)

  def moves(sit: Situation): List[Usi.Move] =
    sit.moveActors.values.toList
      .flatMap(_.toUsis)

  def perft(state: State, depth: Int, log: Boolean = false): Int =
    if (depth > 0) {
      if (state.situation.end) 0
      else {
        val mds: List[Usi] = moves(state.situation) ::: drops(state.situation)
        mds.foldLeft(0) { (p, u) =>
          val newState = state(u).toOption.get
          val cnt      = perft(newState, depth - 1)
          if (log) println(s"${u.usi.padTo(5, ' ')} - $cnt")
          p + cnt
        }
      }
    } else 1

}
