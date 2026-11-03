package shogi

import shogi.format.usi.Usi

object Perft {

  def drops(position: Position): List[Usi.Drop] =
    position.dropActors.values.toList
      .flatMap(_.toUsis)

  def moves(position: Position): List[Usi.Move] =
    position.moveActors.values.toList
      .flatMap(_.toUsis)

  def perft(position: Position, depth: Int, log: Boolean = false): Int =
    if (depth > 0) {
      if (position.end) 0
      else {
        val mds: List[Usi] = moves(position) ::: drops(position)
        mds.foldLeft(0) { (p, u) =>
          val newPosition = position(u).toOption.get
          val cnt         = perft(newPosition, depth - 1, log = false)
          if (log) println(s"${u.usi.padTo(5, ' ')} - $cnt")
          p + cnt
        }
      }
    } else 1

}
