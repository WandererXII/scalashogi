package shogi
package format.usi

import cats.implicits._

// Coordinates adjusted for 9x9 board (a1 == 9i) - works for standard shogi, perhaps for minishogi, depending on the coordinate system
object UciToUsi {

  def apply(uciStr: String): Option[Usi] =
    Usi(uciStr.map(Pos.fromUciMap))

  def readList(moves: String): Option[List[Usi]] =
    readList(moves.split(' ').toList)

  def readList(moves: Seq[String]): Option[List[Usi]] =
    moves.toList.map(apply).sequence

}
