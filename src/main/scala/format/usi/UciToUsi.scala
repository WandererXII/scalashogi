package shogi
package format.usi

import cats.implicits._

// Coordinates adjusted for 9x9 board (a1 == 9i) - works for standard shogi, perhaps for minishogi, depending on the coordinate system
object UciToUsi {

  def apply(uciStr: String): Option[Usi] =
    Usi(uciStr.map(Pos.fromUciMap))

  def readList(steps: String): Option[List[Usi]] =
    readList(steps.split(' ').toList)

  def readList(steps: Seq[String]): Option[List[Usi]] =
    steps.toList.map(apply).sequence

}
