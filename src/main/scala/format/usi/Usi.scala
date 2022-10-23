package shogi
package format.usi

import cats.implicits._

sealed trait Usi {

  def usi: String

  def positions: List[Pos]

}

object Usi {

  case class Move(
      orig: Pos,
      dest: Pos,
      promotion: Boolean = false
  ) extends Usi {

    def keys = orig.key + dest.key
    def usi  = keys + promotionString

    def promotionString = if (promotion) "+" else ""

    def positions = List(orig, dest)

  }

  object Move {

    def apply(move: String): Option[Move] =
      for {
        orig <- Pos.fromKey(move take 2)
        dest <- Pos.fromKey(move.slice(2, 4))
      } yield Move(orig, dest, move.takeRight(1) == "+")

  }

  case class Drop(role: DroppableRole, pos: Pos) extends Usi {

    def usi = s"${Drop.roleToUsi(role)}*${pos.key}"

    def positions = List(pos)

  }

  object Drop {

    def apply(drop: String): Option[Drop] =
      for {
        role <- usiToRole get (drop.takeWhile(_ != '*'))
        pos  <- Pos.fromKey(drop takeRight 2)
      } yield Drop(role, pos)

    val roleToUsi: Map[DroppableRole, String] = Map(
      Pawn   -> "P",
      Lance  -> "L",
      Knight -> "N",
      Silver -> "S",
      Gold   -> "G",
      Bishop -> "B",
      Rook   -> "R"
    )
    val usiToRole: Map[String, DroppableRole] = roleToUsi map { case (k, v) => v -> k }

  }

  case class WithRole(usi: Usi, role: Role)

  def apply(usiStr: String): Option[Usi] =
    if (usiStr contains '*')
      Usi.Drop(usiStr)
    else Usi.Move(usiStr)

  def readList(moves: String): Option[List[Usi]] =
    readList(moves.split(' ').toList)

  def readList(moves: Seq[String]): Option[List[Usi]] =
    moves.toList.map(apply).sequence

}
