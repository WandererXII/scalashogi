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
      promotion: Boolean = false,
      midStep: Option[Pos] = None
  ) extends Usi {

    def keys = orig.key + midStep.fold("")(_.key) + dest.key
    def usi  = keys + promotionString

    def promotionString = if (promotion) "+" else ""

    def positions = midStep.fold(List(orig, dest))(List(orig, _, dest))

  }

  object Move {

    private val MoveRegex = """(\d\d?[a-l])(\d\d?[a-l])?(\d\d?[a-l])(\+|=|\?)?""".r

    def apply(str: String): Option[Move] =
      str match {
        case MoveRegex(origS, midStepS, destS, promS) =>
          for {
            orig <- Pos.fromKey(origS)
            dest <- Pos.fromKey(destS)
            prom = promS == "+"
            midStep = Option(midStepS).flatMap(Pos.fromKey(_))
          } yield Move(orig, dest, prom, midStep)
        case _ => None
      }

  }

  case class Drop(role: DroppableRole, pos: Pos) extends Usi {

    def usi = s"${Drop.roleToUsi(role)}*${pos.key}"

    def positions = List(pos)

  }

  object Drop {

    def apply(str: String): Option[Drop] =
      for {
        role <- usiToRole get str.take(1)
        pos  <- Pos.fromKey(str.drop(2))
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
