package shogi
package format.usi

import cats.implicits._

sealed trait Usi {

  def usi: String

  def positions: List[Pos]

}

object Usi {

  final case class Move(
      orig: Pos,
      dest: Pos,
      promotion: Boolean = false,
      midStep: Option[Pos] = None
  ) extends Usi {

    def keys = orig.key + midStep.fold("")(_.key) + dest.key
    def usi  = keys + promotionString

    def promotionString = if (promotion) "+" else ""

    def positions = midStep.fold(List(orig, dest))(ms => List(orig, ms, dest))

  }

  object Move {

    private val MoveRegex = """(\d\d?[a-l])(\d\d?[a-l])?(\d\d?[a-l])(\+|=|\?)?""".r

    def apply(str: String): Option[Move] =
      str match {
        case MoveRegex(origS, midStepS, destS, promS) =>
          for {
            orig <- Pos.allKeys get origS
            dest <- Pos.allKeys get destS
            prom    = promS == "+"
            midStep = Option(midStepS).flatMap(Pos.allKeys get _)
          } yield Move(orig, dest, prom, midStep)
        case _ => None
      }

  }

  final case class Drop(role: DroppableRole, pos: Pos) extends Usi {

    def usi = s"${Drop.roleToUsi.getOrElse(role, "")}*${pos.key}"

    def positions = List(pos)

  }

  object Drop {

    def apply(str: String): Option[Drop] =
      for {
        role <- usiToRole get str.take(1)
        pos  <- Pos.allKeys get (str.drop(2))
      } yield Drop(role, pos)

    val roleToUsi: Map[DroppableRole, String] = Map(
      Pawn   -> "P",
      Lance  -> "L",
      Knight -> "N",
      Silver -> "S",
      Gold   -> "G",
      Bishop -> "B",
      Rook   -> "R",
      Tokin  -> "T"
    )
    val usiToRole: Map[String, DroppableRole] = roleToUsi map { case (k, v) => v -> k }

  }

  final case class WithRole(usi: Usi, role: Role)

  def apply(usiStr: String): Option[Usi] =
    if (usiStr contains '*')
      Usi.Drop(usiStr)
    else Usi.Move(usiStr)

  def readList(steps: String): Option[List[Usi]] =
    readList(steps.split(' ').toList)

  def readList(steps: Seq[String]): Option[List[Usi]] =
    steps.toList.map(apply).sequence

}
