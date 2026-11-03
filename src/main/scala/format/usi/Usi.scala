package shogi
package format
package usi

import cats.implicits._

sealed trait Usi {

  def usi: String

  def squares: List[Square]

}

object Usi {

  final case class Move(
      orig: Square,
      dest: Square,
      promotion: Boolean = false,
      midStep: Option[Square] = None,
  ) extends Usi {

    def keys = orig.key + midStep.fold("")(_.key) + dest.key
    def usi  = keys + promotionString

    def promotionString = if (promotion) "+" else ""

    def squares = midStep.fold(List(orig, dest))(ms => List(orig, ms, dest))

  }

  object Move {

    private val MoveRegex = """(\d\d?[a-l])(\d\d?[a-l])?(\d\d?[a-l])(\+|=|\?)?""".r

    def apply(str: String): Option[Move] =
      str match {
        case MoveRegex(origS, midStepS, destS, promS) =>
          for {
            orig    <- Square.allKeys get origS
            dest    <- Square.allKeys get destS
            midStep <- Option(midStepS).traverse(Square.allKeys get _)
            prom = promS == "+"
          } yield Move(orig, dest, prom, midStep)
        case _ => None
      }

  }

  final case class Drop(role: DroppableRole, square: Square) extends Usi {

    def usi = s"${Drop.roleToUsi.getOrElse(role, "")}*${square.key}"

    def squares = List(square)

  }

  object Drop {

    def apply(str: String): Option[Drop] =
      if (!str.lift(1).contains('*')) None
      else
        for {
          role   <- usiToRole get str.take(1)
          square <- Square.allKeys get (str.drop(2))
        } yield Drop(role, square)

    val roleToUsi: Map[DroppableRole, String] = Map(
      Pawn   -> "P",
      Lance  -> "L",
      Knight -> "N",
      Silver -> "S",
      Gold   -> "G",
      Bishop -> "B",
      Rook   -> "R",
      Tokin  -> "T",
    )
    val usiToRole: Map[String, DroppableRole] = roleToUsi map { case (k, v) => v -> k }

  }

  final case class WithRole(usi: Usi, role: Role)

  def apply(usiStr: String): Option[Usi] =
    if (usiStr contains '*')
      Usi.Drop(usiStr)
    else Usi.Move(usiStr)

  def readList(steps: String): Option[List[Usi]] =
    readList(fieldSeparator.split(steps).filter(_.nonEmpty).toList)

  def readList(steps: Seq[String]): Option[List[Usi]] =
    steps.toList.map(apply).sequence

}
