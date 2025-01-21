import cats.data.Validated

package object shogi {

  val Sente = Color.Sente
  val Gote  = Color.Gote

  type Direction  = Pos => Option[Pos]
  type Directions = List[Direction]

  type PieceMap = Map[Pos, Piece]
  type HandMap  = Map[DroppableRole, Int]
  type HandsMap = Color.Map[HandMap]

  type PositionHash = Array[Byte]

  @inline implicit final def toRichOption[A](o: Option[A]): RichOption[A] = new RichOption(o)
  @inline implicit final def toRichValidated[E, A](a: Validated[E, A]): RichValidated[E, A] =
    new RichValidated(a)

}

final class RichOption[A](private val self: Option[A]) extends AnyVal {
  def |(default: => A): A = self getOrElse default
}
final class RichValidated[E, A](validated: Validated[E, A]) {
  def flatMap[EE >: E, B](f: A => Validated[EE, B]): Validated[EE, B] = validated.andThen(f)
}
