import ornicar.scalalib

package object shogi
    extends scalalib.Common
    with scalalib.OrnicarOption
    with scalalib.OrnicarBoolean {

  val Sente = Color.Sente
  val Gote  = Color.Gote

  type Direction  = Pos => Option[Pos]
  type Directions = List[Direction]

  type PieceMap = Map[Pos, Piece]
  type HandMap  = Map[DroppableRole, Int]
  type HandsMap = Color.Map[HandMap]

  type PositionHash = Array[Byte]

}
