package shogi
package format.usi

import shogi.variant.Variant

// Every possible legal usi in a given position is mapped to a unique char pair
// Not isomorphic on it's own, but together with position it is
final case class UsiCharPair(a: Char, b: Char) {
  override def toString = s"$a$b"
}

// Assumes usi is a legal move/drop
// We don't want to go above one byte
// Keep 251-255 reserved for some special cases
object UsiCharPair {

  def apply(usi: Usi, variant: Variant): UsiCharPair =
    usi match {
      case Usi.Move(orig, dest, _, Some(midStep)) =>
        UsiCharPair(squareToChar(orig, variant), lionMoveToChar(orig, dest, midStep, variant))
      case Usi.Move(orig, dest, false, _) =>
        UsiCharPair(squareToChar(orig, variant), squareToChar(dest, variant))
      // If we are moving from orig to dest, we know it's not possible to move from dest to orig
      // Therefore that combination can be used for promotions
      case Usi.Move(orig, dest, true, _) =>
        UsiCharPair(squareToChar(dest, variant), squareToChar(orig, variant))
      case Usi.Drop(role, square) =>
        UsiCharPair(
          squareToChar(square, variant),
          roleToChar(role, variant),
        )
    }

  val charOffset = 35        // Start at Char(35) == '#'
  val voidChar   = 33.toChar // '!'

  def squareToChar(square: Square, variant: Variant): Char =
    (charOffset + square.rank.index * variant.numberOfFiles + square.file.index).toChar

  def lionMoveToChar(orig: Square, dest: Square, ms: Square, variant: Variant): Char = {
    // making sure only 8 surrounding squares are encoded, getting rid of the center one
    val toMidStep = ((orig.file - ms.file + 1) + 3 * (orig.rank - ms.rank + 1) + 4) % 9
    val toDest    = ((ms.file - dest.file + 1) + 3 * (ms.rank - dest.rank + 1) + 4) % 9
    (charOffset + variant.allSquares.size + toMidStep + 8 * toDest).toChar
  }

  def roleToChar(role: DroppableRole, variant: Variant): Char =
    variant.dropRoles.zipWithIndex
      .find(_._1 == role)
      .map { case (_, i) => (charOffset + variant.allSquares.size + i).toChar } | voidChar

}
