package shogi
package format.usi

import shogi.variant.Variant

// Every possible legal usi in a given situation is mapped to a unique char pair
// Not isomorphic on it's own, but together with situation it is
case class UsiCharPair(a: Char, b: Char) {
  override def toString = s"$a$b"
}

// Assumes usi is a legal move
// We don't want to go above one byte
// Keep 251-255 reserved for some special cases
object UsiCharPair {

  def apply(usi: Usi, variant: Variant): UsiCharPair =
    usi match {
      case Usi.Move(orig, dest, false) =>
        UsiCharPair(posToChar(orig, variant), posToChar(dest, variant))
      // If we are moving from orig to dest, we know it's not possible to move from dest to orig
      // Therefore that combination can be used for promotions
      case Usi.Move(orig, dest, true) =>
        UsiCharPair(posToChar(dest, variant), posToChar(orig, variant))
      case Usi.Drop(role, pos) =>
        UsiCharPair(
          posToChar(pos, variant),
          roleToChar(role, variant)
        )
    }

  val charOffset = 35        // Start at Char(35) == '#'
  val voidChar   = 33.toChar // '!'

  def posToChar(pos: Pos, variant: Variant): Char =
    (charOffset + pos.rank.index * variant.numberOfFiles + pos.file.index).toChar

  def roleToChar(role: DroppableRole, variant: Variant): Char =
    variant.handRoles.zipWithIndex
      .find(_._1 == role)
      .map { case (_, i) => (charOffset + variant.allPositions.size + i).toChar } | voidChar

}
