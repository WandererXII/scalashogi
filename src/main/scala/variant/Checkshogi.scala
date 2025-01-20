package shogi
package variant

import scala.annotation.unused

import shogi.format.usi.Usi

case object Checkshogi
    extends Variant(
      id = 6,
      key = "checkshogi",
      name = "Checkshogi",
      title = "Check your opponent's king to win",
    ) {

  def initialSfen = Standard.initialSfen

  def numberOfRanks = Standard.numberOfRanks
  def numberOfFiles = Standard.numberOfFiles

  def allPositions = Standard.allPositions

  def pieces = Standard.pieces

  def allRoles = Standard.allRoles

  def handRoles = Standard.handRoles

  def promote(role: Role)   = Standard.promote(role)
  def unpromote(role: Role) = Standard.unpromote(role)

  def backrank(color: Color) = Standard.backrank(color)

  def promotionRanks(color: Color) = Standard.promotionRanks(color)

  def valueOfRole(r: Role): Int = Standard.valueOfRole(r)

  override def isAttacked(
      @unused before: Situation,
      @unused after: Situation,
      @unused usi: Usi,
  ): Boolean =
    false

  override def impasse(sit: Situation): Boolean = Standard.impasse(sit)

  override def perpetualCheck(@unused sit: Situation): Boolean = false

  override def specialVariantEnd(sit: Situation): Boolean =
    sit.check

  override def winner(sit: Situation): Option[Color] =
    if (sit.stalemate || sit.specialVariantEnd) Some(!sit.color)
    else if (sit.impasse) Some(sit.color)
    else None

}
