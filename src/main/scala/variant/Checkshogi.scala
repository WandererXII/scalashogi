package shogi
package variant

import scala.annotation.unused

import cats.syntax.option._

import shogi.format.usi.Usi

case object Checkshogi
    extends Variant(
      id = 6,
      key = "checkshogi",
      name = "Checkshogi",
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

  override def isAttacking(
      @unused after: Situation,
      @unused usi: Usi,
  ): Boolean =
    false

  override def dropFilterPawnCheckmate(@unused a: DropActor, @unused d: Pos): Boolean =
    false

  override def status(sit: Situation): Option[Status] =
    if (sit.check) Status.Check.some
    else super.status(sit)

}
