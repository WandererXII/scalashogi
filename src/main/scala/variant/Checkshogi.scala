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

  override def pawnCheckmate(@unused a: DropActor, @unused d: Pos): Boolean =
    false

  def status(sit: Situation): Option[Status] =
    if (sit.check) Status.SpecialVariantEnd.some
    else Standard.status(sit)

  def winner(sit: Situation): Option[Color] =
    if (sit.status.contains(Status.SpecialVariantEnd)) (!sit.color).some
    else Standard.winner(sit)

}
