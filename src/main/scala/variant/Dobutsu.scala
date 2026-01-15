package shogi
package variant

import scala.annotation.unused

import cats.syntax.option._

import shogi.Pos._
import shogi.format.forsyth.Sfen
import shogi.format.usi.Usi

case object Dobutsu
    extends Variant(
      id = 7,
      key = "dobutsu",
      name = "Dobutsu",
    ) {

  val initialSfen = Sfen("rkb/1p1/1P1/BKR b - 1")

  val numberOfRanks = 4
  val numberOfFiles = 3

  val allPositions = (SQ3D upTo SQ1A).toList

  val pieces =
    Map(
      SQ3D -> Sente.bishop,
      SQ2D -> Sente.king,
      SQ1D -> Sente.rook,
      SQ2C -> Sente.pawn,
      SQ2B -> Gote.pawn,
      SQ3A -> Gote.rook,
      SQ2A -> Gote.king,
      SQ1A -> Gote.bishop,
    )

  val allRoles = List(
    Pawn,
    King,
    Bishop,
    Rook,
    Tokin,
  )

  val handRoles = List(
    Rook,
    Bishop,
    Pawn,
  )

  override def dropFilter(a: DropActor): List[Pos] =
    allPositions.filterNot(a.board.pieces contains _)

  def promote(role: Role): Option[Role] =
    role match {
      case Pawn => Some(Tokin)
      case _    => None
    }

  def unpromote(role: Role): Option[Role] =
    role match {
      case Tokin => Some(Pawn)
      case _     => None
    }

  def backrank(color: Color) =
    if (color.sente) Rank.A else Rank.D

  def promotionRanks(color: Color) = List(backrank(color))

  override def posThreatened(
      board: Board,
      color: Color,
      pos: Pos,
      @unused filter: Piece => Boolean = _ => true,
  ): Boolean =
    board.pieces exists {
      case (from, piece) =>
        piece.color == color && piece.eyes(from, pos) && from.dist(pos) == 1
      case _ => false
    }

  def valueOfRole(r: Role): Int =
    r match {
      case Pawn   => 1
      case Bishop => 3
      case Rook   => 4
      case Tokin  => 5
      case _      => 0
    }

  // filters to one dist moves, ignores king safety
  override def moveFilter(a: MoveActor): List[Pos] =
    a.unfilteredDestinations.filter(_.dist(a.pos) == 1)

  override def isAttacking(
      @unused after: Situation,
      @unused usi: Usi,
  ): Boolean =
    false

  def tryRule(board: Board, color: Color): Boolean =
    board.singleRoyalPosOf(color).exists { kingPos =>
      promotionRanks(color).contains(kingPos.rank) &&
      !check(board, color)
    }

  override def isInsufficientMaterial(@unused board: Board, @unused hands: Hands) = false

  override def status(sit: Situation): Option[Status] =
    if (sit.board.royalPossOf(sit.color).isEmpty) Status.RoyalsLost.some
    else if (tryRule(sit.board, Sente) || tryRule(sit.board, Gote)) Status.TryRule.some
    else if (!sit.hasDestinations) Status.Stalemate.some
    else if (sit.history.threefoldRepetition) Status.Repetition.some
    else none

  override def winner(sit: Situation): Option[Color] =
    sit.status flatMap { status =>
      status match {
        case Status.RoyalsLost | Status.Stalemate => (!sit.color).some
        case Status.TryRule                       =>
          if (tryRule(sit.board, Sente)) Sente.some else Gote.some
        case _ => none
      }
    }

}
