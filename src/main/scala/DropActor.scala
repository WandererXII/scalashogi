package shogi

import shogi.format.usi.Usi
import shogi.variant.Variant

final case class DropActor(
    role: DroppableRole,
    position: Position,
) {

  def board   = position.board
  def color   = position.color
  def history = position.history
  def variant = position.variant

  def check = position.check

  def unfilteredDestinations: List[Square] = position.possibleDropDests

  lazy val destinations: List[Square] =
    if (
      position.variant.supportsDrops &&
      position.variant.dropRoles.contains(role)
    )
      position.variant.dropFilter(this)
    else Nil

  def toUsis: List[Usi.Drop] =
    destinations.map(Usi.Drop(role, _))

  def piece = Piece(color, role)

}

object DropActor {

  def blockades(
      board: Board,
      color: Color,
      variant: Variant,
      royalSquare: Square,
  ): List[Square] = {
    def attacker(realPiece: Piece, from: Square) = {
      val piece = variant.attackingPiece(realPiece, from, board)
      piece.projectionDirs.nonEmpty && piece.eyes(
        from,
        royalSquare,
      ) && piece.color != color
    }
    @scala.annotation.tailrec
    def forward(from: Square, dir: Direction, squares: List[Square]): List[Square] =
      dir(from) match {
        case None                                                => Nil
        case Some(next) if !variant.isInsideBoard(next)          => Nil
        case Some(next) if board(next).exists(attacker(_, next)) => squares
        case Some(next) if board(next).isDefined                 => Nil
        case Some(next) => forward(next, dir, next :: squares)
      }
    Square.allDirections flatMap { forward(royalSquare, _, Nil) } filter { square =>
      board.place(Piece(color, Gold), square) exists { defended =>
        !variant.check(defended, color)
      }
    }
  }

}
