package shogi

import shogi.format.usi.Usi

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

  def blockades(position: Position, royalSquare: Square): List[Square] = {
    def attacker(realPiece: Piece, from: Square) = {
      val piece = position.variant.attackingPiece(realPiece, from, position.board)
      piece.projectionDirs.nonEmpty && piece.eyes(
        from,
        royalSquare,
      ) && piece.color != position.color
    }
    @scala.annotation.tailrec
    def forward(from: Square, dir: Direction, squares: List[Square]): List[Square] =
      dir(from) match {
        case None                                                         => Nil
        case Some(next) if !position.variant.isInsideBoard(next)          => Nil
        case Some(next) if position.board(next).exists(attacker(_, next)) => squares
        case Some(next) if position.board(next).isDefined                 => Nil
        case Some(next) => forward(next, dir, next :: squares)
      }
    Square.allDirections flatMap { forward(royalSquare, _, Nil) } filter { square =>
      position.board.place(Piece(position.color, Gold), square) exists { defended =>
        !position.copy(board = defended).check
      }
    }
  }

}
