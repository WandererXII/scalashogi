package shogi

import shogi.format.usi.Usi

final case class DropActor(
    color: Color,
    role: DroppableRole,
    situation: Situation
) {

  lazy val destinations: List[Pos] =
    if (
      situation.variant.supportsDrops &&
      situation.variant.handRoles.contains(role) &&
      situation.hands.has(color, role)
    )
      situation.variant.dropLegalityFilter(this)
    else Nil

  def toUsis: List[Usi.Drop] =
    destinations.map(Usi.Drop(role, _))

  def piece = Piece(color, role)

}

object DropActor {

  def blockades(sit: Situation, royalPos: Pos): List[Pos] = {
    def attacker(piece: Piece, from: Pos) =
      piece.projectionDirs.nonEmpty && piece.eyes(from, royalPos) && piece.color != sit.color
    @scala.annotation.tailrec
    def forward(p: Pos, dir: Direction, squares: List[Pos]): List[Pos] =
      dir(p) match {
        case None                                                    => Nil
        case Some(next) if !sit.variant.isInsideBoard(next)          => Nil
        case Some(next) if sit.board(next).exists(attacker(_, next)) => squares
        case Some(next) if sit.board(next).isDefined                 => Nil
        case Some(next)                                              => forward(next, dir, next :: squares)
      }
    Pos.allDirections flatMap { forward(royalPos, _, Nil) } filter { square =>
      sit.board.place(Piece(sit.color, Gold), square) exists { defended =>
        !sit.copy(board = defended).check
      }
    }
  }

}
