package shogi

import shogi.variant.Variant

// Actors wait on these lazy vals, so computing them must never wait on an actor.
// Keep this free of Position, which is what guarantees it can't reach one.
final private[shogi] class KingSafety(board: Board, color: Color, variant: Variant) {

  lazy val check: Boolean = variant.check(board, color)

  lazy val checkSquares: List[Square] = variant.checkSquares(board, color)

  lazy val possibleDropDests: List[Square] =
    Some(checkSquares)
      .filter(_.size == 1)
      .fold(
        variant.allSquares.filterNot(board.pieces contains _),
      ) { royals =>
        DropActor.blockades(board, color, variant, royals.head)
      }
}
