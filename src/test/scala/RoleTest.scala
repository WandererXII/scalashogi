package shogi

import shogi.Square._
import shogi.variant._

class RoleTest extends ShogiTest {
  Color.all foreach { case color =>
    Role.all foreach { case role =>
      val variant = Variant.all.find(_.allRoles contains role).get
      val piece   = Piece(color, role)
      val dests   = makePositionWithBoard(
        variant,
        piece.color,
        SQ5E -> piece,
      ).moveDestsFrom(SQ5E).get

      s"$color-$role" in {
        forall(variant.allSquares) { square =>
          piece.eyes(SQ5E, square) must_== (dests contains square)
        }
      }
    }
  }
}
