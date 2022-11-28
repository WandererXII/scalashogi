package shogi

import Pos._

class RoleTest extends ShogiTest {
  Color.all foreach { case color =>
    Role.all foreach { case role =>
      val piece = Piece(color, role)
      val dests = makeSituation(
        SQ5E -> piece
      ).moveDestsFrom(SQ5E).get

      s"$color-$role" in {
        forall(shogi.variant.Standard.allPositions) { pos =>
          piece.eyes(SQ5E, pos) ==== (dests contains pos)
        }
      }
    }
  }
}
