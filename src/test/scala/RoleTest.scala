package shogi

import Pos._
import shogi.variant._

class RoleTest extends ShogiTest {
  Color.all foreach { case color =>
    Role.all foreach { case role =>
      val variant = Variant.all.find(_.allRoles contains role).get
      val piece   = Piece(color, role)
      val dests = makeSituationWithBoard(
        variant,
        SQ5E -> piece
      ).moveDestsFrom(SQ5E).get

      s"$color-$role" in {
        forall(variant.allPositions) { pos =>
          piece.eyes(SQ5E, pos) must_== (dests contains pos)
        }
      }
    }
  }
}
