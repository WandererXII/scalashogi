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
        forall(Pos.all) { pos =>
          if (piece.eyes(SQ5E, pos) != (dests contains pos)) println(s"$piece, $pos, eyes? ${piece.eyes(SQ5E, pos)}, dests? ${dests contains pos}")
          piece.eyes(SQ5E, pos) ==== (dests contains pos)
        }
      }
    }
  }
}
