package shogi
package pieces

import Pos._

class VerticalMoverTest extends ShogiTest {

  "a verticalMover" should {

    val verticalMover = Sente - VerticalMover

    "move to valid positions" in {
      pieceMoves(verticalMover, SQ6F, shogi.variant.Chushogi) must bePoss(
        SQ6A,
        SQ6B,
        SQ6C,
        SQ6D,
        SQ6E,
        SQ6G,
        SQ6H,
        SQ6I,
        SQ6J,
        SQ6K,
        SQ6L,
        SQ7F,
        SQ5F
      )
    }

  }
}
