package shogi
package pieces

import shogi.Pos._

class SideMoverTest extends ShogiTest {

  "a sideMover" should {

    val sideMover     = Sente - SideMover
    val sideMoverGote = Gote - SideMover

    "move to valid positions" in {
      pieceMoves(sideMover, SQ6F, shogi.variant.Chushogi) must bePoss(
        SQ6E,
        SQ6G,
        SQ12F,
        SQ11F,
        SQ10F,
        SQ9F,
        SQ8F,
        SQ7F,
        SQ5F,
        SQ4F,
        SQ3F,
        SQ2F,
        SQ1F,
      )
      pieceMoves(sideMoverGote, SQ6F, shogi.variant.Chushogi) must bePoss(
        SQ6E,
        SQ6G,
        SQ12F,
        SQ11F,
        SQ10F,
        SQ9F,
        SQ8F,
        SQ7F,
        SQ5F,
        SQ4F,
        SQ3F,
        SQ2F,
        SQ1F,
      )
    }

  }
}
