package shogi
package pieces

import Pos._

class FalconTest extends ShogiTest {

  "a falcon" should {

    val falcon = Sente - Falcon

    "move to valid positions" in {
      pieceMoves(falcon, SQ6F, shogi.variant.Chushogi) must bePoss(
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
        SQ11A,
        SQ10B,
        SQ9C,
        SQ8D,
        SQ7E,
        SQ5G,
        SQ4H,
        SQ3I,
        SQ2J,
        SQ1K,
        SQ1A,
        SQ2B,
        SQ3C,
        SQ4D,
        SQ5E,
        SQ7G,
        SQ8H,
        SQ9I,
        SQ10J,
        SQ11K,
        SQ12L,
        SQ6D,
        SQ6E,
        SQ6G,
        SQ6H,
        SQ6I,
        SQ6J,
        SQ6K,
        SQ6L
      )
    }

  }
}
