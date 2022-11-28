package shogi
package pieces

import Pos._

class EagleTest extends ShogiTest {

  "an eagle" should {

    val eagle     = Sente - Eagle
    val eagleGote = Gote - Eagle

    "move to valid positions" in {
      pieceMoves(eagle, SQ6F, shogi.variant.Chushogi) must bePoss(
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
        SQ7G,
        SQ5G,
        SQ8H,
        SQ4H,
        SQ9I,
        SQ3I,
        SQ10J,
        SQ2J,
        SQ11K,
        SQ1K,
        SQ12L,
        SQ8D,
        SQ4D,
        SQ7E,
        SQ5E
      )
      pieceMoves(eagleGote, SQ6F, shogi.variant.Chushogi) must bePoss(
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
        SQ11A,
        SQ1A,
        SQ10B,
        SQ2B,
        SQ9C,
        SQ3C,
        SQ8D,
        SQ4D,
        SQ7E,
        SQ5E,
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
        SQ7G,
        SQ5G,
        SQ8H,
        SQ4H
      )
    }

  }
}
