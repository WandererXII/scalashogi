package shogi
package pieces

import shogi.Pos._

class WhaleTest extends ShogiTest {

  "a whale" should {

    val whale     = Sente - Whale
    val whaleGote = Gote - Whale

    "move to valid positions" in {
      pieceMoves(whale, SQ6F, shogi.variant.Chushogi) must bePoss(
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
      )
      pieceMoves(whaleGote, SQ6F, shogi.variant.Chushogi) must bePoss(
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
      )
    }

  }
}
