package shogi
package pieces

import Pos._

class WhaleTest extends ShogiTest {

  "a whale" should {

    val whale = Sente - Whale

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
        SQ12L
      )
    }

  }
}
