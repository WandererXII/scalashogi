package shogi
package pieces

import shogi.Pos._

class StagTest extends ShogiTest {

  "a stag" should {

    val stag     = Sente - Stag
    val stagGote = Gote - Stag

    "move to valid positions" in {
      pieceMoves(stag, SQ6F, shogi.variant.Chushogi) must bePoss(
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
        SQ7E,
        SQ5E,
        SQ7F,
        SQ5F,
        SQ7G,
        SQ5G,
      )
      pieceMoves(stagGote, SQ6F, shogi.variant.Chushogi) must bePoss(
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
        SQ7E,
        SQ5E,
        SQ7F,
        SQ5F,
        SQ7G,
        SQ5G,
      )
    }

  }
}
