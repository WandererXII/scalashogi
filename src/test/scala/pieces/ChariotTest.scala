package shogi
package pieces

import Pos._

class ChariotTest extends ShogiTest {

  "a chariot" should {

    val chariot     = Sente - Chariot
    val chariotGote = Gote - Chariot

    "move to valid positions" in {
      pieceMoves(chariot, SQ6F, shogi.variant.Chushogi) must bePoss(
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
        SQ6L
      )
      pieceMoves(chariotGote, SQ6F, shogi.variant.Chushogi) must bePoss(
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
        SQ6L
      )
    }

  }
}
