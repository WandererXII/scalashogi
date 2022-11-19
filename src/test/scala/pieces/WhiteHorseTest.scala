package shogi
package pieces

import Pos._

class WhiteHorseTest extends ShogiTest {

  "a whiteHorse" should {

    val whiteHorse = Sente - WhiteHorse

    "move to valid positions" in {
      pieceMoves(whiteHorse, SQ6F, shogi.variant.Chushogi) must bePoss(
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
        SQ5E
      )
    }

  }
}
