package shogi
package pieces

import Pos._

class KirinTest extends ShogiTest {

  "a kirin" should {

    val kirin     = Sente - Kirin
    val kirinGote = Gote - Kirin

    "move to valid positions" in {
      pieceMoves(kirin, SQ6F, shogi.variant.Chushogi) must bePoss(
        SQ6D,
        SQ7E,
        SQ5E,
        SQ8F,
        SQ4F,
        SQ7G,
        SQ5G,
        SQ6H
      )
      pieceMoves(kirinGote, SQ6F, shogi.variant.Chushogi) must bePoss(
        SQ6D,
        SQ7E,
        SQ5E,
        SQ8F,
        SQ4F,
        SQ7G,
        SQ5G,
        SQ6H
      )
    }

  }
}
