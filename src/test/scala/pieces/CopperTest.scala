package shogi
package pieces

import shogi.Pos._

class CopperTest extends ShogiTest {

  "a copper" should {

    val copper     = Sente - Copper
    val copperGote = Gote - Copper

    "move to valid positions" in {
      pieceMoves(copper, SQ6F, shogi.variant.Chushogi) must bePoss(
        SQ7E,
        SQ6E,
        SQ5E,
        SQ6G,
      )
      pieceMoves(copperGote, SQ6F, shogi.variant.Chushogi) must bePoss(
        SQ7G,
        SQ6G,
        SQ5G,
        SQ6E,
      )
    }

  }
}
