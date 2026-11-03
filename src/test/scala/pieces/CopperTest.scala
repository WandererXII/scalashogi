package shogi
package pieces

import shogi.Square._

class CopperTest extends ShogiTest {

  "a copper" should {

    val copper     = Sente - Copper
    val copperGote = Gote - Copper

    "move to valid squares" in {
      pieceMoves(copper, SQ6F, shogi.variant.Chushogi) must beSquares(
        SQ7E,
        SQ6E,
        SQ5E,
        SQ6G,
      )
      pieceMoves(copperGote, SQ6F, shogi.variant.Chushogi) must beSquares(
        SQ7G,
        SQ6G,
        SQ5G,
        SQ6E,
      )
    }

  }
}
