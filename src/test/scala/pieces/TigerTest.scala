package shogi
package pieces

import shogi.Square._

class TigerTest extends ShogiTest {

  "a tiger" should {

    val tiger     = Sente - Tiger
    val tigerGote = Gote - Tiger

    "move to valid squares" in {
      pieceMoves(tiger, SQ6F, shogi.variant.Chushogi) must beSquares(
        SQ7E,
        SQ5E,
        SQ7F,
        SQ5F,
        SQ7G,
        SQ5G,
        SQ6G,
      )
      pieceMoves(tigerGote, SQ6F, shogi.variant.Chushogi) must beSquares(
        SQ7E,
        SQ6E,
        SQ5E,
        SQ7F,
        SQ7G,
        SQ5F,
        SQ5G,
      )
    }

  }
}
