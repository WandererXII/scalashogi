package shogi
package pieces

import shogi.Square._

class PhoenixTest extends ShogiTest {

  "a phoenix" should {

    val phoenix     = Sente - Phoenix
    val phoenixGote = Gote - Phoenix

    "move to valid squares" in {
      pieceMoves(phoenix, SQ6F, shogi.variant.Chushogi) must beSquares(
        SQ8D,
        SQ4D,
        SQ6E,
        SQ7F,
        SQ5F,
        SQ6G,
        SQ8H,
        SQ4H,
      )
      pieceMoves(phoenixGote, SQ6F, shogi.variant.Chushogi) must beSquares(
        SQ8D,
        SQ4D,
        SQ6E,
        SQ7F,
        SQ5F,
        SQ6G,
        SQ8H,
        SQ4H,
      )
    }

  }
}
