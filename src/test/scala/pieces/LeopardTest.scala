package shogi
package pieces

import shogi.Square._

class LeopardTest extends ShogiTest {

  "a leopard" should {

    val leopard     = Sente - Leopard
    val leopardGote = Gote - Leopard

    "move to valid squares" in {
      pieceMoves(leopard, SQ6F, shogi.variant.Chushogi) must beSquares(
        SQ7E,
        SQ6E,
        SQ5E,
        SQ7G,
        SQ6G,
        SQ5G,
      )
      pieceMoves(leopardGote, SQ6F, shogi.variant.Chushogi) must beSquares(
        SQ7E,
        SQ6E,
        SQ5E,
        SQ7G,
        SQ6G,
        SQ5G,
      )
    }

  }
}
