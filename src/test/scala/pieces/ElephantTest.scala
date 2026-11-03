package shogi
package pieces

import shogi.Square._

class ElephantTest extends ShogiTest {

  "a elephant" should {

    val elephant     = Sente - Elephant
    val elephantGote = Gote - Elephant

    "move to valid squares" in {
      pieceMoves(elephant, SQ6F, shogi.variant.Chushogi) must beSquares(
        SQ7E,
        SQ6E,
        SQ5E,
        SQ7F,
        SQ7G,
        SQ5F,
        SQ5G,
      )
      pieceMoves(elephantGote, SQ6F, shogi.variant.Chushogi) must beSquares(
        SQ7E,
        SQ5E,
        SQ7F,
        SQ5F,
        SQ7G,
        SQ5G,
        SQ6G,
      )
    }

  }
}
