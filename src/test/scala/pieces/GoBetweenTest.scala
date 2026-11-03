package shogi
package pieces

import shogi.Square._

class GoBetweenTest extends ShogiTest {

  "a goBetween" should {

    val goBetween     = Sente - GoBetween
    val goBetweenGote = Gote - GoBetween

    "move to valid squares" in {
      pieceMoves(goBetween, SQ6F, shogi.variant.Chushogi) must beSquares(
        SQ6E,
        SQ6G,
      )
      pieceMoves(goBetweenGote, SQ6F, shogi.variant.Chushogi) must beSquares(
        SQ6E,
        SQ6G,
      )
    }

  }
}
