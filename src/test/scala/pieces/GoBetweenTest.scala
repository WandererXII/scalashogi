package shogi
package pieces

import shogi.Pos._

class GoBetweenTest extends ShogiTest {

  "a goBetween" should {

    val goBetween     = Sente - GoBetween
    val goBetweenGote = Gote - GoBetween

    "move to valid positions" in {
      pieceMoves(goBetween, SQ6F, shogi.variant.Chushogi) must bePoss(
        SQ6E,
        SQ6G,
      )
      pieceMoves(goBetweenGote, SQ6F, shogi.variant.Chushogi) must bePoss(
        SQ6E,
        SQ6G,
      )
    }

  }
}
