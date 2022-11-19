package shogi
package pieces

import Pos._

class GoBetweenTest extends ShogiTest {

  "a goBetween" should {

    val goBetween = Sente - GoBetween

    "move to valid positions" in {
      pieceMoves(goBetween, SQ6F, shogi.variant.Chushogi) must bePoss(
        SQ6E,
        SQ6G
      )
    }

  }
}
