package shogi
package pieces

import Pos._

class CopperTest extends ShogiTest {

  "a copper" should {

    val copper = Sente - Copper

    "move to valid positions" in {
      pieceMoves(copper, SQ6F, shogi.variant.Chushogi) must bePoss(
        SQ7E,
        SQ6E,
        SQ5E,
        SQ6G
      )
    }

  }
}
