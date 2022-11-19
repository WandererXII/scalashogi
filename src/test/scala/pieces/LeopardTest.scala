package shogi
package pieces

import Pos._

class LeopardTest extends ShogiTest {

  "a leopard" should {

    val leopard = Sente - Leopard

    "move to valid positions" in {
      pieceMoves(leopard, SQ6F, shogi.variant.Chushogi) must bePoss(
        SQ7E,
        SQ6E,
        SQ5E,
        SQ7G,
        SQ6G,
        SQ5G
      )
    }

  }
}
