package shogi
package pieces

import Pos._

class ElephantTest extends ShogiTest {

  "a elephant" should {

    val elephant = Sente - Elephant

    "move to valid positions" in {
      pieceMoves(elephant, SQ6F, shogi.variant.Chushogi) must bePoss(
        SQ7E,
        SQ6E,
        SQ5E,
        SQ7F,
        SQ7G,
        SQ5F,
        SQ5G
      )
    }

  }
}
