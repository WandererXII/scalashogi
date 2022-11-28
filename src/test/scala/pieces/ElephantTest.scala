package shogi
package pieces

import Pos._

class ElephantTest extends ShogiTest {

  "a elephant" should {

    val elephant     = Sente - Elephant
    val elephantGote = Gote - Elephant

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
      pieceMoves(elephantGote, SQ6F, shogi.variant.Chushogi) must bePoss(
        SQ7E,
        SQ5E,
        SQ7F,
        SQ5F,
        SQ7G,
        SQ5G,
        SQ6G
      )
    }

  }
}
