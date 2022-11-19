package shogi
package pieces

import Pos._

class LionTest extends ShogiTest {

  "a lion" should {

    val lion = Sente - Lion

    "move to valid positions" in {
      pieceMoves(lion, SQ6F, shogi.variant.Chushogi) must bePoss(
        SQ8D,
        SQ7D,
        SQ6D,
        SQ5D,
        SQ4D,
        SQ8E,
        SQ7E,
        SQ6E,
        SQ5E,
        SQ4E,
        SQ8F,
        SQ7F,
        SQ5F,
        SQ4F,
        SQ8G,
        SQ7G,
        SQ6G,
        SQ5G,
        SQ4G,
        SQ8H,
        SQ7H,
        SQ6H,
        SQ5H,
        SQ4H
      )
    }

  }
}
