package shogi
package pieces

import shogi.Pos._

class PhoenixTest extends ShogiTest {

  "a phoenix" should {

    val phoenix     = Sente - Phoenix
    val phoenixGote = Gote - Phoenix

    "move to valid positions" in {
      pieceMoves(phoenix, SQ6F, shogi.variant.Chushogi) must bePoss(
        SQ8D,
        SQ4D,
        SQ6E,
        SQ7F,
        SQ5F,
        SQ6G,
        SQ8H,
        SQ4H,
      )
      pieceMoves(phoenixGote, SQ6F, shogi.variant.Chushogi) must bePoss(
        SQ8D,
        SQ4D,
        SQ6E,
        SQ7F,
        SQ5F,
        SQ6G,
        SQ8H,
        SQ4H,
      )
    }

  }
}
