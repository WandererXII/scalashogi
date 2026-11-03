package shogi
package pieces

import shogi.Square._

class PrinceTest extends ShogiTest {

  "a prince" should {

    val prince     = Sente - Prince
    val princeGote = Gote - Prince

    "move 1 square in any direction" in {
      pieceMoves(prince, SQ6F, shogi.variant.Chushogi) must beSquares(
        SQ6G,
        SQ7G,
        SQ7F,
        SQ7E,
        SQ6E,
        SQ5E,
        SQ5F,
        SQ5G,
      )
      pieceMoves(princeGote, SQ6F, shogi.variant.Chushogi) must beSquares(
        SQ6G,
        SQ7G,
        SQ7F,
        SQ7E,
        SQ6E,
        SQ5E,
        SQ5F,
        SQ5G,
      )
    }

    "move 1 square in any direction, even from the edges" in {
      pieceMoves(prince, SQ1A, shogi.variant.Chushogi) must beSquares(SQ1B, SQ2B, SQ2A)
    }

    "move behind pawn barrier" in {
      """
P P P P P P P P P P P P
L N . G+E . . . . . N L""" moveDestsFrom SQ8L must beSquares(SQ7L)
    }

    "not move to squares that are occupied by the same colour" in {
      val position = """
. . . P . . . . . . . .
N P+E L . . . P . . . .
. . . . . . . . . . . .
P . P . P P P . P . . .
. . . . . . . . . . . .
. . S G+E G S N L . . .
. . . . . . . . . . . .
. . . . . . . . . . . .
. . . . . . . . . . . .
Turn:Sente
"""
      position moveDestsFrom SQ10E must beSquares(
        position,
        """
. . . . . . . . . . . .
. . . . . . . . . . . .
. . . . . . . . . . . .
. x x P . . . . . . . .
N P+E L . . . P . . . .
. x x x . . . . . . . .
P . P . P P P . P . . .
. . . . . . . . . . . .
. . S G+E G S N L . . .
. . . . . . . . . . . .
. . . . . . . . . . . .
. . . . . . . . . . . .
Turn:Sente
""",
      )
    }

    "threaten" in {
      val position = """
+e. B . . . . . . . . .
. . . . . . . . . . . .
. b . B . . . . . . . .
b p p . . . . . . . . .
. .+E b . . . . . . . .
. . P . R . . . . . . .
P P . . . P P P P . . .
. . . . . . . . . . . .
L N S G . G S N L . . .
. . . . . . . . . . . .
. . . . . . . . . . . .
. . . . . . . . . . . .
Turn:Sente
"""
      "a reachable enemy" in {
        position moveActorAt SQ10E map (_ threatens SQ11D) must beSome(true)
      }
      "an unreachable enemy" in {
        position moveActorAt SQ10E map (_ threatens SQ12D) must beSome(false)
      }
      "a reachable friend" in {
        position moveActorAt SQ10E map (_ threatens SQ10F) must beSome(true)
      }
    }
  }
}
