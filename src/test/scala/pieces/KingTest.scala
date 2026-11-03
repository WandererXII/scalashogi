package shogi
package pieces

import shogi.Square._

class KingTest extends ShogiTest {

  "a king" should {

    val king     = Sente - King
    val kingGote = Gote - King

    "move 1 square in any direction" in {
      pieceMoves(king, SQ6F, shogi.variant.Standard) must beSquares(
        SQ6G,
        SQ7G,
        SQ7F,
        SQ7E,
        SQ6E,
        SQ5E,
        SQ5F,
        SQ5G,
      )
      pieceMoves(kingGote, SQ6F, shogi.variant.Standard) must beSquares(
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
      pieceMoves(king, SQ1A, shogi.variant.Standard) must beSquares(SQ1B, SQ2B, SQ2A)
    }

    "move behind pawn barrier" in {
      """
P P P P P P P P P
L N . G K . . N L""" moveDestsFrom SQ5I must beSquares(SQ4I)
    }

    "not move to squares that are occupied by the same colour" in {
      val position = """
. . . P . . . . .
N P K L . . . P .
. . . . . . . . .
P . P . P P P . P
. . . . . . . . .
. . S G K G S N L
Hands:
Turn:Sente
"""
      position moveDestsFrom SQ7E must beSquares(
        position,
        """
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. x x P . . . . .
N P K L . . . P .
. x x x . . . . .
P . P . P P P . P
. . . . . . . . .
. . S G K G S N L
Hands:
Turn:Sente
""",
      )
    }

    "capture hanging opponent pieces" in {
      val position = """
k . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. b p l . . . . .
. . K p . . . . .
. p . . . . . . .
l . . . . . . . .
Hands:
Turn:Sente
"""
      position moveDestsFrom SQ7G must beSquares(
        position,
        """
k . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. x x x . . . . .
. x K p . . . . .
. x x . . . . . .
l . . . . . . . .
Hands:
Turn:Sente
""",
      )
    }
    "threaten" in {
      val position = """
k . B . . . . . .
. . . . . . . . .
. b . B . . . . .
b p p . . . . . .
. . K b . . . . .
. . P . R . . . .
P P . . . P P P P
. . . . . . . . .
L N S G . G S N L
Hands:
Turn:Sente
"""
      "a reachable enemy" in {
        position moveActorAt SQ7E map (_ threatens SQ8D) must beSome(true)
      }
      "an unreachable enemy" in {
        position moveActorAt SQ7E map (_ threatens SQ9D) must beSome(false)
      }
      "a reachable friend" in {
        position moveActorAt SQ7E map (_ threatens SQ7F) must beSome(true)
      }
    }
    "not move near from the other king" in {
      """
. . . k
. K . .
""" moveDestsFrom SQ8I must beSquares(SQ9I, SQ9H, SQ8H)
    }
  }
}
