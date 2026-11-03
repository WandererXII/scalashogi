package shogi
package pieces

import shogi.Square._

class KnightTest extends ShogiTest {

  "a knight" should {

    val knight     = Sente - Knight
    val knightGote = Gote - Knight

    "move in any of 2 squares" in {
      pieceMoves(knight, SQ5E, shogi.variant.Standard) must beSquares(SQ6C, SQ4C)
      pieceMoves(knightGote, SQ5E, shogi.variant.Standard) must beSquares(SQ6G, SQ4G)
    }

    "move in 1 one square when at the edges" in {
      pieceMoves(knight, SQ1D, shogi.variant.Standard) must beSquares(SQ2B)
    }

    "not move to squares that are occupied by the same colour" in {
      val position = """
k . B . . . . . .
. . . . . . . . .
. . . B . . . . .
. . . . P . . . .
. . N . . . . . .
. . . P . . . . .
P P P . . P P P P
. . . . . . . . .
. . . . K . . . .
Hands:
Turn:Sente
"""
      position moveDestsFrom SQ7E must beSquares(
        position,
        """
k . B . . . . . .
. . . . . . . . .
. x . B . . . . .
. . . . P . . . .
. . N . . . . . .
. . . P . . . . .
P P P . . P P P P
. . . . . . . . .
. . . . K . . . .
Hands:
Turn:Sente
""",
      )
    }

    "capture opponent pieces" in {
      val position = """
k . B . . . . . .
. . . . . . . . .
. b . B . . . . .
n . . . . . . . .
. . N . . . . . .
. . . . b . . . .
P P P . . P P P P
. . . . . . . . .
. . . . K . . . .
Hands:
Turn:Sente
"""
      position moveDestsFrom SQ7E must beSquares(
        position,
        """
k . B . . . . . .
. . . . . . . . .
. x . B . . . . .
n . . . . . . . .
. . N . . . . . .
. . . . b . . . .
P P P . . P P P P
. . . . . . . . .
. . . . K . . . .
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
n . . . . . . . .
. . N . . . . . .
. . . . R . . . .
P P P . . P P P P
. . . . . . . . .
. . . . K . . . .
Hands:
Turn:Sente
"""
      "a reachable enemy" in {
        position moveActorAt SQ7E map (_ threatens SQ8C) must beSome(true)
      }
      "an unreachable enemy" in {
        position moveActorAt SQ7E map (_ threatens SQ9D) must beSome(false)
      }
      "an unreachable friend" in {
        position moveActorAt SQ7E map (_ threatens SQ5F) must beSome(false)
      }
      "nothing left" in {
        position moveActorAt SQ7E map (_ threatens SQ8E) must beSome(false)
      }
      "nothing up" in {
        position moveActorAt SQ7E map (_ threatens SQ7D) must beSome(false)
      }
    }
  }
}
