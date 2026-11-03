package shogi
package pieces

import shogi.Square._

class SilverTest extends ShogiTest {

  "a silver general" should {

    val silver     = Sente - Silver
    val silverGote = Gote - Silver

    "move in 6 directions" in {
      pieceMoves(silver, SQ5E, shogi.variant.Standard) must beSquares(SQ5D, SQ4D, SQ4F, SQ6F, SQ6D)
      pieceMoves(silverGote, SQ5E, shogi.variant.Standard) must beSquares(
        SQ5F,
        SQ4F,
        SQ4D,
        SQ6D,
        SQ6F,
      )
    }

    "move in 2 directions, when at the edges" in {
      pieceMoves(silver, SQ1A, shogi.variant.Standard) must beSquares(SQ2B)
    }

    "not move to squares that are occupied by the same colour" in {
      val position = """
k . B . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. N S . . . . P .
. G . . . . . . .
P P P P P P P . P
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
. . . . . . . . .
. x x x . . . . .
. N S . . . . P .
. G . x . . . . .
P P P P P P P . P
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
. . . . . . . . .
. . p . . . . . .
N . S . . . . P .
. . . . . . . . .
P P P P P P P . P
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
. . . . . . . . .
. x x x . . . . .
N . S . . . . P .
. x . x . . . . .
P P P P P P P . P
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
. . r . . r . . .
p . . . . . . . .
. . p . . . . . .
. N S . . . . P .
. . . P . . . . .
P P P . P P P . P
. . . . . . . . .
. . . . K . . . .
Hands:
Turn:Sente
"""
      "a reachable enemy" in {
        position moveActorAt SQ7E map (_ threatens SQ7D) must beSome(true)
      }
      "an unreachable enemy" in {
        position moveActorAt SQ7E map (_ threatens SQ9C) must beSome(false)
      }
      "a reachable friend" in {
        position moveActorAt SQ7E map (_ threatens SQ6F) must beSome(true)
      }
      "nothing down left" in {
        position moveActorAt SQ7E map (_ threatens SQ8F) must beSome(true)
      }
      "nothing left up" in {
        position moveActorAt SQ7E map (_ threatens SQ8D) must beSome(true)
      }
      "nothing right up" in {
        position moveActorAt SQ7E map (_ threatens SQ6D) must beSome(true)
      }
    }
  }
}
