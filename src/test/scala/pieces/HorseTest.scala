package shogi
package pieces

import shogi.Square._

class HorseTest extends ShogiTest {

  "a horse" should {

    val horse     = Sente - Horse
    val horseGote = Gote - Horse

    "move diagonally in 4 directions and all around" in {
      pieceMoves(horse, SQ5E, shogi.variant.Standard) must beSquares(
        SQ4F,
        SQ4D,
        SQ3G,
        SQ3C,
        SQ2H,
        SQ2B,
        SQ1I,
        SQ1A,
        SQ6F,
        SQ6D,
        SQ7G,
        SQ7C,
        SQ8H,
        SQ8B,
        SQ9I,
        SQ9A,
        SQ6E,
        SQ4E,
        SQ5D,
        SQ5F,
      )
      pieceMoves(horseGote, SQ5E, shogi.variant.Standard) must beSquares(
        SQ4F,
        SQ4D,
        SQ3G,
        SQ3C,
        SQ2H,
        SQ2B,
        SQ1I,
        SQ1A,
        SQ6F,
        SQ6D,
        SQ7G,
        SQ7C,
        SQ8H,
        SQ8B,
        SQ9I,
        SQ9A,
        SQ6E,
        SQ4E,
        SQ5D,
        SQ5F,
      )
    }

    "move in 2 directions, when at the edges" in {
      pieceMoves(horse, SQ1C, shogi.variant.Standard) must beSquares(
        SQ2B,
        SQ3A,
        SQ2D,
        SQ3E,
        SQ4F,
        SQ5G,
        SQ6H,
        SQ7I,
        SQ2C,
        SQ1D,
        SQ1B,
      )
    }

    "not move to squares that are occupied by the same colour" in {
      val position = """
k . B . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
N .+B . . . . P .
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
k . B . . . x . .
. . . . . x . . .
x . . . x . . . .
. x x x . . . . .
N x+B x . . . P .
. x x x . . . . .
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
. . . . . r . . .
p . . . . . . . .
. . . . . . . . .
N .+B . . . . P .
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
. . . . . x . . .
x . . . x . . . .
. x x x . . . . .
N x+B x . . . P .
. x x x . . . . .
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
. . r . r . . . .
p . . . . . . . .
. . . . . . . . .
N .+B . . . . P .
. . . . . . . . .
P P P P P P P . P
. . . . . . . . .
. . . . K . . . .
Hands:
Turn:Sente
"""
      "a reachable enemy" in {
        position moveActorAt SQ7E map (_ threatens SQ9C) must beSome(true)
      }
      "an unreachable enemy" in {
        position moveActorAt SQ7E map (_ threatens SQ7B) must beSome(false)
      }
      "a reachable friend" in {
        position moveActorAt SQ7E map (_ threatens SQ9G) must beSome(true)
      }
      "nothing up left" in {
        position moveActorAt SQ7E map (_ threatens SQ8D) must beSome(true)
      }
      "nothing down right" in {
        position moveActorAt SQ7E map (_ threatens SQ6F) must beSome(true)
      }
    }
  }
}
