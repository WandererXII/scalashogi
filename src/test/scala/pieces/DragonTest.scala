package shogi
package pieces

import shogi.Square._

class DragonTest extends ShogiTest {

  "a dragon" should {

    val dragon     = Sente - Dragon
    val dragonGote = Gote - Dragon

    "move to any square along the same rank or file and directly around" in {
      pieceMoves(dragon, SQ5E, shogi.variant.Standard) must beSquares(
        SQ5D,
        SQ5C,
        SQ5B,
        SQ5A,
        SQ5F,
        SQ5G,
        SQ5H,
        SQ5I,
        SQ4E,
        SQ3E,
        SQ2E,
        SQ1E,
        SQ6E,
        SQ7E,
        SQ8E,
        SQ9E,
        SQ6F,
        SQ6D,
        SQ4F,
        SQ4D,
      )
      pieceMoves(dragonGote, SQ5E, shogi.variant.Standard) must beSquares(
        SQ5D,
        SQ5C,
        SQ5B,
        SQ5A,
        SQ5F,
        SQ5G,
        SQ5H,
        SQ5I,
        SQ4E,
        SQ3E,
        SQ2E,
        SQ1E,
        SQ6E,
        SQ7E,
        SQ8E,
        SQ9E,
        SQ6F,
        SQ6D,
        SQ4F,
        SQ4D,
      )
    }

    "move to any square along the same rank or file, even when at the edges" in {
      pieceMoves(dragon, SQ1A, shogi.variant.Standard) must beSquares(
        SQ1B,
        SQ1C,
        SQ1D,
        SQ1E,
        SQ1F,
        SQ1G,
        SQ1H,
        SQ1I,
        SQ2A,
        SQ3A,
        SQ4A,
        SQ5A,
        SQ6A,
        SQ7A,
        SQ8A,
        SQ9A,
        SQ2B,
      )
    }

    "not move to squares that are occupied by the same colour" in {
      """
k . B . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
N .+R . . . . P .
. . . . . . . . .
P P P P P P P . P
. . . . . . . . .
. . . . K . . . .
Hands:
Turn:Sente
""" moveDestsFrom SQ7E must beSquares(
        SQ7F,
        SQ7D,
        SQ7C,
        SQ7B,
        SQ8E,
        SQ6E,
        SQ5E,
        SQ4E,
        SQ3E,
        SQ8F,
        SQ8D,
        SQ6F,
        SQ6D,
      )
    }

    "capture opponent pieces" in {
      """
k . . . . . . . .
. . b . . . . . .
. . . . . . . . .
. . . . . . . . .
n .+R . . . p . .
. . . . . . . . .
P P P P P P P P P
. . . . . . . . .
. . . . K . . . .
Hands:
Turn:Sente
""" moveDestsFrom SQ7E must beSquares(
        SQ7F,
        SQ7D,
        SQ7C,
        SQ7B,
        SQ8E,
        SQ9E,
        SQ6E,
        SQ5E,
        SQ4E,
        SQ3E,
        SQ8F,
        SQ8D,
        SQ6F,
        SQ6D,
      )
    }
    "threaten" in {
      val position = """
k . B . . . . . .
. . r . . r . . .
p . . . . . . . .
. . . . . . . . .
n .+R . . . . P .
. . . . . . . . .
P P P P P P P . P
. . . . . . . . .
. . . . K . . . .
Hands:
Turn:Sente
"""
      "a reachable enemy to the left" in {
        position moveActorAt SQ7E map (_ threatens SQ9E) must beSome(true)
      }
      "a reachable enemy to the top" in {
        position moveActorAt SQ7E map (_ threatens SQ7B) must beSome(true)
      }
      "an unreachable enemy" in {
        position moveActorAt SQ7E map (_ threatens SQ9C) must beSome(false)
      }
      "a reachable friend" in {
        position moveActorAt SQ7E map (_ threatens SQ2E) must beSome(true)
      }
      "nothing left" in {
        position moveActorAt SQ7E map (_ threatens SQ8E) must beSome(true)
      }
      "nothing up" in {
        position moveActorAt SQ7E map (_ threatens SQ7D) must beSome(true)
      }
    }
  }
}
