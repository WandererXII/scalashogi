package shogi
package pieces

import Pos._

class PrinceTest extends ShogiTest {

  "a prince" should {

    val prince = Sente - Prince

    "move 1 position in any direction" in {
      pieceMoves(prince, SQ6F) must bePoss(SQ6G, SQ7G, SQ7F, SQ7E, SQ6E, SQ5E, SQ5F, SQ5G)
    }

    "move 1 position in any direction, even from the edges" in {
      pieceMoves(prince, SQ1A) must bePoss(SQ1B, SQ2B, SQ2A)
    }

    "move behind pawn barrier" in {
      """
P P P P P P P P P P P P
L N . G+E . . . . . N L""" moveDestsFrom SQ8L must bePoss(SQ7L)
    }

    "not move to positions that are occupied by the same colour" in {
      val situation = """
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
      situation moveDestsFrom SQ10E must bePoss(
        situation,
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
"""
      )
    }


    "threaten" in {
      val situation = """
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
        situation moveActorAt SQ10E map (_ threatens SQ11D) must beSome(true)
      }
      "an unreachable enemy" in {
        situation moveActorAt SQ10E map (_ threatens SQ12D) must beSome(false)
      }
      "a reachable friend" in {
        situation moveActorAt SQ10E map (_ threatens SQ10F) must beSome(true)
      }
    }
  }
}
