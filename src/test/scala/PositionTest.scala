package shogi

import scala.language.reflectiveCalls

class PositionTest extends ShogiTest {

  "a position" should {
    "detect check" should {
      "by rook" in {
        """
K . . r . . . . .
""".check must beTrue
      }
      "by knight" in {
        """
. n . . . . . . .
. . . . . . . . .
K . . . . . . . .
""".check must beTrue
      }
      "by bishop" in {
        """
. . b . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . K . . .
""".check must beTrue
      }
      "by pawn" in {
        """
. . . . p . . . .
. . . . K . . . .
""".check must beTrue
      }
      "not" in {
        """
K . . . . . . . .
. . . . . . . . .
.+n . . . . . . .
""".check must beFalse
      }
    }
    "detect check mate" in {
      "by rook" in {
        """
P P . . . . . . .
K . . r . . . . .
""".status must_== Some(Status.Mate)
      }
      "by knight" in {
        """
. n . . . . . . .
P B . . . . . . .
K R . . . . . . .
""".status must_== Some(Status.Mate)
      }
      "not" in {
        """
. n . . . . . . .
. . . . . . . . .
K . . . . . . . .
""".status must beNone
      }
    }
    "stale mate" in {
      "stuck in a corner" in {
        """
b r r . . . . . .
K . . . . . . . .
""".status must_== Some(Status.Stalemate)
      }
      "not" in {
        """
. . b . . . . . .
K . . . . . . . .
""".status must beNone
      }
    }

    "Give the correct winner for a position" in {
      val position =
        """
P P . . . . . . .
K . . r . . . . .
"""

      position.status must_== Some(Status.Mate)
      position.winner must beSome.like { case color =>
        color.gote
      }
    }

    "Not give a winner if the game is still in progress" in {
      val position = """
. . . . p . . . .
. . . . . K . . .
"""

      position.winner must beNone

    }

    "not be playable" in {
      "with touching kings" in {
        val position = "k K . B N . . . ." as Gote
        position.playable(strict = true) must beFalse
        position.playable(strict = false) must beFalse
      }

      "with other side in check" in {
        val position = "k . R . K . . . ."
        position.playable(strict = true) must beFalse
        position.playable(strict = false) must beFalse
      }

      "with doubled pawns" in {
        val position = """
k . . . . . . . .
. . . . . . . . .
P p . . . . . . .
P . . . . . . . .
K . . . . . . . .
"""
        position.playable(strict = true) must beFalse
        position.playable(strict = false) must beFalse
      }
    }

  }
}
