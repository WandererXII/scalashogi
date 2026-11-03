package shogi

import shogi.format.forsyth.Sfen
import shogi.variant.Standard

class DrawTest extends ShogiTest {

  "detect draw" should {
    "by lack of pieces" in {
      "empty" in {
        makeEmptyPosition(Standard).isInsufficientMaterial must beTrue
        makePosition(Standard).status.contains(Status.Repetition) must beFalse
      }
      "new" in {
        makePosition(Standard).status must beNone
      }
      "opened" in {
        makePosition(Standard)
          .playUsisStr(List("5g5f", "5c5d", "7g7f", "5d5e", "5f5e"))
          .toOption
          .flatMap(_.status) must beNone
      }
      "two kings with nothing in hand" in {
        """
. . . . . . k
K . . . . . .""".status.contains(Status.Draw) must beTrue
      }
      "one pawn" in {
        """
. . P . . . k
K . . . . . .""".status.contains(Status.Draw) must beFalse
      }
      "one bishop" in {
        """
. . . . . . k
K . . . . B .""".status.contains(Status.Draw) must beFalse
      }
      "one knight" in {
        """
. . . . . . k
K . . . . N .""".status.contains(Status.Draw) must beFalse
      }
    }
  }
  "do not detect insufficient material" should {
    "on two kings with something in hand" in {
      val sfen = Sfen("4k4/9/9/9/9/9/9/9/5K3 b p 1")
      sfenToPosition(sfen, Standard) must beValid.like { case position =>
        position.status.contains(Status.Draw) must beFalse
        position.end must beFalse
      }
    }
    "on a single pawn" in {
      val sfen      = Sfen("2p2k3/9/9/9/9/9/9/9/4K4 b - 1")
      val validated = sfenToPosition(sfen, Standard)
      val played    = validated flatMap (_.playUsiStr("5i5h"))
      played must beValid.like { case position =>
        position.status.contains(Status.Draw) must beFalse
        position.end must beFalse
      }
    }
  }
}
