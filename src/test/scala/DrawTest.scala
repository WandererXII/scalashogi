package shogi

import shogi.format.forsyth.Sfen
import shogi.variant.Standard

class DrawTest extends ShogiTest {

  "detect draw" should {
    "by lack of pieces" in {
      "empty" in {
        makeEmptySituation(Standard).isInsufficientMaterial must beTrue
        makeSituation(Standard).status.contains(Status.Repetition) must beFalse
      }
      "new" in {
        makeSituation(Standard).status must beNone
      }
      "opened" in {
        makeGame(Standard)
          .playUsisStr(List("5g5f", "5c5d", "7g7f", "5d5e", "5f5e"))
          .toOption
          .flatMap(_.situation.status) must beNone
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
      val position = Sfen("4k4/9/9/9/9/9/9/9/5K3 b p 1")
      sfenToGame(position, Standard) must beValid.like { case game =>
        game.situation.status.contains(Status.Draw) must beFalse
        game.situation.end must beFalse
      }
    }
    "on a single pawn" in {
      val position = Sfen("2p2k3/9/9/9/9/9/9/9/4K4 b - 1")
      val game     = sfenToGame(position, Standard)
      val newGame  = game flatMap (_.playUsiStr("5i5h"))
      newGame must beValid.like { case game =>
        game.situation.status.contains(Status.Draw) must beFalse
        game.situation.end must beFalse
      }
    }
  }
}
