package shogi

import Pos._
import variant.Standard
import format.forsyth.Sfen

class DrawTest extends ShogiTest {

  "detect draw" should {
    "by lack of pieces" in {
      "empty" in {
        makeEmptySituation(Standard).draw must_== true
        makeSituation(Standard).repetition must_== false
      }
      "new" in {
        makeSituation(Standard).draw must_== false
        makeSituation(Standard).repetition must_== false
      }
      "opened" in {
        makeGame(Standard).playMoves(
          (SQ5G, SQ5F, false),
          (SQ5C, SQ5D, false),
          (SQ7G, SQ7F, false),
          (SQ5D, SQ5E, false),
          (SQ5F, SQ5E, false)
        ) map { g =>
          g.situation.draw || g.situation.repetition
        } must beValid.like(_ must beFalse)
      }
      "two kings with nothing in hand" in {
        """
. . . . . . k
K . . . . . .""".draw must_== true
      }
      "one pawn" in {
        """
. . P . . . k
K . . . . . .""".draw must_== false
      }
      "one bishop" in {
        """
. . . . . . k
K . . . . B .""".draw must_== false
      }
      "one knight" in {
        """
. . . . . . k
K . . . . N .""".draw must_== false
      }
    }
  }
  "do not detect insufficient material" should {
    "on two kings with something in hand" in {
      val position = Sfen("4k4/9/9/9/9/9/9/9/5K3 b p 1")
      sfenToGame(position, Standard) must beValid.like { case game =>
        game.situation.draw must beFalse
        game.situation.end(true) must beFalse
      }
    }
    "on a single pawn" in {
      val position = Sfen("2p2k3/9/9/9/9/9/9/9/4K4 b - 1")
      val game     = sfenToGame(position, Standard)
      val newGame = game flatMap (_.playMove(
        Pos.SQ5I,
        Pos.SQ5H
      ))
      newGame must beValid.like { case game =>
        game.situation.draw must beFalse
        game.situation.end(true) must beFalse
      }
    }
  }
}
