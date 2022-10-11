package shogi

import Pos._

class PerpetualCheckTest extends ShogiTest {

  "Perpetual check" should {
    val g = makeGame.playMoves(
      (SQ5G, SQ5F, false),
      (SQ5C, SQ5D, false),
      (SQ2H, SQ5H, false),
      (SQ5A, SQ5B, false),
      (SQ5F, SQ5E, false),
      (SQ5B, SQ5C, false),
      (SQ5H, SQ5F, false),
      (SQ5C, SQ6D, false)
    )
    val m = List(
      (SQ5F, SQ6F, false),
      (SQ6D, SQ7D, false),
      (SQ6F, SQ7F, false),
      (SQ7D, SQ6D, false),
      (SQ7F, SQ6F, false),
      (SQ6D, SQ7D, false),
      (SQ6F, SQ7F, false),
      (SQ7D, SQ6D, false),
      (SQ7F, SQ6F, false),
      (SQ6D, SQ7D, false),
      (SQ6F, SQ7F, false),
      (SQ7D, SQ6D, false),
      (SQ7F, SQ6F, false)
    )
    val mi = List(
      (SQ5F, SQ6F, false),
      (SQ6D, SQ7D, false),
      (SQ6F, SQ7F, false),
      (SQ7D, SQ6D, false),
      (SQ7F, SQ6F, false),
      (SQ6D, SQ7D, false),
      (SQ6F, SQ7F, false),
      (SQ7D, SQ6D, false),
      (SQ7I, SQ6H, false),
      (SQ3A, SQ4B, false),
      (SQ6H, SQ7I, false),
      (SQ4B, SQ3A, false),
      (SQ7F, SQ6F, false),
      (SQ6D, SQ7D, false),
      (SQ6F, SQ7F, false),
      (SQ7D, SQ6D, false),
      (SQ7F, SQ6F, false)
    )
    "not trigger" in {
      "after 2 repetitions" in {
        g must beValid.like { case game =>
          game.playMoveList(m take 5) must beValid.like { case game2 =>
            game2.situation.autoDraw must beFalse
            game2.situation.perpetualCheck must beFalse
            game2.situation.winner must beNone
          }
        }
      }
      "after 3 repetitions" in {
        g must beValid.like { case game =>
          game.playMoveList(m take 9) must beValid.like { case game2 =>
            game2.situation.autoDraw must beFalse
            game2.situation.perpetualCheck must beFalse
            game2.situation.winner must beNone
          }
        }
      }
      "if the checks weren't consecutive" in {
        g must beValid.like { case game =>
          game.playMoveList(mi) must beValid.like { case game2 =>
            game2.situation.perpetualCheck must beFalse
            game2.situation.autoDraw must beTrue
            game2.situation.winner must beNone
          }
        }
      }
    }
    "trigger" in {
      "after 4 repetitions" in {
        g must beValid.like { case game =>
          game.playMoveList(m) must beValid.like { case game2 =>
            game2.situation.autoDraw must beFalse
            game2.situation.perpetualCheck must beTrue
            game2.situation.winner must beSome.like { case color =>
              color.gote
            }
          }
        }
      }
    }
  }
}
