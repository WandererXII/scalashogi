package shogi

import Pos._
import format.forsyth.Sfen
import format.usi.Usi

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
            game2.situation.winner must_== Some(Gote)
          }
        }
      }
    }
  }

  "Chushogi perpetual check" should {
    "not trigger" in {
      val dGame = Game(shogi.variant.Chushogi)
      val dMoves = List(
        "4h4g",
        "9e9f",
        "4g4h",
        "9f9e", // first repetition
        "4h4g",
        "9e9f",
        "4g4h",
        "9f9e", // second repetition
        "4h4g",
        "9e9f",
        "4g4h",
        "9f9e", // third repetition
        "4h4g",
        "9e9f",
        "4g4h",
        "9f9e" // forth repetition
      ).map(Usi.Move(_).get)
      dGame.playMoveList(dMoves.map(u => (u.orig, u.dest, false))) must beValid.like { case game =>
        game.situation.autoDraw must beTrue
        game.situation.perpetualCheck must beFalse
        game.situation.winner must beNone
      }
    }
    "trigger" in {
      val aGame = Game(
        Some(
          Sfen(
            "lfcsgek1scfl/a1b1txot1b1a/mvrhdqndhrvm/pppppppppppp/8i3/3I8/3g8/8I3/PPPPPPPPPPPP/MVRHDNQDHRVM/A1B1TOXT1B1A/LFCSGKEGSCFL w"
          )
        ),
        shogi.variant.Chushogi
      )
      val aMoves = List(
        "1d1e",
        "9f9e", // start
        "4e4f",
        "9e9f",
        "4f4e",
        "9f9e", // 1st
        "4e4f",
        "9e9f",
        "4f4e",
        "9f9e", // 2nd
        "4e4f",
        "9e9f",
        "4f4e",
        "9f9e", // 3rd
        "4e4f",
        "9e9f",
        "4f4e",
        "9f9e"
      ).map(Usi.Move(_).get)
      aGame.playMoveList(aMoves.map(u => (u.orig, u.dest, false))) must beValid.like { case game =>
        game.situation.autoDraw must beFalse
        game.situation.perpetualCheck must beTrue
        game.situation.winner must_== Some(Gote)
      }
    }
  }
}
