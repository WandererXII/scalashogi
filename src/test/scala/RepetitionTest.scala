package shogi

import Pos._
import variant.{ Kyotoshogi, Minishogi, Standard }

class RepetitionTest extends ShogiTest {

  "detect repetition" should {
    val game = makeGame(Standard)
    "standard fourfold" in {
      val moves = List(
        (SQ2H, SQ3H, false),
        (SQ8B, SQ7B, false),
        (SQ3H, SQ2H, false),
        (SQ7B, SQ8B, false),
        (SQ2H, SQ3H, false),
        (SQ8B, SQ7B, false),
        (SQ3H, SQ2H, false),
        (SQ7B, SQ8B, false),
        (SQ2H, SQ3H, false),
        (SQ8B, SQ7B, false),
        (SQ3H, SQ2H, false),
        (SQ7B, SQ8B, false)
      )
      "should be fourfold" in {
        game.playMoves(moves: _*) must beValid.like { case g =>
          g.situation.draw must beFalse
          g.situation.repetition must beTrue
          g.situation.perpetualCheck must beFalse
          g.situation.winner must beNone
        }
      }
      "should not be fourfold" in {
        game.playMoves(moves.dropRight(1): _*) must beValid.like { case g =>
          g.situation.draw must beFalse
          g.situation.repetition must beFalse
          g.situation.perpetualCheck must beFalse
          g.situation.winner must beNone
        }
      }
    }
    "minishogi repetition" in {
      val game = makeGame(Minishogi).withHistory(History.empty.withInitialSfen(Minishogi.initialSfen))
      val moves = List(
        (SQ4E, SQ4D, false),
        (SQ2A, SQ2B, false),
        (SQ4D, SQ4E, false),
        (SQ2B, SQ2A, false),
        (SQ4E, SQ4D, false),
        (SQ2A, SQ2B, false),
        (SQ4D, SQ4E, false),
        (SQ2B, SQ2A, false),
        (SQ4E, SQ4D, false),
        (SQ2A, SQ2B, false),
        (SQ4D, SQ4E, false),
        (SQ2B, SQ2A, false)
      )
      "should be fourfold" in {
        game.playMoves(moves: _*) must beValid.like { case g =>
          g.situation.draw must beFalse
          g.situation.repetition must beTrue
          g.situation.perpetualCheck must beFalse
          g.situation.winner must_== Some(Gote)
        }
      }
      "should not be fourfold" in {
        game.playMoves(moves.dropRight(1): _*) must beValid.like { case g =>
          g.situation.draw must beFalse
          g.situation.repetition must beFalse
          g.situation.perpetualCheck must beFalse
          g.situation.winner must beNone
        }
      }
    }
    "kyotoshogi threefold" in {
      val game = makeGame(Kyotoshogi)
      val moves = List(
        (SQ4E, SQ3D, true),
        (SQ2A, SQ3B, true),
        (SQ3D, SQ4E, true),
        (SQ3B, SQ2A, true),
        (SQ4E, SQ3D, true),
        (SQ2A, SQ3B, true),
        (SQ3D, SQ4E, true),
        (SQ3B, SQ2A, true)
      )
      "should be threefold" in {
        game.playMoves(moves: _*) must beValid.like { case g =>
          g.situation.draw must beFalse
          g.situation.repetition must beTrue
          g.situation.perpetualCheck must beFalse
          g.situation.winner must beNone
        }
      }
      "should not be threefold" in {
        game.playMoves(moves.dropRight(1): _*) must beValid.like { case g =>
          g.situation.draw must beFalse
          g.situation.repetition must beFalse
          g.situation.perpetualCheck must beFalse
          g.situation.winner must beNone
        }
      }
    }
  }
}
