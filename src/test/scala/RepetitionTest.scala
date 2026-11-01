package shogi

import shogi.format.forsyth.Sfen
import shogi.variant.Chushogi
import shogi.variant.Kyotoshogi
import shogi.variant.Minishogi
import shogi.variant.Standard

class RepetitionTest extends ShogiTest {

  "detect repetition" should {
    val state = makeState(Standard)
    "chushogi fourfold after six passes" in {
      val state = State(
        Some(Sfen("k11/12/6n5/12/12/12/12/12/12/5N6/12/11K b - 1")),
        Chushogi,
      )
      val usis = List.fill(3)(List("7j7i7j", "6c6d6c")).flatten
      "should be fourfold" in {
        state.playUsisStr(usis) must beValid.like { case state =>
          state.history.fourfoldRepetition must beTrue
          state.situation.status.contains(Status.Repetition) must beTrue
          state.situation.winner must beNone
        }
      }
      "should not end before the fourth occurrence" in {
        state.playUsisStr(usis.dropRight(1)) must beValid.like { case state =>
          state.history.fourfoldRepetition must beFalse
          state.situation.status must beNone
        }
      }
    }
    "standard fourfold" in {
      val usis = List(
        "2h3h",
        "8b7b",
        "3h2h",
        "7b8b",
        "2h3h",
        "8b7b",
        "3h2h",
        "7b8b",
        "2h3h",
        "8b7b",
        "3h2h",
        "7b8b",
      )
      "should be fourfold" in {
        state.playUsisStr(usis) must beValid.like { case state =>
          state.situation.status.contains(Status.Repetition) must beTrue
          state.situation.winner must beNone
        }
      }
      "should not be fourfold" in {
        state.playUsisStr(usis.dropRight(1)) must beValid.like { case state =>
          state.situation.status must beNone
          state.situation.winner must beNone
        }
      }
    }
    "minishogi repetition" in {
      val state =
        makeState(Minishogi).withHistory(History.empty.withInitialSfen(Minishogi.initialSfen))
      val usis = List(
        "4e4d",
        "2a2b",
        "4d4e",
        "2b2a",
        "4e4d",
        "2a2b",
        "4d4e",
        "2b2a",
        "4e4d",
        "2a2b",
        "4d4e",
        "2b2a",
      )
      "should be fourfold" in {
        state.playUsisStr(usis) must beValid.like { case state =>
          state.situation.status.contains(Status.Repetition) must beTrue
          state.situation.winner must_== Some(Gote)
          val stateNoInitialSfen = state.withHistory(state.history.copy(initialSfen = None))
          stateNoInitialSfen.situation.winner must_== Some(Gote)
          val stateOppositeColorSfen =
            state.withHistory(state.history.withInitialSfen(Sfen("rbsgk/4p/5/P4/KGSBR w - 1")))
          stateOppositeColorSfen.situation.winner must_== Some(Sente)
        }
      }
      "should not be fourfold" in {
        state.playUsisStr(usis.dropRight(1)) must beValid.like { case state =>
          state.situation.status must beNone
          state.situation.winner must beNone
        }
      }
    }
    "kyotoshogi threefold" in {
      val state = makeState(Kyotoshogi)
      val usis  = List(
        "4e3d+",
        "2a3b+",
        "3d4e+",
        "3b2a+",
        "4e3d+",
        "2a3b+",
        "3d4e+",
        "3b2a+",
      )
      "should be threefold" in {
        state.playUsisStr(usis) must beValid.like { case state =>
          state.situation.status.contains(Status.Repetition) must beTrue
          state.situation.winner must beNone
        }
      }
      "should not be threefold" in {
        state.playUsisStr(usis.dropRight(1)) must beValid.like { case state =>
          state.situation.status must beNone
          state.situation.winner must beNone
        }
      }
    }
  }
}
