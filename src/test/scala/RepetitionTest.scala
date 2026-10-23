package shogi

import shogi.format.forsyth.Sfen
import shogi.variant.Chushogi
import shogi.variant.Kyotoshogi
import shogi.variant.Minishogi
import shogi.variant.Standard

class RepetitionTest extends ShogiTest {

  "detect repetition" should {
    val game = makeGame(Standard)
    "chushogi fourfold after six passes" in {
      val game = Game(
        Some(Sfen("k11/12/6n5/12/12/12/12/12/12/5N6/12/11K b - 1")),
        Chushogi,
      )
      val usis = List.fill(3)(List("7j7i7j", "6c6d6c")).flatten
      "should be fourfold" in {
        game.playUsisStr(usis) must beValid.like { case g =>
          g.history.fourfoldRepetition must beTrue
          g.situation.status.contains(Status.Repetition) must beTrue
          g.situation.winner must beNone
        }
      }
      "should not end before the fourth occurrence" in {
        game.playUsisStr(usis.dropRight(1)) must beValid.like { case g =>
          g.history.fourfoldRepetition must beFalse
          g.situation.status must beNone
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
        game.playUsisStr(usis) must beValid.like { case g =>
          g.situation.status.contains(Status.Repetition) must beTrue
          g.situation.winner must beNone
        }
      }
      "should not be fourfold" in {
        game.playUsisStr(usis.dropRight(1)) must beValid.like { case g =>
          g.situation.status must beNone
          g.situation.winner must beNone
        }
      }
    }
    "minishogi repetition" in {
      val game =
        makeGame(Minishogi).withHistory(History.empty.withInitialSfen(Minishogi.initialSfen))
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
        game.playUsisStr(usis) must beValid.like { case g =>
          g.situation.status.contains(Status.Repetition) must beTrue
          g.situation.winner must_== Some(Gote)
          val gNoInitialSfen = g.withHistory(g.history.copy(initialSfen = None))
          gNoInitialSfen.situation.winner must_== Some(Gote)
          val gOppositeColorSfen =
            g.withHistory(g.history.withInitialSfen(Sfen("rbsgk/4p/5/P4/KGSBR w - 1")))
          gOppositeColorSfen.situation.winner must_== Some(Sente)
        }
      }
      "should not be fourfold" in {
        game.playUsisStr(usis.dropRight(1)) must beValid.like { case g =>
          g.situation.status must beNone
          g.situation.winner must beNone
        }
      }
    }
    "kyotoshogi threefold" in {
      val game = makeGame(Kyotoshogi)
      val usis = List(
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
        game.playUsisStr(usis) must beValid.like { case g =>
          g.situation.status.contains(Status.Repetition) must beTrue
          g.situation.winner must beNone
        }
      }
      "should not be threefold" in {
        game.playUsisStr(usis.dropRight(1)) must beValid.like { case g =>
          g.situation.status must beNone
          g.situation.winner must beNone
        }
      }
    }
  }
}
