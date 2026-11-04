package shogi

import shogi.format.forsyth.Sfen
import shogi.variant.Chushogi
import shogi.variant.Kyotoshogi
import shogi.variant.Minishogi
import shogi.variant.Standard

class RepetitionTest extends ShogiTest {

  "detect repetition" should {
    val position = makePosition(Standard)
    "chushogi fourfold after six passes" in {
      val position = Position(
        Some(Sfen("k11/12/6n5/12/12/12/12/12/12/5N6/12/11K b - 1")),
        Chushogi,
      )
      val usis = List.fill(3)(List("7j7i7j", "6c6d6c")).flatten
      "should be fourfold" in {
        position.playUsisStr(usis) must beValid.like { case played =>
          played.history.fourfoldRepetition must beTrue
          played.status.contains(Status.Repetition) must beTrue
          played.winner must beNone
        }
      }
      "should not end before the fourth occurrence" in {
        position.playUsisStr(usis.dropRight(1)) must beValid.like { case played =>
          played.history.fourfoldRepetition must beFalse
          played.status must beNone
        }
      }
    }
    "chushogi fourfold after lion-capture rights expire" in {
      val before = Sfen("11k/12/12/12/6n5/6P5/12/5+h6/5N6/12/12/K11 b - 1")
        .toPosition(Chushogi)
        .get
      val start = before.playUsisStr(List("6f6e", "1a1b")).toOption.get
      val cycle = List("12l12k", "1b1c", "12k12l", "1c1b")

      forall(1 to 3) { repeats =>
        start.playUsisStr(List.fill(repeats)(cycle).flatten) must beValid.like { case played =>
          played.toSfenWithoutMoveNumber must_== start.toSfenWithoutMoveNumber
          played.history.fourfoldRepetition must_== (repeats == 3)
          played.status must_== (if (repeats == 3) Some(Status.Repetition) else None)
          played.winner must beNone
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
        position.playUsisStr(usis) must beValid.like { case played =>
          played.status.contains(Status.Repetition) must beTrue
          played.winner must beNone
        }
      }
      "should not be fourfold" in {
        position.playUsisStr(usis.dropRight(1)) must beValid.like { case played =>
          played.status must beNone
          played.winner must beNone
        }
      }
    }
    "minishogi repetition" in {
      val position =
        makePosition(Minishogi).withHistory(History.empty.withInitialSfen(Minishogi.initialSfen))
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
        position.playUsisStr(usis) must beValid.like { case played =>
          played.status.contains(Status.Repetition) must beTrue
          played.winner must_== Some(Gote)
          val positionNoInitialSfen = played.withHistory(played.history.copy(initialSfen = None))
          positionNoInitialSfen.winner must_== Some(Gote)
          val positionOppositeColorSfen =
            played.withHistory(played.history.withInitialSfen(Sfen("rbsgk/4p/5/P4/KGSBR w - 1")))
          positionOppositeColorSfen.winner must_== Some(Sente)
        }
      }
      "should not be fourfold" in {
        position.playUsisStr(usis.dropRight(1)) must beValid.like { case played =>
          played.status must beNone
          played.winner must beNone
        }
      }
    }
    "kyotoshogi threefold" in {
      val position = makePosition(Kyotoshogi)
      val usis     = List(
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
        position.playUsisStr(usis) must beValid.like { case played =>
          played.status.contains(Status.Repetition) must beTrue
          played.winner must beNone
        }
      }
      "should not be threefold" in {
        position.playUsisStr(usis.dropRight(1)) must beValid.like { case played =>
          played.status must beNone
          played.winner must beNone
        }
      }
    }
  }
}
