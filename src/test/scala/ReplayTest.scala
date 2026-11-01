package shogi

import shogi.format.forsyth.Sfen
import shogi.format.usi.Usi
import shogi.variant.Standard

class ReplayTest extends ShogiTest {
  val usis = format.usi.Fixtures.prod500standard.map(Usi.readList(_).get)

  "replaying to the last game" should {
    "preserve each variant's initial setup with and without an explicit SFEN" in {
      val variants = List(
        Standard,
        shogi.variant.Minishogi,
        shogi.variant.Kyotoshogi,
        shogi.variant.Annanshogi,
        shogi.variant.Checkshogi,
        shogi.variant.Chushogi,
        shogi.variant.Dobutsu,
      )
      forall(variants) { variant =>
        forall(List(None, Some(variant.initialSfen))) { initialSfen =>
          val expected = State(initialSfen, variant)
          Replay.fromUsi(Nil, initialSfen, variant).valid must beValid.like { case state =>
            state.toSfen must_== expected.toSfen
            state.plies must_== expected.plies
            state.startedAtPly must_== expected.startedAtPly
            state.startedAtStep must_== expected.startedAtStep
            state.usis must beEmpty
          }
        }
      }
    }

    "return the initial game for an empty record" in {
      Replay.fromUsi(Nil, None, Standard).valid must beValid.like { case state =>
        state.toSfen must_== State(Standard).toSfen
        state.usis must beEmpty
        state.playedPlies must_== 0
      }
    }

    "preserve the valid prefix and original error after a later illegal move" in {
      val moves = Usi.readList("7g7f 7g7f 3c3d").get
      val first = State(Standard)(moves.head).toOption.get
      val error = first(moves(1)).swap.toOption.get
      Replay.fromUsi(moves, None, Standard) must beLike {
        case result @ Replay.Result.Incomplete(state, failure) =>
          state.toSfen must_== first.toSfen
          state.usis must_== Vector(moves.head)
          state.playedPlies must_== 1
          failure must_== error
          result.valid must beInvalid(error)
      }
    }

    "return the initial game and error when the first move is illegal" in {
      val moves = Usi.readList("3c3d 7g7f").get
      Replay.fromUsi(moves, None, Standard) must beLike {
        case result @ Replay.Result.Incomplete(state, _) =>
          state.toSfen must_== State(Standard).toSfen
          state.usis must beEmpty
          state.playedPlies must_== 0
          result.valid must beInvalid
      }
    }

    "preserve SFEN numbering and a Gote starting turn" in {
      val sfen    = Sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 42")
      val initial = State(Some(sfen), Standard)
      val moves   = Usi.readList("3c3d 7g7f").get
      Replay.fromUsi(moves, Some(sfen), Standard).valid must beValid.like { case state =>
        state.startedAtPly must_== initial.plies
        state.startedAtStep must_== 42
        state.plies must_== initial.plies + 2
        state.stepNumber must_== 44
        state.color must_== Gote
        state.usis must_== moves.toVector
      }
    }
  }

  "all 500 fixtures" should {
    "have no errors and correct size" in {
      usis forall { u =>
        val r = Replay.statesWhileValid(u, None, shogi.variant.Standard)
        r._1.tail.size must_== u.size
        r._2 must beEmpty
        Replay.fromUsi(u, None, Standard).valid must beValid.like { case state =>
          val expected = r._1.last
          state.toSfen must_== expected.toSfen
          state.usis must_== expected.usis
          state.plies must_== expected.plies
          state.history.positionHashes.toList must_== expected.history.positionHashes.toList
          state.history.consecutiveAttacks must_== expected.history.consecutiveAttacks
        }
        val r2 = Replay.usiWithRoleWhilePossible(u, None, shogi.variant.Standard)
        r2.size must_== u.size
      }
    }
  }

}
