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
          val expected = Game(initialSfen, variant)
          Replay.fromUsi(Nil, initialSfen, variant).valid must beValid.like { case game =>
            game.toSfen must_== expected.toSfen
            game.plies must_== expected.plies
            game.startedAtPly must_== expected.startedAtPly
            game.startedAtStep must_== expected.startedAtStep
            game.usis must beEmpty
          }
        }
      }
    }

    "return the initial game for an empty record" in {
      Replay.fromUsi(Nil, None, Standard).valid must beValid.like { case game =>
        game.toSfen must_== Game(Standard).toSfen
        game.usis must beEmpty
        game.playedPlies must_== 0
        game.clock must beNone
      }
    }

    "preserve the valid prefix and original error after a later illegal move" in {
      val moves = Usi.readList("7g7f 7g7f 3c3d").get
      val first = Game(Standard)(moves.head).toOption.get
      val error = first(moves(1)).swap.toOption.get
      Replay.fromUsi(moves, None, Standard) must beLike {
        case result @ Replay.Result.Incomplete(game, failure) =>
          game.toSfen must_== first.toSfen
          game.usis must_== Vector(moves.head)
          game.playedPlies must_== 1
          failure must_== error
          result.valid must beInvalid(error)
      }
    }

    "return the initial game and error when the first move is illegal" in {
      val moves = Usi.readList("3c3d 7g7f").get
      Replay.fromUsi(moves, None, Standard) must beLike {
        case result @ Replay.Result.Incomplete(game, _) =>
          game.toSfen must_== Game(Standard).toSfen
          game.usis must beEmpty
          game.playedPlies must_== 0
          result.valid must beInvalid
      }
    }

    "preserve SFEN numbering and a Gote starting turn" in {
      val sfen    = Sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 42")
      val initial = Game(Some(sfen), Standard)
      val moves   = Usi.readList("3c3d 7g7f").get
      Replay.fromUsi(moves, Some(sfen), Standard).valid must beValid.like { case game =>
        game.startedAtPly must_== initial.plies
        game.startedAtStep must_== 42
        game.plies must_== initial.plies + 2
        game.stepNumber must_== 44
        game.color must_== Gote
        game.usis must_== moves.toVector
      }
    }
  }

  "all 500 fixtures" should {
    "have no errors and correct size" in {
      usis forall { u =>
        val r = Replay.gamesWhileValid(u, None, shogi.variant.Standard)
        r._1.tail.size must_== u.size
        r._2 must beEmpty
        Replay.fromUsi(u, None, Standard).valid must beValid.like { case game =>
          val expected = r._1.last
          game.toSfen must_== expected.toSfen
          game.usis must_== expected.usis
          game.plies must_== expected.plies
          game.history.positionHashes.toList must_== expected.history.positionHashes.toList
          game.history.consecutiveAttacks must_== expected.history.consecutiveAttacks
        }
        val r2 = Replay.usiWithRoleWhilePossible(u, None, shogi.variant.Standard)
        r2.size must_== u.size
      }
    }
  }

}
