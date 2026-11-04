package shogi

import shogi.format.forsyth.Sfen
import shogi.format.usi.Usi
import shogi.variant.Standard

class ReplayTest extends ShogiTest {
  val usis = format.usi.Fixtures.prod500standard.map(Usi.readList(_).get)

  "replaying to the last position" should {
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
          val expected = Position(initialSfen, variant)
          Replay.fromUsi(Nil, initialSfen, variant).valid must beValid.like { case position =>
            position.toSfen must_== expected.toSfen
            position.plies must_== expected.plies
            position.startedAtPly must_== expected.startedAtPly
            position.startedAtStep must_== expected.startedAtStep
            position.usis must beEmpty
          }
        }
      }
    }

    "return the initial position for an empty record" in {
      Replay.fromUsi(Nil, None, Standard).valid must beValid.like { case position =>
        position.toSfen must_== Position(Standard).toSfen
        position.usis must beEmpty
        position.playedPlies must_== 0
      }
    }

    "preserve the valid prefix and original error after a later illegal move" in {
      val moves = Usi.readList("7g7f 7g7f 3c3d").get
      val first = Position(Standard)(moves.head).toOption.get
      val error = first(moves(1)).swap.toOption.get
      Replay.fromUsi(moves, None, Standard) must beLike {
        case result @ Replay.Result.Incomplete(position, failure) =>
          position.toSfen must_== first.toSfen
          position.usis must_== Vector(moves.head)
          position.playedPlies must_== 1
          failure must_== error
          result.valid must beInvalid(error)
      }
    }

    "return the initial position and error when the first move is illegal" in {
      val moves = Usi.readList("3c3d 7g7f").get
      Replay.fromUsi(moves, None, Standard) must beLike {
        case result @ Replay.Result.Incomplete(position, _) =>
          position.toSfen must_== Position(Standard).toSfen
          position.usis must beEmpty
          position.playedPlies must_== 0
          result.valid must beInvalid
      }
    }

    "preserve the valid prefix and error for parsed notation" in {
      val parsed   = format.csa.CsaParser.full("PI\n+\n+7776FU\n-3334FU\n+7776FU").toOption.get
      val result   = Replay.fromParsedNotation(parsed, identity)
      val moves    = Usi.readList("7g7f 3c3d").get
      val expected = Replay.positionsWhileValid(moves, None, Standard)._1.last
      result must beLike { case Replay.Result.Incomplete(position, error) =>
        position.toSfen must_== expected.toSfen
        position.usis must_== moves.toVector
        position.plies must_== 2
        error must_== expected(parsed.parsedSteps.value.last).swap.toOption.get
      }
    }

    "preserve SFEN numbering and a Gote starting turn" in {
      val sfen    = Sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 42")
      val initial = Position(Some(sfen), Standard)
      val moves   = Usi.readList("3c3d 7g7f").get
      Replay.fromUsi(moves, Some(sfen), Standard).valid must beValid.like { case position =>
        position.startedAtPly must_== initial.plies
        position.startedAtStep must_== 42
        position.plies must_== initial.plies + 2
        position.stepNumber must_== 44
        position.color must_== Gote
        position.usis must_== moves.toVector
      }
    }
  }

  "replaying from a SFEN" should {
    val handicap = Sfen("lnsgkgsnl/9/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1")
    val moves    = Usi.readList("3c3d 7g7f").get

    "record it in the history of every position" in {
      Replay.fromUsi(moves, Some(handicap), Standard).valid must beValid.like { case position =>
        position.history.initialSfen must beSome(handicap)
      }
      val (positions, error) = Replay.positionsWhileValid(moves, Some(handicap), Standard)
      error must beNone
      positions.map(_.history.initialSfen).distinct must_== List(Some(handicap))
    }

    "keep it when the record is rejected" in {
      val rejected = Usi.readList("3c3d 7g7f 9i9a").get
      Replay.fromUsi(rejected, Some(handicap), Standard) must beLike {
        case Replay.Result.Incomplete(position, _) =>
          position.history.initialSfen must beSome(handicap)
      }
    }

    "record it when setting up a position to play from" in {
      Position(Some(handicap), Standard).history.initialSfen must beSome(handicap)
    }

    "leave it empty when there is none" in {
      Replay.fromUsi(moves.reverse, None, Standard).valid must beValid.like { case position =>
        position.history.initialSfen must beNone
      }
      Position(None, Standard).history.initialSfen must beNone
    }

    "award minishogi repetition against the starting color" in {
      val usis = List.fill(3)(Usi.readList("2a2b 4e4d 2b2a 4d4e").get).flatten
      val sfen = Sfen("rbsgk/4p/5/P4/KGSBR w - 1")
      Replay.fromUsi(usis, Some(sfen), shogi.variant.Minishogi).valid must beValid.like {
        case position =>
          position.status.contains(Status.Repetition) must beTrue
          position.winner must_== Some(Sente)
      }
    }
  }

  "replaying from an unparsable SFEN" should {
    val moves = Usi.readList("7g7f").get
    val sfens = List(
      Sfen("not a sfen"),
      shogi.variant.Minishogi.initialSfen,
    )

    "report the error, and not play from the default setup" in {
      forall(sfens) { sfen =>
        forall(List(moves, Nil)) { usis =>
          Replay.fromUsi(usis, Some(sfen), Standard) must beLike {
            case result @ Replay.Result.InvalidSfen(failure) =>
              failure must_== s"Invalid SFEN for Standard: $sfen"
              result.valid must beInvalid(failure)
          }
        }
      }
    }

    "report the error when collecting positions" in {
      forall(sfens) { sfen =>
        val (positions, error) = Replay.positionsWhileValid(moves, Some(sfen), Standard)
        error must beSome(s"Invalid SFEN for Standard: $sfen")
        positions must beEmpty
      }
    }

    "resolve no roles" in {
      forall(sfens) { sfen =>
        Replay.usiWithRoleWhilePossible(moves, Some(sfen), Standard) must beEmpty
      }
    }
  }

  "all 500 fixtures" should {
    "have no errors and correct size" in {
      forall(usis) { u =>
        val r = Replay.positionsWhileValid(u, None, shogi.variant.Standard)
        r._1.tail.size must_== u.size
        r._2 must beEmpty
        Replay.fromUsi(u, None, Standard).valid must beValid.like { case position =>
          val expected = r._1.last
          position.toSfen must_== expected.toSfen
          position.usis must_== expected.usis
          position.plies must_== expected.plies
          position.history.positionHashes.toList must_== expected.history.positionHashes.toList
          position.history.consecutiveAttacks must_== expected.history.consecutiveAttacks
        }
        val r2 = Replay.usiWithRoleWhilePossible(u, None, shogi.variant.Standard)
        r2.size must_== u.size
      }
    }
  }

}
