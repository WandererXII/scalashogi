package shogi

import shogi.format.CsaMove
import shogi.format.Drop
import shogi.format.forsyth.Sfen
import shogi.format.usi.Usi
import shogi.variant._

class PositionTimelineTest extends ShogiTest {
  private val board = Standard.initialSfen.boardString.get

  "Position timeline" should {
    "initialize every variant consistently" in {
      forall(Variant.all) { v =>
        forall(List(None, Some(v.initialSfen))) { initial =>
          val setup              = Position(initial, v)
          val (snapshots, error) = Replay.positionsWhileValid(Nil, initial, v)
          setup.toSfen must_== v.initialSfen
          setup.usis must beEmpty
          setup.plies must_== 0
          setup.startedAtPly must_== 0
          setup.startedAtStep must_== 1
          snapshots.head.toSfen must_== setup.toSfen
          snapshots.size must_== 1
          error must beNone
          Replay.fromUsi(Nil, initial, v).valid must beValid.like { case position =>
            position.toSfen must_== setup.toSfen
          }
        }
      }
    }

    "import numbering for both turns, clamp integers and default invalid numbers" in {
      forall(List(Sente, Gote)) { color =>
        forall(
          List(
            ""           -> 1,
            "junk"       -> 1,
            "0"          -> 1,
            "-4"         -> 1,
            "10000"      -> 9999,
            "2147483648" -> 1,
            "1"          -> 1,
            "2"          -> 2,
            "11"         -> 11,
            "42"         -> 42,
          ),
        ) { case (number, expectedStep) =>
          val position    = Sfen(s"$board ${color.letter} - $number").toPosition(Standard).get
          val expectedPly = expectedStep - (if ((expectedStep % 2 == 1) == color.sente) 1 else 0)
          position.stepNumber must_== expectedStep
          position.plies must_== expectedPly
          position.startedAtPly must_== expectedPly
          position.startedAtStep must_== expectedStep
          position.playedPlies must_== 0
          position.fullTurnNumber must_== 1 + expectedPly / 2
          position.usis must beEmpty
          position.history.initialSfen must beNone
          position.toSfen must_== Sfen(s"$board ${color.letter} - $expectedStep")
        }
      }
    }

    "initialize numbering after inferring the turn" in {
      val position = Sfen("4k4/9/4R4/9/9/9/9/9/4K4").toPosition(Standard).get
      position.color must_== Gote
      position.plies must_== 1
      position.startedAtPly must_== 1
      position.stepNumber must_== 1
    }

    "record moves, promotions and drops exactly once through every entry point" in {
      val cases = List(
        (Standard.initialSfen, "7g7f"),
        (Sfen("4k4/9/4P4/9/9/9/9/9/4K4 b - 11"), "5c5b+"),
        (Sfen("4k4/9/9/9/9/9/9/9/4K4 b P 11"), "P*5e"),
      )
      forall(cases) { case (sfen, str) =>
        val position = sfen.toPosition(Standard).get
        val usi      = Usi(str).get
        val direct   = usi match {
          case m: Usi.Move => position.move(m)
          case d: Usi.Drop => position.drop(d)
        }
        val parsed = usi match {
          case m: Usi.Move =>
            position(
              CsaMove(
                m.dest,
                m.orig,
                if (m.promotion) Standard.promote(position.board(m.orig).get.role).get
                else position.board(m.orig).get.role,
              ),
            )
          case d: Usi.Drop => position(Drop(d.role, d.square))
        }
        forall(List(position(usi), direct, parsed)) { result =>
          result must beValid.like { case next =>
            next.usis must_== Vector(usi)
            next.history.usis must_== next.usis
            next.history.lastUsi must beSome(usi)
            next.plies must_== position.plies + 1
            next.stepNumber must_== position.stepNumber + 1
            next.startedAtPly must_== position.startedAtPly
            next.startedAtStep must_== position.startedAtStep
          }
        }
        position.usis must beEmpty
        position(Usi("1a1a").get) must beInvalid
        position.usis must beEmpty
      }
    }

    "record complete Chu lion moves once, including igui and irreversible captures" in {
      val position = Sfen("12/12/12/12/12/6+O5/6p5/12/12/12/12/12 b - 42")
        .toPosition(Chushogi)
        .get
      forall(List("6f5e6d", "6f6g6f")) { str =>
        val usi  = Usi(str).get
        val next = position(usi).toOption.get
        next.usis must_== Vector(usi)
        next.plies must_== position.plies + 1
        next.stepNumber must_== 43
        next.history.lastUsi must beSome(usi)
      }
      val captured = position(Usi("6f6g6f").get).toOption.get
      captured.history.positionHashes.length must_== Hash.size
      val next = captured.switch(Usi("6f5e").get).toOption.get
      next.usis must_== Vector(Usi("6f6g6f").get, Usi("6f5e").get)
      next.startedAtStep must_== 42
    }

    "keep snapshots, final replay and downstream tree indexing aligned on a valid prefix" in {
      val initial            = Sfen(s"$board w - 42")
      val usis               = Usi.readList("3c3d 7g7f 7g7f 8c8d").get
      val (snapshots, error) = Replay.positionsWhileValid(usis, Some(initial), Standard)
      val init               = snapshots.head
      val positions          = snapshots.tail
      val nodes              = positions.zip(usis).reverse.map { case (g, usi) =>
        (g.plies, usi, g.toSfen, g.plies - init.plies, g.check)
      }
      init.plies must_== 41
      init.usis must beEmpty
      nodes.map(_._1) must_== List(43, 42)
      nodes.map(_._2) must_== usis.take(2).reverse
      nodes.map(_._4) must_== List(2, 1)
      snapshots.last.stepNumber must_== 44
      snapshots.last.startedAtPly must_== 41
      snapshots.last.startedAtStep must_== 42
      snapshots.last.usis must_== usis.take(2).toVector
      snapshots.last.toSfen.value must endWith(" w - 44")
      error must beSome(snapshots.last(usis(2)).swap.toOption.get)
      Replay.fromUsi(usis, Some(initial), Standard) must beLike {
        case Replay.Result.Incomplete(finalPosition, _) =>
          finalPosition.toSfen must_== snapshots.last.toSfen
          finalPosition.usis must_== snapshots.last.usis
      }
      forall(snapshots.toList.zipWithIndex) { case (position, i) =>
        position.usis must_== usis.take(i).toVector
        position.plies must_== 41 + i
      }
      val (firstRejected, firstError) =
        Replay.positionsWhileValid(usis.drop(1), Some(initial), Standard)
      firstRejected.size must_== 1
      firstRejected.head.toSfen must_== initial
      firstError must beSome
    }

    "preserve timeline on focused edits and reset it on full history replacement" in {
      val initial    = Position(Standard)
      val provenance = Sfen("explicit provenance")
      val position   = initial
        .withHistory(initial.history.withInitialSfen(provenance))
        .apply(Usi("7g7f").get)
        .toOption
        .get
      forall(
        List(
          position.withBoard(Board.empty),
          position.withHands(Hands.empty),
          position.withColor(Sente),
          position.withVariant(Minishogi),
          position.copy(color = Sente),
          position.switch,
          position.withHistory(position.history.withLastUsi(Usi("3c3d").get)),
          position.withHistory(position.history.withPositionHashes(Array.empty)),
        ),
      ) { edited =>
        edited.usis must_== position.usis
        edited.plies must_== position.plies
        edited.history.initialSfen must beSome(provenance)
      }
      position.withPlies(99).plies must_== 99
      position.withPlies(99).usis must_== position.usis
      val reset = position.withHistory(History.empty)
      reset.usis must beEmpty
      reset.plies must_== 0
      reset.startedAtPly must_== 0
      reset.startedAtStep must_== 1
      reset.history.initialSfen must beNone
    }

    "export beyond the import clamp after successful play" in {
      val position = Sfen(s"$board b - 9999")
        .toPosition(Standard)
        .get
        .apply(Usi("7g7f").get)
        .toOption
        .get
      position.stepNumber must_== 10000
      position.toSfen.value must endWith(" 10000")
    }
  }
}
