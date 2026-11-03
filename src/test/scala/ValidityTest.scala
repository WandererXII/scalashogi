package shogi

import shogi.format.forsyth.Sfen
import shogi.variant._

class ValidityTest extends ShogiTest {
  "board bounds" should {
    "reject a standard king outside the board" in {
      forall(List(Pos.at(9, 8).get, Pos.at(4, 9).get, Pos.at(11, 11).get)) { dest =>
        val position = Position(Standard)
        val invalid  = position.copy(board = position.board.move(Pos.SQ5I, dest).get)
        invalid.playable(strict = true) must beFalse
        invalid.playable(strict = false) must beFalse
      }
    }

    "reject non-royal pieces outside each variant's board" in {
      forall(Variant.all.filterNot(_ == Chushogi)) { variant =>
        val position = Position(variant)
        val orig     = position.board.pieces.find { case (_, piece) => piece.role != King }.get._1
        val invalid  = position.copy(board = position.board.move(orig, Pos.at(11, 11).get).get)
        invalid.valid(strict = true) must beFalse
        invalid.valid(strict = false) must beFalse
      }
    }

    "accept the initial position of every variant" in {
      forall(Variant.all) { variant =>
        val position = Position(variant)
        position.playable(strict = true) must beTrue
        position.playable(strict = false) must beTrue
      }
    }
  }

  val sfens: List[(Variant, String, Boolean, Boolean)] = List(
    (
      Chushogi,
      "lfcsgekgscfl/a1b1txot1b1a/mvrhdqndhrvm/pppppppppppp/3i4i3/12/12/3I4I3/PPPPPPPPPPPP/MVRHDNQDHRVM/A1B1TOXT1B1A/LFCSGKEGSCFL b - 1",
      true,
      true,
    ),
    (Chushogi, "12/12/12/12/7K4/12/12/12/12/12/12/12 b", false, false),
    (Chushogi, "12/12/12/12/7K4/12/12/12/12/12/12/12 b", true, false),
    (Chushogi, "12/12/7k4/12/7K4/12/12/12/12/12/12/12 b", false, true),
    (Chushogi, "12/12/7k4/12/7K4/12/12/12/12/12/12/12 b", true, true),
    (Chushogi, "12/12/7k3p/12/7K4/12/12/9K2/12/3X8/12/12 b", false, false),
    (Chushogi, "12/12/7k3p/12/7K4/12/12/9+E2/12/3X8/12/12 b", true, true),
    (Chushogi, "12/12/7k3p/12/7+E4/12/12/9+E2/12/3X8/12/12 b", false, false),
  )
  sfens foreach { case (variant, sfen, strict, valid) =>
    val position = Sfen(sfen).toPosition(variant).get
    position.valid(strict) must_== valid
  }
}
