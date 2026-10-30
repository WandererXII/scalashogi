package shogi

import shogi.format.forsyth.Sfen
import shogi.variant._

class ValidityTest extends ShogiTest {
  "board bounds" should {
    "reject a standard king outside the board" in {
      forall(List(Pos.at(9, 8).get, Pos.at(4, 9).get, Pos.at(11, 11).get)) { dest =>
        val sit     = Situation(Standard)
        val invalid = sit.copy(board = sit.board.move(Pos.SQ5I, dest).get)
        invalid.playable(strict = true) must beFalse
        invalid.playable(strict = false) must beFalse
      }
    }

    "reject non-royal pieces outside each variant's board" in {
      forall(Variant.all.filterNot(_ == Chushogi)) { variant =>
        val sit     = Situation(variant)
        val orig    = sit.board.pieces.find { case (_, piece) => piece.role != King }.get._1
        val invalid = sit.copy(board = sit.board.move(orig, Pos.at(11, 11).get).get)
        invalid.valid(strict = true) must beFalse
        invalid.valid(strict = false) must beFalse
      }
    }

    "accept the initial position of every variant" in {
      forall(Variant.all) { variant =>
        val sit = Situation(variant)
        sit.playable(strict = true) must beTrue
        sit.playable(strict = false) must beTrue
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
    val sit = Sfen(sfen).toSituation(variant).get
    sit.valid(strict) must_== valid
  }
}
