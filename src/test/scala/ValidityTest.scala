package shogi

import shogi.format.forsyth.Sfen
import shogi.variant._

class ValidityTest extends ShogiTest {
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
