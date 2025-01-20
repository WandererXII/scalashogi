package shogi

import shogi.format.forsyth.Sfen

class HandicapTest extends ShogiTest {

  "handicap" in {
    "count" in {
      Handicap.allByVariant(shogi.variant.Standard).size must_== 23
      Handicap.allByVariant(shogi.variant.Minishogi).size must_== 5
      Handicap.allByVariant(shogi.variant.Chushogi).size must_== 3
      Handicap.allByVariant(shogi.variant.Annanshogi).size must_== 15
      Handicap.allByVariant(shogi.variant.Kyotoshogi).size must_== 7
      Handicap.allByVariant(shogi.variant.Checkshogi).size must_== 10
    }
    "parse all" in {
      Handicap.allByVariant.toList map { case (v, lsh) =>
        forall(lsh) { h =>
          val sit = h.sfen.toSituation(v)
          sit must beSome
          sit.get.playable(true, true) must beTrue
        }
      }
    }
    "recognize handicap" in {
      Handicap.isHandicap(
        Sfen("lnsgkgsn1/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1"),
        shogi.variant.Standard,
      ) must beTrue
      Handicap.isHandicap(
        Sfen("lnsgkgsn1/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1"),
        shogi.variant.Annanshogi,
      ) must beFalse
      Handicap.isHandicap(
        Sfen("lnsgkgsn1/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 2"),
        shogi.variant.Standard,
      ) must beTrue
      Handicap.isHandicap(
        Sfen("lnsgkgsn1/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w -"),
        shogi.variant.Standard,
      ) must beTrue
      Handicap.isHandicap(
        Sfen("lnsgkgsn1/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w P"),
        shogi.variant.Standard,
      ) must beFalse
      Handicap.isHandicap(
        Sfen("lnsgkgsn1/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w"),
        shogi.variant.Standard,
      ) must beFalse
      Handicap.isHandicap(
        Sfen("lnsgkgsn1/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL"),
        shogi.variant.Standard,
      ) must beFalse
      Handicap.isHandicap(
        Sfen("lnsgkgsn1/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b"),
        shogi.variant.Standard,
      ) must beFalse
    }
  }

}
