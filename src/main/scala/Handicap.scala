package shogi

import shogi.format.forsyth.Sfen

final case class Handicap(
    japanese: String,
    english: String,
    sfen: Sfen,
) {
  val fullName = s"$japanese ($english)"
}

object Handicap {

  val allByVariant: Map[variant.Variant, List[Handicap]] = Map(
    variant.Standard -> List(
      Handicap(
        "香落ち",
        "Lance",
        Sfen("lnsgkgsn1/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "右香落ち",
        "Right Lance",
        Sfen("1nsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "角落ち",
        "Bishop",
        Sfen("lnsgkgsnl/1r7/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "飛車落ち",
        "Rook",
        Sfen("lnsgkgsnl/7b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "飛香落ち",
        "Rook-Lance",
        Sfen("lnsgkgsn1/7b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "二枚落ち",
        "2-piece",
        Sfen("lnsgkgsnl/9/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "四枚落ち",
        "4-piece",
        Sfen("1nsgkgsn1/9/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "六枚落ち",
        "6-piece",
        Sfen("2sgkgs2/9/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "八枚落ち",
        "8-piece",
        Sfen("3gkg3/9/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "十枚落ち",
        "10-piece",
        Sfen("4k4/9/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "歩三兵",
        "3 Pawns",
        Sfen("4k4/9/9/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w 3p 1"),
      ),
      Handicap(
        "裸玉",
        "Naked King",
        Sfen("4k4/9/9/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "トンボ＋桂香",
        "Dragonfly + NL",
        Sfen("ln2k2nl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "トンボ＋香",
        "Dragonfly + L",
        Sfen("l3k3l/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "トンボ",
        "Dragonfly",
        Sfen("4k4/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "香得",
        "Lance Gained",
        Sfen("lnsgkgsn1/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w L 1"),
      ),
      Handicap(
        "角得",
        "Bishop Gained",
        Sfen("lnsgkgsnl/1r7/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w B 1"),
      ),
      Handicap(
        "飛車得",
        "Rook Gained",
        Sfen("lnsgkgsnl/7b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w R 1"),
      ),
      Handicap(
        "飛香得",
        "Rook-Lance Gained",
        Sfen("lnsgkgsn1/7b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w RL 1"),
      ),
      Handicap(
        "二枚得",
        "2-piece Gained",
        Sfen("lnsgkgsnl/9/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w RB 1"),
      ),
      Handicap(
        "四枚得",
        "4-piece Gained",
        Sfen("1nsgkgsn1/9/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w RB2L 1"),
      ),
      Handicap(
        "六枚得",
        "6-piece Gained",
        Sfen("2sgkgs2/9/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w RB2N2L 1"),
      ),
      Handicap(
        "八枚得",
        "8-piece Gained",
        Sfen("3gkg3/9/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w RB2S2N2L 1"),
      ),
    ),
    variant.Minishogi -> List(
      Handicap(
        "角落ち",
        "Bishop",
        Sfen("r1sgk/4p/5/P4/KGSBR w - 1"),
      ),
      Handicap(
        "飛車落ち",
        "Rook",
        Sfen("1bsgk/4p/5/P4/KGSBR w - 1"),
      ),
      Handicap(
        "二枚落ち",
        "2-piece",
        Sfen("2sgk/4p/5/P4/KGSBR w - 1"),
      ),
      Handicap(
        "三枚落ち",
        "3-piece",
        Sfen("3gk/4p/5/P4/KGSBR w - 1"),
      ),
      Handicap(
        "四枚落ち",
        "4-piece",
        Sfen("4k/4p/5/P4/KGSBR w - 1"),
      ),
    ),
    variant.Chushogi -> List(
      Handicap(
        "三枚獅子",
        "3-piece lion",
        Sfen(
          "lfcsgekgscfl/a1b1txxt1b1a/mvrhdqndhrvm/pppppppppppp/3i4i3/12/12/3I4I3/PPPPPPPPPPPP/MVRHDNQDHRVM/A1B1T+O+OT1B1A/LFCSGKEGSCFL w - 1",
        ),
      ),
      Handicap(
        "二枚獅子",
        "2-lions",
        Sfen(
          "lfcsgekgscfl/a1b1txot1b1a/mvrhdqndhrvm/pppppppppppp/3i4i3/12/12/3I4I3/PPPPPPPPPPPP/MVRHDNQDHRVM/A1B1T+OXT1B1A/LFCSGKEGSCFL w - 1",
        ),
      ),
      Handicap(
        "二枚王",
        "2-kings",
        Sfen(
          "lfcsgekgscfl/a1b1txot1b1a/mvrhdqndhrvm/pppppppppppp/3i4i3/12/12/3I4I3/PPPPPPPPPPPP/MVRHDNQDHRVM/A1B1TOXT1B1A/LFCSGK+EGSCFL w - 1",
        ),
      ),
    ),
    variant.Annanshogi -> List(
      Handicap(
        "香落ち",
        "Lance",
        Sfen("lnsgkgsn1/1r5b1/p1ppppp1p/1p5p1/9/1P5P1/P1PPPPP1P/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "右香落ち",
        "Right Lance",
        Sfen("1nsgkgsnl/1r5b1/p1ppppp1p/1p5p1/9/1P5P1/P1PPPPP1P/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "角落ち",
        "Bishop",
        Sfen("lnsgkgsnl/1r7/p1ppppp1p/1p5p1/9/1P5P1/P1PPPPP1P/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "飛車落ち",
        "Rook",
        Sfen("lnsgkgsnl/7b1/p1ppppp1p/1p5p1/9/1P5P1/P1PPPPP1P/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "飛香落ち",
        "Rook-Lance",
        Sfen("lnsgkgsn1/7b1/p1ppppp1p/1p5p1/9/1P5P1/P1PPPPP1P/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "二枚落ち",
        "2-piece",
        Sfen("lnsgkgsnl/9/p1ppppp1p/1p5p1/9/1P5P1/P1PPPPP1P/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "四枚落ち",
        "4-piece",
        Sfen("1nsgkgsn1/9/p1ppppp1p/1p5p1/9/1P5P1/P1PPPPP1P/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "六枚落ち",
        "6-piece",
        Sfen("2sgkgs2/9/p1ppppp1p/1p5p1/9/1P5P1/P1PPPPP1P/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "八枚落ち",
        "8-piece",
        Sfen("3gkg3/9/p1ppppp1p/1p5p1/9/1P5P1/P1PPPPP1P/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "十枚落ち",
        "10-piece",
        Sfen("4k4/9/p1ppppp1p/1p5p1/9/1P5P1/P1PPPPP1P/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "歩三兵",
        "3 Pawns",
        Sfen("4k4/9/9/9/9/1P5P1/P1PPPPP1P/1B5R1/LNSGKGSNL w 3p 1"),
      ),
      Handicap(
        "裸玉",
        "Naked King",
        Sfen("4k4/9/9/9/9/1P5P1/P1PPPPP1P/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "トンボ＋桂香",
        "Dragonfly + NL",
        Sfen("ln2k2nl/1r5b1/p1ppppp1p/1p5p1/9/1P5P1/P1PPPPP1P/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "トンボ＋香",
        "Dragonfly + L",
        Sfen("l3k3l/1r5b1/p1ppppp1p/1p5p1/9/1P5P1/P1PPPPP1P/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "トンボ",
        "Dragonfly",
        Sfen("4k4/1r5b1/p1ppppp1p/1p5p1/9/1P5P1/P1PPPPP1P/1B5R1/LNSGKGSNL w - 1"),
      ),
    ),
    variant.Kyotoshogi -> List(
      Handicap(
        "と落ち",
        "Tokin",
        Sfen("pgks1/5/5/5/TSKGP w - 1"),
      ),
      Handicap(
        "銀落ち",
        "Silver",
        Sfen("pgk1t/5/5/5/TSKGP w - 1"),
      ),
      Handicap(
        "歩落ち",
        "Pawn",
        Sfen("1gkst/5/5/5/TSKGP w - 1"),
      ),
      Handicap(
        "金落ち",
        "Gold",
        Sfen("p1kst/5/5/5/TSKGP w - 1"),
      ),
      Handicap(
        "二枚落ち",
        "2-piece",
        Sfen("1gks1/5/5/5/TSKGP w - 1"),
      ),
      Handicap(
        "三枚落ち",
        "3-piece",
        Sfen("1gk2/5/5/5/TSKGP w - 1"),
      ),
      Handicap(
        "裸玉",
        "Naked King",
        Sfen("2k2/5/5/5/TSKGP w - 1"),
      ),
    ),
    variant.Checkshogi -> List(
      Handicap(
        "香落ち",
        "Lance",
        Sfen("lnsgkgsn1/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "右香落ち",
        "Right Lance",
        Sfen("1nsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "角落ち",
        "Bishop",
        Sfen("lnsgkgsnl/1r7/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "飛車落ち",
        "Rook",
        Sfen("lnsgkgsnl/7b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "飛香落ち",
        "Rook-Lance",
        Sfen("lnsgkgsn1/7b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "二枚落ち",
        "2-piece",
        Sfen("lnsgkgsnl/9/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "四枚落ち",
        "4-piece",
        Sfen("1nsgkgsn1/9/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "六枚落ち",
        "6-piece",
        Sfen("2sgkgs2/9/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "八枚落ち",
        "8-piece",
        Sfen("3gkg3/9/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1"),
      ),
      Handicap(
        "十枚落ち",
        "10-piece",
        Sfen("4k4/9/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1"),
      ),
    ),
    variant.Dobutsu -> List(
      Handicap(
        "ひよこ落ち",
        "Chick",
        Sfen("rkb/2/1P1/BKR w - 1"),
      ),
    ),
  )

  lazy val sfensByVariant: Map[variant.Variant, List[Sfen]] =
    allByVariant.map { case (v, hs) => v -> hs.map(_.sfen) }

  def isHandicap(sfen: Sfen, v: variant.Variant) =
    sfensByVariant.get(v).exists(_.exists(_.truncate == sfen.truncate))

}
