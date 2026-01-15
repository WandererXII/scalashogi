package shogi

import shogi.format.forsyth.Sfen

class PerpetualCheckTest extends ShogiTest {

  "Perpetual check" should {
    val g = makeGame(shogi.variant.Standard)
      .playUsisStr(List("5g5f", "5c5d", "2h5h", "5a5b", "5f5e", "5b5c", "5h5f", "5c6d"))
    val u = List(
      "5f6f",
      "6d7d",
      "6f7f",
      "7d6d",
      "7f6f",
      "6d7d",
      "6f7f",
      "7d6d",
      "7f6f",
      "6d7d",
      "6f7f",
      "7d6d",
      "7f6f",
    )
    val ui = List(
      "5f6f",
      "6d7d",
      "6f7f",
      "7d6d",
      "7f6f",
      "6d7d",
      "6f7f",
      "7d6d",
      "7i6h",
      "3a4b",
      "6h7i",
      "4b3a",
      "7f6f",
      "6d7d",
      "6f7f",
      "7d6d",
      "7f6f",
    )
    "not trigger" in {
      "after 2 repetitions" in {
        g must beValid.like { case game =>
          game.playUsisStr(u take 5) must beValid.like { case game2 =>
            game2.situation.status must beNone
            game2.situation.winner must beNone
          }
        }
      }
      "after 3 repetitions" in {
        g must beValid.like { case game =>
          game.playUsisStr(u take 9) must beValid.like { case game2 =>
            game2.situation.status must beNone
            game2.situation.winner must beNone
          }
        }
      }
      "if the checks weren't consecutive" in {
        g must beValid.like { case game =>
          game.playUsisStr(ui) must beValid.like { case game2 =>
            game2.situation.status.contains(Status.Repetition) must beTrue
            game2.situation.winner must beNone
          }
        }
      }
      "5fPJUs7r" in {
        val g = sfenToGame(
          Sfen("ln1skg1nl/1r1sg2b1/ppp1GSppp/3p1p3/9/9/PPPPPPPPP/1B5R1/LN1GK2NL b Sp 1"),
          shogi.variant.Standard,
        ).toOption.get
        val usis = List(
          "4c5b+", // 1st
          "6a5b",
          "S*4c",
          "S*6a",
          "4c5b+", // 2nd
          "6a5b",
          "S*4c",
          "S*6a",
          "4c5b+", // 3rd
          "6a5b",
          "S*4c",
          "S*6a",
          "4c5b+", // 4th
        )
        g.playUsisStr(usis) must beValid.like { case game =>
          game.situation.status.contains(Status.Repetition) must beTrue
          game.situation.winner must beNone
        }
      }
    }
    "trigger" in {
      def isPerpetualWith(g: Game, winner: Color): Boolean =
        g.situation.status.contains(Status.PerpetualCheck) &&
          g.situation.winner == Some(winner)

      "after 4 repetitions" in {
        g must beValid.like { case game =>
          game.playUsisStr(u) must beValid.like { case game2 =>
            isPerpetualWith(game2, Gote) must beTrue
          }
        }
      }
      "on my turn" in {
        val g = sfenToGame(
          Sfen("6k2/1+p7/9/9/9/9/9/1+P2R4/6K2 b - 1"),
          shogi.variant.Standard,
        ).toOption.get
        val usis = List(
          "5h3h",
          "3a2a",
          "3h2h",
          "2a3a",
          "2h3h",
          "3a2a",
          "3h2h",
          "2a3a",
          "2h3h",
          "3a2a",
          "3h2h",
          "2a3a",
          "2h3h",
        )
        g.playUsisStr(usis) must beValid.like { case game =>
          isPerpetualWith(game, Gote) must beTrue
        }
      }
      "on opponent's turn" in {
        val g = sfenToGame(
          Sfen("l2g2B1l/1r2k2g1/p1nsppnp1/2pp4p/9/1pPS2s1P/P2PPPN1B/1PG6/LN2KG2L w S2Prp 66"),
          shogi.variant.Standard,
        ).toOption.get
        val usis = List(
          "9c9d",
          "3a5c+",
          "5b5a", // 1st
          "5c6b",
          "5a4b",
          "6b5c",
          "4b5a", // 2nd
          "5c6b",
          "5a4b",
          "6b5c",
          "4b5a", // 3rd
          "5c6b",
          "5a4b",
          "6b5c",
          "4b5a", // 4th
        )
        g.playUsisStr(usis) must beValid.like { case game =>
          isPerpetualWith(game, Gote) must beTrue
        }
      }
      "on opponent's turn 2" in {
        val g = sfenToGame(
          Sfen("l2gk3l/1r5g1/2ns+Bpnp1/p1pp4p/9/1pPS2s1P/P2PPPN1B/1PG6/LN2KG2L b S3Prp 69"),
          shogi.variant.Standard,
        ).toOption.get
        val usis = List(
          "5c6b",
          "5a4b",
          "6b5c",
          "4b5a", // 2nd
          "5c6b",
          "5a4b",
          "6b5c",
          "4b5a", // 3rd
          "5c6b",
          "5a4b",
          "6b5c",
          "4b5a", // 4th
        )
        g.playUsisStr(usis) must beValid.like { case game =>
          isPerpetualWith(game, Gote) must beTrue
        }
      }
      "on opponent's turn 3" in {
        val g = sfenToGame(
          Sfen("1+s4k2/5r3/9/9/9/9/9/9/1+N4K2 w - 1"),
          shogi.variant.Standard,
        ).toOption.get
        val usis = List(
          "4b3b",
          "3i4i",
          "3b4b",
          "4i3i",
          "4b3b",
          "3i4i",
          "3b4b",
          "4i3i",
          "4b3b",
          "3i4i",
          "3b4b",
          "4i3i",
        )
        g.playUsisStr(usis) must beValid.like { case game =>
          game.situation.winner must_== Some(Sente)
        }
      }
      "Sente starts in check" in {
        val g =
          sfenToGame(Sfen("k5r2/+s8/9/9/9/9/9/9/S5K2 b - 1"), shogi.variant.Standard).toOption.get
        val usis = List(
          "3i2i",
          "3a2a",
          "2i3i",
          "2a3a",
          "3i2i",
          "3a2a",
          "2i3i",
          "2a3a",
          "3i2i",
          "3a2a",
          "2i3i",
          "2a3a",
        )
        g.playUsisStr(usis) must beValid.like { case game =>
          game.situation.winner must_== Some(Sente)
        }
      }
      "Gote starts in check" in {
        val g =
          sfenToGame(Sfen("7k1/9/9/9/9/9/9/+N8/K6R1 w - 1"), shogi.variant.Standard).toOption.get
        val usis = List(
          "2a3a",
          "2i3i",
          "3a2a",
          "3i2i",
          "2a3a",
          "2i3i",
          "3a2a",
          "3i2i",
          "2a3a",
          "2i3i",
          "3a2a",
          "3i2i",
        )
        g.playUsisStr(usis) must beValid.like { case game =>
          game.situation.winner must_== Some(Gote)
        }
      }
    }
  }

  "Chushogi perpetual check" should {
    "not trigger" in {
      val dGame = Game(shogi.variant.Chushogi)
      val dUsis = List(
        "4h4g",
        "9e9f",
        "4g4h",
        "9f9e", // first repetition
        "4h4g",
        "9e9f",
        "4g4h",
        "9f9e", // second repetition
        "4h4g",
        "9e9f",
        "4g4h",
        "9f9e", // third repetition
        "4h4g",
        "9e9f",
        "4g4h",
        "9f9e", // forth repetition
      )
      dGame.playUsisStr(dUsis) must beValid.like { case game =>
        game.situation.status.contains(Status.Repetition) must beTrue
        game.situation.winner must beNone
      }
    }
    "trigger" in {
      val aGame = Game(
        Some(
          Sfen(
            "lfcsgek1scfl/a1b1txot1b1a/mvrhdqndhrvm/pppppppppppp/8i3/3I8/3g8/8I3/PPPPPPPPPPPP/MVRHDNQDHRVM/A1B1TOXT1B1A/LFCSGKEGSCFL w",
          ),
        ),
        shogi.variant.Chushogi,
      )
      val aUsis = List(
        "1d1e",
        "9f9e", // start
        "4e4f",
        "9e9f",
        "4f4e",
        "9f9e", // 1st
        "4e4f",
        "9e9f",
        "4f4e",
        "9f9e", // 2nd
        "4e4f",
        "9e9f",
        "4f4e",
        "9f9e", // 3rd
        "4e4f",
        "9e9f",
        "4f4e",
        "9f9e",
      )
      aGame.playUsisStr(aUsis) must beValid.like { case game =>
        game.situation.status.contains(Status.PerpetualCheck) must beTrue
        game.situation.winner must_== Some(Gote)
      }
    }
  }
}
