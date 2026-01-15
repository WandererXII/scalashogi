package shogi

import shogi.format.forsyth.Sfen
import shogi.variant._

class ImpasseTest extends ShogiTest {

  "Pieces in promotion zone" should {
    "not be enough" in {
      "starting position" in {
        val g = sfenToGame(
          Sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1"),
          Standard,
        )
        g must beValid.like { case game =>
          game.situation.status.contains(Status.Impasse27) must beFalse
          game.situation.winner must beNone
        }
      }
      "position with less than 10 other pieces in promotion zone" in {
        val g = sfenToGame(Sfen("2SGS4/+B+RGKG2RB/9/9/7pp/8k/9/9/9 b g2s4n4l16p 1"), Standard)
        g must beValid.like { case game =>
          game.situation.status.contains(Status.Impasse27) must beFalse
          game.situation.winner must beNone
        }
      }
      "position without the king in promotion zone" in {
        val g = sfenToGame(Sfen("2SGS4/+B+RG1G2RB/3G5/9/7pp/8k/9/9/4K4 b - 1"), Standard)
        g must beValid.like { case game =>
          game.situation.status.contains(Status.Impasse27) must beFalse
          game.situation.winner must beNone
        }
      }
      "position without enough value" in {
        val g = sfenToGame(Sfen("9/1G2K2G1/PPPPPPPPP/9/9/7ss/7sk/9/9 w 2r2b2gs4n4l9p 2"), Standard)
        g must beValid.like { case game =>
          game.situation.status.contains(Status.Impasse27) must beFalse
          game.situation.winner must beNone
        }
      }
      "one move away" in {
        val g = sfenToGame(Sfen("2SGS4/+B1GKGLLRB/3G5/9/1+R5pp/8k/9/9/9 b - 1"), Standard)
        g must beValid.like { case game =>
          game.playUsiStr("8e8b") must beValid.like { case game2 =>
            game2.situation.status.contains(Status.Impasse27) must beFalse
            game2.situation.winner must beNone
          }
        }
      }
      "opponent prevents impasse" in {
        val g = sfenToGame(Sfen("2SGS4/+B1GKGLLRB/3G5/9/1+R5pp/8k/6b2/9/9 b - 1"), Standard)
        g must beValid.like { case game =>
          game.playUsisStr(List("8e8b", "3g2f")) must beValid.like { case game2 =>
            game2.situation.status.contains(Status.Impasse27) must beFalse
            game2.situation.winner must beNone
          }
        }
      }
      "26 points for gote" in {
        val g = sfenToGame(Sfen("9/9/9/9/9/9/3r1lllg/+P+P1+bkssgg/K+P4ssg w r 2"), Standard)
        g must beValid.like { case game =>
          game.situation.status.contains(Status.Impasse27) must beFalse
          game.situation.winner must beNone
        }
      }
      "21 points for gote (uwate) plus 1 piece handicap" in {
        val g = sfenToGame(Sfen("9/9/9/9/9/9/3p1lllg/+P+P2kssgr/K+P4ssg w r 2"), Standard)
        g must beValid.like { case game =>
          val handicapGame = game.withHistory(
            game.situation.history.withInitialSfen(
              Sfen("lnsgkgsnl/7b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1"),
            ),
          )
          handicapGame.situation.status.contains(Status.Impasse27) must beFalse
          handicapGame.situation.winner must beNone
        }
      }
      "17 points for gote (uwate) and pieces in opponent hand - not missing" in {
        val g = sfenToGame(Sfen("9/9/9/9/9/9/3p1lllg/+P+P2kssgg/K+P4ssg w r 2"), Standard)
        g must beValid.like { case game =>
          val handicapGame = game.withHistory(
            game.situation.history.withInitialSfen(
              Sfen("lnsgkgsnl/9/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w RB 1"),
            ),
          )
          handicapGame.situation.status.contains(Status.Impasse27) must beFalse
          handicapGame.situation.winner must beNone
        }
      }
      "27 points for sente" in {
        val g = sfenToGame(Sfen("G3+R3S/GG5SS/GLPBKBPLS/9/9/7+p+p/7+pk/7+p+p/9 b - 1"), Standard)
        g must beValid.like { case game =>
          game.situation.status.contains(Status.Impasse27) must beFalse
          game.situation.winner must beNone
        }
      }
    }
    "be enough" in {
      "all pieces on the board" in {
        val g = sfenToGame(Sfen("2SGS4/+B+RGKGLLRB/3G5/9/7pp/8k/9/9/9 b - 1"), Standard)
        g must beValid.like { case game =>
          game.situation.status.contains(Status.Impasse27) must beTrue
          game.situation.winner must beSome.like { case color =>
            color.sente
          }
        }
      }
      "some from hand" in {
        val g = sfenToGame(Sfen("G8/4K4/PPPPPPPPP/9/9/7ss/7sk/9/9 b 2R2B 1"), Standard)
        g must beValid.like { case game =>
          game.situation.status.contains(Status.Impasse27) must beTrue
          game.situation.winner must beSome.like { case color =>
            color.sente
          }
        }
      }
      "after moves" in {
        val g = sfenToGame(Sfen("2SGS4/+B1GKGLLRB/3G5/9/1+R5pp/8k/9/9/9 b - 1"), Standard)
        g must beValid.like { case game =>
          game.playUsisStr(List("8e8b", "2e2f")) must beValid.like { case game2 =>
            game2.situation.status.contains(Status.Impasse27) must beTrue
            game2.situation.winner must beSome.like { case color =>
              color.sente
            }
          }
        }
      }
      "27 points for gote" in {
        val g = sfenToGame(Sfen("9/9/9/9/9/9/3r1llll/+P+P1+bkssgg/K+P3ssgg w r 2"), Standard)
        g must beValid.like { case game =>
          game.situation.status.contains(Status.Impasse27) must beTrue
          game.situation.winner must beSome.like { case color =>
            color.gote
          }
        }
      }
      "17 points for gote (uwate), but 2 piece handicap" in {
        val g = sfenToGame(Sfen("9/9/9/9/9/9/3p1lllg/+P+P2kssgg/K+P4ssg w r 2"), Standard)
        g must beValid.like { case game =>
          val handicapGame = game.withHistory(
            game.situation.history.withInitialSfen(
              Sfen("lnsgkgsnl/9/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1"),
            ),
          )
          handicapGame.situation.status.contains(Status.Impasse27) must beTrue
          handicapGame.situation.winner must beSome.like { case color =>
            color.gote
          }
        }
      }
      "28 points for sente" in {
        val g = sfenToGame(Sfen("G3+R3S/GG2P2SS/GLPBKBPLS/9/9/7+p+p/7+pk/7+p+p/9 b - 1"), Standard)
        g must beValid.like { case game =>
          game.situation.status.contains(Status.Impasse27) must beTrue
          game.situation.winner must beSome.like { case color =>
            color.sente
          }
        }
      }
    }
    "annanshogi" in {
      "yes" in {
        "all pieces on the board" in {
          val g = sfenToGame(Sfen("2SGS4/+B+RGKGLLRB/3G5/9/7pp/8k/9/9/9 b - 1"), Annanshogi)
          g must beValid.like { case game =>
            game.situation.status.contains(Status.Impasse27) must beTrue
            game.situation.winner must beSome.like { case color =>
              color.sente
            }
          }
        }
        "some from hand" in {
          val g = sfenToGame(Sfen("G8/4K4/PPPPPPPPP/9/9/7ss/7sk/9/9 b 2R2B 1"), Annanshogi)
          g must beValid.like { case game =>
            game.situation.status.contains(Status.Impasse27) must beTrue
            game.situation.winner must beSome.like { case color =>
              color.sente
            }
          }
        }
        "17 points for gote (uwate), but annanshogi handicap" in {
          val g = sfenToGame(Sfen("9/9/9/9/9/9/3p1lllg/+P+P2kssgg/K+P4ssg w r 2"), Annanshogi)
          g must beValid.like { case game =>
            val handicapGame = game.withHistory(
              game.situation.history.withInitialSfen(
                Sfen("lnsgkgsnl/9/p1ppppp1p/1p5p1/9/1P5P1/P1PPPPP1P/1B5R1/LNSGKGSNL w - 1"),
              ),
            )
            handicapGame.situation.status.contains(Status.Impasse27) must beTrue
            handicapGame.situation.winner must beSome.like { case color =>
              color.gote
            }
          }
        }
      }
      "no" in {
        "position with less than 10 other pieces in promotion zone" in {
          val g =
            sfenToGame(Sfen("2SGS4/+B+RGKG2RB/9/9/7pp/8k/9/9/9 b g2s4n4l16p 1"), Annanshogi)
          g must beValid.like { case game =>
            game.situation.status.contains(Status.Impasse27) must beFalse
            game.situation.winner must beNone
          }
        }
        "position without the king in promotion zone" in {
          val g = sfenToGame(Sfen("2SGS4/+B+RG1G2RB/3G5/9/7pp/8k/9/9/4K4 b - 1"), Annanshogi)
          g must beValid.like { case game =>
            game.situation.status.contains(Status.Impasse27) must beFalse
            game.situation.winner must beNone
          }
        }
        "position without enough value" in {
          val g =
            sfenToGame(
              Sfen("9/1G2K2G1/PPPPPPPPP/9/9/7ss/7sk/9/9 w 2r2b2gs4n4l9p 2"),
              Annanshogi,
            )
          g must beValid.like { case game =>
            game.situation.status.contains(Status.Impasse27) must beFalse
            game.situation.winner must beNone
          }
        }
        "17 points for gote (uwate), but standard handicap" in {
          val g = sfenToGame(Sfen("9/9/9/9/9/9/3p1lllg/+P+P2kssgg/K+P4ssg w r 2"), Annanshogi)
          g must beValid.like { case game =>
            val handicapGame = game.withHistory(
              game.situation.history.withInitialSfen(
                Sfen("lnsgkgsnl/9/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1"),
              ),
            )
            handicapGame.situation.status.contains(Status.Impasse27) must beFalse
            handicapGame.situation.winner must beNone
          }
        }
      }
    }
    "checkshogi" in {
      "yes" in {
        "all pieces on the board" in {
          val g = sfenToGame(Sfen("2SGS4/+B+RGKGLLRB/3G5/9/7pp/8k/9/9/9 b - 1"), Checkshogi)
          g must beValid.like { case game =>
            game.situation.status.contains(Status.Impasse27) must beTrue
            game.situation.winner must beSome.like { case color =>
              color.sente
            }
          }
        }
      }
    }
  }
}
