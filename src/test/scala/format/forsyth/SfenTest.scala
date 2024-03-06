package shogi
package format
package forsyth

import format.usi.Usi
import variant.{ Chushogi, Minishogi, Standard }

import cats.syntax.option._

class SfenTest extends ShogiTest {

  "the forsyth notation" should {
    "export" in {
      "game opening" in {
        val steps = List("7g7f", "3c3d", "8h2b", "3a2b")
        val game  = makeGame(Standard)
        "new game" in {
          game.toSfen must_== Sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1")
        }
        "new game board only" in {
          Sfen(Sfen.boardToString(makeSituation(Standard).board, Standard)) must_== Sfen(
            "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL"
          )
        }
        "one step" in {
          game.playUsisStr(steps take 1) must beValid.like { case g =>
            g.toSfen must_== Sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL w - 2")
          }
        }
        "2 steps" in {
          game.playUsisStr(steps take 2) must beValid.like { case g =>
            g.toSfen must_== Sfen("lnsgkgsnl/1r5b1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL b - 3")
          }
        }
        "3 steps" in {
          game.playUsisStr(steps take 3) must beValid.like { case g =>
            g.toSfen must_== Sfen("lnsgkgsnl/1r5B1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/7R1/LNSGKGSNL w B 4")
          }
        }
        "4 steps" in {
          game.playUsisStr(steps take 4) must beValid.like { case g =>
            g.toSfen must_== Sfen("lnsgkg1nl/1r5s1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/7R1/LNSGKGSNL b Bb 5")
          }
        }
        "5 drop" in {
          game.playUsisStr(steps take 4) must beValid.like { case g =>
            g.playUsiStr("B*5e") must beValid.like { case g2 =>
              g2.toSfen must_== Sfen("lnsgkg1nl/1r5s1/pppppp1pp/6p2/4B4/2P6/PP1PPPPPP/7R1/LNSGKGSNL w b 6")
            }
          }
        }
      }

    }
    "import" in {
      val steps = List("7g7f", "3c3d", "8h2b", "3a2b")
      def compare(u: List[String], sfen: Sfen) =
        makeGame(Standard).playUsisStr(u) must beValid.like { case g =>
          sfen must_== g.toSfen
        }
      "new game" in {
        compare(
          Nil,
          Sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1")
        )
      }
      "one step" in {
        compare(
          steps take 1,
          Sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL w - 2")
        )
      }
      "2 steps" in {
        compare(
          steps take 2,
          Sfen("lnsgkgsnl/1r5b1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL b - 3")
        )
      }
      "3 steps" in {
        compare(
          steps take 3,
          Sfen("lnsgkgsnl/1r5B1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/7R1/LNSGKGSNL w B 4")
        )
      }
      "4 steps" in {
        compare(
          steps take 4,
          Sfen("lnsgkg1nl/1r5s1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/7R1/LNSGKGSNL b Bb 5")
        )
      }
      "invalid" in {
        Sfen("hahaha").toSituation(Standard) must beNone
      }
    }
    "promoted pieces in sfen" in {
      Sfen("+l+n+sgkg+s+n+l/1r5+b1/+p+p+p+p+p+p+p+p+p/9/9/9/PPPP+PPPPP/1B5R1/LNSGKGSNL b - 1").toSituation(
        Standard
      ) must beSome
        .like { case s =>
          val ps = s.board.pieces.values.toList
          ps.count(_.role == Tokin) must_== 10
          ps.count(_.role == PromotedLance) must_== 2
          ps.count(_.role == PromotedKnight) must_== 2
          ps.count(_.role == PromotedSilver) must_== 2
          ps.count(_.role == Horse) must_== 1
        }
    }
  }
  "export to situation plus" should {
    "with plies" in {
      "starting" in {
        Sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1").toSituationPlus(
          Standard
        ) must beSome.like { case s =>
          s.situation.color must_== Sente
          s.plies must_== 0
          s.stepNumber must_== 1
        }
      }
      "sente to play" in {
        Sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 11").toSituationPlus(
          Standard
        ) must beSome.like { case s =>
          s.situation.color must_== Sente
          s.plies must_== 10
          s.stepNumber must_== 11
        }
      }
      "gote to play" in {
        Sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 2").toSituationPlus(
          Standard
        ) must beSome.like { case s =>
          s.situation.color must_== Gote
          s.plies must_== 1
          s.stepNumber must_== 2
        }
      }
      "gote to play starting at 1" in {
        Sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1").toSituationPlus(
          Standard
        ) must beSome.like { case s =>
          s.situation.color must_== Gote
          s.plies must_== 1
          s.stepNumber must_== 1
        }
      }
    }
  }
  "make game" should {
    "sente starts - 1" in {
      val sfen = Sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1")
      val game = Game(sfen.some, shogi.variant.Standard)
      game.plies == 0
      game.startedAtPly == 0
      game.startedAtStep == 1
      game.toSfen must_== sfen
    }
    "sente starts - 2" in {
      val sfen = Sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 2")
      val game = Game(sfen.some, shogi.variant.Standard)
      game.plies == 2
      game.startedAtPly == 2
      game.startedAtStep == 2
      game.toSfen must_== sfen
    }
    "gote starts - 1" in {
      val sfen = Sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 1")
      val game = Game(sfen.some, shogi.variant.Standard)
      game.plies == 1
      game.startedAtPly == 1
      game.startedAtStep == 1
      game.toSfen must_== sfen
    }
    "gote starts - 2" in {
      val sfen = Sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w - 2")
      val game = Game(sfen.some, shogi.variant.Standard)
      game.plies == 1
      game.startedAtPly == 1
      game.startedAtStep == 2
      game.toSfen must_== sfen
    }
  }
  "pieces in hand" should {
    "read" in {
      "makeHands" in {
        Sfen.makeHandsFromString("", Standard) must beSome.like { case hs =>
          hs must_== Hands.empty
        }
        Sfen.makeHandsFromString("-", Standard) must beSome.like { case hs =>
          hs must_== Hands.empty
        }
        Sfen.makeHandsFromString("10p25P", Standard) must beSome.like { case hs =>
          hs must_== Hands(Hand(Map(Pawn -> 25)), Hand(Map(Pawn -> 10)))
        }
      }
      "empty hand" in {
        Sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1").toSituationPlus(
          Standard
        ) must beSome.like { case s =>
          s.situation.hands must_== Hands.empty
        }
      }
      "simple hand" in {
        Sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b PNr 1").toSituationPlus(
          Standard
        ) must beSome.like { case s =>
          s.situation.hands must_== Hands(Hand(Map(Knight -> 1, Pawn -> 1)), Hand(Map(Rook -> 1)))
        }
      }
      "hand with numbers" in {
        Sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b 15P3Nr2LB7R3s230gG12pl 1")
          .toSituationPlus(Standard) must beSome
          .like { case s =>
            s.situation.hands must_== Hands(
              Hand(Map(Rook -> 7, Bishop -> 1, Gold -> 1, Knight -> 3, Lance -> 2, Pawn -> 15)),
              Hand(Map(Rook -> 1, Gold -> 81, Silver -> 3, Lance -> 1, Pawn -> 12))
            )
          }
      }
      "hand repeating" in {
        Sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b ppBG15ppppP 1").toSituationPlus(
          Standard
        ) must beSome.like { case s =>
          s.situation.hands must_== Hands(Hand(Map(Bishop -> 1, Gold -> 1, Pawn -> 1)), Hand(Map(Pawn -> 20)))
        }
      }
      "invalid roles" in {
        Sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b T13k10t 1").toSituationPlus(
          Standard
        ) must beNone
      }
      "open number" in {
        Sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b 120 1").toSituationPlus(
          Standard
        ) must beSome.like { case s =>
          s.situation.hands must_== Hands.empty
        }
      }
      "ignore wrong input" in {
        Sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b 3P2ljk 1").toSituationPlus(
          Standard
        ) must beNone
      }
    }
  }
  "minishogi" in {
    "default" in {
      Situation(Minishogi).toSfen.value must_== "rbsgk/4p/5/P4/KGSBR b -"
    }
    "too many ranks" in {
      Sfen("rbsgk/4p/5/PG3/K1SBR/PG3").toSituation(Minishogi) must beNone
    }
    "too many files" in {
      Sfen("rrbsgk/4p/5/PG3/K1SBR/PG3").toSituation(Minishogi) must beNone
    }
  }
  "chushogi" in {
    "default" in {
      Situation(
        Chushogi
      ).toSfen.value must_== "lfcsgekgscfl/a1b1txot1b1a/mvrhdqndhrvm/pppppppppppp/3i4i3/12/12/3I4I3/PPPPPPPPPPPP/MVRHDNQDHRVM/A1B1TOXT1B1A/LFCSGKEGSCFL b -"
    }
    "all promoted" in {
      Sfen(
        "+l+f+c+s+g+ek+g+s+c+f+l/+a1+b1+t+x+o+t1+b1+a/+m+v+r+h+dqn+d+h+r+v+m/+p+p+p+p+p+p+p+p+p+p+p+p/3+i4+i3/12/12/3+I4+I3/+P+P+P+P+P+P+P+P+P+P+P+P/+M+V+R+H+DNQ+D+H+R+V+M/+A1+B1+T+O+X+T1+B1+A/+L+F+C+S+GK+E+G+S+C+F+L b - 1"
      ).toSituation(Chushogi)
        .get
        .toSfen
        .truncate
        .value must_== "+l+f+c+s+g+ek+g+s+c+f+l/+a1+b1+t+x+o+t1+b1+a/+m+v+r+h+dqn+d+h+r+v+m/+p+p+p+p+p+p+p+p+p+p+p+p/3+i4+i3/12/12/3+I4+I3/+P+P+P+P+P+P+P+P+P+P+P+P/+M+V+R+H+DNQ+D+H+R+V+M/+A1+B1+T+O+X+T1+B1+A/+L+F+C+S+GK+E+G+S+C+F+L b -"
    }
    "double digit files" in {
      Sfen("11k/11d/12/12/12/11N/10N1/9N2/12/12/D11/K11 b").toSituation(Chushogi) must beSome.like { case s =>
        s.toSfen.truncate.value must_== "11k/11d/12/12/12/11N/10N1/9N2/12/12/D11/K11 b -"
        s.board(Pos.SQ1F) must beSome
        s.board(Pos.SQ2G) must beSome
        s.board(Pos.SQ3H) must beSome
      }
    }
    "last lion capture" in {
      Sfen(
        "lf1s1x1kg1sl/ac1degt1ocfa/mb1h1tn1drvm/pv6pbp1/3pp1p1i2p/1prI2P3h1/3PP1+H1IPPP/1Pp4pPR2/P3DP1S2Q1/MR1HO1E1B1VM/AVBCTKDT2FA/LF1S1G1GXC1L b 5h"
      ).toSituation(Chushogi) must beSome.like { case s =>
        s.toSfen.truncate.value must_== "lf1s1x1kg1sl/ac1degt1ocfa/mb1h1tn1drvm/pv6pbp1/3pp1p1i2p/1prI2P3h1/3PP1+H1IPPP/1Pp4pPR2/P3DP1S2Q1/MR1HO1E1B1VM/AVBCTKDT2FA/LF1S1G1GXC1L b 5h"
      }
    }
    "last lion capture from play" in {
      Sfen("12/12/12/12/12/4r7/12/12/5o3n2/4N7/12/6B5 w - 1").toSituation(Chushogi) must beSome.like {
        case s =>
          val s1 = s(Usi("8f8j+").get).toOption.get
          val s2 = s(Usi("8f3f").get).toOption.get
          s1.toSfen.truncate.value must_== "12/12/12/12/12/12/12/12/5o3n2/4+r7/12/6B5 b 8j"
          s2.toSfen.truncate.value must_== "12/12/12/12/12/9r2/12/12/5o3n2/4N7/12/6B5 b -"
      }
    }
  }
}
