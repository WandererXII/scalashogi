package shogi
package variant

import format.forsyth.Sfen
import format.usi.Usi

class AnnanshogiTest extends ShogiTest {

  def perft(game: Game, depth: Int) = Perft.perft(game, depth)

  "calculate annanshogi perfts" should {
    val game = Game(shogi.variant.Annanshogi)
    "1 depth" in {
      perft(game, 1) must be equalTo 28
    }
    "2 depth" in {
      perft(game, 2) must be equalTo 784
    }
  }

  val annanPerfts = List(
    ("4k4/4p4/9/4P4/4L4/4N4/9/9/9 b - 1", 10),
    ("2k6/9/9/9/9/1P1P1P1P1/1R3K1+B1/3R5/9 w - 1", 1),
    ("4k3S/r1l1gs3/n+N1s5/L+P4G1+B/1Pp1p1Rb1/4PNpN1/3+pPGPP1/6G1P/1p1K2S1+l w - 1", 35),
    ("P1G+N3s1/lp7/2+N+Pp+P1+L+P/1sB1kP3/N1pl1s3/g6p1/+p1B2+p+p+l+p/1+s+r+n4R/1g1pK1+p2 b G3P 167", 2),
    ("1n2g1g2/l2k1p3/p1p1psb1l/r4Ppp1/1p6p/P2p1PPpP/L3+n1S1L/1BKN2GS1/r1G4N1 b 2Psp 83", 7),
    ("Pn2g4/+S2ps3l/4p1p1+P/sPpg1Pk2/lS1rr2pp/p2GnnPP1/LB+p4+nL/2B2K2+p/1p1GP2+p1 b - 129", 46),
    ("9/1k7/9/4b4/4p4/5N3/6K2/9/9 b - 1", 6)
  )

  "default positions" should {
    "be identical" in {
      Game(shogi.variant.Annanshogi).toSfen must_== shogi.variant.Annanshogi.initialSfen
    }
  }

  "annanshogi positions" should {
    "forall" in {
      forall(annanPerfts) { line =>
        line match {
          case (sfenStr, d1) => {
            val game = Game(Some(Sfen(sfenStr)), shogi.variant.Annanshogi)
            perft(game, 1) must be equalTo d1.toInt
          }
        }
      }
    }
  }

  "move/drop legaility" should {
    "not checkmate" in {
      val sit = Sfen("4k4/4+R4/4L4/9/9/9/9/7GS/7GK w").toSituation(shogi.variant.Annanshogi).get
      sit.check must beTrue
      sit.end(false) must beFalse
      val sit2 = Sfen("5lkl1/5p1p1/9/6P2/6r2/9/9/9/K8 w").toSituation(shogi.variant.Annanshogi).get
      sit2.check must beFalse
      sit2.end(false) must beFalse
    }
    "checkmate" in {
      val sit = Sfen("4k4/4+R4/9/4L4/9/9/9/7GS/7GK w").toSituation(shogi.variant.Annanshogi).get
      sit.end(false) must beTrue
    }
    "capture move giver to defend from check" in {
      val sit = Sfen("5k3/9/9/5l3/5p3/9/2B6/9/5K3 b").toSituation(shogi.variant.Annanshogi).get
      sit(Usi("7g5e").get).isValid must beFalse
      sit(Usi("7g4d").get).isValid must beTrue

      val sit2 = Sfen("5k3/9/9/7b1/5P3/5L3/9/9/5K3 w").toSituation(shogi.variant.Annanshogi).get
      sit2(Usi("2d4f").get).isValid must beTrue
      perft(Game(sit2), 1) must_== 6
    }
    "capture move giver to defend from check with king" in {
      val sit = Sfen("9/8k/9/9/9/9/9/p+r7/K8 b").toSituation(shogi.variant.Annanshogi).get
      sit(Usi("9i8e").get).isValid must beFalse
      sit(Usi("9i8h").get).isValid must beTrue

      val sit2 = Sfen("9/7Pk/7+R1/9/9/9/9/9/8K w").toSituation(shogi.variant.Annanshogi).get
      sit2(Usi("1b2c").get).isValid must beTrue
    }
    "do not allow capturing move givers that prevent check" in {
      val sit = Sfen(
        "2k5N/3p2+P1G/1+P+P1s2+P+L/1pg1bS3/6Bp1/6G1P/1K1SL+pP2/+ln+pRG1+p1+p/1+lP1+p2R1 b SN2Pnp"
      ).toSituation(shogi.variant.Annanshogi).get
      sit(Usi("4d5c").get).isValid must beFalse

      val sit2 = Sfen("3k5/9/9/3p5/3bG4/9/9/6K2/9 b").toSituation(shogi.variant.Annanshogi).get
      sit2(Usi("5e6d").get).isValid must beFalse
      perft(Game(sit2), 1) must_== 13

      val sit3 = Sfen("6k2/p8/9/3Bs4/3R5/9/9/6K2/9 w").toSituation(shogi.variant.Annanshogi).get
      sit3(Usi("5d6e").get).isValid must beFalse
      perft(Game(sit3), 1) must_== 10
    }
    "king captures protected piece" in {
      val sit = Sfen("k8/9/9/5K3/6g2/6p2/9/9/9 b").toSituation(shogi.variant.Annanshogi).get
      sit(Usi("4d3e").get).isValid must beTrue
    }
    "only friendly pieces give you moves" in {
      val sit = Sfen("9/9/9/4P4/4r4/9/9/9/K7k b").toSituation(shogi.variant.Annanshogi).get
      perft(Game(sit), 1) must_== 5
    }
    "pawn checkmate not valid" in {
      val sit = Sfen("4k4/9/4G4/9/9/9/9/9/9 b P").toSituation(shogi.variant.Annanshogi).get
      sit(Usi("P*4b").get).isValid must beTrue
      sit(Usi("P*5b").get).isValid must beFalse
    }
    "backrank drops" in {
      val sit = Sfen("9/9/9/k8/g8/G8/K8/9/9 b NLP").toSituation(shogi.variant.Annanshogi).get
      sit(Usi("P*5a").get).isValid must beTrue
      sit(Usi("N*5a").get).isValid must beTrue
      sit(Usi("N*5b").get).isValid must beTrue
      sit(Usi("L*5a").get).isValid must beTrue
    }
    "don't force promotions" in {
      val sit = Sfen("9/3PL4/2P5N/k6N1/g8/G8/K8/9/5L3 b").toSituation(shogi.variant.Annanshogi).get
      sit(Usi("2d3b").get).isValid must beTrue
      sit(Usi("2d3b+").get).isValid must beTrue
      sit(Usi("1c2a").get).isValid must beTrue
      sit(Usi("1c2a+").get).isValid must beTrue
      sit(Usi("5b5a").get).isValid must beTrue
      sit(Usi("5b5a+").get).isValid must beTrue
      sit(Usi("6b6a").get).isValid must beTrue
      sit(Usi("6b6a+").get).isValid must beTrue
      sit(Usi("4i4a").get).isValid must beTrue
      sit(Usi("4i4a+").get).isValid must beTrue
      Sfen("5L3/3PL4/2P5N/k6N1/g8/G8/K8/9/9 w")
        .toSituation(shogi.variant.Annanshogi)
        .get
        .valid(true) must beTrue
      Sfen("5L3/3PL4/2P5N/k6N1/g8/G8/K8/9/9 b")
        .toSituation(shogi.variant.Annanshogi)
        .get
        .valid(true) must beTrue
    }
    "double pawns" in {
      val sit = Sfen("9/9/9/9/9/k8/n1PPPPPP1/N2G2B2/K8 b 3P").toSituation(shogi.variant.Annanshogi).get
      sit(Usi("6g7f").get).isValid must beTrue
      sit(Usi("P*8f").get).isValid must beTrue
      sit(Usi("P*7f").get).isValid must beFalse
      Sfen("9/9/9/9/9/k3P4/n1P1PPPP1/N2G2B2/K8 w 3P")
        .toSituation(shogi.variant.Annanshogi)
        .get
        .valid(true) must beTrue
      Sfen("9/9/9/9/9/k3P4/n1P1PPPP1/N2G2B2/K8 b 3P")
        .toSituation(shogi.variant.Annanshogi)
        .get
        .valid(true) must beTrue
    }
    "king move not working in shogiops 0.16.0" in {
      val sit = Sfen("5K3/9/9/9/3B5/4k4/9/9/9 w - 1").toSituation(shogi.variant.Annanshogi).get
      val dests = sit.moveDestsFrom(Pos.SQ5F).get
      sit(Usi("5f6f").get).isValid must beTrue
      dests.size must_==7

      val sit2 = Sfen("5K3/9/9/9/3B5/3Bk4/9/9/9 w - 1").toSituation(shogi.variant.Annanshogi).get
      sit2(Usi("5f6f").get).isValid must beTrue
    }
  }

  "parse" in {
    Sfen("lPsgkgLnP/1r5b1/p1ppp1p1p/1p5pp/9/1P3P1P1/P1PP1P2P/1B5n1/lNSGKGpNL b").toSituation(
      shogi.variant.Annanshogi
    ) must beSome
  }

  "fixture perfts" should {
    "forall" in {
      forall(fixturePerfts) { line =>
        line match {
          case (sfenStr, d1) => {
            val game = Game(Some(Sfen(sfenStr)), shogi.variant.Annanshogi)
            game.situation.end(false) must beFalse
            perft(game, 1) must be equalTo d1.toInt
          }
        }
      }
    }
  }

  // format: off
  val fixturePerfts: List[(String, String)] = List(
    ("3k2s1l/l1g1Rg1+P1/6+P1n/PPn1p1bP1/1pPpppP2/1p3P2p/+s1NSP3N/5G1RL/B2GK1S2 w Plp 66", "101"),
    ("P2p3rb/l6sp/n1pg2k1n/gPnPp4/2pP2p+BP/4pP1l1/L+pS1G1KRL/G1P3P2/1N5P1 w 2sp 138", "103"),
    ("6PG1/P1ks1p1sl/l1ng4n/p2p1n1Pp/p1r1P3B/spP2P2P/N1BP2+p1L/L1S1G1p+p1/3GK2R1 b p 103", "38"),
    ("3g1g1+P+P/l2s4+P/1k3+N+P2/p1s1P2N1/P1b4b1/2PP1p2N/+p+p3G2L/L1pG1PS2/1+rlK4+p w RSN2Pp 124", "85"),
    ("2+P1g1s1g/+N7l/2s1+Nl3/1p1k1+P2p/pr4pPg/1npS1pb2/L+pP2S1+p1/1P1PGK3/B2Np1+l1R b P2p 163", "71"),
    ("2s1P1gn1/4s1k1l/n1pg5/1P1P2p1b/PPrPp2P1/L3GP2p/N1B3P1P/L1S1KS2L/n1P2G1+r1 b 3P 63", "31"),
    ("1n3g1nl/l3p1k2/psgp2s1p/P3pppRR/3pP1b1P/1pKPPPP2/NG5+pN/LB4G1L/1Sp3S2 w - 54", "40"),
    ("1n5rl/2gkg4/l+Np3ps+P/P1rsn1P2/4ppb2/1PBP3PP/L1P1+p1N2/5G1G1/3S1KS+pL w 3Pp 102", "84"),
    ("1n1k1g1nB/l8/p1p3p2/pgppp1Pp1/1SrPGp1Nl/2B1S3p/P1P3KS1/LS2P3R/1N2PG2L w Pp 70", "29"),
    ("p1s1k4/l5g1p/1+N+P2Pp1l/1P1Ppg1s1/2p3R2/1p1pp2S1/+nB+p1PKP+pR/LP1G3S1/3nB2gL w np 106", "104"),
    ("2s1k2nP/3ggs1p1/n2ppp1nl/5PpP1/1PpB1P3/2r1P1+pbp/+p2K2R2/2S3S1L/L+l1G1GP2 w n2p 70", "108"),
    ("2+PNgk1s1/N7P/1psp2pp1/l1P6/PN2pP1P1/1r1+p1bP1G/+p1p1s4/LS1GP3p/2K1R2NL b GLbp 99", "130"),
    ("1Pgpk1+N2/+Lps1gps2/+N2b1r2l/1L1Ps4/5P2p/2Pr1GPp1/N1+p1+p3P/1BG1pS1KL/p1p2P1N1 w P 96", "58"),
    ("3g3gp/pr1s2l1l/3p1p2+R/3kPP1P1/1P6P/3PP2p1/+p1KG1G2L/SBP1L1S2/P3+pPPNn b BSNn 143", "171"),
    ("3g5/l+P+N2p1s+B/+L2+N1kp+P1/1P2g1R2/2Pp1P3/1p2S2nL/2+p1+s1+p2/6G2/3G1K3 w RL4Pbsn3p 112", "232"),
    ("2pg2snl/1rk1g1l2/l1sp+Pp1p1/p7p/2P1p1p1R/+pp3P2P/2nPN2+p1/3GG2S1/2S1K2NL b Bb2p 57", "77"),
    ("7+PL/p1p1g2+N1/kr+P3+P2/lL1gp1p+L1/6bp1/B2Pnr3/2NSG2PS/PP1+nG2Kp/4P1+p2 w 2S2Pp 188", "68"),
    ("1P3gLP1/l1s1ks2+P/n1+Bg1p+B1+R/1rP1p3P/5PP2/1nppPn3/+p+p4G1L/L1SP1S3/1pG1K2+np w p 100", "66"),
    ("2pkr4/1s1g3Bl/p1n1+P2sn/l2p2p1P/Pp3G1nL/3pg2p1/+pRB1pS3/2K1SP3/L+p2G2N1 w P3p 126", "65"),
    ("1g3+P3/2+Nsg2b1/2+P2s+P1n/1P3P2+P/lr3P1k1/B6lP/3P+p2PN/Lp+p2SR1L/P4K1G1 w GSN4P 110", "54"),
    ("1+P1+P2p1P/4+N1PG1/lP5+P1/3pG1B2/2p5l/PpSPP1kN1/L+pK2+s1+l+p/n+s2g1G2/1N1+p3+p1 b 2Rbsp 149", "98"),
    ("3k2s2/2pp1+Pp1l/1+P1sg2gp/1p1p1p3/lnPP3p1/pn5b1/LBPK4L/5G1P1/1N+p1GpS2 b RNrsp 79", "136"),
    ("+Pg3P1+N+P/p1g4P1/s1k+P3+P1/pPb2pP2/p3Sn3/2S4K1/N+rP1P2G1/L3+p1+n1+r/1G3B2L w SLl3p 178", "4"),
    ("2s1gk1n1/lrg5l/p+P1p2p2/2sPppp1p/1NS3P2/B1p2P2N/P+p7/L2GKR1pL/1p3G3 b BSNPp 77", "202"),
    ("ll2k1+P2/Gs3g1R1/+Pp+N1+N2pl/3p1Pp2/g1+P4nL/4P4/+p+p+pS2+pr1/8S/2+pGK4 w 2bsn4p 114", "226"),
    ("P2gp+P1+Pn/2bsk1+S2/r3g2s1/1p1N4b/3P1GN2/lpp1K3L/1S2R4/7p+p/1+p1p3+l1 w G2Pnl4p 196", "168"),
    ("1g1s1k1n1/l2+P4l/2n1p1p1p/PP1Pg2s1/1SBr3Pb/LPPp2p2/4PPP1L/3KS1r1P/1NG2G1N1 b 2P 81", "4"),
    ("3k2g2/l1gp2s1l/p1n2pn1p/s1rp1p3/1p1p4N/PPPNrPP1L/4P4/1BS1G1GS1/L2K5 b BP3p 57", "102"),
    ("1+R+PL3g1/+P+Ps2ks2/3n2b1P/PL3+P2L/9/LpB1KR1p1/2N1G+n+pP1/1N7/1+p2Sp3 b GS6Pg 143", "171"),
    ("1n7/ls2g2s1/p1kp2p+Pl/3g1p3/4PP1n1/B5bpP/P+pN2SPpP/LPPSGr2L/2R1KG1N1 b 2p 73", "31"),
    ("1nk2gsn1/lp1g2P1l/p4pp1p/1PsB2pPb/5SlP1/1P2P4/2P2PN1P/S1K+r3R1/1N3G2L b 2Pgp 59", "4"),
    ("3gg4/1s5P1/l4kp+Pl/PP4P1p/1n1pPN1nR/7R1/1+pPP1PS1P/L+b2KG2L/1pSG5 w SPbn2p 72", "170"),
    ("1n4s2/lsg5+P/pp1p1k3/3p2p1p/1Nr1Bg1pn/1PPPpGP2/P1P2PN2/L2S2KSL/B4GR2 b L2p 69", "90"),
    ("1ns1kg1n1/1rg3s2/l+R3p2l/1p1p2p2/p1P1p4/BPpP1P3/N4KP+p+p/LS1GG1S1L/7Np b BP2p 57", "111"),
    ("1rPs3P1/1+P1klg1pl/+P3psp+P1/2pgs4/3PPPR1p/1pN2gPLP/2SP2+n1+n/1B2K2nL/PG5B1 w - 130", "37"),
    ("+P1+P3p2/2Npk1s2/Rg1g1p3/1L5g1/L2prp1NS/1SP+p1+b1PP/L6NL/1pK3P2/pNpG+b+s3 w 2Pp 140", "58"),
    ("l3gg1n1/4+Pskb1/ps2p3l/np3p2p/2pP1Pp1P/Bp2PRRpL/PsN3P2/2g1K1S2/LP1+pG2N1 b - 59", "35"),
    ("l1+Br1P3/+P1Pk2r1g/1+P2g2+P1/4P+P1P1/Pn1G3PP/p1p2p3/+n1+pKS1+p2/4L+bS1n/1+sL1G+p2L w SNP 176", "48"),
    ("P2r1k3/2+B3s1p/2g5l/ls+PPp1sNL/5p1PG/7P1/1+p+p1+p1P1R/1+nPN1P1GN/+p3K1pS1 b bgl2p 141", "35"),
    ("2s2gs2/1r5bl/l1ngk1n2/1pp2ppP1/1N4P1p/pP3P2R/1+pP+pGG2P/3S2S1L/L1B1K2N1 b 4p 55", "31"),
    ("1+P6+P/1+P2g4/lp2k1ns1/6L1P/1n1P4L/L3Kp1G1/1+B3+p3/+p4+b3/g1p1G2n1 w 2RSN8P2s 152", "88"),
    ("1p2p2+PP/2P1k+P2+L/1+N1g2s2/lrg3N2/g1N2p3/1s1SP1+p2/+p1g+p2R2/+p2L1K3/3P2+p+p1 w 2BSNl3p 170", "2"),
    ("1n1gg3l/l2s2s2/p5+P1n/r3k2pp/BP1p3NP/PP3p1S1/L1NP+p3L/1GpS3R1/4K1+p2 b B2Pg2p 73", "114"),
    ("rNr3k2/5s2l/l+B2g+P1g1/2Plp1P1P/p5PN1/SP2PGPP+p/L1S3+n2/P1KpG2p1/4b2+p1 w Psnp 150", "167"),
    ("+B1np3+N1/Bs1ks+P3/2n2+S+P2/1np2pPG1/2r5g/pp1PPK1pl/2+p1pP2L/+p1P1S4/4g1pR1 b Pg2l 177", "59"),
    ("4g1klb/l1s1g4/r1ns4n/3pps+P2/1PpPp2R1/1G2PPpP1/P1+pP2P1P/1+p3G2L/LN+b1SK1N1 w P 82", "58"),
    ("1Pk4n1/3g2+B1l/ps1p+Pp2+P/p5s2/1N2P1P1b/3P1lr1S/P1P1G1NGP/GS2KRpP+n/1+l4+p2 b 2Pl 103", "59"),
    ("+P3P4/3pg2s1/n+N+Ns+P2+R1/2rk3P1/P5+P2/4K1p2/+p+B4+p1+p/1gGP2+p2/+bps+pG1n1+l b S3L3P 167", "178"),
    ("+Pn5n1/+P1g1k3p/4sP1+S1/2r1R4/1pPp2p2/n+p3S1PL/2P6/1+p2GBb2/1+l1KP1G1g w Lsnl5p 140", "225"),
    ("p6P1/3s2g1l/1p2spk1n/RL1P1R1N1/G+B3P1p1/N1p+ppp1pP/1s5+p1/+lB2GGS1L/1N1K5 w 3Pp 134", "58"),
    ("2rg5/2s2g1pl/2np3s1/l2kp1pbn/1NL1Pp3/pppP1P3/1B1P2P1P/2S1G3L/PP3K2S w Nrg2p 96", "132"),
    ("Pb1Lk2n1/5ss+P1/l1Pg2p+P+S/GP2P3p/1Ppppp3/5+r1b1/L3G1P1N/6+p1L/1NSGK3P b RNPp 107", "146"),
    ("r1k6/1p2g3p/l+Pl1g4/3s1PRP1/n1p1p1P2/P1G1P3l/+p1PP3K1/LBS3+p2/7G+p w N2Pb2s2np 124", "219"),
    ("2g2k1P+P/r7l/2+Bs+B+P1s+P/P2p4L/Np6G/1+p+p1Sp3/L5PK1/3+rSPG2/1n+p6 w 2NPgl4p 146", "183"),
    ("1+S2+Nr1+P1/1r+L1P+P3/3g1Pn1l/+L+S2p2Pb/P1PPg3p/3pk1p1S/+nP4P1P/1+p1+pGGKSL/1+p1b3N1 w - 180", "43"),
    ("+Pr2p3b/1Pk2+B3/l1pg2G1r/2S6/p1LG3N1/NpS5P/P2P+pN1P1/1S2G1K2/9 w N2L2Ps5p 168", "119"),
    ("1ns1p3l/2g1k2ns/4bg3/lp2PpPR1/p1Pp1p2P/1PS4pL/1+rNP1+p1P1/L5GSn/4KG1B1 b P2p 87", "65"),
    ("1n5nl/2g2ks1l/2p1p2g1/lPsP2b1p/5np2/2BGPpP2/1+pP4+pN/1+p6L/1+sGp1KSR1 b 2Pr2p 73", "49"),
    ("2k2+P+P2/1s2g3l/1+Pr3+Np+P/lS1p5/pBSgp4/PGNPS1p1N/2P1K3+p/4PB1+R1/n1G1l1+l2 b 2P2p 153", "90"),
    ("3k3P1/2pggs2l/l+S2bp1pn/p1sp2p2/BN2pN1P1/P1PPp1Kp1/3P4N/L4S1rp/LpG1+p4 w Rg 106", "92"),
    ("2sG+Nn3/1+R1p1sgb1/2+P6/1P1PPN1bR/5l3/l1pk3nP/P1+p3K1L/3G1SP1+p/1pl3pp1 w gs4p 156", "159"),
    ("l2g3n1/3skgs+Pl/n+Pp1p3p/7PN/1p2ppp2/B3PP3/L+p+rP3p1/2S2K1RL/1N1G1GS2 b B2Pp 51", "115"),
    ("PP2gk2P/2+R+Bs1L2/+P3+Pb3/2p2+P1s1/3P1p3/6lpl/1+p+s1+pPRN1/1+p+p3K1+n/3G1Gp1l b G2NPsp 177", "161"),
    ("4s1k2/1p1s4l/+B5n1p/N1g1ppp2/1B6L/p2p2RP1/2SG1PG2/L2K2PGn/2Lp1S2P b N3Pr3p 123", "118"),
    ("l2g2sP1/4pkg1l/1r1p4n/p2sP1P1p/BPp2P2S/6R1P/P1+n5N/LpPS1KG1L/1N1G1p+p2 w B2P 74", "34"),
    ("3s3n1/lr3sk1l/p+P3g3/P1pp2p2/2B1Pp2p/2Pn1sPpP/L3+p2Bg/3S4L/1N1K1G1N1 w Prg3p 84", "154"),
    ("l4ks2/2g2g2l/1+Ppp+N3n/P4ppbR/2r1p1P2/BP1p1PGp1/3PP3P/L4K2L/1SPG1NSN1 w SPp 66", "58"),
    ("3+S5/g3rk3/lp2b1g1l/np+P1p4/B3PP1N1/PP1pSp1r1/LSP4NL/1SG3KP1/pN5G1 b 4Pp 101", "70"),
    ("+S3k2n1/p1rg1g3/s1P3s1l/2B3p1R/L2L5/1pNPppGpP/2+ppN1P1L/P4S1P1/1PG1K2Np b b2p 111", "51"),
    ("3k1g1nl/lrgs1ps2/p1np+N1pb1/2P6/B2Np1pPp/1p2PP1+p1/PP5pP/3SPS2R/LG2KG2L w - 56", "62"),
    ("4rp+N2/l1r+Sk1+P1l/2pg3b1/1+Ps1BG2S/p1p2P3/Pn5K1/N+p4+pP+l/2GN1PS2/+p1P+p2ppp w Gl 148", "5"),
    ("1Pbs1k1L1/+Pr3g1g+P/nn4bP1/1g1P3pL/4psP2/Np2P3N/P1+p4+r1/LSG2S3/2P3pLK w P3p 120", "88"),
    ("4k2n1/lr2g1g2/ns1p4l/L1pPpps1b/1p2P1ppp/1PB2P2P/N1PG2PPL/4SG1R1/4KS1N1 w Pp 54", "47"),
    ("6P1P/p+S+N2+P1+P+P/s4G1+B1/gPp1k2n1/1SP5l/NL1Bppp1L/3K1S+p1p/P+p1L2Gp1/R1Gn4+p w r 168", "67"),
    ("ln2k3l/s3g1sb+S/2r+P5/1p3g1p1/1P2p1PnR/8P/1+pG1G1N1P/lSb3+p1L/1NK1P4 w 5P2p 100", "107"),
    ("1ns+L2kn1/l6b1/p1p1pg1s+P/+r1pplPPp1/1B1P3PR/1P7/gPN2+p1lP/2SP1+p3/2GK3N1 w GPs 76", "90"),
    ("2+P2N1s1/+P3k2+P1/3p2l+N1/3gSP1P1/PR1n5/Lb2p1g2/2B1S1+p2/2R3g+l+n/2P1K1Gp1 b SL7P 149", "177"),
    ("4k4/l+P1sgg1+Ll/1b1+P1sp1n/p3pb2p/Pn1N2p1P/4SPp1+p/G2PGPP1N/L1+p2PSR1/4KR1p1 b - 95", "51"),
    ("2+P2k2g/3+Pr4/2n4s1/2R1Bp+P1+P/lP3S3/PN+p2GK1+p/1S1BG4/LP2P1+p+p1/3+p1G2+n b SNL2Pl2p 177", "248"),
    ("k3Pp3/1p3s3/l+Pp2g1+N1/PS2B1P1+l/p2G+P2P1/2PN1G2P/5S+p2/+r2GKPpR1/Ll+n3s2 b 3Pbn 147", "63"),
    ("1nk4p1/4+Sg1b1/2s+Pnpg+P+P/lPr1pnp2/pNPppP3/B8/1G2P1+p1L/LK1S2SR1/6G2 b L3P 95", "134"),
    ("l2r5/1s1k3b1/2pgp+Pnsl/p2p4p/Pn1r3P1/1pBS2p2/L1P5P/4PGG1+p/1N1PK1SN1 w Gl3p 66", "117"),
    ("B+N1+R1P1k1/1+Sg1p4/2N+B4l/+S1PP2P1p/1N7/LGK2pppN/+p3G4/2l+pP2+sP/+p1pp4+r b GSLPp 197", "186"),
    ("l1s+P1+N3/1P3+PS1+b/p1g1b1k1n/1pr1p3l/r1P3P2/P2p1P2p/1+p+n2G3/L5+nPS/2K1G1SpL b G2Pp 119", "104"),
    ("p4p+Np1/lsrPg+P2p/n+P1g2r2/1b1pks1P1/P1p5l/4p3L/LG1B1+p+p2/1pSK1GPS1/n1P6 w NP 146", "60"),
    ("1n2ggb2/l3p1k2/ps2+Psn2/2p3pPl/1prp1P1p1/3P2R2/PPPS2PS1/LB2K3L/1N1G1G1N1 b 2Pp 53", "62"),
    ("4k2+P1/3gN2+S1/1+P1S1+P1P1/lrBpPRGsl/3P2pn1/pPS2p2b/L4PN+pP/1KP+p2P2/1NG3G1L b P 121", "56"),
    ("1n1k2s1l/2s1g4/l6g+L/pprpp1R2/4Pp1b1/P1BPP1Pp1/L1+p1GPN2/1Pp1KS1+s1/1N3G3 w N2Pp 68", "80"),
    ("1nP2g3/3k1g1P1/lrpp2s+P1/4p1p1l/1P1P1b1nR/p1p1PPPpP/L1GP1S1b1/2K5L/1N3G1N1 w Ssp 74", "106"),
    ("2gs2gsl/1+P1+P1b1Np/2p1k4/l2ppP1Nn/1pP2pP2/ppLS3p1/+r3P1pS1/1BG3+pR1/l2KG3P w N 126", "33"),
    ("kP4sPl/4g1+N2/3+P2b2/1r3+S1s1/1Np2P3/lp5p1/4G1P+p+p/+p+n2Kp2L/p1G3R2 b BGSL3Pn2p 145", "278"),
    ("1npk3+P1/2g2p3/3p1g1bl/lrP1pP3/1P2p1s1L/p3P1S1P/P1N3P2/L1GSPG3/4K1pN1 w SN2Prb 78", "124"),
    ("1n1k1r1nl/1b2ss1PG/l4gp1p/pP2p3b/2p2PpNR/P4SP2/N1P+p2KGP/L+p6L/3S1p3 w Pgp 74", "95"),
    ("2gkg1s2/7+Pl/p1s+P2p2/1ppGP1P2/1r1N1n2P/2B2P3/+l2+p+nSGp1/5l2L/1S1KR2+p1 b BN5P 97", "172"),
    ("3+RB4/1+S1s1+PG1l/P1p3+P+Pp/p2kp2P1/1+p1lg3S/r7N/2B2G2N/+p2S+n1KGL/+p2p1nl+p1 w 4p 182", "64"),
    ("1+P2p4/1L+P+P1+R+R1s/L+P2g2+P1/+L2SG3P/2pkP1Ps1/+n1n1PG3/2+p2P1+l1/1B2K1B+n1/4G+p2+p w N3Ps 160", "85"),
    ("1nLkS4/3g2+P1g/P1+L3+Pg+R/b2P1l2p/1+RP1pB1n1/5P1l1/N1S1+s2+pP/4+nP3/p+p1+s2Kp1 b 3Pgp 185", "72"),
    ("3kg4/1rs2g2l/2+P1p1+P1p/lp1p2p1R/P2p4L/5K3/2PG3G1/5S1p1/+pN+n6 b B2SN5Pbnlp 89", "245"),
    ("6r+NP/+P1Pn1G3/2Nsp2G1/1p1Pb3l/5spBn/1k7/1+l+r1GKP+pL/1l2P1S1+s/2+p2P1p1 b 2Pg3p 171", "62"),
    ("1G2gs3/+P5pbp/1+P+P4P1/SPB+Ppknp1/r7P/2P1P1n1g/L5+l2/1p1KG1S1L/1N2+p2N1 w RSL3P 118", "47"),
    ("1s2k1b2/+P5S1l/3g1+Pp+P1/2pgp1b1n/p1P5L/rNn+ps4/L4pPpN/2S2G2P/3p1KG2 w L3Prp 108", "98"),
    ("7n1/1Rsgg3l/lP4p1p/n1p1kp1bs/2P1b2p1/Pp2GPR1P/1GN3S1L/3Pp2P1/3+p2KN1 b sl3p 101", "29"),
    ("1n2p2np/lg2k1s1+P/p+P1p2p2/B1spglR1s/P1P1RP3/L2p2P2/1GP1P+p1+p1/2S4+l1/3G1K1N1 w BNP 92", "42"),
    ("2g6/lrs1+N1r1l/3+bnp1+P1/1k4P2/2pPp2p1/1p3N2p/+pG1pKG2L/1S1P5/LN3S+p2 w BGS4Pp 90", "71"),
    ("1n1ggks2/l7l/p+Pp5n/3pppp1P/rPB2NbP1/N1PPrPP2/4P4/PS2GS2L/L1K2G3 b S2P 55", "83"),
    ("lP7/r1ssg2+Pl/p2kpg2p/3p2pn1/1p2G1b2/PSP2P2R/1N2+p1+p1P/LBpG1S3/2N1K2NL w 3p 82", "59"),
    ("5g3/2b1kS3/np1+P3+P+P/2G6/6S2/2rGp2S1/+lS1P1+p+p1L/3K3RN/1+p2l2N1 w BGL6Pn3p 108", "4"),
    ("+P1+N1g1p2/+P4g2s/3k2b1+R/2P2+Rss1/1NLPP3n/2p1G2PP/K4p+nLL/P1S2+p1G1/1p7 w bl5p 172", "158"),
    ("1n1g3nP/5k1sp/sn1+S1gp2/rPp1P1P2/Lr1p4l/P1N2p+b1l/9/+p1PG1PGPb/5K3 b SLP3p 137", "132"),
    ("1n2kg2l/ls4p2/3g+P1p+Pn/b1pp1p2p/p4P3/r3ps1pn/1+pPSSG2P/LP4RNL/B3G1K2 w 2p 84", "60"),
    ("1s1g2gp1/2R1bk2+P/2s3n1l/S+P3p3/3pPNpN1/Pl1GR1Pbp/NpKPp+p1+pL/L1P2G3/6S1P w p 144", "61"),
    ("2k4+P1/lgg2s3/p1+P1P1r1+N/3Pp1pPl/sN4b2/P2PpP2p/1BG3PG1/3K1S3/LPS3+p2 b R2NPlp 97", "158"),
    ("1n2s1k1l/1Bg6/lp2p+Ps2/rP1P4p/1PpgPppnP/L1S1P3R/1BP2+p2L/3GGP3/1N3K1N1 b s2p 105", "45"),
    ("r4k1+P1/P1Bss4/1+PN3p1l/l7p/p2PpG1P1/+n4P3/2P3P1N/2SGS1GRL/LN3K1pP b G2Pb2p 69", "110"),
    ("1+P1nkPb1l/2gs4s/4Pgn1p/1Pr1+N1PpR/2PK2PP1/lp1B4P/L1+p1+p+p3/3+n2S2/P1G3S1L b g2p 93", "52"),
    ("l1+P2gp1G/r2ss2+Pl/bk1p5/PbPpPpP2/LP1p2S2/2SP1+n1pp/1p2G3N/pnKG4L/1+p4R2 w N 106", "40"),
    ("2g3sn1/l3k1g1l/Pn1P2p1p/1sP1N3P/pr3pR2/L2pPPSPL/1+p1GB1P+p1/+p3P4/1S2GK1N1 w BP 90", "50"),
    ("l1P2k3/+B2s1pgR1/3g2n1l/p2S3+P1/PPnP1pP2/5b3/L1N1+pP+p2/+n2+p1G1SL/1G4K2 w RPs4p 90", "152"),
    ("2g6/l3gks1l/n1s+Pp3n/pP3Pb1P/r1p1P1P2/P1B1P1Rp1/1+p1G1p2L/L4S1P1/1NSK2GN1 b 2Pp 63", "64"),
    ("pnP1k1g2/lrgs3bl/+P2p2p2/3Bp1sP1/2p1PpR1n/PPNP1P1pp/L5P1P/2G1GS2L/2SK3N1 w - 62", "35"),
    ("p2gp3p/l8/1+Ps1k1g1+P/n1Pps3L/1P1r3rb/5Kp1l/2B1G3P/NNGS1P1pS/L1p4N+p w 3Pp 130", "71"),
    ("lns6/3gg1s2/2pppkp2/1PrP2p2/4N1R1l/ppP1P1P1p/P2G4+b/L2KPS3/1NS2G1NL b Pb2p 51", "51"),
    ("1s1Pg2k1/8l/l+N4nrp/1Lp1pPppg/p1P4PP/3G1psP1/1PSRPS2B/3+nn2KL/BG2+p4 b Pp 139", "50"),
    ("2g6/+N1g4Pl/4sk+P+B1/l1P3PSr/n2p3g1/LS2KpSbP/5PNpL/2+n+pG4/R2P4p b 6P 119", "71"),
    ("lP1k1g1n1/1r3s2l/2s+Pp1p1+P/1g7/1np2p1NL/1p2P1Rp1/+p1P2PP+pS/L3KG3/1NS2G3 b 2P2bp 57", "59"),
    ("L2r1sP1+P/P3g+P1+P1/n1gpP+Pb2/2pk+p2Pl/p1s3S1b/3P1N1L1/Ns4N2/+pG+p2GR1P/7K1 b Plp 145", "64"),
    ("1rs1k2n1/+P5gb1/3p+Ps1+P1/2pp4l/2Pn1KPNp/1pl2p2R/+l1PGp2PP/2S1G3L/1N2G1b1S b 3P 87", "79"),
    ("2s4+P1/+S1g6/3k2p1l/2g3P1P/4PN3/P2K1PLR1/2+p5+p/1B1P3p1/1N2GSl+pN w BN4Prgsl2p 158", "283"),
    ("3p2g1+P/l1+P1+Np3/p1gps1p1+N/2P2n1n1/r2kpS1PL/Ppp3R2/1S1GP1+p2/3GKPS2/2+pP5 b 2b2l 135", "58"),
    ("B2G2P2/+S+P2+Ppk2/2s2P1n1/lB+PNP1P2/+p1P1N2P1/1p5p1/SR1G+l3+l/2Kp2S1+l/GG6+n w 4Pr 190", "76"),
    ("2+N2P1+R1/+B+P1l1+P2p/2pgk4/L7L/gNS2sp2/4p1Gp1/P1S+p4P/1K7/6S2 b RBGNL2Pn5p 159", "374"),
    ("p4k2+L/r2gps1+N1/2s+P1p1g1/r3pPnP1/1N5n1/1LpG1PP2/L1B2S1+p1/2+pGK4/9 w BSL4P2p 96", "83"),
    ("2k4P1/p3pg2+N/lN1bP1+P+P1/lP1r1s1g1/RNBG3+ls/L1S+n2P2/2P5+p/8S/K2+p+p4 b Pg5p 193", "60"),
    ("Pn1p1s3/l1psg3l/p+Pp1k1p2/r5pp1/1Ngpp1bn1/3P3G1/2PS1PS1L/1B1GK2R1/L4nP2 w 3p 88", "47"),
    ("1P2k1s1P/ls4gbl/5+P2n/p1P1P1p2/7p1/rp1P1K1PS/P1+n3P1L/1S2G2+p1/L2G3N1 w G2Prbn2p 66", "221"),
    ("3gk3s/1rs6/1+P5+Pl/p1pPpp3/8p/B1GprPp2/L1S1P1+n+pN/4K1S1L/NN2G4 b GL2Pb3p 87", "173"),
    ("+P1+P2+N3/1+P1sks2P/3+L2PP1/1G1PN1+S2/lNP2r1G1/Lp1K2pl1/5PR2/1+p1+p4g/N+pb2+bG2 w s4p 184", "142"),
    ("3k3s1/l1p1g2sp/2ns1N3/1p2PP1L1/pP1p4l/P1Pb2b2/L1r1P3P/1S1Gp2G1/K3+p2G1 w R2N2P2p 124", "71"),
    ("7n1/+S1p2s3/1L2+Rp3/3p3Pk/+Ln1g1bP2/1Gs+s+pG2P/3PP+p1+p1/1P1L2R2/1gB+ppKn1L w 4Pn 152", "101"),
    ("Pn2g4/+S2ps3l/4p1p1+P/sPpg1Pk2/lS1rr2pp/p2GnnPP1/LB+p4+nL/2B2K2+p/1p1GP2+p1 b - 129", "46"),
    ("3g1k2l/l2s1s2g/3+P2b+Pn/ppp2pP2/1P1Pp2PR/2BP1PP2/2G1G2+l1/2PS2S+p1/+rN1K3N1 b NPlp 75", "108"),
    ("3P3+P1/P2p1g1+S1/2pk1s1pl/1PsNP+P3/l1n2gN1b/p3SB1P1/3K1PP+pp/+r1P3G1L/gn+p2l1RP w - 132", "43"),
    ("2s2k3/7+R1/l1+P2G+S2/1G7/+pp5KP/1GNp1p+sP1/p+r2+l+p3/P+np1L1+b2/2G5P w S2Pb2nl4p 174", "226"),
    ("2g3knL/l1Psg1pp1/np2p2n1/pP1pnp+Brb/L1p3P2/5p1P1/1+p2+p1S1L/1S1GP2S1/3P2KGp w r 106", "92"),
    ("g2r1ks2/+P1+P6/1+L6+P/1SPPP1L2/3GrK2L/3pN1+p1g/1p3+b2+p/1S3+pGp1/p1S5+p b B3NL4P 183", "3"),
    ("7+P1/l+N2+P4/G2k5/1P1ss1g2/2pR1pPn1/2P5R/K4+p2+l/L+p2n2+p1/3+n4p w S2P2b2gsl5p 150", "293"),
    ("l3k4/1+N1sgsg1l/p5n2/rP+PKp3P/L2P1+pp2/1pB3gP1/4+p4/1S1Gp3L/2P2RS+p1 b BNPn3p 93", "160"),
    ("7+P1/+B5+S1+P/4k2+S1/2P3+LPR/lsP5p/1+p3KN2/2+l3+p1+n/+n3+pG+p2/2+pL4R b b3gsn7p 167", "54"),
    ("+P1+P2k1+P+L/1s4r2/1+P1g2bp1/l1P3PNp/1S1gpn1L1/1r1+pSG1g1/B6P+p/3SPKp2/1p+p2p1NN b Pl 171", "74"),
    ("1n2l1s2/l1s1gg2l/n1kp+N+P1p1/p1p3pRb/1ps1P2n1/PPP3p2/2b3+p2/1r3+pS2/L1GK2P2 w Pg2p 84", "120"),
    ("1r1k3g1/1+N7/1+P5Gb/l1p1p3l/p2G2n1L/2P1n4/4+s2p+n/LK2+p+p+pS1/B3S4 b G8Prsp 153", "138"),
    ("+Pl2sNg2/P5g1l/s1g1pp2n/3Pkl2p/1N1p2Pb1/1p5G1/3+pP+b3/5SPPr/+l4KS2 b NPr4p 117", "93"),
    ("1k4+Nr1/3g2+S+L1/p+P+Pn1+Rn1+P/g+P2pp2P/L1p5S/6Gpl/1+p1B1BSP+p/3K1PS2/+p1+nG4l w P2p 192", "49"),
    ("1GP2g2b/3+P+P1bs1/k1n+R1p3/lp1nP3L/r4P3/1PpSp1pp1/+pP3KP+p1/LSS1G4/1N3G3 b NLPp 91", "171"),
    ("1n4k2/S2+P2s2/l1p3n+Pl/PP1g2b1P/2rp3N1/4p1pP1/L+pP1PS2L/2GPbP+r2/1NK1G3+s w G3P 106", "66"),
    ("2N1+P4/3+P5/lpPGs1n2/1L1PppLpp/2p5l/1s1p1kP1R/N+p+b3p+pP/BP2KSR2/6gN1 b GSgp 145", "119"),
    ("p3Pk1+N+P/l1p1p4/2+Ppl3+P/1p6L/1n2gp1p1/S3S2n1/L1+p+r3+r1/5PK2/2+p1G3n w G2S2P2bg2p 162", "150"),
    ("1+N3+P1+N1/p2p3P+B/2p+P3+P+P/1+Ln2kp2/1p1g1gs2/K2L4L/PB4+nLS/SP2+p+p1GP/R3+p3p w RGSP 200", "34"),
    ("1Ls2k1Pb/l4s2l/2n2+Pp2/pSp4p1/1P1pPr3/R7S/N1B1G1K1P/L6+p1/1p2G2N1 w 2GN5Pp 98", "58"),
    ("bn4s2/1+L4g1l/3skp2g/1Lp2Pp1P/1n1pPP2L/p1P3Gp1/3+r+p4/3N1G1+b1/1S2K1nP+p b RPs3p 141", "99"),
    ("2N2knG1/lP+P2s1+S1/5S3/2gp5/+Rp2S2pl/r1BG2p+nP/L+p1KP+pP1L/p1+p2P2p/B1PP3Ng b P 153", "63"),
    ("6k2/1gb2g2l/l1+Pp+P+P2b/7+Rp/2ns4R/+l+p6P/5S+pn1/3KG3+p/6+n2 b SN6Pgsl3p 147", "209"),
    ("2s+N+P2+P1/1r3+N3/+PpS+P1g1+P1/3Lg1p1b/LSp1k4/1r6l/+l3P+pP2/2KS3p1/2Bp1G1N+p b Ng4p 163", "99"),
    ("1s5n1/r1p1kg1bl/l1n1gps1p/P3Rp1p1/LPB3P2/2Ppp4/3PPPN+pP/4GS1PL/1NG1SK3 b P 63", "40"),
    ("1ns2g1nl/l3gksp1/p8/3pp1snp/1p3P1p1/P1S1+p1pBL/B1P4+p1/2KG1rGR1/LN7 b 2P3p 69", "74"),
    ("2+B1g4/l1g3gPl/+L+P1k3sp/5n2b/Nn1s1pPsp/Rpp5P/2NG4L/3PK1SR1/2+p1PP3 w 2P3p 136", "85"),
    ("1n3k1n1/2g3sp1/5ppgl/r+P1pp1PSp/l1p4PR/Np2P3P/p1PG1P3/LB2B3L/3SKGSN1 w Pp 64", "33"),
    ("lPs1k1s2/4g3l/p1npppp2/gP4b1L/4p1NN1/2rp2RP1/NSP+p1PP2/LB2G2S1/4P3K w Pg2p 66", "101"),
    ("P2s2Psl/1kr1B3+N/4gNg1+P/4N3P/2P1PGSPL/1nB3pl1/3S1P3/2P1K4/1+p2G+pLR1 b 2P4p 131", "73"),
    ("+Nng2k1n1/1s3gsPl/3+P2b1p/1Pr3p2/L1p2PPp1/3P5/5+p3/3B1S2+p/2S1KG1NL w RL3Pg3p 80", "137"),
    ("4ks1n1/l1gs1g2l/1r2p4/1p2p1B1p/nPBpPpPP1/PpP2r2P/L+pP3P2/1S3GK1L/1N1G2SN1 b p 53", "28"),
    ("1n+P4n1/l1g2pr2/pk1sg2sl/p1pPppb2/1PBp2P2/4PG1NP/P1N2GR2/LS6L/5KS2 b 2P2p 81", "57"),
    ("+PP1Ng1g2/6sbp/ns1+Rpkp2/l1P2p2l/1p1PsPPpL/6Sp1/1+pp1GP1G1/+pr7/LN+p2K1N1 b b 91", "41"),
    ("1nsgkg1n1/lP3p2s/1r1p1+P2l/8b/1pp5P/p2Pp1PG1/P1P2PP2/L2GP2RL/1NS2KSN1 b b2p 55", "34"),
    ("2s+P2Pn1/l+P3g2+R/3g1kp1p/1L1n1B2b/3p5/2pGS2Ps/1p4P1P/g1+n2KS1+r/4P1+p2 w L4Pnlp 130", "183"),
    ("4k1P1l/lsg3s1p/pr1np1+Np+P/3P2pbL/1p1Bg3P/1pP2p3/PS5N1/L1p2KSR1/1N1G1G3 w 2Pp 82", "43"),
    ("1n2gg2l/l1n1k3l/p1spbpp1n/P1p1P2pp/5R3/1NP1PP2R/3+pG1P1+p/L1GPK2S+s/B8 b 2Ps 67", "9"),
    ("2P5k/4+P4/l+Pg1+P3n/1P5p1/1P2pLS1R/2pPs2gP/L1+n1G1PPL/1+n1BG2K1/1S7 b S2Prbn3p 139", "122"),
    ("s1P2k3/lg+P+S1+P1+P1/2N2B2p/1P2P4/r1pp1pppl/L5R+pL/3+b1KP2/1GS2G2S/6N1P w 3Pg2n 130", "125"),
    ("2p1Ps3/+Pp3k2p/n3gg2b/2sps1G1P/5n1N1/Lp1P1pPpL/p1K1S1R2/5Pp2/RN1pG4 b B2LPp 141", "161"),
    ("4g2n1/lsp2s+P+Nl/+B1nk4p/p3pp1P1/5PR2/LpP4p1/1+pP3P2/1S1GKS1GL/1N4R2 b BG3Pp 67", "184"),
    ("P2l1g1+P1/+P1+Ns4+P/N+P1p3P1/5sr1+N/1G1p2p1P/1PPbP1k1L/1+pG5L/1S1K1+pGS1/+l1+p2+b3 b Nr2p 185", "91"),
    ("2s2P2l/lp1gg+P2l/1rp2s1+P1/2PPp4/pk2bp3/n5p1p/+nS+p1KS1+pL/1B1GG1+n2/9 w rn4p 106", "155"),
    ("+P1pg3l1/1Gn2s1b1/2n3k2/p1b+R4L/Lp1p+r1p2/LSs+pG1ppp/2P+pG2+p1/1P1P1+p2n/1K2p3+s w Np 180", "51"),
    ("4gR1s1/1s1k4p/l1p4+P1/1P1g3p1/5p3/LprnP3S/1+nP2G3/3BSG2L/2K2L+pN1 b BN7Pp 93", "185"),
    ("2gk1N3/3p1+N2+P/+B2p3gl/l1+N+BpP2P/4S1pRN/p+p2PPP1L/9/RPPG2S+s1/4K1Gp1 w SL3P 170", "35"),
    ("p2g5/2sk1r1+Ll/l+P1g4P/r1p3pbs/P1P4P1/3Ppp1P1/L3P1N2/1+p2KNP+p1/1NGSG1S2 b bn2p 87", "42"),
    ("9/2g1kss1+P/l+P6P/p4pb1l/P1pgP2P1/6G1n/2+pS5/LG1P1KPR1/1N4S2 b R3Pb2nl3p 83", "107"),
    ("+P2gg1k1P/1+P2b4/p+Ppsp2Pl/2r1rNsPp/2P2nb2/1S1K1p1S1/N2PGP+p+pL/6P2/L2G3N1 b LP 105", "100"),
    ("P4sg2/1+Pg1P1s1s/2np2n1+P/1L1k1p1P1/2Pp1PP2/lP2pP2L/gG1P2R1P/L1KB1+b3/4R1+pNn w Sp 132", "74"),
    ("1Ng1s1k2/bl2N2gp/+P1r1pp2l/1+P1P1Gs2/1p6P/1pPSP1P1L/2N2P2S/LB1G5/1P1pKR1P1 w NPp 108", "62"),
    ("4s1+P1b/1+Nlks3P/4b+Pgl+P/1PP4S1/3P1r3/1n2PG+pPN/P1r5L/1G1SK1+p2/1p1+l1+p3 w N3Pgp 166", "165"),
    ("1PLrgN3/p+P3sp+P1/Gg1k2n+Pl/PN1p1pPPp/4P3b/3B1s2P/1+p1s1P2N/L3Sr1+pL/2PGK4 w - 114", "68"),
    ("2k2+P3/5s1+P1/+N1n1+P4/+l2g4l/1p1P4B/1p6+l/3S1+lGS1/N+p1K5/2p1p1GR1 w GS7Prbn2p 166", "237"),
    ("3skgsnl/r1g5P/1+Pn1p4/3P1PpPb/4p3p/pp1P1pPpR/2+pS5/1B2K3L/L1GG2SN1 b Pnlp 69", "68"),
    ("1n3k3/lrs1gs1bl/1p3ppP1/1N2pG2P/p8/1P4P1n/B1+pPP2R1/L3GSP+pL/2SGK2+n1 b 3Pp 55", "60"),
    ("6LPN/1k+N+P5/1BN1+Pg1+L1/1p3P3/1g1S2G2/2P1K1BS1/pP1+p2G1p/+l4R2+R/1+ppp+lppp+n w 2S2P 192", "23"),
    ("1r3g3/l3k3+P/nLG1s1p1+P/2p1+B3L/SSP1P1bN1/1p1pK1P2/1P3+p1+pP/+pG2S1R+pn/L1G4P+p b np 113", "61"),
    ("1ns1k1s2/l1g1g3l/3pp1n+P1/1PB4R1/5p3/S1PP1PS1p/P1N1P1P1P/L2B1G1+pL/3GK1pN1 b R3P 53", "97"),
    ("2g2B1P+P/1P1Lbk1+P1/3g5/3+S3G1/l5p2/PpGpP2p1/1+pNNPK2+p/+p+lp+n1n1SR/+r1+p2l+p2 w 2Sp 184", "61"),
    ("l1p1Lk3/Gn+Ppbpr2/+P2+P2p1l/P1p1PP3/1p3S1p1/2r2Pbn1/+p1NK3+n1/s2G1G3/2SP+l2s1 w G2P 156", "67"),
    ("1n2kg3/lrgp4l/3sp1n1b/pK1psP2S/P8/3G1+pp1R/N+pPG2N+pP/L1PS4L/3P4+b b P3p 109", "47"),
    ("1+Bp2kP+NP/1ps2p3/1+P1p2gp+L/1N2Pp3/1S2Pgpbl/p2K2rP1/PP2SN3/3PL1+pS1/+l1G3Gn1 w R 196", "42"),
    ("5P+P2/sg1p1g2p/2pk1+P1+Ln/1sPp4S/lp2P1P2/ps1LB2P1/+r+nGPN4/2+nGp1K+p+b/6p2 b RLP 189", "5"),
    ("4k1p1+P/N5g1l/l1+N+PP1s2/p2P1SN1P/P1g2pbP1/2P2PP2/L+p2+p3+p/3+nS3R/1+s2G1GK1 b RPblp 127", "101"),
    ("1n1gkgs2/l3s2+Pl/2pp2pPn/pprB1P3/1PP1p2p1/B3P2pP/2NKG1PSp/L1S1G2RL/7N1 w P 60", "28"),
    ("1ng2k3/l2s1g1b1/pP4s2/3Gp2np/B1pN3PN/1p1PPpP2/P4+p+p2/L6SL/2S1KPR2 b RLPg2p 93", "139"),
    ("4gkb1l/l+PP3p1p/pn2gps+Pn/3p1p3/N8/L2npbPp1/2S2GS+pL/S1K3R2/1pp1G3P b RPp 95", "6"),
    ("1n1g2g2/lrps4l/2p3+P2/1p2pk2n/1P1P1p2P/p1B3G2/1SP1P1P+p1/3G1S2L/1N1p1K1N1 b BS2Prlp 59", "147"),
    ("1rg2g2l/l1psk2b1/nps2p3/p3P3p/P1P3p1P/1+pB1SPPp1/L1P1+n1N1L/2G3G+r1/1N1K5 w Ps2p 70", "110"),
    ("4k1s2/l2s2g2/ngPp1+P1N1/p1p1p1pPl/1Pp2PPpR/PprPN3p/B8/L1S1GNS1L/4GK2+b w P 78", "36"),
    ("P3g2p1/1rsk1+P1+Rl/n+Pg3n+P1/2P1P1P1p/l2KPN2s/3G1pp2/1+p3s2L/+p4B2P/1N1+pBpS2 w Pgl 124", "130"),
    ("1ns2g1n1/3kgs1bl/l2p1+Pp2/1p2pp1p1/1NpPP1P1p/1P5PR/4S+p2P/LB+p4SL/3GKGRN1 w p 52", "59"),
    ("1P2kgPs1/l3b4/2n1+P2r1/s3P3P/3P1Ngp1/4L2n1/L1SK1S2R/2PG1P2L/1+p1pG4 b 4Pbn3p 127", "34"),
    ("1s2kPP+R1/1r5+P1/6p1P/S1P1g4/1G2PpK1p/PB1p2s1L/LG2B4/1+pN1P+n1G1/4+p2L1 b S2NL4P 149", "197"),
    ("1n1g1r3/1+P5bl/2pk3s1/lprpgp1P1/L4P2p/2BPNPpp1/1+p3S2P/1S2G1K1L/1N1G3N1 b 4Ps 71", "72"),
    ("P6PP/1kb1+N4/1rg1Lgn+Pl/1pps1s2p/Ln1PP1Pp1/1PG3S1l/2N1PP1K1/1SP5+p/1B3Gp2 b Pr 129", "36"),
    ("p+N1g1k2+P/1P5b1/3g2r2/2l5L/1Npnp2p1/lp1S1KpSp/P+p1PG1N2/4G1RP1/+p1+b2pP1L w 2S2p 112", "58"),
    ("p1kg4b/5S2l/1+BP1g+Ps1n/1pp6/PnL3Pp1/N1S1S1Rp1/L3KP+p2/+pG2G3L/4p+p1N1 w Pr3p 124", "116"),
    ("lPs4n1/+LN1gg1k1l/+P2p+Ps2p/8R/1N3p3/2pp1SSpP/B+pPPGP2L/9/3K3N1 w R3Pbgp 74", "157"),
    ("lns1kgs2/2g6/b2p1P2l/pP2R1pPP/3pPp1n1/PpB3P1L/4PGNp1/2SKP2R1/LNp1G1S2 w P 68", "34"),
    ("1r4PnP/+S6+P1/2g3p1l/p3k2P1/B3p1sN1/Pp2g2N1/5+R1K1/L1S6/2p+p1pS2 w 2L2Pb2gn4p 126", "224"),
    ("g2+P1b3/2+L5+S/n+P2+P1k1+P/5s2N/4n1n1L/1lspKp3/p+p1PP2+p1/2+b2+lR2/G+pGRp4 b GS4Pp 183", "174"),
    ("lk7/r1gsg2bl/1pn3n1p/1s1p2N1P/3sPpPpP/1Ppp1P3/N1bP5/L1SG2G1R/3K4L w 2P2p 78", "53"),
    ("1n2g1g2/l2k1p3/p1p1psb1l/r4Ppp1/1p6p/P2p1PPpP/L3+n1S1L/1BKN2GS1/r1G4N1 b 2Psp 83", "7"),
    ("5gs2/l2g4l/p+P1p2S+Pn/B2kPP3/2P6/PR1K1G1p1/NS+n1G1P+bP/L7+n/4S2+r1 b 6Plp 87", "68"),
    ("2P+L2+PN1/1+R4+Pg+P/1+N1r1p1Pn/PP2skP1S/p2pPs3/3P1B2G/2+B+p2K1L/g2+pG1L2/7+pN b Pslp 175", "56"),
    ("3g5/bs2g2pl/2n+P2pkn/+B2pP2s1/6PP1/P1KN1P2P/2S2+p1+pN/L3+pS+p1L/3R3R1 w L3P2gp 104", "135"),
    ("1ng4n1/lskP3bl/1P2+P1g2/lppps1rpp/Bp3pp2/2P2PPPP/P1P2S2N/+r1GSK1G1L/1N7 b - 65", "45"),
    ("2gPk3s/1rp5l/3spgn1p/1pnpp1ppb/lP2P2b1/p5PPP/N1PSKP2N/4PGS1L/L1G3R2 b - 63", "29"),
    ("Gn6b/1r1kg4/l3ss3/g1p1l1P1+B/PPP1GP1PL/2Spp4/N3+p1N1+p/LpKP5/1+p2PS1RP w NPp 136", "60"),
    ("1+P4GnP/+P+N1sk1s2/p2g1p1P1/4PLpb1/bNp3PL1/5P1n1/+p1g+r+p1P1L/1p4SK1/LS1GR4 b 3p 125", "57"),
    ("l4P1n1/pgs1ks+P2/1r2n1+P+Pl/n3p4/1+pppP3p/2PP4P/+bPB2KN+p1/L2SS1P1L/3GG1G1+r b p 85", "47"),
    ("s2k1g1nP/l1Pgs+Bp2/2p+Pp4/P1+BL5/l1rP1p1S1/+n1N1G1p2/p+p3+l2+p/1p2P1+p2/3SK2N1 w Prgp 94", "163"),
    ("1n1gk4/l2pg2b1/2p2sn2/pPP2pppl/sp1P5/P3pPSPp/l5N1+p/1BKGP4/LNSGR3R w P 62", "38"),
    ("2s1r1n1P/pP3k3/P+N+P2p2l/lp2pP1+Pb/4P2P1/1n2P1S2/+s+pPG2P1L/2S6/L1GK2+p2 w GNPrbg 114", "181"),
    ("S5+Pn1/ppsg1G3/1r1p1+Pp+Nl/2p1p1k1b/3pP2Pp/2B5P/+l1P2PSRN/+p3G3L/1NSK2Gp1 b Pl 77", "70"),
    ("2s1k1s1b/7S1/lr1g1sgln/P1p1p2P1/1P1NPp1R1/3P2P2/2P4+pP/LBK1G3L/1N1G3N1 w 3P3p 66", "71"),
    ("1P3g1n1/lrskgB1sl/2n3p+P1/p2pPp3/1p4Rp1/P1PP3P1/1+pN+nP1P2/2S2G2P/L2G1KS1L w Pb 54", "94"),
    ("+B2g4l/1rs1k1N1s/3gpn2n/1l1B1ppp1/Pp1LPP1Rp/2pP2P2/+pS1P4N/p1PKGG3/2+l3S2 b Pp 75", "54"),
    ("l6n1/2skgp1+Bl/1g1pp+PpPp/p5p+P1/rnP3PNP/1P2P4/+p2P3G1/LS3S1R1/3GK3L b BSPn 63", "155"),
    ("l5l2/2P1k1p1+L/+Pp6+B/1s1G1KnR1/1P1P1p1NP/1N3G1P1/1+p+p2S3/4+s+s3/+rN3P3 w 2GLb6p 186", "130"),
    ("r+P5r1/lP4g+N1/sg1+P1kp2/B2p1s1P1/1L1Pp4/P1p2P2l/2NP1SP1N/N3GK2L/1GS6 w B5P 92", "37"),
    ("3k3n1/1sg2g2l/l+P1pppp+P+L/r2nP1sP1/3g1Pn2/P1ps5/PBP+p3S1/L1b2GR+p1/1NpPK3p w - 92", "60"),
    ("6gn1/1rs1g+P2l/1pkpp1pp1/3P2bN1/lnp1S2Pp/p4p2L/NS1PP1P2/B2GR4/L2K1G2+p w S2p 62", "39"),
    ("+N1g4+P1/p5+Ps1/l1s+P1knsp/2GN1g3/n3p1b1p/P1Gp1RPb1/L+p1P4+l/3KR3+l/2S1P+pp+p1 w 2P 158", "53"),
    ("P2k1+P1+P1/5b1n1/+N3p1+P2/l1p+PgS1R1/1n2b2p1/1ps2p1N1/1S1G1+p2+l/1P1G+p1KP+p/2GRP3+s b P2lp 155", "78"),
    ("2+NP2kL1/l1+P5+P/n1p1+P2PR/p4p1pP/5PRb1/s1Pp4L/1b1G+pP1Gp/1G1S3S1/2+p1K1PNN w GSl 112", "5"),
    ("6g1b/2gR3l1/lnk1s2p1/3p1pPPP/LNp5L/1P3p3/2+sSP4/1+p1GNG3/3P1KSR1 b 3Pbn3p 105", "68"),
    ("4kg1n1/l4P2+P/n1+Rg2p2/sP1N3R1/p1Lsppb1p/P3P+b1p1/L1PPG3N/1+p3GS1L/2s2K3 b 3Pp 83", "76"),
    ("l1S3s1s/+P1pg3l1/1pnk3gp/KPP2p2r/P2ppNP2/3G1P1p1/p+p+b4+bS/L2PG3L/1N4p2 b NPr 115", "91"),
    ("2pg5/lr1ngk2l/r1s3s1n/pP1p1p2p/2P3S2/P1P1p4/L2B2G+p1/1pSKG4/1N5NL w 2Pb4p 70", "102")
  )

}