package shogi
package variant

import format.forsyth.Sfen
import format.usi.Usi

class KyotoshogiTest extends ShogiTest {

  def perft(game: Game, depth: Int, log: Boolean = false) = Perft.perft(game, depth, log)

  "calculate kyotoshogi perfts" should {
    val game = Game(shogi.variant.Kyotoshogi)
    "1 depth" in {
      perft(game, 1) must be equalTo 12
    }
    "2 depth" in {
      perft(game, 2) must be equalTo 137
    }
    "3 depth" in {
      perft(game, 3) must be equalTo 1636
    }
    "4 depth" in {
      perft(game, 4) must be equalTo 18268
    }
    // "5 depth" in {
    //  perft(game, 5) must be equalTo 225903
    // }
  }

  val kyotoPerfts = List(
    ("1S3/L2k1/5/1Kl2/2n2 w Psgp 58", 120),
    ("5/2k1l/5/5/pBK2 w Ggtsp 28", 170),
    ("kl3/1n3/G4/5/TSK1P b P", 47)
  )

  "kyotoshogi positions" should {
    "forall" in {
      forall(kyotoPerfts) { line =>
        line match {
          case (sfenStr, d1) => {
            val game = Game(Some(Sfen(sfenStr)), shogi.variant.Kyotoshogi)
            perft(game, 1) must be equalTo d1.toInt
          }
        }
      }
    }
  }

  "default positions" should {
    "be identical" in {
      Game(shogi.variant.Kyotoshogi).toSfen must_== shogi.variant.Kyotoshogi.initialSfen
    }
  }

  "move legality" should {
    "drops" in {
      val sit = Sfen("5/5/5/5/k3K b PTGS").toSituation(shogi.variant.Kyotoshogi).get
      sit(Usi("T*3a").get).isValid must beTrue
      sit(Usi("S*3a").get).isValid must beTrue
      sit(Usi("G*3a").get).isValid must beTrue
      sit(Usi("P*3a").get).isValid must beTrue
      // promoted
      sit(Usi("L*3a").get).isValid must beTrue
      sit(Usi("B*3a").get).isValid must beTrue
      sit(Usi("N*3a").get).isValid must beTrue
      sit(Usi("R*3a").get).isValid must beTrue
    }
    "pawn checkmate" in {
      val sit = Sfen("kl3/1n3/G4/5/TSK1P b P").toSituation(shogi.variant.Kyotoshogi).get
      sit(Usi("P*5b").get).isValid must beTrue
      sit(Usi("P*5b").get).toOption.get.checkmate must beTrue
    }
    "tolerate + in usi" in {
      val sit = Situation(shogi.variant.Kyotoshogi)
      val s1  = sit(Usi("2e3d").get)
      val s2  = sit(Usi("2e3d+").get)
      s1.isValid must beTrue
      s2.isValid must beTrue
      s1.toOption.get.toSfen must_== s2.toOption.get.toSfen
    }
    "move to last rank" in {
      val sit = Sfen("pgkst/R3P/5/5/TSKG1 b P").toSituation(shogi.variant.Kyotoshogi).get
      sit(Usi("1b1a").get).isValid must beTrue
      sit(Usi("5b5a").get).isValid must beTrue
    }
  }

  "parse" in {
    Sfen("PgksL/3N1/5/1n3/pSKGl b").toSituation(shogi.variant.Kyotoshogi) must beSome
    Sfen("PgksL/3N1/5/1n3/pSKGl b").toSituation(shogi.variant.Kyotoshogi).get.valid(true) must beTrue
  }

  "fixture perfts" should {
    "forall" in {
      forall(fixturePerfts) { line =>
        line match {
          case (sfenStr, d1) => {
            val game = Game(Some(Sfen(sfenStr)), shogi.variant.Kyotoshogi)
            perft(game, 1) must be equalTo d1.toInt
          }
        }
      }
    }
  }

  // format: off
  val fixturePerfts: List[(String, String)] = List(
    ("nB3/3Bk/GP3/1LK1l/5 b p 55", "17"),
    ("1k3/2bNT/5/3B1/p1K2 b gtp 61", "12"),
    ("p4/1LG1k/2SN1/5/PK1l1 b S 53", "47"),
    ("L4/3s1/1Lk2/Kn2R/5 b GSp 45", "84"),
    ("5/4N/p1k2/LP3/G3K w TSs 32", "42"),
    ("S2L1/1LN2/K2k1/5/p3p w gs 50", "76"),
    ("5/L2N1/5/K1sk1/1Sg1P b Tp 21", "41"),
    ("k2l1/5/pL1Rs/5/n2SK b g 29", "10"),
    ("2k2/G1bn1/T2r1/2P2/2Kl1 b s 41", "9"),
    ("t3t/1S3/4K/k1g1n/5 w S2P 40", "16"),
    ("5/2k1l/5/5/pBK2 w Ggtsp 28", "170"),
    ("t4/2p2/S1N2/1rk2/4K b GSt 39", "78"),
    ("5/1k3/4p/1KB1t/1S2l b P2g 47", "45"),
    ("1R1p1/K1T2/SS2k/3nt/3g1 w - 40", "6"),
    ("1kL2/2P2/5/1BSK1/1N2p w Tg 52", "38"),
    ("RK3/3ps/1T3/3k1/2nn1 b ts 59", "14"),
    ("5/kLbB1/2K2/4P/p2t1 w 2G 40", "11"),
    ("L2S1/2Bk1/5/3T1/p1NKP b g 31", "16"),
    ("1k3/1p1t1/G2R1/B4/1K3 w GSt 34", "45"),
    ("5/2NK1/b4/1lk2/pn3 w Pts 70", "78"),
    ("Lp3/2K1k/4s/2p1t/5 b 2Gs 41", "40"),
    ("5/r1k2/P1n1S/3KL/T4 w Gs 34", "44"),
    ("L3k/1P2N/3S1/2B1K/1pl2 w g 38", "33"),
    ("2gs1/1p1k1/KT2t/B3R/3n1 w - 24", "12"),
    ("1L1G1/3Pb/1k2s/2n2/3K1 w tp 58", "80"),
    ("pN3/1s1NK/5/T1k2/5 w TSP 48", "10"),
    ("4K/3B1/4l/k2nR/1n3 b Stp 47", "46"),
    ("3kN/3p1/S1K2/L3R/5 w GTS 22", "2"),
    ("s2LG/5/k2p1/pnbl1/2K2 b - 65", "2"),
    ("2k2/B3T/L4/1B1K1/N1pp1 b g 59", "21"),
    ("3L1/1k1K1/1s1P1/1t3/2nSr w G 58", "17"),
    ("1l2P/1k1N1/1s1K1/1b3/2g1P w t 48", "47"),
    ("4k/3Nt/S4/4K/2n1l w 2Ps 66", "41"),
    ("kLNT1/1p2S/5/1s1K1/n3P b - 41", "10"),
    ("5/nlk1P/s2t1/4P/3K1 w Gs 42", "52"),
    ("2LkP/4P/1gBTK/n4/3s1 w - 56", "8"),
    ("1N3/r1N1k/1TK2/4t/1s3 b sp 23", "5"),
    ("2T1k/3b1/1K1Ps/5/2n2 w Tgp 28", "79"),
    ("1N2K/p4/1k3/1lln1/p1S2 w S 68", "8"),
    ("1s3/2k2/3g1/4t/K4 b Pgtsp 29", "43"),
    ("1K1n1/1nL2/3k1/sB3/4P b TP 33", "80"),
    ("4k/1B3/1R2l/2K1b/1L2P b 2G 41", "50"),
    ("kL3/B4/1B1K1/5/2Nlp b GP 49", "81"),
    ("S2kP/T4/1P3/K4/Ggt2 w S 62", "9"),
    ("3l1/1Bn2/3kP/1T3/pK3 w gs 26", "74"),
    ("1NRbk/L4/4l/2n2/K1pS1 b - 55", "10"),
    ("1NNk1/5/1r2l/K1b2/5 w tsp 42", "126"),
    ("1pk2/3b1/2s1g/1L3/K1g1l w p 44", "47"),
    ("4P/T3N/2k2/Kb2n/4P w TS 42", "8"),
    ("2pk1/2p2/3t1/2s1n/1L2K b gs 27", "4"),
    ("5/1N1l1/5/k1P2/1gK2 b TSsp 53", "0"),
    ("S4/r2k1/l4/2K1R/tg3 w GS 48", "10"),
    ("LN2k/N2s1/1P3/4K/1p3 w Ts 60", "40"),
    ("1g2k/1SS1p/2GK1/2t2/p3t b - 41", "12"),
    ("4S/Nkb2/4P/3N1/3K1 b TPt 39", "79"),
    ("1g3/rt2P/k1B2/2K2/2L2 w GS 28", "8"),
    ("2B1P/2N2/1k2K/2nt1/p4 w St 60", "44"),
    ("2P1P/1B1N1/L4/1nk1K/5 w ts 70", "71"),
    ("2P2/K4/1N1k1/2bl1/s2pt w g 52", "42"),
    ("B3S/2N2/5/k3n/2Kll w 2P 52", "3"),
    ("4p/1K1k1/g2s1/B1tP1/1l1G1 b - 31", "12"),
    ("G1P1R/5/k2L1/3g1/1K3 b T2S 53", "82"),
    ("5/k1lP1/2B2/1R1K1/3l1 w GSg 66", "38"),
    ("3S1/1p2b/1T1l1/2kgR/K4 b g 27", "13"),
    ("4R/2k2/2l2/p1N1t/1g2K b 2s 31", "2"),
    ("5/1bPNk/5/BK3/2n1l w tp 54", "78"),
    ("L1K2/5/2b2/g2kl/1g3 w s2p 52", "91"),
    ("nk3/L4/s1P2/Sp1Kl/3G1 b - 27", "14"),
    ("2L2/k1l1G/5/p4/psg1K b s 65", "6"),
    ("2k2/L3N/1G3/1K3/2p1t w 2sp 58", "77"),
    ("GLP2/3B1/1K3/5/2kl1 w gsp 60", "110"),
    ("3G1/5/1G1R1/k4/p2lK w T2S 44", "1"),
    ("2Lk1/2p1s/4K/1NB2/l4 b GP 53", "3"),
    ("3l1/1pk2/T4/1KNP1/5 b G2S 21", "83"),
    ("p4/2s2/2skP/K2N1/1L3 w Gt 44", "48"),
    ("5/1bkNr/5/3t1/1Kl2 b Pgs 59", "39"),
    ("TB3/2N1k/5/g2K1/pp2T b s 41", "9"),
    ("lsn2/4k/pn1L1/5/3K1 b SP 49", "74"),
    ("P3k/g1b2/1L1t1/3BN/1K3 b P 35", "44"),
    ("1k1tR/r2G1/S3K/5/1n3 w St 32", "42"),
    ("k4/1s3/5/p1KNt/N3P w St 24", "45"),
    ("pgg1t/2b2/4K/LBk1R/5 w - 24", "4"),
    ("2k2/5/Kl1BP/2t2/4P b 2GS 59", "82"),
    ("2k2/l2B1/3r1/2b1K/n4 w GTP 70", "6"),
    ("5/1T2b/3s1/k1N1R/pt1K1 b g 55", "12"),
    ("2k2/r4/1p3/2Kl1/1S1gT w GS 44", "16"),
    ("2k1P/1b2n/3LL/1K3/5 w GPs 54", "44"),
    ("1L2R/5/2k2/4l/K3P b G2Sg 29", "84"),
    ("P1NS1/L4/3S1/3k1/K1nlr w - 68", "7"),
    ("2s2/2Nks/n4/p2K1/5 b 2tp 39", "5"),
    ("p4/2sk1/P1t1t/1Bg2/4K b g 37", "4"),
    ("k4/1gN2/1p1t1/TB2P/K1S2 w - 40", "14"),
    ("4N/Tb1P1/P1S1K/5/1k3 w GT 52", "8"),
    ("4P/Tk1t1/1s3/3K1/1S3 w Ggp 28", "4"),
    ("KLk2/P3g/5/1S3/1nl2 w SP 64", "7"),
    ("p4/T3b/5/BkPK1/4t b 2g 25", "11"),
    ("p2SG/1G1b1/3kt/1T3/2K1P b - 23", "18"),
    ("3g1/1pk2/5/tsP2/1g1K1 b ts 57", "4"),
    ("K2kP/5/1S3/5/p1l2 b GSgt 41", "84"),
    ("k3t/5/1K3/1bB1R/3l1 w Ggp 26", "81"),
    ("5/1kB2/2s2/r2P1/1n2K b G2t 27", "45"),
    ("5/1Lsk1/1P3/1ln1P/1n1K1 w s 70", "43"),
    ("4P/3k1/5/5/1sK2 w 2GTPts 44", "94"),
    ("Bg3/2k2/2s1p/L3R/2K2 b Tg 23", "45"),
    ("3T1/3SG/L2sp/g3k/2K2 w p 26", "41"),
    ("TB2N/1n1k1/5/p4/1btKP b - 37", "3"),
    ("N2K1/1TS2/1p2S/2k1l/5 b GP 43", "83"),
    ("4t/k4/2K2/p2N1/S1s2 w gtp 34", "117"),
    ("2L1P/1g1K1/pbS2/k3t/1g3 w - 44", "17"),
    ("3k1/r1l1l/5/s1r2/2SK1 w 2G 54", "18"),
    ("1L1k1/1L1B1/5/3K1/1n2r b GPs 41", "82"),
    ("N2k1/3B1/1sp2/3K1/1T3 w gtp 62", "117"),
    ("p2sk/T4/2s2/2t1P/K4 w 2G 36", "15"),
    ("2S1k/1Nn2/1l1tp/1b3/p3K b - 37", "2"),
    ("4R/k1PTG/5/1SK2/gt1s1 b - 49", "4"),
    ("2k1N/rs2T/2r2/K2s1/2l2 b g 67", "2"),
    ("p2l1/3bP/1k1g1/LB3/4K b G 23", "42"),
    ("2Lk1/N1s2/4s/n1p2/2p1K b t 63", "1"),
    ("gk1s1/p1B2/1r1t1/1T3/3K1 w g 54", "4"),
    ("3n1/1k1l1/2b2/2n2/1Kl2 b Psp 61", "39"),
    ("k1N2/2plb/3sg/1K2p/5 w t 42", "41"),
    ("1P3/L1G2/1b3/nK1tk/5 b sp 37", "11"),
    ("2LL1/2k1K/p1sR1/1n3/1n3 b s 49", "7"),
    ("4k/1K3/3b1/p1l1g/pt3 b Gs 69", "40"),
    ("Lk3/3N1/3t1/nb2P/1ps1K b - 33", "2"),
    ("5/BG2R/2k2/1L3/K2l1 b gsp 41", "18"),
    ("2P1k/rg3/1T1K1/2B2/3n1 b Ts 41", "45"),
    ("5/k1LP1/5/3K1/p4 w 2GSts 36", "85"),
    ("4T/2nkS/r3g/2L1S/p2K1 b - 39", "9"),
    ("LP3/N2K1/5/2ks1/1n3 b Tsp 37", "41"),
    ("p4/LL1Gb/2K2/3gk/3p1 w s 24", "43"),
    ("p1kl1/2s2/2s2/1n1P1/1K3 b gt 31", "4"),
    ("2k1P/KLB2/3B1/N1N1R/5 w T 48", "0"),
    ("4R/2S1P/K2l1/5/1n1lk w Gs 56", "37"),
    ("5/r2N1/3kt/1LN2/1K3 b Ssp 21", "41"),
    ("4s/1kp2/p1s1K/5/4l w 2gt 56", "84"),
    ("3s1/g1p1S/2k1p/KL3/1G2t b - 29", "10"),
    ("T4/3N1/1k1pK/5/3l1 b G2Sp 39", "81"),
    ("5/r1LL1/1b3/2KPb/kg3 b G 55", "2"),
    ("1g1k1/1bN2/4K/2l2/5 b TS2P 41", "116"),
    ("1Ns2/Bp2k/K1G1p/3t1/4t b - 43", "11"),
    ("1k3/1b1B1/3P1/1K2G/P3l w GT 42", "8"),
    ("1g1L1/5/2K2/5/G1Spk b SPt 53", "83"),
    ("2L2/3pk/2br1/2K1t/5 w Ggs 32", "88"),
    ("5/1sk2/5/pK2t/3l1 b 2GPs 51", "80"),
    ("1L1kN/3b1/5/2g2/4K b 2Pts 63", "39"),
    ("1Lk2/2br1/1S3/5/2nK1 b GTP 37", "15"),
    ("1g3/2k2/1L1N1/3S1/1K1pl w sp 38", "75"),
    ("4p/1k3/3S1/1K1tl/r4 b S2g 41", "42"),
    ("pLk2/3bG/5/BPN2/1K2t w - 34", "8"),
    ("L4/g1bk1/5/B2K1/4P b GTP 21", "115"),
    ("2k2/5/SK3/1S3/1p1l1 w GPgt 50", "79"),
    ("5/TG2B/2G2/1KP1k/4l w Sp 22", "37"),
    ("1T1k1/1b1p1/4t/b1P1G/2K2 b G 33", "42"),
    ("1kPP1/5/1Kbl1/2BG1/5 w Gt 70", "44"),
    ("3B1/1L1B1/ktK2/1r3/1n2p b G 63", "3"),
    ("L2L1/K1R2/3k1/1s3/3n1 w GSp 64", "45"),
    ("5/1kn2/4t/5/r1P1K b 2Sgt 23", "39"),
    ("ks1B1/1p2N/3G1/2K2/3lr w t 32", "39"),
    ("T2P1/3kN/1b3/B3n/KP2l w - 40", "14"),
    ("N3S/p1k2/3t1/1L3/1K3 w gsp 34", "118"),
    ("1g2N/2pTB/1k2K/5/1p2t b S 25", "42"),
    ("PL3/sk1Ns/5/3nr/3Kl b - 63", "3"),
    ("p1sG1/1k3/sT3/2K1R/5 w GT 38", "1"),
    ("3B1/rk1Tg/5/3SK/3tP w G 36", "14"),
    ("STNk1/5/P1RS1/1K3/3gl w - 42", "3"),
    ("2kn1/5/2l1P/T4/K2bP b gs 43", "7"),
    ("5/kB1t1/l4/4P/1K3 w 2GSP 48", "11"),
    ("2k2/rN3/5/1L1K1/2S2 w GTSp 24", "47"),
    ("p4/1N2S/1k3/4K/pnl2 b Ts 57", "42"),
    ("s2k1/5/2KG1/2Rl1/3p1 w gts 56", "112"),
    ("5/1k3/p1sKP/1B1N1/T2T1 b g 27", "14"),
    ("3k1/P1L1n/2G2/1K1tR/1S3 w S 26", "7"),
    ("3B1/G2T1/2k2/K2n1/4p b SPt 35", "87"),
    ("k1TG1/5/P4/2B2/K1P2 b Sgt 51", "50"),
    ("2N1k/3p1/5/1K1B1/ps2l b Gt 53", "46"),
    ("2sT1/1B2k/1R3/P2K1/3nt w g 70", "37"),
    ("1P1L1/S3k/L4/1K2s/3gg w p 48", "40"),
    ("5/k1L2/3r1/2KS1/1L3 w 2gsp 34", "123"),
    ("1k3/4P/S4/1p2K/1l3 w 2GSt 68", "41"),
    ("L2sP/k4/1nt1G/2pK1/1S3 b - 69", "4"),
    ("LNN2/1r3/2tb1/K3k/3p1 b s 69", "2"),
    ("5/3k1/1G3/1L3/1K2t w SPgsp 24", "128"),
    ("k2GP/bt3/P4/2n2/s2TK b - 59", "9"),
    ("1S2N/3sp/5/1K1kp/4l b Gt 63", "41"),
    ("1T1G1/5/S2k1/5/rK3 b TPgs 45", "2"),
    ("5/1Gk2/4s/K2P1/p1N2 w 2Ts 26", "4"),
    ("k1P2/5/5/1b2K/5 b G2TSPg 65", "173"),
    ("5/BN3/3k1/5/4K w G2TS2p 54", "47"),
    ("1k3/Rn1b1/1L1R1/1L3/1S1K1 w G 38", "8"),
    ("1k3/rnNb1/Ls2T/3KR/5 w - 30", "17"),
    ("n2lP/3N1/k4/4b/rLKS1 b - 21", "5"),
    ("1L2k/pG3/2LK1/5/1g3 w Ssp 36", "77"),
    ("1TK1L/k3g/5/2bB1/1n2P b P 43", "42"),
    ("2G2/1BrkN/5/B3K/1p1T1 w t 66", "39"),
    ("5/1Nk1b/P4/S3N/3Kp w Tt 58", "44"),
    ("pp1k1/3s1/1K3/1G3/3l1 b gts 61", "8"),
    ("1L1PN/Pk3/s4/3sK/4N w t 66", "45"),
    ("5/1GLkp/1S2l/pK2n/5 b s 51", "13"),
    ("p1TN1/5/1k1SK/p4/s1n2 b T 61", "43"),
    ("2L2/k1bs1/5/1KP2/g4 b Gtp 57", "39"),
    ("L4/1KnB1/3k1/b4/3l1 w g2p 70", "81"),
    ("t3s/1n1k1/1rg2/1L1PK/5 w s 34", "48"),
    ("5/1GL2/1KS2/4l/S3k w G2P 66", "1"),
    ("5/1G1kS/1T3/1n3/1pPKs w t 52", "35"),
    ("T3k/2K2/3B1/l3b/5 w P2gp 64", "79"),
    ("1k3/5/5/sgb2/1g1K1 b 2TPp 41", "3"),
    ("2k2/t2N1/3PS/1B2N/1Kl1P w - 56", "8"),
    ("T4/2kpK/1ss2/t1n2/p4 w g 38", "43"),
    ("3s1/rT1lR/5/BK3/g2k1 w G 32", "11"),
    ("1g3/k1G2/p4/1KB2/4P w TSt 24", "3"),
    ("G4/pk3/3B1/Kst2/4n b Pt 51", "41"),
    ("4N/3k1/K2s1/2t2/3t1 w S2Pg 64", "57"),
    ("R3N/2S2/5/2Bk1/1Klnl w P 60", "3"),
    ("1k3/1sRG1/b1T2/1NK2/1P3 b T 65", "47"),
    ("3t1/r2B1/kP1G1/5/T2K1 b Sg 57", "53"),
    ("Nsk2/5/R4/1K1g1/p2l1 w ts 60", "80"),
    ("L1s2/1N2k/1P2t/B1n2/4K w P 30", "10"),
    ("1k1l1/1BP2/1t3/2K2/SrNN1 b - 35", "2"),
    ("P3k/pN3/t2K1/B1n2/1s3 w T 54", "4"),
    ("1n3/4S/k1Ps1/5/1K2t w TPg 46", "47"),
    ("3k1/r3l/2G1P/sT3/2K1N w S 26", "11"),
    ("1P2k/5/1BK2/2l2/S3P w Ggt 68", "74"),
    ("2kP1/1g3/1S3/1LK1l/5 w gsp 66", "118"),
    ("1L2N/5/1K1t1/4k/3g1 w SPsp 26", "87"),
    ("TRp2/4N/2k2/B2l1/1K3 b GS 59", "78"),
    ("Sk3/4R/5/LK1t1/5 w 2GSP 40", "7"),
    ("Gp3/1k3/1b1P1/5/K1t1g w ts 64", "84"),
    ("1L1L1/S3r/2Sk1/G1P2/1nK2 w - 40", "10"),
    ("5/P4/1k1t1/3bp/SK3 b 2Gt 51", "39"),
    ("1G2k/1Knbl/r4/1L3/1S2P w - 24", "18"),
    ("3l1/2TP1/5/k1BK1/p1n2 w Gs 50", "37"),
    ("2kp1/1n3/rS3/1KTns/5 b t 41", "12"),
    ("L1L2/r2s1/k2K1/3S1/2nn1 b p 63", "3"),
    ("2k2/2n2/ST3/3lb/2K1P b Pg 21", "45"),
    ("2L2/1N3/K2l1/1ps1k/Sn3 b P 57", "37"),
    ("2n2/2Lkl/1g2P/1s3/3KP w s 28", "50"),
    ("1T3/l3P/1k3/1B1K1/pg3 w gs 28", "75"),
    ("1KN2/4b/1L2k/l4/1n3 b Psp 43", "41"),
    ("p1S1k/lK3/5/3P1/n1s1G w t 42", "38"),
    ("5/3N1/2Lk1/sK3/4S b GT2P 57", "121"),
    ("2N2/3B1/1K3/4k/1sp2 b GTPt 39", "125"),
    ("1sLN1/r1k1n/1r3/3sl/1K3 b - 45", "1"),
    ("k4/3p1/N1K2/5/1b1lr b gts 49", "6"),
    ("1L3/1nk2/4R/bKBt1/5 b Pg 29", "50"),
    ("1P2N/N1L2/1kb2/ls2K/2p2 w - 68", "15"),
    ("1k2S/r2G1/1L2S/L3K/2n1P w - 24", "2"),
    ("5/Nk3/5/L1t2/K1pbn b sp 53", "1"),
    ("ST3/2NKP/1S3/3n1/ptk2 w - 66", "3")
  )

}
