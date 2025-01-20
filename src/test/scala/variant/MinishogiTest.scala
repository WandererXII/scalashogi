package shogi
package variant

import shogi.format.forsyth.Sfen

class MinishogiTest extends ShogiTest {

  def perft(game: Game, depth: Int) = Perft.perft(game, depth)

  "calculate minishogi perfts" should {
    val game = Game(shogi.variant.Minishogi)
    "1 depth" in {
      perft(game, 1) must be equalTo 14
    }
    "2 depth" in {
      perft(game, 2) must be equalTo 181
    }
    "3 depth" in {
      perft(game, 3) must be equalTo 2512
    }
    "4 depth" in {
      perft(game, 4) must be equalTo 35401
    }
    // "5 depth" in {
    //  perft(game, 5) must be equalTo 533203
    // }
  }

  "default positions" should {
    "be identical" in {
      Game(shogi.variant.Minishogi).toSfen must_== shogi.variant.Minishogi.initialSfen
    }
  }

  "fixture perfts" should {
    "forall" in {
      forall(fixturePerfts) { line =>
        line match {
          case (sfenStr, d1) => {
            val game = Game(Some(Sfen(sfenStr)), shogi.variant.Minishogi)
            perft(game, 1) must be equalTo d1.toInt
          }
        }
      }
    }
  }

  // format: off
  val fixturePerfts: List[(String, String)] = List(
    ("2sg1/3kp/1RpB1/K1SR1/1G3 b B 15", "35"),
    ("r2k1/b1sg1/S4/K4/3+p1 b RBPg 33", "54"),
    ("1+S3/P1g2/2Sg1/bK2k/2+p2 b Rrb 59", "28"),
    ("1r1gk/4p/1S3/PKS1b/3BR w g 12", "27"),
    ("r4/PB1s1/1b2k/1K1Rp/5 b GSg 53", "49"),
    ("4k/bRKP1/G4/P4/2+s2 b Rbgs 39", "29"),
    ("+P2sk/3r1/2+B1G/2G1p/K4 w Rbs 36", "35"),
    ("2sg1/+P3k/1bS2/4p/KG1BR w R 12", "16"),
    ("r1s2/P2k1/3gB/1SG1p/1K1BR w - 16", "6"),
    ("Gb3/2sk1/S2gp/5/K2BR w RP 20", "14"),
    ("1g3/ps1Sk/5/1K3/B3+p b Rrbg 47", "29"),
    ("s1r2/5/2S2/1K1kB/1G1g1 w Prbp 42", "4"),
    ("1b1g1/2sk1/4R/K1B2/RGS2 w Pp 14", "22"),
    ("5/1s1k1/1rr1P/1G2K/1B3 w BGSP 48", "16"),
    ("+P1s1R/2b1p/1g3/1SB1r/K1G1k b - 25", "20"),
    ("1b2s/4P/Ks3/2k2/R4 w Br2gp 52", "63"),
    ("+P2+Bg/3s1/G2k1/2R2/1K1+p1 w RBs 40", "25"),
    ("3g1/bsk1p/2r1R/K1G2/2S2 b Pb 19", "26"),
    ("2+P2/3k1/2G1r/K+Rg1p/2+s1+b w Bs 60", "6"),
    ("2k2/b3G/1B1S1/4p/1K1P1 w GS2r 52", "22"),
    ("2s2/1g3/GBsbk/4p/K1R2 b Rp 27", "31"),
    ("r1s2/P2g1/2S1k/2P2/1K3 b R2Bg 27", "44"),
    ("1bSSk/rB2g/2R2/GKP1+p/5 w - 54", "14"),
    ("1rk2/b2g1/1BS1p/P1G1R/1K3 b S 17", "30"),
    ("1b1g1/P1B2/2k2/KG2p/2SR1 w RS 14", "2"),
    ("5/P+S1b1/1B1g1/1G1k1/SKR1+p b R 53", "38"),
    ("2Gbk/4p/K1S1g/4R/1P1B1 b RS 35", "51"),
    ("3k1/sBr2/3B1/1S2p/GK3 w rgp 42", "56"),
    ("4k/PS2g/GR1s1/4B/1K1+p1 b RB 37", "47"),
    ("4k/G1P2/5/1KSR1/+pGS2 b Brb 35", "37"),
    ("kr3/2sRb/P1G1P/KS3/1G1+b1 w - 26", "22"),
    ("5/b3k/5/R2Gb/K1S2 w RGS2p 38", "28"),
    ("r1k2/2s1g/PB1b1/1G3/KS3 w Pr 12", "33"),
    ("3k1/1g3/1G2r/2pSp/K2+b1 w RSb 44", "36"),
    ("4s/BP2g/1Spk1/K4/2+R+b1 w RG 48", "13"),
    ("2S1k/G+B3/2S2/2K2/+rrB1+p b Gp 41", "35"),
    ("1+Rgkr/5/s4/4p/Ks1B1 b BPg 25", "44"),
    ("+Pb1+S1/1g2r/2k2/K4/G2+p1 w RBS 38", "22"),
    ("4k/4g/1s1Bp/1r1GB/3K1 w Rsp 22", "42"),
    ("r2g1/P1bs1/3kp/1S3/KG2R b b 11", "12"),
    ("5/1sbk1/GR1B1/K1Rp1/s2+p1 w G 48", "17"),
    ("gs3/2gk1/1s3/2R1p/K2BR b Bp 23", "25"),
    ("1r1b+P/1+S1B1/5/5/K3k b RGSgp 49", "69"),
    ("1rs1g/P1bk1/4p/3SB/KGR2 w - 14", "17"),
    ("3gk/K4/p1G2/2B+p1/5 w BS2rs 30", "47"),
    ("4k/K1g1b/3Rg/1sp2/3+p1 b BSr 53", "43"),
    ("1r1gk/P4/1G2p/1S2+b/KR3 w Sb 26", "30"),
    ("1b1k1/2rsP/Pg3/5/K1S2 b rbg 19", "6"),
    ("5/1g2g/4k/S1Kp1/+r2B1 b RBSp 57", "55"),
    ("+P1R2/b4/3k1/4s/K1+s2 b BGPrg 43", "69"),
    ("2s1r/1P1+B1/3s1/K2kp/3+Bg w RG 52", "2"),
    ("2sk1/1+Pr2/3P1/KGsgb/5 b rb 31", "14"),
    ("2Bk1/1sg2/p3p/4S/K+B3 b Rrg 51", "32"),
    ("5/3k1/2gr1/2r1p/K4 b BSbgsp 55", "39"),
    ("1bsg1/1k3/5/rKG1B/1S2R b Pp 13", "3"),
    ("3k1/2bsg/p4/g2Kp/5 w RBSr 30", "34"),
    ("3gg/bs2R/1s1kp/1R1B1/+p3K b - 51", "15"),
    ("3g1/Ps1k1/3bp/KRr2/1g1B1 b s 25", "5"),
    ("1b1k1/5/S1p1R/K1g2/1G1B1 b RSp 21", "50"),
    ("2+P2/3g1/G+bSsk/3+b1/K3+p w 2r 36", "42"),
    ("1g2+R/3P1/2K1k/2S2/3+p+s w R2bg 42", "2"),
    ("5/4k/5/KP1g1/3+s1 w RBrbgsp 28", "108"),
    ("2g1k/bs3/4p/rG1SB/1KR2 b p 15", "22"),
    ("3k1/1r2g/P1Ss1/1GB1p/K3R w b 28", "29"),
    ("1bg1k/2sr1/P2S1/2P1B/1GK1R b - 19", "13"),
    ("1b1gk/s2R1/S1G2/r3p/1KP2 b b 23", "19"),
    ("r1sg1/1Pb1r/P3k/GK2B/2S2 b - 13", "13"),
    ("3b1/+PPg1k/1Rs1b/5/1K1R+s b g 43", "16"),
    ("1G3/5/K1kp1/s2r+p/b1G+r1 w bs 60", "44"),
    ("1B2k/2B1g/1KSs1/1R2+p/G1+p2 w R 46", "12"),
    ("2kg1/2p2/2+bRp/K1SbS/5 b RG 49", "44"),
    ("2s1r/P2g1/1k3/2b1s/KG1B1 w rp 30", "51"),
    ("+S4/+Pbrk1/2g2/1Br2/1K3 b GPs 49", "38"),
    ("3rk/S1gG1/5/3Sp/K2+b1 w rbp 22", "3"),
    ("2g1k/Ps2b/4p/bSR2/K2G1 b R 17", "31"),
    ("1g1g1/2b1k/P1Ss1/1K3/5 b 2Rbp 39", "25"),
    ("+P1s2/4k/1KG2/2Sgb/3Br w rp 22", "44"),
    ("2rk1/5/rG1B1/1S2g/1K1B1 w SPp 32", "32"),
    ("1+P2k/1s3/1B1g1/5/K1g2 w 2RBPs 30", "34"),
    ("B4/2r1k/5/p2Ss/K1gR+p b bg 39", "2"),
    ("1b3/rsgkS/1G3/1K3/P3+b w rp 28", "48"),
    ("+P2k1/1P1gs/4R/K1b2/2+s2 b rbg 35", "13"),
    ("+P1gk1/b4/R4/1K2p/2Sr1 w bgs 50", "70"),
    ("4k/PS1r1/3bs/K2gp/GR1B1 b - 45", "19"),
    ("3g1/1Kb1p/5/G3k/3+b1 w RSrsp 26", "66"),
    ("2s1k/2Bg1/P1S2/1KB1p/2r2 w Rg 20", "34"),
    ("5/bS2k/GB2p/K4/R1+r1g b SP 33", "41"),
    ("1pk2/2B2/4g/1KG1P/R1SB1 w RS 28", "8"),
    ("1+R1bk/+P1+Ss1/B3r/G1g2/K2+p1 w - 54", "20"),
    ("s2k1/3b1/+B1G2/K1g+p1/5 b RSrp 47", "48"),
    ("2rgk/bs3/4S/P3R/K1GB1 b P 11", "28"),
    ("r+S3/2gk1/P1b2/K2Sp/5 w RBg 34", "32"),
    ("s2gk/P4/3b1/RSG2/K1R1+p w B 26", "15"),
    ("+P2k1/2b1g/1R3/1S3/1K1+bR w Sgp 52", "47"),
    ("3+S1/+P+B2k/1s3/3r1/+b1K2 w Pr2g 58", "57"),
    ("p1kgB/1R3/r1K2/s1b2/3+p1 b Sg 59", "4"),
    ("+P1G2/2b1k/1Ss2/R4/1GKR+p b b 27", "25"),
    ("3r1/b2kB/3g1/1G1P1/K1S2 b RSP 29", "56"),
    ("r2gk/1s3/P1P1S/1K2b/1G3 w Rb 20", "32"),
    ("1bs1g/r2kR/5/P1KG1/2SB1 w P 12", "3"),
    ("1+S3/1K1sk/3g1/2+b+bG/1P3 b RPr 47", "5"),
    ("rbr1k/3sp/PG3/5/K1SB1 b G 21", "28"),
    ("+P2k1/+S4/3+rB/KR1gs/G3+p b b 51", "17"),
    ("1k3/5/1K1GG/rb2p/5 b RB2SP 45", "78"),
    ("r4/3k1/P1sg1/K1b2/2g+p1 b rbs 29", "2"),
    ("rb2r/1s1g1/3Bk/PS3/1G1K1 b P 33", "27"),
    ("4k/1+PR2/sGS1b/1P2r/1K3 w bg 56", "44"),
    ("4k/rsbgp/5/PKS2/1GRB1 b - 11", "7"),
    ("3k1/S3p/2G2/K2sb/2+r1+p w RBg 46", "35"),
    ("1b3/R2rk/Ks2p/s3g/1G3 w bp 52", "45"),
    ("1b1ks/5/1S3/GK2p/4R w RBGP 22", "9"),
    ("1+PK1+S/5/4k/1G3/+r1g2 w RBSPb 42", "30"),
    ("5/G1b1k/R3p/1K1s1/2+p2 w RGSb 28", "35"),
    ("G1sbk/R4/1Bsg1/4p/2K2 w RP 46", "16"),
    ("4+P/P1k2/+b4/2s1R/1K3 b GSrbg 41", "3"),
    ("s2k1/3sg/2R2/2Gp1/K2BR b BP 33", "46"),
    ("1k1b1/1s3/3s1/2Gg1/1K1B+r b R2p 41", "24"),
    ("1bs1g/2bkp/P4/1G2R/K1S2 b R 15", "28"),
    ("1g2B/2s1R/Pk3/2BP1/K2S1 w RG 50", "2"),
    ("sk3/Rb3/s2bg/5/K3+p b Rgp 57", "20"),
    ("5/brBkg/P4/5/K3R w SPgs 22", "52"),
    ("pgk2/3sb/3R1/2g1b/K2+s1 b RP 47", "36"),
    ("1+Rsk1/4p/KGg2/1pb1B/1s2R b - 19", "17"),
    ("1b1gk/rss1p/4R/PK2B/1G3 w - 12", "15"),
    ("rb2k/3sg/1K1B1/P2R1/1GS1+p w - 12", "15"),
    ("1kSb1/1s3/3+r1/5/K1+pg1 w RBGp 54", "42"),
    ("+Pbsk1/1g2p/5/1K2B/1GS1R b R 11", "30"),
    ("G2k1/1P3/KG1s1/S1B+B+p/5 w Rr 52", "27"),
    ("1+P1+B1/k2S1/2r2/2s1p/KG3 b RBG 27", "69"),
    ("+P3k/2b2/s3g/G3S/2KB1 w RPr 48", "32"),
    ("1+P2s/1Sg1k/R4/3Kb/1G2P b RB 49", "55"),
    ("1b1g1/1k3/4g/3S+p/K4 b 2RBSp 39", "62"),
    ("+B1s2/2k2/2Bg1/P2S+p/2KR1 w Rg 30", "28"),
    ("5/3k1/rS1s1/4B/2K2 w BG2Prg 40", "55"),
    ("3k1/2g2/G1bSB/5/1KPR1 b RPs 49", "45"),
    ("+P1s+B+R/2b2/3k1/1K3/5 w RGSPg 26", "25"),
    ("1r3/1s1kg/4G/P1r1b/1K1B1 w sp 36", "7"),
    ("+P3k/2+B2/G1SB1/1K3/3R+p w RGS 34", "2"),
    ("+PbG1k/1s3/1r3/1K1G1/4+p b RSb 39", "4"),
    ("2sg1/1r1k1/PK1bp/r1S2/1G1B1 b - 19", "1"),
    ("1R3/P+P3/1sK2/1s1gk/4g b R2B 59", "2"),
    ("1bsg1/3kp/KGp2/1R1S1/3BR w - 12", "11"),
    ("Rb3/Ps1k1/1S1gp/K4/1G1B1 b r 15", "13"),
    ("2bg1/+P1b1k/2s2/4P/K2+s1 w Rrg 54", "54"),
    ("1+Rsk1/5/3RB/K1gB1/4s w Pgp 60", "4"),
    ("4R/2Bks/5/K1srg/3+p1 b BPg 43", "48"),
    ("3k1/P4/1b1s1/1GG2/1KSB+p b Rr 31", "27"),
    ("rb3/P3k/1Bs1p/1K1SR/5 b 2G 21", "7"),
    ("2k2/2B1R/s4/1Sb1p/KG3 b Grp 29", "36"),
    ("4r/+Sbsk1/1R2p/P1Bg1/K2G1 b - 47", "21"),
    ("G2+P1/1Rb2/4k/1RG2/1K1sb w Sp 42", "31"),
    ("2kS1/4P/K2+R1/1gbr1/G4 w bsp 60", "61"),
    ("1bsg1/P4/1G1k1/K3p/2SBR w R 12", "13"),
    ("4k/2gs1/5/K1bSp/G2R1 b Brp 25", "28"),
    ("1b2r/s1k1s/5/5/1P1K1 b GPrbg 59", "36"),
    ("+P3g/1s3/R2bk/B1r1p/GKS2 w - 26", "19"),
    ("1r3/P1Ssk/4g/5/KG1B1 b Brp 17", "33"),
    ("1B2k/r1B+rg/5/1KS2/1G3 b SPp 19", "46"),
    ("r3s/PK1gk/1SG1p/2+b2/3+b1 w r 24", "35"),
    ("1b2k/1Sg2/3r1/2Kps/G2B1 w RP 32", "20"),
    ("2g1k/r1G1S/1R3/1K3/3+p1 w BPbs 36", "1"),
    ("2k2/r3p/1BKBg/4r/G4 b Ssp 31", "33"),
    ("k4/3G1/2gb1/bS2r/1RK1+p w SP 54", "26"),
    ("1r2k/1sg1B/P3R/K1G2/3S1 w Bp 34", "25"),
    ("5/+P1k2/G2r1/3+s1/1KP+s1 b R2BG 59", "61"),
    ("4r/3Bk/GBs1p/PS3/K3g b r 41", "17"),
    ("2k2/2rP1/sG1B1/3Rp/K1g2 w BS 42", "14"),
    ("+P3k/B1B2/1gs1r/RG3/1K2+p w s 34", "31"),
    ("+P3k/2B2/KBr1g/3s1/1RS1+p w g 34", "31"),
    ("r1skr/2b2/P3g/G4/K1SB1 b p 15", "12"),
    ("rG3/2Gsk/2Pbp/1K3/R3+b w s 34", "34"),
    ("r3+S/1+Bgk1/1S2g/4r/2K2 b b2p 55", "17"),
    ("1r2+P/Pss2/4k/b3r/K3g w BG 38", "23"),
    ("3k1/+P1bs1/5/1K2p/G2g1 b Rrbs 19", "26"),
    ("2+Pb1/rb2S/P1Sgk/2g2/KR3 w - 30", "22"),
    ("1r2g/Pk2R/1B3/3s1/KGSB1 w P 20", "5"),
    ("1bsk1/g+P1gP/2B2/R1RS1/2K2 w - 50", "15"),
    ("+P1+B2/5/3k1/1G3/K1+bg+p w RSrs 34", "44"),
    ("r2gk/1s2p/P1S2/4b/1KG1R b b 13", "16"),
    ("1rG2/Pr2b/K4/2k1B/1s1+pg b S 57", "23"),
    ("1b1gk/1rs2/PB2g/1K1S1/4R b P 15", "25"),
    ("5/r1Gg1/1KB1k/5/2SB1 w S2Pr 34", "31"),
    ("4k/1r1g1/P3p/2BRb/1GSK1 b s 23", "2"),
    ("1+Bg1k/1sR1B/p3S/1K3/1G2+p b R 21", "38"),
    ("3g1/bsk1p/p2r1/G1B2/1KR2 b s 17", "12"),
    ("r4/P1gk1/K2b1/2G2/2SR+p w Sb 28", "31"),
    ("1r2k/1sSgp/5/bK2B/2G1R b p 17", "18"),
    ("1+Ss1k/5/2g1p/P1G1R/K2B1 w RB 12", "13"),
    ("r2g1/b1sk1/P4/2S1R/KG1B1 b P 11", "25"),
    ("1+B2k/5/P1G1g/4R/3K1 w R2SPb 42", "22"),
    ("1r2k/b2sg/5/R1G1+p/P1K2 w Bs 44", "36"),
    ("rpg1k/G1B2/4p/K2sb/3RS b - 43", "20"),
    ("2+P2/s2kg/g4/1Sr1p/K1B1R w B 48", "19"),
    ("sb2k/r4/1B2p/G1S2/1K1R1 w gp 36", "37"),
    ("+P1sgk/5/1bGb1/1K2R/1S3 w Pr 30", "5"),
    ("+R4/2bgk/1Bs2/RG3/1KS2 b 2p 31", "20"),
    ("2k2/3P1/1B3/K4/G2S+p b RBGSr 29", "86"),
    ("+Pbk2/2gR1/1s3/1K1Sp/G3R b b 23", "3"),
    ("r1sgk/b4/5/K3B/1GS2 b Prp 11", "29"),
    ("1b2k/1s2g/r2S1/1KG1B/4R b Pp 15", "26"),
    ("2r1g/b1Bk1/5/PKS1p/G1S2 w r 20", "27"),
    ("5/1Gsk1/K4/4r/1GSB+p b RBp 35", "50"),
    ("r1s1k/1rB2/Pbg1p/2S2/1K3 w g 16", "34"),
    ("1b3/rsgk1/1B3/2K2/1GS1+p w Rp 24", "34"),
    ("3gk/1sp2/PK1B1/BS3/RG1R1 b - 27", "3"),
    ("1rs2/b2k1/S1gBp/P3G/K4 w r 22", "35"),
    ("S+P1B1/2k2/1g3/1R1Rp/K1+sB1 w g 56", "2"),
    ("+S4/G1bkg/P4/1R2p/KB2R w S 36", "14"),
    ("2s1k/4p/2SgR/rG3/1K1B1 w bp 14", "43"),
    ("K3k/2p2/Rbg2/S1g2/2S1+p w rb 48", "45"),
    ("k2+Bg/B1s2/3S1/pR2R/1K3 b GP 51", "59"),
    ("2gg1/b2kp/pB1sR/r2S1/3K1 b - 27", "12"),
    ("1k2b/r2s1/1b3/1KG1p/R1S2 w gp 36", "45"),
    ("G+S1k1/1+r3/3bp/3r1/2K1+b b Pgs 53", "17"),
    ("1+S2k/p3B/3g1/rG1sB/1K3 b Rp 35", "29"),
    ("+Sb2k/1G2+r/K1s2/2+p2/P2B1 b RG 45", "42"),
    ("3k1/K+Prr1/G2b1/1Ps2/2+b2 w gs 60", "48"),
    ("1G2k/r4/P1g1+p/1SBS1/1K3 b Br 25", "36"),
    ("1b2k/S2g1/KBS1p/3R1/2G1R b p 29", "31"),
    ("1+B2k/1s3/1S2g/rK1+p1/2G1R b BP 31", "3"),
    ("rbp1k/5/sg1Bp/G2R1/K1S2 w - 18", "16"),
    ("r2g1/1s1kp/1b3/PSG1B/KR3 b - 13", "18"),
    ("2sB1/rg1k1/P4/2G1p/bKS2 b r 19", "18"),
    ("1b1k1/1pgs1/1G1BP/R2S1/K4 w R 34", "13"),
    ("5/1+PGbk/2Prs/5/1K1S1 w RBG 50", "10"),
    ("+P1sgk/5/2bG1/2r1p/KRS2 b b 29", "3"),
    ("2r1k/P1+B2/SG1Br/1K2p/5 b GS 35", "50"),
    ("2+Pg1/PG3/1S1bk/K4/1+s1Rr b b 59", "15"),
    ("2sgk/P1b2/3S1/K3p/1G2R b Rb 13", "5"),
    ("1bkg1/5/PR2p/2G1B/K2R1 b 2S 23", "44"),
    ("5/1rb1k/P2g1/1G3/K2+p1 w rb2s 24", "72"),
    ("5/K2k1/R4/2G2/+r2+p1 b 2bg2sp 59", "12"),
    ("1+P2k/b1G2/2r1g/K1B2/4S w RSP 32", "12"),
    ("s4/b1gk1/r3p/PK1S1/2GBR w - 22", "20"),
    ("2+Bpk/1s2p/1G3/KSg2/3R1 w RB 28", "13"),
    ("1rk1G/1s2P/2P1K/1+r3/3B1 w Gbs 60", "47"),
    ("rbk2/5/Pg1Gp/3R1/1KSB1 w S 24", "14"),
    ("1+P1k1/2b2/K2g1/s2rb/5 w Rgsp 50", "66"),
    ("+B4/+b1s1k/1g3/rS3/4K b R2Pg 49", "43"),
    ("3r1/PK2k/5/RG1Sb/1GS2 b Pb 33", "26"),
    ("r3k/3sg/P3R/1G1S1/K1P+b1 w b 12", "32"),
    ("2rk1/S1p2/2g2/K3R/1G1+p1 b S2b 43", "35"),
    ("+P1gk1/B1s2/1+RG2/R3p/KbS2 b - 43", "25"),
    ("3G1/P+b3/1k1r1/R3p/2KG1 w BSs 60", "36"),
    ("3rk/2G2/1R1p1/2K2/+s1S1+p w Bbg 40", "42"),
    ("r2g1/1s1kp/4R/PG2b/K1SB1 b - 11", "19"),
    ("2k2/2b1g/3S1/rS1+p1/1K1B1 b rgp 31", "14"),
    ("+P2b1/gk3/2s2/S1GRB/K2R1 b P 45", "33"),
    ("s4/Bsgk1/1G3/2R1p/+p1K1R b B 25", "32"),
    ("+Pr1k1/1p3/1GS2/K1BSR/2g2 w B 50", "7"),
    ("r4/+S1sk1/P2g1/1KB2/2R1+p w bg 42", "51"),
    ("G1s2/r1Pg1/2B2/2K1k/5 w Rbsp 32", "62"),
    ("r1k2/3sS/1G3/2p2/1K3 b 2BPrg 39", "46"),
    ("1+P2g/3sk/1+r3/1G1+p1/K4 b RBSb 47", "61"),
    ("2sb1/3k1/+R1s1p/G4/+b1RK1 b Pg 45", "26"),
    ("1+B2k/s1G1g/3S1/K1R1+p/R1b2 w p 44", "29"),
    ("2s1k/1+r3/2gBp/1S2R/1KPB1 w g 38", "34"),
    ("4g/1+Bg2/P1skp/4r/1KS2 w Rb 22", "35"),
    ("5/2G1+B/pks1B/3S1/K1GR1 b RP 41", "48"),
    ("+P4/RsrGs/3GP/2k1B/K2b1 b - 51", "16"),
    ("K4/1+PR1R/1B1k1/3g1/1+s3 b bgsp 45", "21"),
    ("rbr1k/3s1/3g1/P2S1/K1GBP b - 13", "14"),
    ("3gk/P3s/1P3/2r1G/1KR2 b BSb 23", "43"),
    ("2sk1/2g2/PBS1R/5/2K2 w RBGP 24", "3"),
    ("3k1/+P1rg1/G1r1P/2B1S/2K2 w bs 50", "43"),
    ("s2k1/1g2R/G2b1/2P1+p/K1R1S b B 49", "33"),
    ("1+B3/1Gg1k/K4/2R+b+p/1s3 w RPs 54", "42"),
    ("3p1/p1Bk1/5/rg3/1R1K1 b BG2S 41", "65"),
    ("1k3/1s1S1/g2+r1/B4/1K1+p1 w rbgp 58", "87"),
    ("3gk/rG1Pb/3p1/s2K1/3B1 b RS 29", "6"),
    ("+P4/2bgk/K1s2/3GB/5 w Rrsp 24", "61"),
    ("1GGbk/5/5/P2sK/2r1b w Prs 56", "56"),
    ("4k/+P4/P4/2RKG/5 w R2BG2S 50", "3"),
    ("5/1gsk1/r4/S1b2/2gBK b RPp 39", "34"),
    ("1b1k1/r2g1/P1s1R/1G3/1SKB1 b p 21", "18"),
    ("+P1s1k/5/1P3/Kg1g1/B4 b Rrbs 35", "3")
  )

}
