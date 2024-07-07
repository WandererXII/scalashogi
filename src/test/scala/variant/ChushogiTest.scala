package shogi
package variant

import format.forsyth.Sfen
import format.usi.Usi

class ChushogiTest extends ShogiTest {

  def perft(game: Game, depth: Int) = Perft.perft(game, depth)

  "calculate chushogi perfts" should {
    val game = Game(shogi.variant.Chushogi)
    "1 depth" in {
      perft(game, 1) must be equalTo 36
    }
    "2 depth" in {
      perft(game, 2) must be equalTo 1296
    }
  }

  "default positions" should {
    "be identical" in {
      Game(shogi.variant.Chushogi).toSfen must_== shogi.variant.Chushogi.initialSfen
    }
  }

  val chushogiPerfts = List(
    ("12/12/12/12/12/12/5N6/12/12/12/12/12 b", 88),  // solo lion
    ("12/12/12/12/12/12/5+O6/12/12/12/12/12 b", 88), // solo +lion
    ("12/12/12/12/12/12/5+H6/12/12/12/12/12 b", 41), // solo falcon
    ("12/12/12/12/12/12/5+D6/12/12/12/12/12 b", 40), // solo eagle
    ("12/12/12/12/7g4/6n5/5N6/12/12/12/12/12 b", 88),
    ("12/12/12/12/4B2l4/4S7/5N6/7n4/12/12/12/12 b", 98),
    ("11k/12/12/12/12/4r7/12/12/9n2/4+o7/12/6B4K b -", 8),
    ("11k/12/12/12/12/4r7/12/12/9n2/4+o7/12/6B4K b 8j", 7)
  )

  "chushogi positions" should {
    "forall" in {
      forall(chushogiPerfts) { line =>
        line match {
          case (sfenStr, d1) => {
            val game = Game(Some(Sfen(sfenStr)), shogi.variant.Chushogi)
            perft(game, 1) must be equalTo d1.toInt
          }
        }
      }
    }
  }

  "move legaility" should {
    "lion" in {
      val sit = Sfen("12/12/12/12/12/6+O5/12/12/12/12/12/12 b").toSituation(shogi.variant.Chushogi).get
      // 1 step, 0 dist move
      sit(Usi("6f6f").get).isValid must beFalse
      // 1 step, 1 dist move
      sit(Usi("6f5e").get).isValid must beTrue
      sit(Usi("6f7g").get).isValid must beTrue
      // 1 step, 2 dist move
      sit(Usi("6f7d").get).isValid must beTrue
      sit(Usi("6f4h").get).isValid must beTrue
      // 2 step, back to start
      sit(Usi("6f6g6f").get).isValid must beTrue
      // 2 step
      sit(Usi("6f5e6d").get).isValid must beTrue
      sit(Usi("6f7g8h").get).isValid must beTrue

      val sit2 = Sfen("3l8/11p/10p1/3n5+D2/2i1f7/3N5G2/9+H2/2t2P1i4/2p3+D5/2+H9/12/12 b")
        .toSituation(shogi.variant.Chushogi)
        .get
      sit2(Usi("3g3e").get).isValid must beTrue
      sit2(Usi("3g3f").get).isValid must beFalse
      sit2(Usi("6i4g").get).isValid must beTrue
      sit2(Usi("6i8g").get).isValid must beTrue
      sit2(Usi("6i7h").get).isValid must beFalse
      sit2(Usi("6i5h").get).isValid must beTrue
      sit2(Usi("6i5h6i").get).isValid must beTrue
      sit2(Usi("6i5h4g").get).isValid must beTrue
      sit2(Usi("3d2c").get).isValid must beTrue
      sit2(Usi("3d1b").get).isValid must beTrue
      sit2(Usi("3d2c1b").get).isValid must beTrue
      sit2(Usi("3d1b2c").get).isValid must beFalse
      sit2(Usi("10j10i").get).isValid must beTrue
      sit2(Usi("10j10h").get).isValid must beTrue
      sit2(Usi("10j10i10h").get).isValid must beTrue
      sit2(Usi("10j10h10i").get).isValid must beFalse
      sit2(Usi("9f10e").get).isValid must beTrue
      sit2(Usi("9f8e").get).isValid must beTrue
      sit2(Usi("9f9d").get).isValid must beFalse
      sit2(Usi("9f10e9d").get).isValid must beFalse
      sit2(Usi("9f8e9d").get).isValid must beTrue
      sit2(Usi("9f9e9d").get).isValid must beFalse

      val sit3 =
        Sfen("11l/6l5/5Nn5/11n/9N2/12/1N10/9n2/1n7N2/4r7/4nN6/4r7 b").toSituation(shogi.variant.Chushogi).get
      sit3(Usi("7c6c").get).isValid must beTrue
      sit3(Usi("7c6b6c").get).isValid must beTrue
      sit3(Usi("3e1d").get).isValid must beFalse
      sit3(Usi("3e2e1d").get).isValid must beFalse
      sit3(Usi("3i3h").get).isValid must beTrue
      sit3(Usi("3i3h3g").get).isValid must beTrue
      sit3(Usi("3i2h3h").get).isValid must beTrue
      sit3(Usi("7k8k").get).isValid must beTrue
      sit3(Usi("7k8j8k").get).isValid must beTrue
      sit3(Usi("7k8l8k").get).isValid must beTrue
      sit3(Usi("7k8k8j").get).isValid must beTrue
      sit3(Usi("7k8k8l").get).isValid must beTrue
      sit3(Usi("7k7j8k").get).isValid must beTrue
      sit3(Usi("11g11i").get).isValid must beTrue
      sit3(Usi("11g11h11i").get).isValid must beTrue

      val sit4Base = Sfen("11k/12/12/10bm/9N2/12/5n6/6N5/5r6/12/9K2/12 b")
        .toSituation(shogi.variant.Chushogi)
        .get
      val sit4     = sit4Base(Usi("6h7g").get).toOption.get
      val sit4Alt  = sit4Base(Usi("6h7i6h").get).toOption.get
      val sit4Alt2 = sit4Base(Usi("6h5g6h").get).toOption.get

      sit4.history.lastLionCapture must beNone
      sit4(Usi("2d3e").get).isValid must beTrue
      sit4Alt.history.lastLionCapture must beNone
      sit4Alt(Usi("2d3e").get).isValid must beTrue
      sit4Alt2.history.lastLionCapture must beNone
      sit4Alt2(Usi("2d3e").get).isValid must beTrue

      val sit5Base = Sfen("12/12/12/12/12/12/12/12/4+ho3n2/4N7/12/6B5 w")
        .toSituation(shogi.variant.Chushogi)
        .get
      val sit5     = sit5Base(Usi("8i8j8i").get).toOption.get
      val sit5Alt  = sit5Base(Usi("8i8j").get).toOption.get
      val sit5Alt2 = sit5Base(Usi("7i8j+").get).toOption.get

      sit5(Usi("6l3i").get).isValid must beFalse
      sit5.history.lastLionCapture must beSome
      sit5Alt(Usi("6l3i").get).isValid must beFalse
      sit5Alt2(Usi("6l3i").get).isValid must beFalse
      sit5Alt2(Usi("6l8j").get).isValid must beTrue
    }
    "wiki and more" in {
      val sit = Sfen("12/12/12/12/7g4/6n5/5N6/12/12/12/12/12 b").toSituation(shogi.variant.Chushogi).get
      sit(Usi("7g6f").get).isValid must beTrue
      sit(Usi("7g6f5e").get).isValid must beTrue
      sit(Usi("7g6f6e").get).isValid must beTrue

      val sit2 = Sfen("12/12/12/12/4B2l4/4S7/5N6/7n4/12/12/12/12 b").toSituation(shogi.variant.Chushogi).get
      val sit2opp = sit2.switch
      sit2(Usi("7g5h").get).isValid must beFalse
      sit2(Usi("7g5e").get).isValid must beTrue
      sit2(Usi("8e5h").get).isValid must beTrue
      sit2opp(Usi("5h7g").get).isValid must beFalse
      sit2opp(Usi("5h5j").get).isValid must beTrue

      val sit3    = Sfen("12/12/12/12/3n8/12/5N6/5P6/7b4/12/12/12 b").toSituation(shogi.variant.Chushogi).get
      val sit3Opp = sit3.switch
      sit3(Usi("7g9e").get).isValid must beFalse
      sit3(Usi("7g5i").get).isValid must beTrue
      sit3Opp(Usi("9e7g").get).isValid must beFalse
      sit3Opp(Usi("9e10e").get).isValid must beTrue
      sit3Opp(Usi("5i7g").get).isValid must beTrue

      val sit4 =
        Sfen("12/12/12/12/3n1H6/3sp7/5N6/5P6/1k5b4/12/12/12 b").toSituation(shogi.variant.Chushogi).get
      sit4(Usi("7g9e").get).isValid must beTrue
      sit4(Usi("7g8f9e").get).isValid must beFalse

      val sit5 = Sfen("12/12/12/12/12/4N7/4p7/4n7/12/12/12/12 b").toSituation(shogi.variant.Chushogi).get
      sit5(Usi("8f8g").get).isValid must beTrue
      sit5(Usi("8f8h").get).isValid must beFalse
      sit5(Usi("8f8g8h").get).isValid must beFalse

      val sit6 =
        Sfen("12/12/12/12/6+o1r3/4gi6/6N5/7s4/8n3/12/12/12 b").toSituation(shogi.variant.Chushogi).get
      sit6(Usi("6g4i").get).isValid must beFalse
      sit6(Usi("6g5h4i").get).isValid must beTrue
      sit6(Usi("6g6e").get).isValid must beFalse
      sit6(Usi("6g7f6e").get).isValid must beFalse
      sit6(Usi("6g6f6e").get).isValid must beFalse
      sit6(Usi("6g7f").get).isValid must beTrue
      sit6(Usi("6g7f8f").get).isValid must beTrue

      val sit7Base = Sfen("12/12/12/12/12/4r7/12/12/5o3n1n/4N6P/12/6B5 w - 1")
        .toSituation(shogi.variant.Chushogi)
        .get
      val sit7     = sit7Base(Usi("8f8j").get).toOption.get
      val sit7Alt  = sit7Base(Usi("7i8j+").get).toOption.get
      val sit7Alt2 = sit7Base(Usi("7i8j").get).toOption.get
      val sit7Alt3 = sit7Base(Usi("3i4i3i").get).toOption.get
      sit7(Usi("6l8j").get).isValid must beTrue
      sit7(Usi("6l3i").get).isValid must beFalse
      sit7Alt(Usi("6l8j").get).isValid must beTrue
      sit7Alt(Usi("6l3i").get).isValid must beFalse
      sit7Alt2(Usi("6l8j").get).isValid must beTrue
      sit7Alt2(Usi("6l3i").get).isValid must beFalse
      sit7Alt3(Usi("6l3i").get).isValid must beTrue
      sit7Alt3(Usi("1j1i").get).isValid must beTrue

      val sit8 =
        Sfen("12/12/3l8/12/3b4+o3/6n5/4N7/6R5/7K4/12/12/12 b - 1").toSituation(shogi.variant.Chushogi).get
      sit8(Usi("8g6f").get).isValid must beFalse
    }
  }

  "falcon/eagle second move" should {
    "not allow lion recapture" in {
      val sit = Sfen("k11/12/12/12/6n5/6P5/12/5+h6/12/5N6/12/11K b")
        .toSituation(shogi.variant.Chushogi)
        .get(Usi("6f6e").get)
        .toOption
        .get
      sit(Usi("7h7i").get).isValid must beTrue
      sit(Usi("7h7j").get).isValid must beFalse
      sit(Usi("7h7i7j").get).isValid must beFalse

      val sit2 = Sfen("k11/12/12/12/6n5/6P5/12/7+d4/12/5N6/12/11K b")
        .toSituation(shogi.variant.Chushogi)
        .get(Usi("6f6e").get)
        .toOption
        .get
      sit2(Usi("5h6i").get).isValid must beTrue
      sit2(Usi("5h7j").get).isValid must beFalse
      sit2(Usi("5h6i7j").get).isValid must beFalse
    }

    "allow lion recapture directly after kirin promotion" in {
      val sit3 = Sfen("k11/12/12/12/12/12/12/7+d4/12/5n6/12/5O5K b")
        .toSituation(shogi.variant.Chushogi)
        .get(Usi("7l7j").get)
        .toOption
        .get

      sit3(Usi("5h6i").get).isValid must beTrue
      sit3(Usi("5h7j").get).isValid must beTrue
      sit3(Usi("5h6i7j").get).isValid must beTrue
    }
  }

  "isAttacked" in {
    val sit = Sfen("12/12/12/12/12/4r7/11n/12/5o5n/4N6P/12/6B5 w - 1")
      .toSituation(shogi.variant.Chushogi)
      .get
    val sitMove     = sit(Usi("1g2g1g").get).toOption.get
    val sitMoveAlt  = sit(Usi("1i2i1i").get).toOption.get
    val sitMoveAlt2 = sit(Usi("8f8g").get).toOption.get
    sitMove.history.consecutiveAttacks must_== ConsecutiveAttacks.empty
    sitMoveAlt.history.consecutiveAttacks must_== ConsecutiveAttacks(0, 1)
    sitMoveAlt2.history.consecutiveAttacks must_== ConsecutiveAttacks(0, 1)
  }

  "bare king" in {
    val sit = Sfen("12/12/12/12/6k5/4g7/4G7/6K5/12/12/12/12 b - 1").toSituation(shogi.variant.Chushogi).get
    sit.bareKing(Gote) must beFalse
    sit.bareKing(Sente) must beFalse
    val sitAfter = sit(Usi("8g8f").get).toOption.get
    sitAfter.bareKing(Gote) must beTrue
    sitAfter.bareKing(Sente) must beFalse
    sitAfter.winner must_== Some(Sente)

    val sit2 = Sfen("12/12/12/12/3I2k5/12/12/5+pK5/12/12/12/12 b - 1").toSituation(shogi.variant.Chushogi).get
    sit2.bareKing(Gote) must beFalse
    sit2.bareKing(Sente) must beFalse
    val sit2After1 = sit2(Usi("6h7h").get).toOption.get
    sit2After1.bareKing(Gote) must beFalse
    sit2After1.bareKing(Sente) must beFalse
    val sit2After2 = sit2After1(Usi("6e7d").get).toOption.get
    sit2After2.bareKing(Gote) must beFalse
    sit2After2.bareKing(Sente) must beFalse
    val sit2After3 = sit2After2(Usi("9e9d+").get).toOption.get
    sit2After3.bareKing(Gote) must beTrue
    sit2After3.bareKing(Sente) must beFalse
    sit2After3.winner must_== Some(Sente)

    val sit3 =
      Sfen("1P3PP3P1/12/12/12/6k5/12/12/6K5/12/12/12/l10l b - 1").toSituation(shogi.variant.Chushogi).get
    sit3.bareKing(Gote) must beFalse
    sit3.bareKing(Sente) must beFalse
    sit3.draw must beTrue

    val sit4 = Sfen("12/12/12/12/6k5/5g6/4G7/6K5/12/12/12/12 b - 1").toSituation(shogi.variant.Chushogi).get
    sit4.bareKing(Sente) must beFalse
    sit4.bareKing(Gote) must beFalse
    sit4.draw must beFalse
    val sit4After1 = sit4(Usi("8g7f").get).toOption.get
    sit4After1.bareKing(Gote) must beFalse
    sit4After1.bareKing(Sente) must beFalse
    sit4After1.draw must beFalse
    val sit4After2 = sit4After1(Usi("6e7f").get).toOption.get
    sit4After2.bareKing(Gote) must beFalse
    sit4After2.bareKing(Sente) must beFalse
    sit4After2.draw must beTrue
    val sit4After2Alt = sit4After1(Usi("6e6d").get).toOption.get
    sit4After2Alt.bareKing(Gote) must beTrue
    sit4After2Alt.winner must_== Some(Sente)
    sit4After2Alt.draw must beFalse

    val sit5 = Sfen("12/12/12/12/6k5/5K6/12/4G7/12/12/12/12 w - 1").toSituation(shogi.variant.Chushogi).get
    sit5.bareKing(Sente) must beFalse
    sit5.bareKing(Gote) must beFalse

    val sit6 =
      Sfen("5P3P2/12/12/12/6k2i2/12/12/6K1I3/12/12/12/5p5p b").toSituation(shogi.variant.Chushogi).get
    sit6.bareKing(Gote) must beFalse
    sit6.bareKing(Sente) must beFalse
    sit6.draw must beFalse
  }

  "fixture perfts" should {
    "forall" in {
      forall(fixturePerfts) { line =>
        line match {
          case (sfenStr, d1) => {
            val game = Game(Some(Sfen(sfenStr)), shogi.variant.Chushogi)
            game.situation.end(false) must beFalse
            perft(game, 1) must be equalTo d1.toInt
          }
        }
      }
    }
  }

  // format: off
  val fixturePerfts: List[(String, String)] = List(
    ("lfcsg2gso1l/a1bdtet2fc1/2r2q1k1r1a/mv1x+P1+Pp1bvm/3p3d2pp/1Q4h1p2h/P1piD5PP/5B2iPV1/2HP1ONPP3/MCVTED1GBT1M/A1S4XH1RA/LF1RGK2SCFL w - 142", "113"),
    ("lt5s1v1l/a1v1q3s2a/f2gge2kcf+A/2bc+H+Pt2+Po1/mpS1P3bOV1/1VD9/5E6/4+iB1PP1M+L/1+p6R3/A1C1K2R3S/L2XGBG2F2/2F2TT1HD2 b - 369", "171"),
    ("l2e1tsg1hbl/avbs1dxtn1ca/f1g2k1d2rm/1r1h1q2ov1f/m3c1p5/P1Pp2OP1P2/RVD2p5p/3P1P4pM/B5PG1B1V/1M2SNQE1R2/AC3TDTHF1A/LF1G1K1H1SCL w - 230", "126"),
    ("lbc1dxkg1c1l/a2rg1otsbfv/1v1se1d1hrma/mf1tq2pp+B+P1/3hppp2P1n/1p9p/1P3P1PG2P/p1ppP1G1I3/3P1DP1P2R/MVDHON2H1VM/AF1CSEXCT2A/L1R1KT2SFBL b - 159", "106"),
    ("lf1s1kqg1sfl/a3t3tbcv/vbcdgxe2r1a/2rpo4ndm/m+PPh4p3/P2Ip1pNIp1P/6Pp4/3P3TPP2/M3PHDPGR+pM/AFRV2XT1D1V/L1B1G2SCBFA/2CSK1E4L w - 178", "118"),
    ("l1t2qe1tx2/1h1f2s4b/ar3k4f1/2s4dd1+Bl/2+A3g5/b5Xh1C2/1cR1p1g2V2/L2iG4HM1/7R1F1+a/VFD1T6L/3S3D2GS/M4K+p2T2 b - 359", "132"),
    ("l1f1gxg2ft1/a1b1teks1r1l/v1s5r1ma/2h3p2vob/2cnd2Pp1p1/mPPpP3q1Ph/1C1P2P1P3/9MB1/+p2HDQ2D1C1/A1VRK2H3R/L1F1G1X1GS1A/O1S1TNE1T1FL b - 243", "129"),
    ("1v1q2khbv1l/l1sg2exgf1a/1trfd3t3/a1m1o4d2/pp1p1P1Hprcm/P1ph3s4/2cP3P1Pp1/RP6P3/AMVF1K2M3/1C2HG1R2V1/L1B1XO1E1SCA/3T1S1G1DFL b - 247", "113"),
    ("lf2ho1gs2l/avcrg1kec1fa/m4b1h1dtm/1+Psd3bn1v1/4t4r2/2p3PN1p1p/3p3PiB2/3Pp2DH2A/2P1SK1CPM1L/R2H4D1F1/A1C1BOXG1R2/LVFGTE1TS1V1 w - 210", "134"),
    ("l3gtn1s1fl/1cbso1xktgca/af1hdr4vm/1m1r1e1b4/2pp1p4Pq/1v4p5/p2iPP2PP1p/1pP2N1D2V1/A2PX1K2CHP/LMHBO2EBR1M/1V1D1TGT3A/1FCS2QGS1FL b - 185", "126"),
    ("l2b1dgt2bl/af1sex1kshf1/1mrc2d1cnv1/1v1pq+Pgpo1m1/ptP5pr2/1P1i1BPPi1Ra/H2P5p2/P3p2CI3/3MXOD1PHM1/AR4Q1B2A/LVFG1KTTGVFL/2CSN1E1S3 w - 178", "109"),
    ("l1t1gxe1t1bl/avfcs1okg1ca/mb1hdq1dsf1m/2rp1p2p1r1/1pP1H2p2vp/p2I1P2i3/3P2PPIp2/P3P4h1P/MP2D1Q1P1M1/1VRCON1HDRC1/AF1S1KXGTB1A/LB1TG1E1S1FL b - 107", "102"),
    ("1f1g1e1xt1fl/1cb2dsgc1m1/1r1vts1krvb1/3mpdo2hpa/l+P1p3Pp3/4P3i3/P1Hi1P4P1/6q1IH1P/1O+pD1STBP1CM/1BMFG1G2RV1/A2C3T+pFSA/LV3K1E3L w - 224", "102"),
    ("2f4h1o1l/lt4k1ns1a/2hb+B1gqt1f1/acs1g1d5/8mc1+P/5m6/1v1O4E3/p6DS2G/1MR1+p1P2CV1/6+p1R2M/T1F2C4FA/L3DSNKBT1L w - 338", "152"),
    ("lvt3k1s1fl/a1bf1g2o1cb/r2e1q2grv1/1d1s1+P1t3+P/m1x1c1D5/P2pB3ph2/AP1i3p2p1/2pPQN2K3/2H1TX+d4M/1M1F2E2SVA/L1V1RGT1R1F1/1BCS3GOC1L b - 225", "200"),
    ("lfcstegtb1fl/a1bgox1s3a/1mv2qkdhcv1/2rpdp1pnr1m/pp2php4p/3i2P1Ip2/4PP3PpP/1PpIQ1NPP3/P2PX2DH3/MVRHD4RVM/ACBTG1GSCB1A/LF1SOKE2TFL w - 104", "103"),
    ("v1f2dk3bl/lg2t5mf/5o1n4/a4h1t+P3/cmr1X5sc/3x4e3/6pP1R1v/A1RM2S1T3/3q3E3L/3KD3C1HF/LCSD1G3M2/1BO6H2 b - 399", "124"),
    ("lb1sgekgthbl/af1c1x2s1fa/mvrh1tn1r3/1p1pdpd1c1vm/p1pipo2p3/6PpqpP1/1P3N6/1HPI1P1PC3/PVTPP1Q2PR+p/MB1DGD2BH1M/A2C1OXST1FA/LFRSKE1G3L w - 104", "104"),
    ("lcds2t1s1fl/a1fxegk1o2a/m1v1tq3v2/b1r3ghp1c1/pphpP5pm/1Ppi4irP1/1C1In1H5/M1PP5HXM/2V5PR2/A1R1ODQK1TBA/1FBS1N2CSV1/L2TG1EG2FL w - 194", "162"),
    ("l2s2rt2fl/avch3bsrca/q6k3v/1+X1f2e1p1h1/3g4i2P/x4g1p2P1/9C1A/3Md1S1dF1V/1+pG+o2H2M1L/RB3HO5/AFCTK3G3/L3DERS2T1 w - 348", "218"),
    ("lv1be1sg2fl/as1rtdkt1b1h/2gh2qcxv2/fcmp+X1od+I3/P2ip5m1/1ppI1ppp1DPa/1PP1PP2P3/N5PP4/1R1PD2BR1V1/M1G1QE1S1HM1/AVBH1T1O1F1A/LFCS1TK1G1CL w - 170", "91"),
    ("lb2e2g3l/1vfstgk1f2a/+P3ht1m4/2x5ob2/3m1d1d1P1P/1qrpP3I3/1VQ2OF5/5HP2M2/M3CG2P3/A1DH2D2SVT/S2NT2E1BCA/LB1G1XK3FL b - 289", "144"),
    ("lf1bge1ogdfl/acsdtk1s1bca/m1r1h1th1r1v/1v2qn1xppm1/1p1ppp2i2p/p6p2pP/2p1PP4P1/2PiR1QPIP2/PP1P1HPHP1V1/AVMB1NDX1R1M/FCSODTT1GBCA/L3KGE1S1FL b - 107", "86"),
    ("lf1g1kgdsf2/acb1sx2tb1l/mv1tdq2hora/2r2e2ppv1/P2p2h3P1/6ppi1c1/1p1iNP2I3/3HP1BBP3/1V1P3HR2+p/M1RSDGT1F1V1/AC2KOE1n2A/L2FTX2S1CL w - 180", "172"),
    ("lft3ktr2l/a1b1s1dsofcb/mvcgd4v1a/p1rhqgx1e2m/3p1Ph1p3/P3p1ppippp/1ppI3P1P1P/2PP2P1IR2/N3P2QP1PM/MR1H2XGHTVC/AVGTK1DO1B1A/LFCSD2S1EFL w - 172", "93"),
    ("l3to1df3/s+P1v3k1h2/a3b2t1cv1/2ep+D1s5/mhqf2x1dr1l/VPCP4n3/2B1HOQSP2A/12/A5+pX4/1+p2TRE1BG+a1/3K7L/LFS1G1T5 w - 326", "180"),
    ("lbg2t1e1t2/afr3gs1d2/1dnsh1k+P3l/1mc3x2b1f/C2pP3rH2/2V6m2/p2i4P3/12/5DT1X1C+p/1MR3E3BR/A1BFKON1S2F/L3ST5L b - 291", "160"),
    ("1o2f3t1b1/lcgt1srh3l/a1v2n2+P2f/m+P+P1bk2p2a/1d6i2m/P3d1s3c1/1N2G6+r/1VX9/M2HS7/ABR1Q1K1RCM1/L1FOT4VS1/1C2TG1DF3 b - 359", "158"),
    ("2f3b1g1t1/lm1se1o2f2/a1cbt1gx1rcl/3h1p3s2/P4+h1D+P3/2rp1k1d1R1v/12/AVKP4Q3/L1CNH6F/2SH2ES+p1+p1/1FT1T2G4/4O7 w - 348", "128"),
    ("1fcknebg1t1l/lv1q2r1s1f1/as1r3t1oc1/m2p1gdd1v1a/3P1P1Pp2m/b3P3ippP/6M3x1/p3B1P1D3/H3S1DGI1V1/A1HBX2RTS1A/1C1O1GN1F1CL/LF1TK2E4 b - 249", "140"),
    ("lf2ex1tho1l/a6k1sca/1hgs3dbr1f/mv2b1t3mp/4c1d3V1/HP5p1g1P/1Vr2OH2C2/P2p2n1p3/4Q3X3/M1CSR3R2M/AFBG1KGT3A/L1T1E3B1FL b - 221", "116"),
    ("lf2ke1tdo1l/adrst1gs2fa/m1vh1x2cvb1/+P+P2g2+Hpr2/5Pp3hm/6N5/1RCpp1P1IpP1/1V2D2PnP2/M1SP1XD1HV1+p/1F5SR1CM/A1G1K2O1B1A/L2B1TTE1GFL w - 190", "155"),
    ("3x1t4fl/lmf2ets1bca/3bg4v1m/a1+P3r4r/1P4k4o/p7p3/C4g5A/8IS2/1G3XD5/ARM+cT1O3F1/H1S2HGQ1CV1/L1FKT1E4L w - 342", "92"),
    ("1f1s1ek1s1fl/lc1tg1ot1bca/avrbdn1dgrvm/m2h3x4/p3pp1Qpp2/3p2q1i3/PppPNPp5/1PPhP2PIP2/1V3RP1P1+p+p/MFSHO3HRVM/ADB1TX1DGCFA/L1C1GKTES1BL b - 107", "121"),
    ("lfcb3tb1c1/ar1stgegmfvl/2+I2k1d1o1a/1mv1p2+P3r/3q1d2ph2/1x2P1pHiP1P/2p2B1O4/pp4KHIM1A/M2P2PTP3/1V1X1BQ2CV1/AR1n1GTDR2F/L1CFSDE1G1SL b - 211", "127"),
    ("l3tt1gfcv1/a1freqk2m2/1o4d1r2l/v1sx4s2h/2c2g6/3+R3b4/1p3Q1Xp3/1M+bP3P1d1p/C1V3K2CVP/DF1H1S1DG1BM/AT3O1T2HA/L1G2B1E1SFL b - 267", "132"),
    ("4sx6/l8t1l/2v1f1s1b2c/3m1ke5/3b3p1m1C/a1Dc6v1/B8V1D/2O4E3A/4+r2n1M2/AC6F1B1/L1F1T7/4SK3T1L w - 374", "202"),
    ("lfr1tet1sc2/a1s1hgo1kfgl/1cvbd2h2va/2ppqp1m3r/mp1ip1d5/4xP2p1Vp/1P1I1Bp1Ip1P/3PD3P3/1VP2OX+p1PMA/M1CRHN1D1R2/A1B1SQG1TCFL/LF1T1G1KS3 w - 176", "94"),
    ("l1r2et3fl/a1fsgko1sgra/t3d1dh1vc1/1cmhqpx1pp1m/p2p4i3/1v1ip1p1P1p1/1H1I2Pp2H1/PV2PD1P1M2/2B+bXP1ONP2/M1R1EDQSTFVA/AF1CGT2R3/L2S1KG1BC1L b - 173", "131"),
    ("l1c1x1k1t1bl/a1h1gdogs1f1/fvr1stedcrva/mpppb1qhp2m/3ipn4p1/p4ppp1p2/1P2PPH1IP2/P1PID1PPPRHM/1MRP6P1/VBG2NQG1SVA/ACT2OXD1BCL/LF1S1KET3F w - 114", "85"),
    ("3tstk1sofl/1fb1gx1hdv1b/lhc2g3e1a/a1r6+P1m/vmQ3np2rc/1P2p2P2B1/5p2p3/+pR1dD1M2X1A/B1VC1DT3R1/2OHG1T5/ASF5SFCL/L2K1E1G4 b - 283", "147"),
    ("lb1t2h4l/1vc1g1e1sfta/f1r1d1kxgrcm/1m1so4d1b/9v2/2R1NP1Pp2H/1p2X3PCPP/aMK1O1p3V1/A2P3E3R/1BCST1Q4A/L1VH1GG1D2L/1F2D2TS1BF w - 236", "94"),
    ("2vgett4f/1f1s2d1m1c1/2mn2+Bsb1+Pl/lpc+H+P2k4/1d8ga/1PO8p/A4P5P/1V4N2r2/2RK+pG1TF+p2/L5T4A/FH1CS1DS1CV+v/3G2E5 w - 340", "136"),
    ("3q3t4/lfms1t3bfl/1b1d1seg2m1/2+P1o4+P+P1/AhP8V/1V3kC1r3/3G8/L1HX6R1/2RFS7/1C4S1O3/6KTGBMA/2E3T2F1L b - 353", "126"),
    ("3e2ttbcf1/l3n2sd3/ag+vdhok2+P1l/+L3f2r3+P/1ms9/11A/1FV1cDxM4/1H6p2L/GM2S1H+p1+r2/8GQ2/2BT1K1XSC1F/7E4 b - 389", "129"),
    ("lv3t2t3/3m4kbs1/+Af3sq1e1f1/4o2g+P2l/2+h2+bP3ma/3P8/L1N2c4+H1/1d8DA/2C3+hMG+pTL/1F3+p3EC1/2BS2X5/2O1KT1RS1RF b - 361", "182"),
    ("lf1se1kgs1fl/1cbg1xot1bca/av1t1d2hdvm/m1+P1qpn1pr1p/3p2ppip2/1p2p7/3iPP1N1Pp1/pP2X1PPI3/V1RP2H1PR1P/MCFHD1GDQ1VM/A1BSTO2SBCA/L1G2KTE2FL b - 103", "129"),
    ("f1c1txg1s2l/l1b1sqokdf1a/a2r1e1nb1r1/hpv2h1t+I1cm/m1gpd4p2/3I1pP5/Q7P2p/1H1PpPTH1P2/1PD2G1P3M/MFVNR2SCF2/A2OSDX2R1A/LC1BGKE1T1BL w - 162", "112"),
    ("lb2gxkgt1cl/af1ctdnh1fsa/v2r3d1orm/2mpeq4v1/3i3ppp2/3Bp1O5/Pp1s1ppP1P1p/2HP3NXR1P/MR2P1P1P2A/A1V2TGDH1M1/L1C1DKGST1VL/1F1S1E2BCF1 w - 188", "107"),
    ("l1tsxok2g1l/1fr1eq2sbf1/a1m1d1tg2c1/3v4+Pr2/1c1p6m1/2bi1Ppp2Pa/1pX7M1/A2Np1h3T1/KRCP1O1C1P1V/6S3FA/VT4GG1DRL/L2SH1Q1E3 w - 246", "126"),
    ("4to2chf1/3c+P1kg1b1l/1bs2gd1h2+P/l1m1+P1t5/6s1v3/7n3a/5D5V/3K+r2dR3/+aG9A/2M2X3EC1/L+p2HS1T3L/2F7BF b - 383", "120"),
    ("l1k3xv1t1l/c1f1gd1m1f1c/as1+P3e1rb1/3+P3so3/5m5a/6h1d3/1v1R1pp3Vp/M2N5h2/A1C2F1B2M1/L3V1QG4/D1ESGK2S1FA/1B4TT2HL w - 358", "146"),
    ("lfd1te1ks2l/2q1sgft3a/1m3od2g1m/ac3+P2+Pn1b/1bp2r6/2vp2P2R2/p2Q1x1RV1c1/P1HPD4Fp1/3S3E3+v/1MV1GN4ML/AF2K2XGC1B/LBCTO1S4D b - 247", "186"),
    ("lfc1g1gs2fl/2bte2nobta/am1hsx3+Pvm/4r+Pdd1k2/1v1P4p3/p1p1p2pr3/Hp2PO3p2/PV3Q1P1S1p/2B1X1P3VP/M1RSD2DHRMA/AF2GKNTCBF1/L1TC1E1G3L w - 148", "136"),
    ("4g1t2hfl/fcd1b1qk1sca/1r1estn1brvm/l1v2+P1x2g1/1P1p2d1ppp1/2pIp1h3P1/a7PQ1P/1H1BD2p1PV1/AG1PPO2E1HB/1M2C+pXDR1M1/L1SV3GTSFA/1F2TNK2C1L b - 173", "127"),
    ("lfhse1k1s2l/ac2d1gtc1fa/mvrbg1d1nrvm/pp1t1x1b4/1Pp1Po2p1p1/5ppPip2/2PpXPh1I2P/q2P2P3P1/1V2D3P2A/FMRH2DQBR1M/ACSOG1T1G1FV/LB3KETSC1L w - 144", "119"),
    ("1f1se1ko2fl/lvc1rtxdcsva/a2mgq2ht1m/1p1h1d2p1r+P/p2p3pgp2/2p1ppb1n1P1/2PiP2P4/PP1I1NO2PVM/2HP2+p1P2A/M1V1D1QDR3/AFRCT1XG1BC1/LB1SGKETS1FL b - 121", "126"),
    ("2vfsek1g1v1/lcr3ft1b1l/2mg3c4/2+P1nd1sprp1/1b3xdhip1a/P2Po7/1hR2Pp1PM2/1VC3O1CP2/5G1+p4/MF1S1KE1RGBV/A10A/LBN1DX2TFSL w - 276", "161"),
    ("3g1k1osx1l/l1sct1et4/ab1dg3vcra/1fmn2r+Ppm+H1/1vq2P2i1P+P/P3P2QO3/2h9/1V1p4I2h/M2S3HP+pSM/A1R1K1T1G2C/LFB1+p1E1DV1A/2D1X1N1B1FL b - 303", "141"),
    ("l2gtekdb2l/a1vfx1dtscv1/2rbc1qh1f1a/mp2ops1p1pm/3p3PiP2/1P2pnp3Pp/p3PD2IR2/P1NI2P1P2P/1M1PTP1D2MA/1VR1GQ1GK1V1/ACB2O4CF/LF1H1SETSHBL w - 164", "109"),
    ("l4ekd1o1l/afcs5b1a/1m1dgth1+P1vf/1v2+P6m/3b5r2/2rp1pP1S2A/A2i1P2P3/1V1P4M1ng/L+p1D1T1D2C1/1C1+p3K1H2/1FBOGN1TER1L/3S2G1B1F1 b - 291", "168"),
    ("lfsg1okt3l/acb1t1dscbfa/m1vhgxe3rm/4r2ph2v/pPp2pp2p2/P3pq1npP1p/3QPPP4P/2PP3NX3/M1RS1D1P2+pA/1B1H2D1THRL/AVCTG3GBVF/LF4KESC2 w - 130", "136"),
    ("lvst1gd5/fcr4t1bcf/a1hbeq1ksorl/1m1dngpxp1va/pp3p3pPP/P1PIph2I3/3PP7/1PM2PDpPP2/2RFXOP2RM1/VC2DQ1HC1V1/A1BGTNKE1GFA/L1S3TS2BL b - 155", "106"),
    ("lb1hg3ecbl/afc1sx1ts1fa/2m1dgqh1o1m/1vrtd2k2r1/2p1p2p3p/10vP/pP1I1ppN1p1X/P1P1PE1Pp2M/M1CP3Q2FV/AVSKO2D2R1/B1FG2GHST1A/L2TR4CBL b - 201", "160"),
    ("lfcste1gs1bl/avrdq2k1rca/mg1b1ohdt1v1/1p1pp4pm1/Ph6pfpp/2piPpp1n1x1/3I1P2Q2P/1PP3P1PP2/1V1PDO1+p2P1/M1CNR2XH1RM/AFHGTET2BVA/LB1S1K1GSCFL w - 110", "137"),
    ("l3t1et2f1/a1f2x1s1b2/m+P1b1k1d2rl/1h6pn2/s2pPo1gi1mc/P2i1r2h3/B1R6Rv1/3I2H1P3/MCVP1O2S1+pB/1ST2G1C3+a/A1FK1E1D3A/L5D1T1FL w - 250", "135"),
    ("l1tsq2k2fl/v1f2ge2gc1/3c1x1thrba/a1bd2nsd+P2/hR1P3Q4/2P3D1m3/5P1oI3/PV2p2PP2M/2D1B1N1RH1F/1M2X1T2E2/AC1H2KG1C1A/LF1STG2S1BL b - 199", "185"),
    ("f1t1s2h1k2/l7sm1l/1rce2t2f1a/11b/3V3P+C3/a11/1+b8g1/5P2g3/n1M1H2GS2R/AB3G2OR+pA/2S1T2T2FL/LDF1K2E4 b - 387", "109"),
    ("1v1cg1k2rbl/l1d1exdtg1fa/1r1sht3vcm/af2+Pqohsppp/1m1I6Pn/b5PPp3/VR6P2P/2pP1D3P2/1MHE1+p2XRVC/2F2NODM3/ATCK3H1B1A/LB1SG2GS1FL w - 182", "138"),
    ("lf1bgxegs1fl/ac1s1t1hobca/m2d1kt1qr1v/p1r1ppd1p1pm/4h1ppi3/Pvpp5pPp/3IPB1QP3/1p1P1P1P3H/2P2OP4M/MR2HT1DXCV1/AVBSD2ETS1A/LFC1GK1GR1FL w - 118", "81"),
    ("lb1g1k1o1gfl/1fshtdeqs1va/2vr1x1tbcnm/acmp2pdp1rp/2p3Ppi1ph/3i1P5P/M2Ip5P1/FpP4PIP1M/A2PP1QHP1V1/1VGH1N2SR2/1CRDTOXGE1FA/LB1SK1TD1CBL w - 156", "76"),
    ("l2b1e1gsnbl/af1s1xdk1cfa/m1ch1tto1r1m/1r1p1dqh+P1vp/p2H2gP1pp1/Pvpi1pp5/2P1pP4X1/M+p1I2D2PPP/V2PP1PTRMV1/1B2HN1D1F2/ACRS1KQS1BCA/LF1TGE1GO2L w - 132", "98"),
    ("3st1k3sf/lv1g1xe1t+P1l/1f1d+Po1h1c1a/2cr1q5m/3p2gdpb2/2XIB5Vb/a1p4pH2C/1PNP4P3/1FP2O+p1M2R/AVRB1EHD1G1+p/L1GT3D1S1A/2CS1K2TF1L w - 242", "129"),
    ("l3g2ng3/2bmxk3cfb/fcsh1t1d1rv1/3p1e+P1sh+Pl/a2i1o3m1a/1v1d3P2q1/3P1p2pV2/M1D1P3iC1M/1RV2TQ2F1A/AF1T1E5D/1CBHSOXN1S2/L3GKG1RHBL b - 255", "145"),
    ("lbc1gxgt2fl/a1vs1eoksbc1/1r1t1qndhr+P1/1fd4pp3/mp3p1P1pp1/2Pp2p4a/1P2pQ2P3/3P1PP5/2H2ODHGPP1/V1RG1N1C1RVM/ACBTXD1S1B1A/LF1S1KTE2FL w - 128", "97"),
    ("lg3xkgscbl/fchr1eo4a/av1btsht1rf1/1+Pp2p1d3v/1m1p1dqp2pm/3i3P3n/2V3P1p3/2P1HP1H1NP1/+p2P4SD1M/2RBO2Q2RA/A1FT1BGTCV1L/LCSGK1E3F1 w - 178", "124"),
    ("l1r5ko2/a1m1t5dl/cg1s2h1s2a/1f6g1+Pc/3ex2b3v/3B8/2D3n5/1VC1p3+p3/S3+p2S2CM/MR9A/LF1THENGTG1L/1H1K4V2R b - 399", "149"),
    ("lf1s1tk5/vr5tgc1l/3g1e1s1f1a/a3cx1h3m/1P1b3P1R2/p2P2d4b/4D1G1v3/1BpC3N3M/2P2OP3+r1/VMR1TS2H1F1/AF4XT1C1A/LS1KE2G3L w - 252", "145"),
    ("lfkts4fcl/ac3egths1a/1brx3+P1r2/m1g2Nd2bdm/vh7+I1v/p2pp4P1L/1H1i2X1P1H1/M2PPD4P1/A1+p3DV2+p1/1B1Tn1GRS3/V1RG1O2E1F1/L1FSK2T4 b - 217", "204"),
    ("2tf8/l1+Rs1e3k1l/1v1b2b+P1gfa/6sxt1m1/a4g2c3/4o2G4/1VQc3P1p1p/A2CD1D5/1O5SR1T1/2FMS3M2L/L7C2V/2T2E1KF3 b - 393", "137"),
    ("l4t1nsofl/afbcsx1ktcva/1v1r1q2d1bm/m1p3e2r+Pp/V1gpdP3gP1/8p3/pRPi4X3/3Pp1hp2CP/P1CKP3P1RM/M1H1OT3V2/A1BD1T1HG2A/L1FSG1E1BSFL b - 177", "103"),
    ("lf1gde2sc2/a1b1skxgrbfl/cm2n1t3v1/2vh2ot3+A/p2pr3ppm1/2P1P3i2L/P6Q1P2/1N1P2p1XGp1/1PCD1+pT1I1C1/1MR5R3/AVBOKGSE2V1/LF1S5TF1 b - 205", "144"),
    ("l2sgek2gf1/afr2totd1cl/vbchqx2+B1va/mp1p4d1pm/p2i1pp1nr1p/P1p1p7/1PPPP1P1hp2/7pPPP1/M2HNP5P/A2Q1TTGHRVM/L1SDGOX2BCA/1FC2K1ES1FL b - 101", "113"),
    ("l3ge1gs1fl/a1b2kot1dca/mfr1htn1bxrv/3q1s1pp1p1/2c2dp1i1hm/2vppP1P4/1PpiP3IP1p/Q1PI2D4P/P1HP2PEP1P1/MRC2N1SHR1M/AVBD1OKGCBVA/LF1SGTT1X1FL w - 128", "91"),
    ("lf2g1hk1s2/a1r1td4cl/+P2msentrf2/c3+P4vm1/3p1Q1x1g2/3PXP2pb2/pO5pP2a/4D1pC3p/1MV4D1+pMB/AFC4HHV2/2TSG1K2RFA/L2B1T1G1S1L w - 278", "142"),
    ("2m2ot5/lc1s1e1sN2l/aq2dkxdm1ca/1hf2g6/1b1v8/MPP3p1v2A/4n5bD/A1RP1p3C2/FCBH2G4L/L3T1E2D1+V/S4GK5/1V1TX3B1F1 w - 376", "189"),
    ("l1cbgte1b1f1/a2s2s1tcvl/mvf1no2Q1ra/2rd1hd1kp2/2BP4p1mp/p3B1xp3g/1pP3H1i1PP/P2RP4N1M/A3SP2PP2/1MDCTD1K1STA/1V2GXR2CVL/LF4EGO1F1 w - 190", "115"),
    ("l1f2t1g1sfl/amb2k3gv1/2q1st1hrc1a/r3+H+P1d1ppm/v2dx1H1p3/2Ppp2pPPPp/KPhi1N6/7PD3/M3G1P4M/VFC+bOGXDS1QV/A1R1T1S1RTFA/L3E1B2C1L w - 246", "118"),
    ("3b4sgfl/ft3ksr2c1/l2m1n3xd1/2hg1te1v1+P+L/3p1r2B1P1/2Pi8/M4o1NH3/C1Dc1G1SB3/6+p5/1VTT1H2CE1X/A1S2G3D1V/LFK8F b - 367", "189"),
    ("v1cn1t2bg2/3g1xks4/l1r2oeh+Pfrl/+P1d6c2/4P4Pvm/2P7P1/5NHP4/2VS2p3V1/BR1P1B2H+bT1/M6DM3/AF2TD2GRCA/L1CSKG1XE1FL w - 316", "107"),
    ("1h1senkgb2l/lc2tx2sf2/a1f+P2t5/Q2+R5hra/1P2pod3Pm/1V6p1c1/4P2d1pV1/A1C2p6/3M4+b2+p/2F2N+pXDRMC/L1BTSK3B1A/4GH1EGSFL w - 248", "141"),
    ("2e1rbg1td1l/1c5k1b2/l2o1g2sv2/1m3dh+Pc2f/a1+Rp4m3/A1Oi8/11F/FB1D1+p4P+L/V2+s3C1+p2/C1N2K1X1T2/L1ST2GS2VB/10D1 w - 382", "128"),
    ("l3g2t3l/1tfdb1ke1f2/a2ox2d2sc/r+P1c5v1a/1h2p3+D1+V1/A2pPq3r2/L1v8+h/1M1G1DR5/6EM2+p1/2C2TT1H2F/+HF1S1S5L/1V1BOK1GB3 b - 315", "145"),
    ("6s3b1/l1t1kx3g1l/f2d2gv4/m1sd2h1+Pcfr/1+Hc1t1pP3a/M5P5/L1CPS3Im2/4O3C1Q1/1D4DGP3/3R2BE4/2nKq4T1A/4TG3F1L b - 383", "130"),
    ("lf2ekt2ofl/a1gst1g2hva/m2d1bs2+Hr1/2c+I1q2p3/v1r3b3Pp/P1h1D1cpip2/2xPNp2PP1P/12/3+p2B1C1SM/3C1ED1RF2/AMR2T1O1BVA/LF1SGK1TG2L w - 198", "104"),
    ("l2s1k1x1gbl/1fctge1sct+Pa/a2b3r2v1/m1hp2dd3+P/P1r3nop3/Av1i1P2i1p1/3XpH6/2P1PHp1q1P1/CMRPB6D/S1VF1DOS1FBM/L3NGTG2VA/2TK2E2C1L b - 213", "123"),
    ("2k1ttx5/1e1h3gsr1b/l2fg1d2f1+P/s4+Po1p2l/1c1bm3iv1a/1V2K4m2/6Q1hC2/8P1R1/+i2S3FD3/DF2O+r1S2M1/4G1TG1B1A/L1RB3N3L w - 352", "137"),
    ("lf1st1d1k1vl/a2ct1g1cb1a/1b1x2gdsr2/m2phe2o1f+P/2pi1q1+Pi3/r1P1N4PPX/P1R5I3/3D1HH1P2A/1+p1P+p3RM2/AM1S3CT2D/LVB1TO1GSB1F/1FC2K2EV1L b - 213", "188"),
    ("lbcqg1gtso1l/afst1exk2fa/1mrd1d2brcm/2h+P2php1v1/5p1p4/4p1P1i2p/v4K3ppP/p1DNP1DPPP1H/A+p+p9/L1MH1Q2CRMA/1FR1SGX1SV1F/2C1TEOTG1BL b - 169", "144"),
    ("l1f1ke1gt2l/2tcg1o3ca/as2d1dfsh2/mvb4qv1mr/2r1pp1P1p1p/1pPp2x1Ib2/11P/2MP1Hp1P1C1/CPN1BPG1HPFA/V1R1K4Q2/AGS1E1XOT1RB/LF2D1T1SV1L w - 204", "105"),
    ("lb2gx5l/a1s4tov1f/cvdt1esh+P2a/2m1k1d2b+P1/3pP3p3/f2h1n5m/p2P1p2P1VP/Hp10/2V4+p1Mq1/MCSBD1O1D2C/A1KT2GXSB1L/L1F3E2TRF b - 281", "106"),
    ("1b1f4f2l/3cd1t1k2a/l3q3x1vg/a4mob2h1/4sp1g3m/7Bc1C1/M1R9/Av1V5+pR1/3G+p3X3/LS5TK2+r/3F1DTM1F2/2N2D2G3 w - 396", "133"),
    ("lfcsg1kg1tfl/av3eos2va/m1r1tdqd1cbm/3pb1x1p2r/4p3n1pp/hpp3p2p1h/2NP1p1pI1P1/M1P1PP2QR1P/1VH5P2H/R2TC1XS1VDM/A1BGDOTB1GFA/L1FS1KE1C2L w - 146", "132"),
    ("1h3t2b3/10f1/2+Bok1+Id1x1Q/m3dc6/4N2m1g1+P/2f7n1/3GH7/2v3c4D/3CD2S3M/1+pH2G2E2A/F6+p1O1L/LVK4ST1F1 w - 392", "169"),
    ("3c2k3bl/ftb2se3f1/1+P10/4x4vsa/l2pX+P2mc2/2Pi3C1g2/5d2S3/3dG3M1BA/1+hqC3D1F2/+a3SK1ER1G1/1M1HN1QT2VL/LF1D8 b - 367", "169"),
    ("lbcs1ok1scfl/av1edgt1gbva/f1t3r2r1m/1p1p1pqdphp1/m2xp2pi2P/2pI1P3pP1/p2P8/P1P1PhQPI3/MP2H2HPR1M/V1R1DN1XG1V1/AFSCKT1OTB1A/LB2GE2SCFL b - 119", "112"),
    ("4d2sb3/lc1f2t3t1/1b2ge3oml/a2p3k1vfr/m3rq3Qg1/5x2ch2/M2P1D5a/1+RS2X2p2A/5M3+p1V/A+p1E1RGC1HB1/LV1CT5FL/4K4T2 b - 375", "112"),
    ("f1h2t1ebr1l/3cs1g1k2a/l1g2n5m/avb1+Pp1sof2/3p1d2Rq2/1+P1P7c/5C4v1/5N1p1C2/AMB2K1GV1FM/5R2E2A/L1GS2D1TBSL/2FT2O1H3 b - 323", "186"),
    ("f1g1tk1+V4/l9sl/av+Ds5g2/3d6ea/m5hPph2/3i5q2/1GC5iR2/L1M3OSP3/3P5T1+r/H2S2V1E1M1/2RFX4B2/5K2GNFL b - 381", "134"),
    ("lb2t2ksrc1/af1cnex1hf1l/m1rd1og1t2a/1gpp1+Hspd1m1/1vPiph2p1vp/P2PPPp1iP2/6PP2b1/1P4Q1IG1P/M1B3D1P2M/C1V2KO1D1R1/AFR1T1TXSBCA/L1S1G1E3FL w - 178", "93"),
    ("1t3s2c1tl/l+B2bgo2b1a/a3q7/v4p1fk2+P/3edg2P3/A3x1ps1r2/3I2P5/L2P1G1G1v2/4S1K1H1V1/1R+pEO1QD1R1L/FM5T1CM1/3BT1HS2F1 b - 305", "134"),
    ("lf2e2s1gf1/a2shtk2v1l/1bc1xd3t1a/2rg1q+Pbn1om/m6h2cP/3pOD2I1p1/1v10/p2P3PP1SM/M1+p2PQ3PC/3HSD2HNV1/A1VRGKETGRFA/LFCBX1T1B2L b - 183", "124"),
    ("1cr1t1d1s2l/+P1xts1kgc1fa/4dh1h1n2/1f+P1e4r1m/1v6H3/a2IpbX1gpvp/4P2p2po/1p1P4p2P/B1V1KMD1SC1B/1MqN1T4RA/AR1GD2T2VL/LF1SE1G3F1 w - 292", "170"),
    ("lb1td2gs1bl/1vcs1d1et2a/1f4n1h1c1/5+P1kr2m/2rqh3p1f1/1p1px5v1/aPM1CDN5/R2PP1O1PpC1/QVH2D1+p3+p/4S1GXRFMA/L1BG3TSB2/1FT2K1E1H1L b - 247", "154"),
    ("2fe1kt3f1/2shgc4+H1/1b4tg4/2r1+d1h2+Psl/6m4a/l2p2D3d1/a4xS5/A1N9/L1DM1K2F1VM/2G1HE5A/1+vFR1R2GXCL/1S3TB5 b - 349", "163"),
    ("1re2d1g2fl/l1g1ht1sov1a/1m2s2dr2m/ac4k1+Rc2/2fp4P3/Pv1q1t3P2/A2P2D2B1C/3GD5pT/2N1QO+p4M/LCM6SV1/1V+xTB1S3FA/1F1K1H2GE1L w - 280", "135"),
    ("1f3t1gs2l/3e1xqkc1vf/l2+P2t+Pdrm1/a3gd6/1mh1s4n1a/vr4p1I2b/3D4Pp2/1BC4D1PB1/4+p1+pK2TM/MH2T3G1FC/A1F2O1EH3/L5S2V1L b - 337", "126"),
    ("l2s1ksg2b1/af3xe1tn1l/1v1gt3rc1a/2b3+P1h1q1/m3r+P1d1vHm/3pp7/2RIc1G3pG/Ap6pXC1/1K1PN1S1P+V1F/1B1MO5M1/2D2ET1HD1A/L1F2T5L b - 337", "187"),
    ("l2r1e1k2fl/af2gtt1o1v1/3d2hg2b1/6cc1mra/2+Pp3P4/s2i2n1xsV1/LM3Pp3S1/3I1DO5/3P3T4/2R1HT1G2+b1/BCFN1GEB1FCA/4DS1K+p1HL w - 322", "172"),
    ("lf2gek1s1fl/acb1x3ocva/2rsdqdghtbm/m1hpt1p1pr2/pvni3pipp1/4p3I2p/P2I1p1P1PP1/1pp2N2P2P/M1PPX1P1R3/1BRHDEQ1H2M/AFVGT2STBVA/L1CSKDOG1CFL b - 103", "119"),
    ("2t5ks2/1fb1e2g1tfl/3+I1g1x1v1a/2+Pc3mo1cm/lv4d5/a2P1b3r2/2N5p3/p4B4C1/n+pDOSE5B/1R2T2RTF1A/LF1V2X3SL/2KG2G5 w - 296", "155"),
    ("l1c1ng1ksc1l/afse2tg1rf1/mv+P1t2dh2a/p2xb1r1p2m/h+P1pPdp4p/3i1popIv2/7PP3/1H1I1bD2p+p1/P2PX1PC3P/MRGBOEQKBRMA/AV3N1H1GFL/LFCSTDS1T3 b - 171", "99"),
    ("l2b1e3s2/1fq1g1tkd1fl/amr1to2hcva/+P1+P+Px1d1p1nm/1V5g2r1/3H4R1Bb/7p1C2/2F3+h1P1VA/2+p2+p1DX3/1MCRD1O1G3/A3G1QEF3/LB1ST1KS1T1L b - 263", "157"),
    ("lb1se1tks1fl/af1gh1x1obca/1mr1td2hrv1/1cvdgqp3pm/P3pp1Hpp2/1pp1P3in1p/1PXp1P1p2P1/2PD2PPIP1P/MV1PB3PH1V/A3RQD1R1M1/FCG1TOGETBCA/L1S1K2NS1FL b - 133", "99"),
    ("2c1en2tgfl/1v1txdo2hca/1+Pf5kv1m/2hgb3s3/lD2s1r3+P1/a1r3Xp3V/3pB4d2/SP1P1pGP1pDp/2C2K2+p2M/AMV1O1Q1R1BA/2F3G1H1CL/L1RTE1T1S1F1 b - 257", "137"),
    ("lfeg1k2s2l/a1vh1tdq1bfa/2r1dc2tv+C1/1sm1n4+R2/6gP3p/2PpPx1Gc2B/p2D4P2P/A2P3Q4/1+pV1T1G5/2SR1+o3SMA/4KO1TEX1L/L1CF2D3F1 w - 292", "222"),
    ("lb4e2rfl/a2t2ks2va/rc1sdqgcth2/1vph7d/f1xp3op2m/m1P1g5C1/1p1P2ppIppP/pM4DP4/1DVXG+p1BPR1M/A3RHE1T1HV/1FB1S1K1GSFA/L1O1T6L b - 223", "104"),
    ("ldc1tstd3l/a4g1k1sfv/f1+Hb1on1r3/1+P2g2e3Q/4x2c4/4P7/M2P1P+A5/1+pC7P1/1+r1S1GD1+p3/A3+h1N2C1+p/1F1T2E3R1/L1+p2TK1+b1G1 b - 321", "166"),
    ("3s1eg1st2/v2r2ok1f2/1cmfd1g2mc1/lph1+I+P2d2l/a1pPt3C1b1/b1D1Q1ppR1P1/1P4P2x2/p4D1P4/1MPT1OEH+i1+h1/R1V1H1GT3+v/ACB1G2XSBFA/LF1KS6L b - 243", "130"),
    ("5e6/1vf1gk1ts2l/+l1c1d1gom3/1+Pxsh1+P3v1/5d2c2a/2P4r1Cf1/6+b5/1VC5R3/1D3E1N2V1/L1MH1S1G1F+p1/2R3GS1X1A/1F1T1K2BHTL b - 335", "169"),
    ("x1q3tkbf1l/lfgc1h5a/2rest2gvm1/2+P6r2/a2X1Pp4c/b5sQp1P1/B7Pp2/A1NdP2RG1VP/1+p3O3S2/1C1R2D1ECFM/SM1TGDH2B1A/L1K5T2L w - 310", "113"),
    ("lfcbgek1stfl/1vr1tx1goh1a/2mhs1qd1cbm/ap1p1dn1pr2/2pip1Pp3p/P6Pi1P1/1PP2p3v1P/1V1IPP2PR1V/3PH1Q5/M1RND1OD1C1M/ACBG2TEGBFA/LFST1KX1S2L b - 137", "107"),
    ("3f2t5/l2gx1ktcr2/2hs1+B1nbf2/am2cg4ml/2+P2Pp5/P5P2vV1/5F6/A1Mp4p2a/1C2RD3+p2/L1S2E1DM1+pA/2T1GO1K1GT1/1F1+r7L w - 312", "156"),
    ("lfct3xs2l/a2hog2tbca/1vr4g1v1m/m1+Pdsk2r1f1/P5nPe1h1/3R1q2p2P/2D5i2M/1+p2D4p1V/A1X1P+p1+bS1F1/LBM1C1G5/1VS2T2QBCA/1F1G1TEK3L w - 234", "198"),
    ("l1c1t1s1k1b1/1fb1o1enhcf1/a1mgr2g1t1l/2+Ppq1dxpv1a/1v6I1m1/pp1i1PpHP3/P5Pr3p/1G1I3H2VA/V2P3+pDR2/MFKC2X1BCM1/A1RO1D1TSGF1/LB1S2NT3L b - 217", "128"),
    ("lfrb1o1t1v2/g2s1h2s2f/1m1d1r1xm3/+L1ve2k1+P3/1Pc1h1P2c1l/2d2D2C1p1/3Vpq3Vga/1M5B4/1FC2NGS3M/4RK1HB2L/S2+i2D3F1/O1G3T1EQ2 w - 338", "136"),
    ("lhf1gekgb2l/b3tqosc1fa/avd3t1rhvm/m2csdpx1p2/3+P2n4p/3I1P1P2PP/1p4H1p1F1/p1rP1GP2P2/M1X3D1P3/A1RB1DQCH1VM/VFCT1OGT1R1A/L2S1KE1S1BL b - 157", "89"),
    ("lf1gte1gb1h1/ac1s1xokstfl/1m1b1q1d1c1a/1v1pn3pr1m/P2r4ipv1/2pid1p4p/1p3PPpIP1P/1HPIp3P3/MPF2O1HXD2/AVD1R1QN1RVM/B1C1K1TBC2A/L2+h1GEGS1FL b - 135", "95"),
    ("lfhs1t1d1tfl/2vg1gekc2a/am1b1onsb+Dv1/r2pd1p4m/cp5x2P1/2pi4p2P/P2Pp3P3/1PP1h2p2V1/1V1GP1P2S1M/1MC1HNGR1KH1/ARBDSTX2CFA/LF4E1T1BL b - 183", "112"),
    ("l2t2etds1l/arf4k1xfa/1mhgsg1q2cm/1v1cb4n1v/1D+B1Pd3hro/2P2P1Dpp2/1R3QH1P2p/p2V2N2P1V/F1MT1+pB2FHM/A3O5RC/1C1S1GX1G2A/L1K2ET1S2L b - 229", "192"),
    ("1t1bg1k2t1l/l2e2gs3a/2fxcq1dbf1m/a1mr4c3/3pno1r2vp/2dI1PP1p2P/p1R2C1PDPp1/1S1P6VC/P3+pOQ4M/AFVB1K1EH1RA/3D1GT2F2/L2TG3BS1L w - 250", "144"),
    ("2r2kg1bv1l/lfe1ts2t3/3b5r1a/am+Px2+P1s2m/v4h1d1XVf/c2Bd7/SP3p1pPq1P/8TS2/2T1+iCD1H2A/FMR3EG1MF1/L2G4C2L/2V2KD3B1 w - 296", "127"),
    ("1r1n3k2ft/l1cqs1o1s2l/1v2egg2+V1c/a2t6rd/1m6+R2a/1PPmf3D2A/1b4Pp4/CV2p1H4R/Q2K1ODM1CT1/A1FB4S3/L2GHB1XF1E1/2T3SG3L b - 335", "148"),
    ("lt2e2o3l/bc1gqs1d1fga/2v3k2sv1/2fp3tpr1m/a4h6/3n2PP2c1/B1rQpB6/1R6i2p/+p1D6+pMF/A1V1OR5A/LXF4T3L/3STKGES2H b - 271", "124"),
    ("l3ge1xscbl/a1fr2okd2a/cvt2sn3vf/m3qd1t1gm1/p2p4p1h1/P3NhP4p/Ap1IQP1pir2/H2PPB2b2P/1P1XD2DP+pFA/M1V2T1HS2L/FC1SGEKO1VM1/L1R3G1TCB1 w - 182", "139"),
    ("lf1sd1e4l/1vb1tg2tsca/2crkog1h3/a+P2+P1p3rf/d5m5/p1Pp3x1npH/3q4R3/1V3RPHP1P+v/M2XDP1+p1B2/AC2G2D1S2/LFBK1OGQV1F1/2S1TN1E1T1L w - 214", "196"),
    ("lf3e1g1f2/arbg1x1kcr2/1c2htt2o1l/v1ds1q6/mp1P3sm1+V1/P1p3d2p1a/4p1p2P2/1P2O2pP2p/MCR1P4M1A/1F3GDG3L/AVH5TSF1/L1T1SBEK2B1 w - 238", "113"),
    ("lvc1to1kc1f1/f1bgxe2gbvl/amr2nt1s1ra/4q1dhp2m/2PpsdH1i2P/p2I2PP1p2/3D4I1pS/P2P4PR2/M1+p1+p+p4P1/AR2OKD2TVM/L1FH1TQXG2A/1C1SGE2BCFL w - 176", "112"),
    ("lbct1gt1x1bl/Nfdek2cs1f1/ms2qo1g1dm1/v1h2p4+P1/p1PpPr1pp1h1/P4PpH2Pv/1P1i4I3/2nI3+rP1V1/1M1P1Q2HR1M/AVRGCT2S3/L1S2DDCEB1A/1BF1TKO1GF1L w - 202", "198"),
    ("1b1ntd1s3l/l3g5dc/a1se3ft2a/c1v+Xx4v1b/2m1+Vp2gkm1/pf1IO7/4p5HR/2M1P5r1/AR1P1q4+p1/L1CEN1T3G1/1V1G7L/1SK2H3TF1 w - 364", "142"),
    ("l2ge1t1r1b1/1cfstko1chfl/1d1b1xd4a/1v1mnp1psg1v/1r3h2p1+Pm/a7ip2/2pp2p1IH2/R2PpPPPP1Vp/1OC1D1X2C2/1M1VTGD2M2/A1B1SKQ1SG1A/LF2H1ET1FBL w - 204", "131"),
    ("lds1e2k2sl/a1n1thg4f/c2t4rvca/1vf6+P2/m2hq1P3dm/1P1rp3P2b/2P1P4D1x/V2pD1N2C2/AFRG1R1SVBF1/C1MT5G1M/B1S1X1K1E2A/L5O1T2L w - 308", "146"),
    ("lf1s1x1kg1sl/ac1degt1ocfa/mb1h1tn1drvm/pv6pbp1/3pp1p1i2p/1prI2P3h1/3PP1+H1IPPP/1Pp4pPR2/P3DP1S2Q1/MR1HO1E1B1VM/AVBCTKDT2FA/LF1S1G1GXC1L b 5h 115", "80"),
    ("l4k2n1x1/v4t1q1+B1l/2+P1s2e2ga/1+P+Pc4ct1f/2Cgp1m1C3/a1o5P3/2M8A/1D10/VG3O+p2+r2/2FDS+p3V2/A2K3E2TL/L7BFN1 b - 387", "135"),
    ("lbt1n1hg4/ar1g2k2vfb/1chse1xsr2l/1+I2m1+Pp2t1/H2PCp1Ppc1a/P3P1o1i3/4G7/HR4B5/9C+pM/1M1BT1KS2VF/AF1OD2GTNRA/L2SE6L w - 332", "115"),
    ("1n1g2ht2r1/1tb2ef1of1l/3k3d2va/5gd1+Bc2/1mcq1PbP1P1A/l2psx6/G2P8/Lv1B4PSp1/1C1SR1Q1TCP1/3K3XR1F1/2T1NEG5/V1F3DDVH1L w - 358", "131"),
    ("lv3et1fn1l/a1sdt3o2b/3g2k5/mf6sm1g/4+CP6/bVr1x6P/A7+V3/3p3FH3/L2HKSES4/2G6NGM/1F1R2X1C2A/3BO2TT1+vL w - 374", "122"),
    ("l2ggx1t1r2/fmb2t1kq1sl/c3he2fd1a/2+P1+P2b4/6p1m2c/1G1p1n1dP1p+v/a2i2h1R3/p2PTp3SP1/1+p1RH3D2V/1+v3XGQ1FMA/ACFOE2TC2L/L3KS4H1 w - 312", "176"),
    ("2gt3t4/lms4k3l/afc1reso3a/b2n6mb/P3d2h2cf/4xGd+D1v2/1G1H3q4/MB2E3iCpp/A2+h1O1M3B/LS1RT5FA/2F1C2+p4/V1N1K4VTL w - 322", "200"),
    ("lfcdsek3fl/a1bg1totgb1r/mv2nx1s1cva/3ph4d1m/p1r1pp1pp3/3PP1P2pp1/1pp7Pp/1P3PQ2PRP/P1PD3P2XM/MVR1DN1HTV1A/A1BCHOKE1B2/LF1SGT1GSC1L b - 115", "100"),
    ("l2bgk2s1fl/acfstegxob1a/1v1rdqnh1cvm/m1N+I+P1dtprpp/p1p3p5/1p3p3p2/PP3P1p4/2P3PPiPPP/MR1P3DP3/2C3Q2RBM/A1SDTOXTHGVA/LFV1GKES1CFL b - 103", "152"),
    ("lc1gst1oscbl/a1fd1egktr1a/1vr1q2xdvfm/3mb1n1ph1p/2p2p1H1pp1/3pp2Pi3/p1Pi4IOP1/1p1P8/M1R1DPD1PPVP/AB1HT1SNGR1M/LCVSQ1X2BCA/1F2GKE1T1FL w - 132", "116"),
    ("l1ctseqg1kbl/1fb2xdh1tc1/1vdrgo4ra/8ps2/2p3hpf2m/1mPp6v1/+A2i2DnIpV1/4ppP1P3/1+p1XD2BRPCM/2CGQTR1G1FA/L1MF1T2SH1L/HB1S1KE1O3 b - 225", "85"),
    ("lfx1e1tg1sfl/a1b1sq2t+H1a/mvr1g1kdr1cm/3pc1o1n1v1/p4p2P2p/3h5P2/3I3p2P1/1p1PDP+p1D1RP/2B4PX1CM/M1O1+d2GQ1FV/A1F2KS2B1A/LC1SG2ETH1L w - 162", "170"),
    ("lf1s2tt1d1l/ac1goe1kg2f/m2b1dnhrsca/1v2p1x1ppvm/3p4i1pp/ppri1p1PIP1P/6H1P1P1/1PR1PPPC2M1/P2P3D1B2/CMNGOD1Q1TRA/A1BS2XE3L/LF2KT1GSV1F w - 138", "86"),
    ("l1rb5g2/a3skg1c2l/1dvth1e1b+Drf/m2+P2dts1o1/1c8P1/6P5/1P3OH5/V1Rp3Dp3/1+pMP3T2M+a/2N1S1TSBVFA/5K4CL/1CG3R1EG2 w - 320", "114"),
    ("3e1sk1to1l/lfb2xdsqf1a/a1vdgtg2rcv/mcr6ppm/3pP2pp2h/p5pPD3/1p1ib2n1PPp/3P2G3V1/CM+p2+pP1IRF1/4RDXQHSBM/AVS1HGK2C1A/LF5TE2L b - 191", "80"),
    ("4e2kbt1l/lhfcsg3rsa/a2td1g+P3f/1vx+I1pn1+I1cp/1o1P2p1P2d/3mq1b2DV1/2P8P/V5OC4/AR1EH2S1Q2/1M3GXR1H1M/LF1CBT1TGB1A/3S1K4FL w - 230", "146"),
    ("l1rsge1o2fl/fv1ct1k1scva/a1d3g1bm2/m2p3xtrd1/2qO4p1p1/1P4Pp1p2/p2I1p2i1Pp/1V1Pp4P2/1M1SD2XVC1Q/1FR1KG2R1MA/A1B3+h+h1BD1/LC3TE1S1FL b - 179", "94"),
    ("l3h1kot1b1/f3t1ns3l/2c1gq1v2f+P/avs1dd3g1m/1re5p3/m2ppC6/G2i3Di3/3PTN5p/B2OQD1HPMVA/AFV5ER2/3C2KXSG2/LR1S2TH1FBL b - 267", "163"),
    ("1f4b4l/levkh2rh1ma/a3g3s3/1+Ps+P4N2t/m1td2cpp3/3B1nP2g2/M1q5iErL/3C8/1S1PX1FDG1C1/V1O2GS2M2/AF1T1K1BDTH1/L11 b - 375", "209"),
    ("lbv1g3gtfl/a1schxtksb1a/1f3e1h1rcm/m1r2p1dppv1/p5p1iP2/2BId2p2p1/P2P3qI1Pn/1p2pPNP3p/6P1PR1P/1VR2D1GV2M/ACBOS1DQEGCA/LF2TKT1S1FL b - 157", "122"),
    ("1f1stxb1s2l/lvcget1gc1of/2m1dq1k1m2/a2pd4v2/1r2pp2p2a/pPh1bh2Irpp/P1BiP1pPPP1P/A1pI2P3P1/2VPG1E2V2/1MRN2HTRCFM/L1F1S1GO1B1A/H2CTD1KS2L w - 180", "98"),
    ("l1cs1g2b1fl/avfhtxektg1a/m1rb1osdhcv1/1p2pdp4m/pP1Iq4r2/5p2P1p1/2p2P1P4/P2PPnP2pPP/1MP3QE4/FVRT1NOT1RVM/A1B1DHDHGBCA/L1CSGKX1S1FL w - 116", "165"),
    ("lsq2e2k3/afgdt1tgc1f1/1br4shmbl/1mp2+Hd5/v2c3pP1Q1/p3p7/hpPpn2XN2r/P3PT1P3V/1M1P2+pD1+pCB/AVCR3R3A/L1BT4G1SL/F1G1OKED1F2 w - 268", "172"),
    ("l2tg1kgsc2/1f2seo2rfl/avc1q1t2m1a/m2r1d1b+Inv1/pp1X1P1d3p/2QIP2p1p2/H10h/P1P3pPPP2/1P2T3D+p1P/AV1RD1T1H2M/LFMCS1EOCBSA/1B1G1K1G2FL w - 154", "152"),
    ("l1n1e6l/fvsc2kxsctv/a4tdg+X2a/m2hg1pfor2/3p1b2pP1P/1P3p2I3/1HrIdO6/VR1P1P2P2A/MCG1+pENDqM+pB/2FD3GH3/A2S1T2C1FV/LB2K2T2SL w - 230", "132"),
    ("2h1gegtd3/lfb1dno2fcl/1vct3qhsm1/m1x2s3b2/a1r5pk1a/1P1I2P1IP1v/5p6/2MP4P1r1/2D1T2O2GC/AVR1H1XE1RV1/1C1KHT1NSBFA/LF1BGS1D3L b - 243", "115"),
    ("lb2gegqs1fl/2sc1t1kdrca/a1f1dot3vm/+P2Nnh1hpb2/3p7p/4p1pxiP2/1v6I1P1/3P1Q2PR2/BV4SX3P/MTFH1GDDH1VM/A1C3EOTBCA/L5KGS1FL b - 157", "173"),
    ("lf1btehgs1fl/ac1s1x2cbva/mvd1hgkor1m1/2rp1+P2t2p/p1pi4p2d/3Ipp6/Pp3P2iPp1/1M1PQ2H3P/1PG1PN+p1GH1V/ARC1D1X1DR1M/1VBT2SESBCA/LF2OK1T2FL w - 156", "75"),
    ("3be1t1x2l/1cr1g2s1vca/1t+P1hd3d1m/l1+P3kq+Ph2/7P1o1A/7D4/10r1/1S2s7/M3+p1H1FT2/AVFD4R1ML/5G1ECB1T/L1O1+b1K5 w - 354", "148"),
    ("l3t1d4l/afbsok1es1f1/1m1q1h4+P1/2+P1Nh2pgva/3c1cngb3/5x2i2X/8P1r1/1+Gp1B6H/A1R2FSOF+p1C/1SM4Q1M2/L1D3K4A/3T1E1T3L b - 365", "202"),
    ("3mnh1g2+P1/l1f3ktsbt1/a2gs2x1c1f/pcb1e3o2r/3B5p2/3C4qS2/P1pd4i1P1/1M5+h1V2/1VS2K1+p3M/A1GBO2+p1+r1A/L1RT2DTDHCL/1F4X3F1 b - 333", "125"),
    ("lf2t2gb3/bvse1tksc1fl/a1rg3d1h1a/1m+R3+Bdxrvm/3p2h2P2/pp1cN7/1P1PpP1p4/P1V2E3D2/2MD3OGR+p1/3BK1QH2V1/ACSXTTG1SM1A/LF8FL w - 240", "89"),
    ("1v3e1g1hf1/l1bg1sks1bvl/r2q1t1ot1ra/+P1f1hdp1ncp1/1p1pp4p1m/m1p1cp2px1P/2PiNdPp1P2/3IP2P2P1/MP1PCXD1G1VM/3K1RB1TR1A/AVST2GODCS1/LBF3E3FL w - 174", "121"),
    ("lb1h1tk1cq1l/1vsf1ge1t1fb/a1x3g2rs1/1+B1cp3p1m1/m2prd1PP2o/5N6/+ppPRPQT3Va/M8p1L/2C2E1DRH2/3S2GS4/AFK1G3C3/L1O2TDXB2+p w - 316", "94"),
    ("l4+H1t4/a3k3cf2/2+Pe1x+P4l/mc8oa/3pt3n3/1r3b4sm/1r5H3L/C2R5+Rq1/1MB2G2X3/A6T4/L2FKS6/7G3C b - 395", "133"),
    ("3s3ts1r+L/l4Qt3c1/axefv2q4/m1+Pd1k2hg2/3p2g3C1/P3o7/7pv1b1/AD1i2nH4/L1V4R1V2/2G2N1GXS2/F1CS1TK1T1F1/8R1E1 b - 371", "200"),
    ("l1c3g1b2l/1fbd2ektcfa/1v1s1g1ond2/1r3h1s1r1+P/2t9/m6ppp2/1C1ppNBP1P2/A1pE2p1I1vM/1O2+p1Q1PB2/2RHD6F/1F2KTXS3A/L2SG1TGD1RL b - 249", "183"),
    ("l1s1g1tg4/a1f1tdh2bcl/1r1e4+Pf1a/m2pq1k2s2/4P1p1r2v/5D2ppRm/3P5H1A/1HR3P5/+pM2G1CB1D1L/V3O6F/A1XT1SGTEV2/LBFQK3S3 b - 293", "143"),
    ("1h1be2ks1fl/1f3go4a/lvct1qh1c1br/a2t1gm2d2/mrsp3x2v1/p1PipdP5/PR5p2np/1V1INP2Pp2/2BPD1X2PM1/1MO1HEH2V1R/ACFGSTS1KF1A/L3TQGDBC1L w - 234", "155"),
    ("3gt1k1sfhl/lfr4eg2a/1dv2o3v1m/ac1ps2t1c1+P/3b2px4/3n3dp1Qp/1p1I2q2p1P/FVpP1P1piP1A/2B1D2X2MC/A1R1T1ODB2V/LHSCE2G2SF/5KT3RL w - 252", "173"),
    ("lfc1ge1kscfl/a1rstxotgb1a/1v3qdr3m/mh1d3pph1p/1p3p1n1pvP/P1p1p1p1I1p1/1PHpP2b2P1/4QP2PP2/M1PO2P1X3/1VRBDE1DV2M/AC2SKGTRBCA/LFN1G1T1S1FL b - 117", "100"),
    ("l2s2kgsf1l/a1fce1gd1b1a/r4tn+P1rc1/2m3t4v/1vhd3DpR2/3p6pV/p2P3xP3/1Vp1p7/1MFS+b1+pTXC1B/H1RC3H1DTM/A4OE1Q1FA/L2BK2GS2L w - 240", "159"),
    ("1tv7f1/x3g1k3o1/3ds1et3l/lcfdb1g4a/am5s2m1/2r5pc2/2b3P1i1vV/pGPn3EP2A/1M1K+p+pNHHCR1/7X1TG1/AV1CS7/LF1B1SF2D1L w - 340", "180"),
    ("l2q4c2l/1vb3o1N2a/2+Pxse1kh1r1/6t4m/1f+V1d4P1L/3H2g5/a2G5s2/A7+in2/5C4F1/1QKDT+pSGR3/L4E2S3/1BF2TX1B1M1 b - 399", "195"),
    ("l2b2e1ttfl/af3x1dks2/1r1s3o4/1c+I+P2pbn+P1a/1vP3d4c/3g3N2v1/1C2q5+VR/A3p1D1TSF1/3R1T4C1/L4G1K3L/1O1V6M1/4M1QEG3 b - 333", "210"),
    ("7k1rbf/l1f1s1e1q3/c1g1rt1cv1m1/m2pg1os+P3/2H7R1/1PP9/A1x3D4l/L2PX3TM1L/1MB1SK2+p1+p1/2R5T1V1/F1G2O3G2/1CE3B2CF1 w - 384", "90"),
    ("lf1sg1ko2fl/a2detd1sbha/vbch2+H1gr1v/m7xc2/3+HP1p2p2/1p5pIPm1/3i1O4Pp/PPrP3N2TP/M1V1T1PPP1V1/1C2S1XDSFM1/AFBD1G2CQRA/L1G2K2E1BL b - 175", "160"),
    ("l2sge2gsfl/acf1tk1t1bma/1vd1roqd3c/mp2b4rv1/p1pp1P1npph1/h2i2ppi1p1/3I4IV1P/1PPP3PPP2/P1R1PDP1QH2/MVT1DN4RM/ACBHG1KTS1FA/LF1SOE1G1C1L w - 106", "122"),
    ("1b1sgoxt3l/lfctqe1kg2a/av2h1h1cfsm/mpp3rd1r1v/p2pnd1p4/2PI1b3p1P/P6Pp1p1/1P1P4i1P1/MOB2X1D1S1M/1VR1D2G1R1A/A1SCGK+pH1BVL/LF2HT2C1F1 w - 194", "139"),
    ("1vrget1o3l/4b1xkg1f+P/5q1sbh2/1f1h4r3/l2d3HI1v1/2s5P3/4P1p3R1/aCpc2GpB2M/B2+p1+d2H+p2/AM3C2S2A/L1KS1E1D1F1V/FRT1N2X1G1L b - 297", "140"),
    ("l1csxt1ngc1l/afbe2ok1bfa/1vrhg2drsv1/m1pp1qdth2m/pp2pP3ppP/2Qi2p1p3/1PP3P1iPP1/4O2pI2H/P1HPP3P2M/MVR2DT1BRV1/ACBSTKXDGFCA/LF1NG1E2S1L w - 104", "84"),
    ("lg9l/2h2xos3a/1f2ekt1tgmr/2s5h3/2Bd6f1/2F3D4A/3pq3P1b1/5p5L/3+cEM3CP1/1H1D3G1R2/L1K1T2QS1F1/4+b2T1VB1 w - 390", "150"),
    ("lcv2te3f1/+Ahfsg1okd2l/1m2g1t2s1a/7d2rb/5+P2Xmc1/b1pI3pP3/1r1PB1p4P/1+pP2Q3v2/4GT1D3F/1F2q3+pSRM/4S1TNCG1A/LCRK1DE4L w - 314", "130"),
    ("3bg1n2c1l/1c1skexbr1vf/lvfr1ost1g2/a1p4p+Pm2/m1t5i2a/p2p2qP2p1/2Pi4P3/PV1PdPP2H2/3RS1S1R3/M1NDKQD2CM1/A1BCG1G+h1V1A/LFO1ET4FL b - 203", "112"),
    ("lf1st1gksofl/acb1ex1t3a/mvr1q2nhr1m/2g1dpc2dvb/3pp3p2P/p1pIPP1N1P2/2P1CHphi1p1/M3D3Q3/AV1P2P1PCP1/LFRHOTXRV1M1/4GEKTDBFA/1BS4GS2L b - 159", "182"),
    ("lft2oetbgfl/acbsxgksc1va/m1r2q3drm/v1hd1pppphp1/P1ppp7/1pniP3ipP1/1PPI8/3P1PP1IP2/MFR1D2PP2+p/V3Q2DHRVM/A1HGKOT1SBFA/LBCSX1ETGC1L w - 110", "117"),
    ("lf1bh1ek1sfl/2csgxo1rb1a/a1r1dq1dgc1+P/+Pvtpp1pt3+V/1p3p2p3/1P1i1n1p1P2/3N1PPPI3/V1pPP6P/1+h2X3P3/MBRHDQD2R1M/AC1T1OTHGBCA/LF1SGK1ES1FL w - 122", "129"),
    ("1tv2d4vl/lfb1cn1sgc1a/a1h5rtf1/1+Rx2k3g2/m3s3p1pm/2p5i2X/5S2P2+h/A1q9/LF1D8/1T+pG1K1E1VRM/1C2DO1G2CA/1V4T1SF1L w - 314", "177"),
    ("lfr3khs1fl/1cbgte1t1b1a/2msd2gr1vc/a1xd3pp1pm/p2pn2qI3/3ip1o2pPP/PvpIP1pP1P2/2PP1+p5V/AV4PNPF2/M1RG2QD2BM/1FB1T1GXHRCA/L1CSKEOT1S1L b - 149", "100"),
    ("lf1se1n1kofl/a1b1tg1xs2a/1vchg1t1hcbv/m4p3rp1/1pd2PP1p2m/Pr1ppqd1i3/2pPPN1XP1P1/H11/MP5R2+pM/1VSDC2RB2V/A1T1GOG1S1CA/LF1B1K1ET1FL b - 181", "154"),
    ("lfc1sk1ts1fl/1vbe7a/1m1t1gqdgr2/a3pd4cm/4n1p1ppbp/1PPp2oPI3/1h1ihP2P2P/2MPP3xP2/1O3QP2R+vM/V1RDN3D1V1/AFCTSEXTSCFA/L2BG1KGH1BL w - 158", "167"),
    ("l1cs1okgs3/afb1ge1tcbfl/1mhr1xndh1ma/t3dp2pv1p/q2pP1p1i1p1/1vpI3pI3/1p3PP3P1/1P1P6DP/1VPRXOQPP2M/MC2DNT1H+rVA/A1FGST1EC1F1/L1HB1KG1S1BL b - 123", "92"),
    ("ld1m1o2q2l/1tbksdg3fa/2v2e2t1s1/ar1c1x1b1r2/P1g1pnh1P3/1P5V1pP1/6D4A/1MVC8/4GDR3+h1/A2S2S1TG2/L2XE4OFB/1T2K2H3L b - 335", "131"),
    ("lb1sg1ets2l/a3k1gtofvh/mvfh1d2b2a/r5d1c1rm/Ppcpp2Pp3/2p2pp2pp1/1PPi1n3xPp/4PD3PM1/1V1P1DPQ4/M1RGHNT1F1B1/AFCT1OXRH1CA/LB1SK1EGSV1L b - 149", "98"),
    ("lf4esd1fl/at1rgxb1tbca/c1sh3o4/1v1pd2kprm1/m2i2g1ip2/1p3pD3v1/p1p3p1h1V1/1S1IpP1p1RHp/PR1P2SQE3/AMC1OXN3M1/V1BDTG1TGC1A/LF1H1K2B1FL w - 208", "98"),
    ("lfxdgte3f1/avc3o4l/1bst2kcs1ha/mr1p1+P+P3dm/3I2b5/8g3/1p1P1D1HP3/pVp4R2p1/2SHDO1RC2M/MCF3EG2T1/A1B1G6A/L3KTSQBXFL w - 274", "93"),
    ("3b3t1sfl/lc4ek1r2/1f1tg1sh1gc1/avmp3dp1na/3x3o3m/LH5d4/3rBpPXqv1p/1R3P4pP/SF2+p1H1+iM2/1C1G2QD1CRA/1V1T1D1OSBTL/3E2K1G1F1 w - 250", "173"),
    ("lfcs1o1t1c1l/avgdt1eb1h2/m2b1qnmkxfs/2rpg2p4/1h2p5va/1Ppi2pdPrP1/2PFPp6/p2I2P2p2/1V1PXQD2OVA/A1MRSNG1R1F1/L1BG1T3B2/2C1K1ETSH1L w - 190", "129"),
    ("l2sgekgs1cl/afbd1xo1hf1a/m1cth1drv1bm/2rppq1tpp1p/P6piP2/2pi8/1v2P1pPP1pB/1p1P6HP/1MP3P3P1/1VT1OEQ2RCM/AFR1H1DTSV1A/LBCSGKG2NFL w - 110", "76"),
    ("l1s1ek1gs1bl/arbgx2tc1da/3h2torf1v/mf1pdc2pm2/p1p2p2iPP1/3PPq2I1Rp/6p1P3/n3DP1D1T1P/2P1B4HV1/A1RGH1X1C2M/L1SC1O1S1B1A/1FVKTN1E1GFL w - 196", "129"),
    ("l1cg1ekgt2l/f1bdsx2c1va/a4o1r1fb1/1vmtr1d2s+P1/3p1n1p1Hm1/3i1p3P2/3IpPpP1R1p/pp2P3p3/1R+pP5EV1/MVGH1N2X3/AFSDT2SGBMA/L1CTO2K2FL w - 172", "132"),
    ("lbfse1kgsc2/1m1gttx2bfl/2rod1nh1rma/1Q1pq2d3p/ac1i1p1p4/p1N3p1Pp1v/1pPI1PX3p1/1P1P1D3PPP/M3+p2P2MB/CVRT2O1SH2/A1B1KH1T1V1A/LFDSG1EG1CFL w - 142", "118"),
    ("l3e3h1ft/a1f2t2o3/cs2h4k2/vmr1b2g3l/A3bQG3+Pa/g2P3spc2/2p3P5/2n1D2SB+r2/1+p1O1X4M1/2VB1GEHD2+A/L3T1T1F1V1/1F1H3KSC1L w - 302", "180"),
    ("lf3o1g2fl/ac1gsx1sh2a/vbdhetktb1v1/mp6prcm/1P3pp3p1/1Vrpp4pP1/P1pI2dpQR2/3PNPn1i2p/M1B1PT1PM2A/R2CD1D1H3/AHFSKO1TGBVL/L4GXESCF1 b - 151", "118"),
    ("l1r1kt1+P1f1l/1c6g3/a1e4o3a/1sv1+P+P2ic2/4g5dm/mfbpO1P5/1D1i4H2p/C2b3p3M/1MFSq3QF1C/R1G1KS5A/A4E1T3L/L3T3G1V1 b - 381", "122"),
    ("lf1beo1k3l/avgstxtdgsfa/m3d+Hb1hv1c/r1p+I2npp1m1/1c2P2P3p/p4P2i1PP/1PPP2p1PrV1/12/MV3ODH1R2/A1DR1E3C1M/LCBSTXKQ1T1A/1F3GNGSFBL b - 167", "115"),
    ("lf1t1t3sf1/avse2qhd2l/m3g1x1bcva/2c1op1pg2m/P2p3k3p/6PPp3/1R1P4PprP/5F6/M1Q1EPTG+h1+p1/1V1CO1DX1C1M/A1B1H2B1FVA/LG1TSK1S1D1L w - 234", "116"),
    ("7ek3/l1r1s1h1f2t/3f2st4/ac2d1mx1cv1/8prh1/1PP5gH1l/v3Xq2IF2/1B1+pM4R1G/F4+p2P2A/1MR3ECO2L/AT7S2/LV1K1GT4V w - 344", "123"),
    ("lfcs1egr2v1/2gq1tos2m1/avmrt1k2nfc/7h2dl/p1db1P4x1/P1p3pPp1QP/2NP6P1/ApP2T1KiC2/L3M3D3/V1RCG1E2R1M/1F1DHG1B3A/1B1SXT1H1S+pL w - 254", "151"),
    ("1f1sg1k1sf1l/1v2ted1g1ca/l1r1cdn2r2/2m2ht1v+P1m/4p1p4p/1xppb1o1p1pP/P1Pi1p2I1P1/1V2PPD1Pq1C/M1BP1D1+p1G1M/1RH1O1SNKFV1/AF1GST5A/L1C1T1E3BL w - 202", "127"),
    ("ls1bte3ftl/2fr2k2b1a/a3xdsc1h1m/v1gn2o1r3/1m5g4/p3q1B1P3/1p1p3d1p1p/5N3FG1/AR1H1R2TP1M/LMCBX2+p1SVC/V1T1K2O3A/FG2S1E4L b - 273", "154"),
    ("lbcktebgtcfl/afsghxod1r1a/1v2d3sh2/m4q2+P1v1/4rP1pp1p1/2P1p1p1i3/p6P4/4P3P1Mp/+p1RHN1Q1T1X1/VM1B1K1DGFR1/AOSCD1E1C1VA/LF1TG3S1BL b - 187", "125"),
    ("l4o1tn2l/acsgkegdsvc1/1vf1t3qrfa/3xb+P2p2b/mrP1d1Pp1p1m/1p1I7p/p2P4CPPP/MP1HpE1P4/2BOP2Q3M/1V1RG1DSDRV1/AFSCXK1GT2A/L4T2B1FL b - 205", "115"),
    ("lfctk3s1fl/av2oegn1b1a/1b1sdq1dhc1r/1ph1pt1x2vm/P1r5I1pp/3p1p6/1g3P3p1P/3iN1PpPP2/3CPSK1XRPM/M1HR2DDBFVA/AVB1GO1ET1H1/L1F1TG2SC1L b - 159", "117"),
    ("l1ts3og1bl/c1bnxkgt1s1a/a2mrd2hf2/1f1pe3p2+P/5p2P1v1/2p3Hp2P1/PCB1pc1h4/5P1q2V1/1SPPPM1Q3B/1M1T1NTD1+r1A/A1RH2GX1C1F/LDF1OK2ERSL b - 233", "128"),
    ("lb3o6/a1c1ttkh1f1l/v5dsb+H1g/fs1+P1g1hp2a/3xP1q5/1emp4P1c1/3r1OG5/A1MI1N6/3P6C+d/R2T1K2M1V1/F1DCHG3SFA/L2S2E1X2L w - 322", "120"),
    ("l1v9/1s1dfkd3Ql/4c1ng1hr1/1m2g1t5/1Xh6s1a/2P9/ap5Hf3/4TpC1I+p+v1/5R2P3/MFSM3GF3/AG4K3T1/L1C1DE3R2 b - 393", "107"),
    ("4d5f1/1fc1t2e2c1/lbrhgx1s1kbl/a3sdnp2t1/m2PP3P2a/2p3+v5/p7C1Q1/C1R2P1H4/3MT1+p5/2G1H1S2V2/AF1D3T1FXA/L2S1E1KG2L w - 314", "143"),
    ("l2s6bl/afbgxde1snfa/1m1d1ok1h1cm/+P6g3p/3ptc2p1t1/r1N3p1i2P/3P1pP1qvr1/1S3P1GI1pV/BR1D+p3P2C/M1O1CTDR2XA/A4E2ST2/LVF1GK3F1L b - 247", "157"),
    ("lf1tgktg2fl/a1sd1eo1cbva/cmrh2sdr1hm/1vxp2qpp2+P/p1p3p5/1p2pPP3P1/3I3PIP2/2bP4P3/PPP1PNDQ3M/MVRH1D2BRVF/ACBOTEXTG2A/LFSG1KHS1C1L w - 124", "72"),
    ("4q1e3s1/l2g3d1kf1/svf1t7/3cmtn2+Pm1/1x1i3g4/4ph1K1F2/aD9C/1P1SM+oE1O2A/2M+h4N1VL/FV1C1DS5/L9G1/6XG3T w - 388", "251"),
    ("lf1s1ekob1fl/acb2tt1sgca/2d1g1ndr1v1/mv1q2p1php1/3pp2p3m/1p1i1x2iPhP/p3P7/1PrP3P4/M1H1X1PQ3M/AR1S1+pG1HRD1/1VB1GOEBSCVA/LFC1TK1T2FL b - 129", "81"),
    ("l2sg1kts2l/f1b1tq1ghb1a/2c1d1ec1fvm/a2p2o2r2/1vm1nhx1pp1p/p2ip1PpIdp1/5p4RP/PV2P3PP2/M1+rP2GP4/CB1HOQX1HV1M/A2T1G1DSBFA/LFDS1KET1C1L b - 125", "96"),
    ("lfc2x2g3/3toe1h2dl/a1v3k2r2/m1bs1K3tcs/1r2g1b1p1ma/2pp1D6/CMH5i2A/A1P1p1dp1pV1/1V3G4S1/2R2S2GCRM/LF3N2DTF1/2T2XOE3L w - 344", "128"),
    ("l3k7/avt2+B3ftl/m2f3o1v1a/8gs2/p1scg1P2cpm/x1h1d1d4P/7+p4/2V1pr1G2B1/X3GRM2T1C/M3O1QN2VA/AS4T1D3/LF2C+r2K1FL b - 337", "158"),
    ("lb2ck1dsh1l/2gf3g3a/avrx1edt1c1m/m1h1spo1fb1p/3ppt5v/1qp3Opp1pP/B1PnP1P1P2r/P2I1P1H2PM/MP1P1D1T1F2/RFT1V3D1V1/A1SNKEGBC2A/L1C1GH4SL w - 224", "161"),
    ("1f9l/2ndkgstcf1a/lbm1se1d3v/+Prtpc+P+Pxr3/1v6P1b1/1P1i8/4M7/RVOP2BPH1V1/MHC2X3R1A/A1DSGB3DCL/3F1QEGS3/L4K2T1F1 b - 271", "159"),
    ("1f1r3k2b1/lc1gesgst1fl/ab1h3h3v/2mn2t1rcm1/1p2p3p1p1/p1vpPdX1o3/7dq1Pa/1P3Pp2Q1A/BM+pH3DC3/A2D1S1G1TVF/VGFO1K1EH3/L1T3B2S1L w - 238", "169"),
    ("l1c1et1g2vl/1fgstqk1dscf/1m1b3hboma/3xdh1p1r1p/3p1n4P1/1p4Q1P3/1P4PP3P/a1viNP3p2/3PPO1S3M/MV1RDH1CDRV1/ACBTSEX2GFA/LF1G1K1T2BL b - 177", "126"),
    ("3+P2k1g1f1/4o2t1h1l/f1ds2e2d1a/2h1+P1r4m/l1P2nPx1cP1/p1D9/2m1N+B6/7Rp3/2FV1G1OH3/1M1CT2R1V2/A1BHXEG1S2L/L2K1S1T1F2 b - 331", "198"),
    ("l3g2g2fl/avftsed1trca/mb1c3obhm1/1hr2dk1p3/1Px1pppP1p2/2p1P1P2Hv1/2PpX3I2P/1VRI1HQ4R/+p1CP1P2P1M1/M3ON1GDV2/A1BDTK1ETBFA/L1FS1G2SC1L b - 165", "150"),
    ("l1hb1k2g3/4o1e1rbvl/7xt2f/1cfg5+X1a/2m3dtd2h/L9C1/2v3psD2L/5H4F1/3n1O1G+p3/2R6M2/1+pTESN1HS1T1/6K5 w - 374", "203"),
    ("lfn1t1kt1v1l/a4e4d1/+P2bs2gbsca/3g1p3+P1m/8qS1P/o2d4P3/9V2/1vrP1CPG2RX/MVF1Q4C1F/A5DT3A/1+hR1DTEK4/L1SB4G1HL w - 352", "171"),
    ("lfcsgk1gs2l/avqhtx1tofca/mb2d3hr1m/1r3edn3b/p2pp2Pppv1/1p3Pp1iPP1/2P3PDI2p/1P1iN3P3/PVRPP1D2RVP/M1H1S1QSH1BM/A1GT1OX1EC1A/LFCB1K1GT1FL b - 127", "131"),
    ("lsct2e2ckl/3r1g1t1gfa/a4x1dr1bm/mv1+H1+Posph1v/2f3d1i3/7Q2P1/2p3B1I3/AMbCO1p2p2/1V1P4PF1R/2R2N1EGC1M/1F1SH1XDTS1A/L3GKT3BL w - 228", "100"),
    ("1f6t1bl/1v2bggk2fa/l10m/a1+Hstpd2+P2/hr3q3s2/1c3P1P4/L5D5/3h2pTD2M/3R1R3C2/M2S1HX1G1+pA/FOB1GE1KF2L/8S3 b - 343", "137"),
    ("lf3o1d1ft1/a2cex2s3/mvr5k1bl/1+Pngs4r1a/2P6c1m/2Dd3g4/5h1t4/AFM1+i+BGC4/D5K5/1V6SFM1/B1C2T1GT2A/L2S1R1E3L b - 311", "106"),
    ("lbc3kg2fl/afs1t1ocsbv1/m1rd2e1tr1a/1vg2pp1p2m/1p1x1h1hd2p/1PppP4ppQ/p6nIP1P/PqP2PT1P1P1/4H1P2H1M/MR1D2OT+pS1V/AFBNS1X2BCA/L1CGK1EGRD1L b - 143", "105"),
    ("l2beo1g1ft1/ag1ts1kd3v/m1d3b1s1rl/2c1r3p1c1/P1v3p1im1a/5p4P1/4f2PM3/1pp1P3V3/F1K1TD2+h1FA/AV1Sn2D1RCL/L1C1E3S2Q/1B3X1TG3 b - 281", "89"),
    ("l4ek5/a1v2d3f1l/1b1cg1tt+V1ma/m1r2g1s1Q1c/1f7H2/1s4h5/1x4p3DM/5p1D1RG1/+pV1H+p2+o3L/A4N1S2TF/1O1GS7/L2E1TK3C1 w - 350", "199"),
    ("lbt2e2ts1l/a4xgk1bca/mrch1s2rh1+B/f1+H2gd1of1m/pp2P5v1/1Q1P8/PP4q3P1/2OG1p1dp2P/6+pTH+p1M/MRTN4CS1A/AVF1S2D3L/L1C1KE1G1VF1 b - 237", "152"),
    ("lfd1t1k2cbl/Q3e5oa/2cbhg1xh2m/4d1g2t1s/m2sp4Pvf/2P2p1r4/4P5R1/v1NRM1pXH3/+p2DCP1S1V2/AM4KT1FG1/2FG1HET3A/L1V3OD1CBL w - 266", "114"),
    ("3t1e2t1fl/l3fk1g3b/g1r1cN1ovcm1/2s1d2q+Hs1a/1b3C3r2/a1h2P2I3/1MFQ7P/A1p5d2M/L1S2D1ET2A/1R1BO4FCL/4D1KS1+p2/3TG3GR2 w - 304", "138"),
    ("2nbt3s1f1/l3g1d1tbmv/a1c1sqkc1h1l/4m1o1p3/P2r2gP4/3pPeP1Pr2/1v7p+a1/2dP1PBD1P2/VMF2E6/3DO2R2HV/ACRS3T2SA/L2BGKGX2FL b - 255", "100"),
    ("l1f3g3d1/atb1g2s1cv1/1mn1het2f2/v1qs2km4/3c2h3Pl/5P3r1C/1HC4ppo2/+p2Gx3ESB1/5Q+p4+a/L6G1F2/1FBS2T3T+d/2R3K1RX1L w - 368", "161"),
    ("l5d3fl/bcf1g1kgt2v/a2os3hr2/m2m8/x2ds1e+P2ca/2R3h2R2/2H8p/9V2/AT1G1PE+p+p2B/1BCKD2X1C1A/1O4Q1GSM1/LF2S2T1F1L b - 301", "139"),
    ("lf1g3dt1bl/avch7a/1bt1kngorscm/msrp5fv1/5d5p/1P1QqP1e2x1/H1V2O2P2P/3Pp1Np4/RF5BR+hV1/M3D1T1CHM1/A1BG2XDS2A/L1CSKTE1G1FL w - 230", "168"),
    ("lf2h3s3/a1bt1etk2vl/mc2rq2grf1/p1sp1d2+P1d1/P2ov5Ra/2X2gP4c/3i3pNP1M/2pR3b3A/B2P1H1D1E2/LF2G3DFVL/2HOSKG3C1/6T1TSB1 w - 288", "118"),
    ("lftsek1gbc1l/a1bg1xqtsr2/mvchd3f1v1/3p1pm3+P1/pp1oP2n1p2/r6h1V1a/1P4d1pP2/3i1N6/1MDOHPT1MR2/AVR3Q+pG2A/L1B1K1TD1BCL/1FCSG1E1S1F1 w - 156", "176"),
    ("1f2t1kt3l/1cbsg1osf3/3rded4a/l3phgb1+P1m/a3B2q1rp1/3pPn4h1/A4p1p1BP1/L1MiNP1P4/1+VPPH3QFR1/3SO1DH2VM/C1R1KX1DG1CA/3GT1E1ST1L b - 213", "160"),
    ("2v1d2skn1l/lfbe1s2rf2/3t3o1v1a/4hg2g2m/5Pb1hdc1/3xD5p1/1cH1C2XpC2/2D4PH1B1/1+p5KP1VM/MF1T3GS2A/ASV1G1T2O1L/L5EF4 b - 327", "104"),
    ("l3e4c1l/1fbt1xskdfva/a4+P5m/+Hc1dhq2pb1p/1svQ3o1D1P/3pP2P1P1n/1p2N1p1i3/M4R2P1rM/1P2G2GX1V1/AVCTK1O2D1R/L1BS1E3BCA/1F3HT1S1FL b - 241", "188"),
    ("lf2e1g3bl/a2t1k2os1a/mcrsght3cf/1v2dp2pr1m/QPpIh6n/5d2qp2/PV2pPp2v1p/2PPN4Dp1/D3P2OX1F1/ACM1GH5M/B1FRSE1SRT1A/L3TKGH1CBL w - 224", "132"),
    ("3sge1gt1fl/lcbdtxokcsva/1f1h1n3d1r/1mrp4pp1m/av5h2p1/3iqpPPI3/2p1pP3PP1/M2P1N2P2p/B+p2POXT1RV1/2RK1Q1H2CM/ACG1H2TS2A/LF2SDEGBDFL b - 147", "117"),
    ("l2ft2rf1v1/a2gokt4l/1bvr1dds2c1/2+P1en1b1m1a/c2psG6/M2i1P1p3p/1+V2D1p5/R2I4C2M/BC1P1K1D2G1/T2H2O1HF1A/AF3Q5B/L1S4E1T1L b - 303", "146"),
    ("1b1httk1g2l/lfc3dsrfv1/avs1r3g1ha/3e2o+P3n/6m2P1m/3I3TPQc1/2SP2XO4/3D1p3D2/1+pRC5B1V/VN2G4R1A/AFBK1E1GC3/L4T2S1FL b - 263", "160"),
    ("l2ft2nv3/3qb4t1l/a+Pv4k1s1f/6er3c/3r3TPDp1/1GP3sF4/1R2b4BP1/M4p1X4/2F1N1CS1DG1/AV1S1TH5/L2O1KE3V1/2C9 b - 373", "175")
  )

}