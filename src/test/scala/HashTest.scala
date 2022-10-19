package shogi

import variant.Standard
import format.forsyth.Sfen
import format.usi.Usi

class HashTest extends ShogiTest {

  "Hasher" should {

    "be consistent" in {
      val sfen          = Sfen("lnsgkgsnl/1r5b1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL b - 3")
      val situation     = sfen.toSituation(Standard).get
      val sitAfter      = situation(Usi.Move(Pos.SQ8H, Pos.SQ2B, false)).toOption.get
      val hashAfterMove = Hash(sitAfter)

      val sfenAfter      = Sfen("lnsgkgsnl/1r5B1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/7R1/LNSGKGSNL w B 4")
      val situationAfter = sfenAfter.toSituation(Standard).get
      val hashAfter      = Hash(situationAfter)

      hashAfterMove mustEqual hashAfter
    }

    "board diffs" in {
      val sfen          = Sfen("lnsgkgsnl/1r5b1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL b - 3")
      val sit           = sfen.toSituation(Standard).get
      val hashAfterMove = Hash(sit)

      val sfenAfter = Sfen("nnsgkgsnl/1r5b1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL b - 3")
      val sitAfter  = sfenAfter.toSituation(Standard).get
      val hashAfter = Hash(sitAfter)

      hashAfterMove mustNotEqual hashAfter
    }

    "color diffs" in {
      val sfen          = Sfen("lnsgkgsnl/1r5b1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL w - 3")
      val sit           = sfen.toSituation(Standard).get
      val hashAfterMove = Hash(sit)

      val sfenAfter = Sfen("lnsgkgsnl/1r5b1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL b - 3")
      val sitAfter  = sfenAfter.toSituation(Standard).get
      val hashAfter = Hash(sitAfter)

      hashAfterMove mustNotEqual hashAfter
    }

    "hash hands" in {
      val gotePawn =
        Sfen("lnsgkgsnl/1r5b1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL b p 3").toSituation(Standard).get
      val gotePawnHash = Hash(gotePawn)

      val sentePawn =
        Sfen("lnsgkgsnl/1r5b1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL b P 3").toSituation(Standard).get
      val sentePawnHash = Hash(sentePawn)

      sentePawnHash mustNotEqual gotePawnHash
    }

    "many rooks in hands" in {
      val goteRooks =
        Sfen("lnsgkgsnl/1r5b1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL b 1r 3")
          .toSituation(Standard)
          .get
      val goteRooksHash = Hash(goteRooks)

      val senteRooks =
        Sfen("lnsgkgsnl/1r5b1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL b 2R 3")
          .toSituation(Standard)
          .get
      val senteRoksHash = Hash(senteRooks)

      senteRoksHash mustNotEqual goteRooksHash
    }

  }

}
