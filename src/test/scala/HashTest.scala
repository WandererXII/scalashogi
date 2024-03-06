package shogi

import variant.Standard
import format.forsyth.Sfen
import format.usi.Usi

class HashTest extends ShogiTest {

  "Hasher" should {

    "be consistent" in {
      val sfen          = Sfen("lnsgkgsnl/1r5b1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL b - 3")
      val situation     = sfen.toSituation(Standard).get
      val sitAfterMove  = situation(Usi.Move(Pos.SQ8H, Pos.SQ2B, false, None)).toOption.get
      val hashAfterMove = Hash(sitAfterMove)

      val sfenAfter = Sfen("lnsgkgsnl/1r5B1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/7R1/LNSGKGSNL w B 4")
      val sitAfter  = sfenAfter.toSituation(Standard).get
      val hashAfter = Hash(sitAfter)

      hashAfterMove mustEqual hashAfter
    }

    "board diffs" in {
      val sfen = Sfen("lnsgkgsnl/1r5b1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL b - 3")
      val sit  = sfen.toSituation(Standard).get
      val hash = Hash(sit)

      val sfenDiff = Sfen("nnsgkgsnl/1r5b1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL b - 3")
      val sitDiff  = sfenDiff.toSituation(Standard).get
      val hashDiff = Hash(sitDiff)

      hash mustNotEqual hashDiff
    }

    "color diffs" in {
      val sfenGote = Sfen("lnsgkgsnl/1r5b1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL w - 3")
      val sitGote  = sfenGote.toSituation(Standard).get
      val hashGote = Hash(sitGote)

      val sfenSente = Sfen("lnsgkgsnl/1r5b1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL b - 3")
      val sitSente  = sfenSente.toSituation(Standard).get
      val hashSente = Hash(sitSente)

      hashGote mustNotEqual hashSente
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
