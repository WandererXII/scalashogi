package shogi

import shogi.format.forsyth.Sfen
import shogi.format.usi.Usi
import shogi.variant.Standard

class HashTest extends ShogiTest {

  "Hasher" should {

    "preserve captured position bytes and hashes for every variant" in {
      val expected = List(
        shogi.variant.Standard   -> List(-22, -98, 115),
        shogi.variant.Minishogi  -> List(122, -36, -31),
        shogi.variant.Chushogi   -> List(-15, 45, 26),
        shogi.variant.Annanshogi -> List(-121, -79, 127),
        shogi.variant.Kyotoshogi -> List(-15, -10, -123),
        shogi.variant.Checkshogi -> List(-22, -98, 115),
        shogi.variant.Dobutsu    -> List(-70, -123, 56),
      )
      forall(expected) { case (variant, bytes) =>
        val sit = Situation(variant)
        sit.toSfenWithoutMoveNumber must_== variant.initialSfen.truncate
        Hash(sit).toList.map(_.toInt) must_== bytes
        val imported = Sfen(s"${variant.initialSfen.truncate.value} 42").toSituation(variant).get
        imported.toSfenWithoutMoveNumber must_== sit.toSfenWithoutMoveNumber
        Hash(imported).toList must_== Hash(sit).toList
        val edited = imported.withHistory(
          imported.history.copy(
            usis = Vector(Usi("7g7f").get),
            plies = 99,
            startedAtPly = 80,
            startedAtStep = 100,
            initialSfen = Some(Sfen("provenance")),
          ),
        )
        Hash(edited).toList must_== Hash(sit).toList
        edited.toSfen.truncate must_== edited.toSfenWithoutMoveNumber
      }
    }

    "include Chu lion-capture metadata and side to play in position hashes" in {
      val original = Situation(shogi.variant.Chushogi)
      val captured = original.withHistory(original.history.withLastLionCapture(Some(Pos.SQ5H)))
      captured.toSfenWithoutMoveNumber.value must endWith(" b 5h")
      Hash(captured).toList mustNotEqual (Hash(original).toList)
      Hash(original.switch).toList mustNotEqual (Hash(original).toList)
      val imported = captured.toSfen.toSituation(shogi.variant.Chushogi).get
      imported.history.lastLionCapture must beSome(Pos.SQ5H)
      imported.history.initialSfen must beNone
      Hash(imported).toList must_== Hash(captured).toList
    }

    "be consistent" in {
      val sfen         = Sfen("lnsgkgsnl/1r5b1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL b - 3")
      val situation    = sfen.toSituation(Standard).get
      val sitAfterMove = situation(Usi.Move(Pos.SQ8H, Pos.SQ2B, false, None)).toOption.get
      val hashAfterMove = Hash(sitAfterMove)

      val sfenAfter = Sfen("lnsgkgsnl/1r5B1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/7R1/LNSGKGSNL w B 4")
      val sitAfter  = sfenAfter.toSituation(Standard).get
      val hashAfter = Hash(sitAfter)

      hashAfterMove.toList mustEqual hashAfter.toList
    }

    "board diffs" in {
      val sfen = Sfen("lnsgkgsnl/1r5b1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL b - 3")
      val sit  = sfen.toSituation(Standard).get
      val hash = Hash(sit)

      val sfenDiff = Sfen("nnsgkgsnl/1r5b1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL b - 3")
      val sitDiff  = sfenDiff.toSituation(Standard).get
      val hashDiff = Hash(sitDiff)

      hash.toList mustNotEqual hashDiff.toList
    }

    "color diffs" in {
      val sfenGote = Sfen("lnsgkgsnl/1r5b1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL w - 3")
      val sitGote  = sfenGote.toSituation(Standard).get
      val hashGote = Hash(sitGote)

      val sfenSente = Sfen("lnsgkgsnl/1r5b1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL b - 3")
      val sitSente  = sfenSente.toSituation(Standard).get
      val hashSente = Hash(sitSente)

      hashGote.toList mustNotEqual hashSente.toList
    }

    "hash hands" in {
      val gotePawn =
        Sfen("lnsgkgsnl/1r5b1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL b p 3")
          .toSituation(Standard)
          .get
      val gotePawnHash = Hash(gotePawn)

      val sentePawn =
        Sfen("lnsgkgsnl/1r5b1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL b P 3")
          .toSituation(Standard)
          .get
      val sentePawnHash = Hash(sentePawn)

      sentePawnHash.toList mustNotEqual gotePawnHash.toList
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

      senteRoksHash.toList mustNotEqual goteRooksHash.toList
    }

  }

}
