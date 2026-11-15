package shogi

import shogi.format.forsyth.Sfen
import shogi.format.usi.Usi
import shogi.variant.Standard

class HashTest extends ShogiTest {

  private def hashPosition(position: Position): PositionHash =
    Hash(
      position.board,
      position.hands,
      position.color,
      position.variant,
      position.history.lastLionCapture,
    )

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
        val position = Position(variant)
        position.toSfenWithoutMoveNumber must_== variant.initialSfen.truncate
        hashPosition(position).toList.map(_.toInt) must_== bytes
        val imported = Sfen(s"${variant.initialSfen.truncate.value} 42").toPosition(variant).get
        imported.toSfenWithoutMoveNumber must_== position.toSfenWithoutMoveNumber
        hashPosition(imported).toList must_== hashPosition(position).toList
        val edited = imported.withHistory(
          imported.history.copy(
            usis = Vector(Usi("7g7f").get),
            startedAtPly = 80,
            stepShifted = true,
            initialSfen = Some(Sfen("provenance")),
          ),
        )
        hashPosition(edited).toList must_== hashPosition(position).toList
        edited.toSfen.truncate must_== edited.toSfenWithoutMoveNumber
      }
    }

    "include Chu lion-capture metadata and side to play in position hashes" in {
      val original = Position(shogi.variant.Chushogi)
      val captured = original.withHistory(original.history.withLastLionCapture(Some(Square.SQ5H)))
      captured.toSfenWithoutMoveNumber.value must endWith(" b 5h")
      hashPosition(captured).toList mustNotEqual (hashPosition(original).toList)
      hashPosition(original.switch).toList mustNotEqual (hashPosition(original).toList)
      val imported = captured.toSfen.toPosition(shogi.variant.Chushogi).get
      imported.history.lastLionCapture must beSome(Square.SQ5H)
      imported.history.initialSfen must beNone
      hashPosition(imported).toList must_== hashPosition(captured).toList
    }

    "hash current Chu lion-capture rights when they are set and cleared" in {
      val before = Sfen("11k/12/12/12/6n5/6P5/12/5+h6/5N6/12/12/K11 b - 1")
        .toPosition(shogi.variant.Chushogi)
        .get
      val captured = before(Usi("6f6e").get).toOption.get
      val expired  = captured(Usi("1a1b").get).toOption.get

      captured.history.lastLionCapture must beSome(Square.SQ6E)
      expired.history.lastLionCapture must beNone
      forall(List(captured, expired)) { position =>
        position.history.positionHashes.take(Hash.size).toList must_== hashPosition(position).toList
      }
    }

    "be consistent" in {
      val sfen     = Sfen("lnsgkgsnl/1r5b1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL b - 3")
      val position = sfen.toPosition(Standard).get
      val positionAfterMove = position(Usi.Move(Square.SQ8H, Square.SQ2B, false, None)).toOption.get
      val hashAfterMove     = hashPosition(positionAfterMove)

      val sfenAfter     = Sfen("lnsgkgsnl/1r5B1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/7R1/LNSGKGSNL w B 4")
      val positionAfter = sfenAfter.toPosition(Standard).get
      val hashAfter     = hashPosition(positionAfter)

      hashAfterMove.toList mustEqual hashAfter.toList
    }

    "board diffs" in {
      val sfen     = Sfen("lnsgkgsnl/1r5b1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL b - 3")
      val position = sfen.toPosition(Standard).get
      val hash     = hashPosition(position)

      val sfenDiff     = Sfen("nnsgkgsnl/1r5b1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL b - 3")
      val positionDiff = sfenDiff.toPosition(Standard).get
      val hashDiff     = hashPosition(positionDiff)

      hash.toList mustNotEqual hashDiff.toList
    }

    "color diffs" in {
      val sfenGote     = Sfen("lnsgkgsnl/1r5b1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL w - 3")
      val positionGote = sfenGote.toPosition(Standard).get
      val hashGote     = hashPosition(positionGote)

      val sfenSente = Sfen("lnsgkgsnl/1r5b1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL b - 3")
      val positionSente = sfenSente.toPosition(Standard).get
      val hashSente     = hashPosition(positionSente)

      hashGote.toList mustNotEqual hashSente.toList
    }

    "hash hands" in {
      val gotePawn =
        Sfen("lnsgkgsnl/1r5b1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL b p 3")
          .toPosition(Standard)
          .get
      val gotePawnHash = hashPosition(gotePawn)

      val sentePawn =
        Sfen("lnsgkgsnl/1r5b1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL b P 3")
          .toPosition(Standard)
          .get
      val sentePawnHash = hashPosition(sentePawn)

      sentePawnHash.toList mustNotEqual gotePawnHash.toList
    }

    "many rooks in hands" in {
      val goteRooks =
        Sfen("lnsgkgsnl/1r5b1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL b 1r 3")
          .toPosition(Standard)
          .get
      val goteRooksHash = hashPosition(goteRooks)

      val senteRooks =
        Sfen("lnsgkgsnl/1r5b1/pppppp1pp/6p2/9/2P6/PP1PPPPPP/1B5R1/LNSGKGSNL b 2R 3")
          .toPosition(Standard)
          .get
      val senteRoksHash = hashPosition(senteRooks)

      senteRoksHash.toList mustNotEqual goteRooksHash.toList
    }

  }

}
