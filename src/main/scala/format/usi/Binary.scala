package shogi
package format.usi

import shogi.variant.Chushogi
import shogi.variant.Variant

// Assumes usis are valid - no squares outside of variant board size or promotions not touching the promotion zone
object Binary {

  def decode(bs: Seq[Byte], variant: Variant, nb: Int): Vector[Usi] =
    Reader.decode(bs, variant, nb)

  def encode(ms: Seq[Usi], variant: Variant): Array[Byte] =
    Writer.encode(ms, variant)

  private object Reader {

    def decode(bs: Seq[Byte], variant: Variant, nb: Int): Vector[Usi] =
      if (variant.chushogi) ChushogiDecoder.decodeMoves(bs map toInt, nb)
      else BaseDecoder.decodeMovesAndDrops(bs map toInt, nb, variant)

    private object BaseDecoder {
      def decodeMovesAndDrops(mds: Seq[Int], pliesToGo: Int, variant: Variant): Vector[Usi] = {
        @scala.annotation.tailrec
        def mk(usis: List[Usi], mds: List[Int], pliesToGo: Int): List[Usi] =
          mds match {
            case _ if (pliesToGo <= 0) =>
              usis
            case i1 :: i2 :: rest if bitAt(i1, 7) && variant.supportsDrops =>
              mk(decodeDrop(i1, i2, variant) :: usis, rest, pliesToGo - 1)
            case i1 :: i2 :: rest =>
              mk(decodeMove(i1, i2, variant) :: usis, rest, pliesToGo - 1)
            case _ => usis
          }

        mk(Nil, mds.toList, pliesToGo).reverse.toVector
      }

      private def decodeMove(i1: Int, i2: Int, variant: Variant): Usi =
        Usi.Move(
          square(right(i1, 7), variant.numberOfFiles),
          square(right(i2, 7), variant.numberOfFiles),
          bitAt(i2, 7),
          None,
        )

      private def decodeDrop(i1: Int, i2: Int, variant: Variant): Usi =
        Usi.Drop(Encoding.intToRole(right(i1, 7)), square(right(i2, 7), variant.numberOfFiles))
    }

    private object ChushogiDecoder {
      def decodeMoves(ms: Seq[Int], pliesToGo: Int): Vector[Usi] = {
        @scala.annotation.tailrec
        def mk(usis: List[Usi], ms: List[Int], pliesToGo: Int): List[Usi] =
          ms match {
            case _ if pliesToGo <= 0                        => usis
            case i1 :: i2 :: i3 :: rest if lionMoveType(i1) =>
              mk(decodeLionMove(i2, i3) :: usis, rest, pliesToGo - 1)
            case i1 :: i2 :: rest if iguiMoveType(i2) =>
              mk(decodeIguiMove(i1, i2) :: usis, rest, pliesToGo - 1)
            case i1 :: i2 :: rest =>
              mk(decodeMove(i1, i2) :: usis, rest, pliesToGo - 1)
            case _ => usis
          }

        mk(Nil, ms.toList, pliesToGo).reverse.toVector
      }

      private def decodeLionMove(i1: Int, i2: Int): Usi = {
        val orig    = square(i1, 12)
        val midStep = squareDirection(orig, i2 >> 3)
        Usi.Move(
          orig,
          squareDirection(midStep, right(i2, 3)),
          false,
          Some(midStep),
        )
      }

      // or jitto
      private def decodeIguiMove(i1: Int, i2: Int): Usi = {
        val origDest = square(i1, 12)
        Usi.Move(
          origDest,
          origDest,
          false,
          Some(squareDirection(origDest, normalizeDirection(i2))),
        )
      }

      private def decodeMove(i1: Int, i2: Int): Usi =
        Usi.Move(
          square(normalizePromotionSquare(i1), 12),
          square(normalizePromotionSquare(i2), 12),
          isPromotion(i1, i2),
          None,
        )

      private def squareDirection(square: Square, dir: Int): Square =
        Encoding.directions(dir % 8)(square) getOrElse !!(
          s"Invalid direction (square: $square, dir: ${showByte(dir)})",
        )

      //   0..143 - base board
      // 144..191 - promotion zone top
      // 192..239 - promotion zone bottom
      // 240..247 - igui/jitto direction
      // 255      - flag for three byte move
      private def lionMoveType(i: Int)             = i == 255
      private def iguiMoveType(i: Int)             = i >= 240 && i <= 247
      private def normalizePromotionSquare(i: Int) =
        (i % 144) + ((i / 192) * 48)
      private def normalizeDirection(i: Int) =
        i - 240
      private def isPromotion(i1: Int, i2: Int) =
        i1 > 143 || i2 > 143
    }

    private def square(i: Int, files: Int): Square =
      Square.at(i % files, i / files) getOrElse !!(
        s"Invalid square (files: $files, byte: ${showByte(i)})",
      )

    // right x bits
    private def right(i: Int, x: Int): Int = i & ((1 << x) - 1)
    // from right, starting at 0
    private def bitAt(i: Int, p: Int): Boolean = (i & (1 << p)) != 0

    private def !!(msg: String)          = throw new Exception("Binary usi reader failed: " + msg)
    private def showByte(b: Int): String = "%08d" format (b.toBinaryString.toInt)

    @inline private def toInt(b: Byte): Int = b & 0xff
  }

  private object Writer {

    def encode(usis: Seq[Usi], variant: Variant): Array[Byte] =
      if (variant.chushogi)
        usis.flatMap(ChushogiEncoder.encode).toArray
      else usis.flatMap(BaseEncoder.encode(_, variant)).toArray

    private object BaseEncoder {
      def encode(usi: Usi, variant: Variant): Seq[Byte] =
        usi match {
          case Usi.Move(orig, dest, prom, _) => encodeMove(orig, dest, prom, variant)
          case Usi.Drop(role, square)        => encodeDrop(role, square, variant)
        }

      private def encodeMove(
          orig: Square,
          dest: Square,
          prom: Boolean,
          variant: Variant,
      ): Seq[Byte] =
        Seq(
          squareInt(orig, variant.numberOfFiles),
          (if (prom) 1 << 7 else 0) | squareInt(dest, variant.numberOfFiles),
        ).map(_.toByte)

      private def encodeDrop(role: DroppableRole, square: Square, variant: Variant): Seq[Byte] =
        Seq(
          (1 << 7) | Encoding.roleToInt(role),
          squareInt(square, variant.numberOfFiles),
        ).map(_.toByte)
    }

    private object ChushogiEncoder {
      private val promotionRanks =
        Chushogi.promotionRanks(Sente) ::: Chushogi.promotionRanks(Gote)
      private val files = Chushogi.numberOfFiles

      def encode(usi: Usi): Seq[Byte] =
        usi match {
          case Usi.Move(orig, dest, _, Some(midStep)) => encodeLionMove(orig, dest, midStep)
          case Usi.Move(orig, dest, prom, _)          => encodeMove(orig, dest, prom)
          case _                                      => Nil
        }

      private def encodeMove(orig: Square, dest: Square, prom: Boolean) =
        if (prom)
          Seq(
            if (promotionRanks contains orig.rank) promotionZoneInt(orig)
            else squareInt(orig, files),
            if (promotionRanks contains dest.rank) promotionZoneInt(dest)
            else squareInt(dest, files),
          ).map(_.toByte)
        else Seq(squareInt(orig, files), squareInt(dest, files)).map(_.toByte)

      private def encodeLionMove(orig: Square, dest: Square, midStep: Square): Seq[Byte] =
        if (orig == dest)
          Seq(squareInt(orig, files), 240 + encodeDirection(orig, midStep)).map(_.toByte)
        else
          Seq(
            255,
            squareInt(orig, files),
            (encodeDirection(orig, midStep) << 3) | (encodeDirection(midStep, dest)),
          ).map(_.toByte)

      private def promotionZoneInt(square: Square): Int = {
        val index = squareInt(square, files)
        144 + index - (if (index < 48) 0 else 48)
      }

      private def encodeDirection(from: Square, to: Square): Int =
        ((from.file - to.file + 1) + 3 * (from.rank - to.rank + 1) + 4) % 9
    }

    private def squareInt(square: Square, files: Int): Int =
      files * square.rank.index + square.file.index
  }

  private object Encoding {
    val roleToInt: Map[DroppableRole, Int] = Map(
      Tokin  -> 0,
      Pawn   -> 1,
      Lance  -> 2,
      Knight -> 3,
      Silver -> 4,
      Gold   -> 5,
      Bishop -> 6,
      Rook   -> 7,
    )
    val intToRole: Map[Int, DroppableRole] = roleToInt map { case (k, v) => v -> k }
    val directions: Directions             =
      List[Direction](_.right, _.upLeft, _.up, _.upRight, _.downLeft, _.down, _.downRight, _.left)
  }

}
