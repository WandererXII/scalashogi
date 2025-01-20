package shogi
package format.usi

import scala._

import shogi.variant._

class BinaryTest extends ShogiTest {

  import shogi.format.usi.BinaryTestUtils._

  def compareStrAndBin(usisStr: String, variant: Variant) = {
    val bin = Binary.encode(Usi.readList(usisStr).get, variant).toVector
    Binary.decode(bin, variant, 600).map(_.usi).mkString(" ") must_== usisStr
  }

  "binary encoding" should {
    "util test" in {
      showByte(parseBinary("00000101")) must_== "00000101"
      showByte(parseBinary("10100000")) must_== "10100000"
    }
    "write single move" in {
      "simple move" in {
        encodeUsi("1a1b") must_== "00000000,00001001"
        encodeUsi("1b1a") must_== "00001001,00000000"
        encodeUsi("1a2a") must_== "00000000,00000001"
        encodeUsi("7g7f") must_== "00111100,00110011"
        encodeUsi("8h2b") must_== "01000110,00001010"
        encodeUsi("1i1h") must_== "01001000,00111111"
        encodeUsi("1i2i") must_== "01001000,01001001"
        encodeUsi("9a8a") must_== "00001000,00000111"
        encodeUsi("9a9b") must_== "00001000,00010001"
        encodeUsi("9i8i") must_== "01010000,01001111"
        encodeUsi("9h9i") must_== "01000111,01010000"
        encodeUsi("1a9i") must_== "00000000,01010000"
      }
      "move with promotion symbols" in {
        encodeUsi("8h2b+") must_== "01000110,10001010"
        encodeUsi("8h2b=") must_== "01000110,00001010"
      }
      "drop" in {
        encodeUsi("P*5e") must_== "10000001,00101000"
        encodeUsi("L*6c") must_== "10000010,00010111"
        encodeUsi("N*3d") must_== "10000011,00011101"
        encodeUsi("S*2b") must_== "10000100,00001010"
        encodeUsi("G*9a") must_== "10000101,00001000"
        encodeUsi("B*9i") must_== "10000110,01010000"
        encodeUsi("R*1i") must_== "10000111,01001000"
      }
      "simple usi minishogi" in {
        encodeUsi("1a1b", Minishogi) must_== "00000000,00000101"
        encodeUsi("1a2a", Minishogi) must_== "00000000,00000001"
        encodeUsi("4e5e", Minishogi) must_== "00010111,00011000"
        encodeUsi("5a1e", Minishogi) must_== "00000100,00010100"
        encodeUsi("B*3d", Minishogi) must_== "10000110,00010001"
      }
      "simple kyotoshogi" in {
        encodeUsi("1a1b", Kyotoshogi) must_== "00000000,00000101"
        encodeUsi("1a2a", Kyotoshogi) must_== "00000000,00000001"
        encodeUsi("4e5e", Kyotoshogi) must_== "00010111,00011000"
        encodeUsi("5a1e", Kyotoshogi) must_== "00000100,00010100"
        encodeUsi("T*3d", Kyotoshogi) must_== "10000000,00010001"
      }
      "simple move chushogi" in {
        encodeUsi("1a12l", Chushogi) must_== "00000000,10001111"
        encodeUsi("12l1a", Chushogi) must_== "10001111,00000000"
        encodeUsi("12a1l", Chushogi) must_== "00001011,10000100"
        encodeUsi("1l12a", Chushogi) must_== "10000100,00001011"
      }
      "simple move chushogi with promotion" in {
        encodeUsi("1a2a+", Chushogi) must_== "10010000,10010001"
        encodeUsi("2a1a+", Chushogi) must_== "10010001,10010000"
        encodeUsi("1a5e+", Chushogi) must_== "10010000,00110100"
        encodeUsi("5e1a+", Chushogi) must_== "00110100,10010000"
        encodeUsi("12d11d+", Chushogi) must_== "10111111,10111110"
        encodeUsi("11d12d+", Chushogi) must_== "10111110,10111111"
        encodeUsi("12l11l+", Chushogi) must_== "11101111,11101110"
        encodeUsi("11l12l+", Chushogi) must_== "11101110,11101111"
        encodeUsi("1i2i+", Chushogi) must_== "11000000,11000001"
        encodeUsi("2i1i+", Chushogi) must_== "11000001,11000000"
      }
      // 1 2 3
      // 7 x 0
      // 4 5 6
      "lion move" in {
        encodeUsi("5e4e4f", Chushogi) must_== "11111111,00110100,00000101"
        encodeUsi("5e6d6e", Chushogi) must_== "11111111,00110100,00001101"
        encodeUsi("5e5d4d", Chushogi) must_== "11111111,00110100,00010000"
        encodeUsi("5e4d5d", Chushogi) must_== "11111111,00110100,00011111"
        encodeUsi("5e6f6e", Chushogi) must_== "11111111,00110100,00100010"
        encodeUsi("5e5f6e", Chushogi) must_== "11111111,00110100,00101001"
        encodeUsi("5e4f3g", Chushogi) must_== "11111111,00110100,00110110"
        encodeUsi("5e6e7e", Chushogi) must_== "11111111,00110100,00111111"
      }
      "igui/jitto" in {
        encodeUsi("5e4e5e", Chushogi) must_== "00110100,11110000"
        encodeUsi("5e6d5e", Chushogi) must_== "00110100,11110001"
        encodeUsi("5e5d5e", Chushogi) must_== "00110100,11110010"
        encodeUsi("5e4d5e", Chushogi) must_== "00110100,11110011"
        encodeUsi("5e6f5e", Chushogi) must_== "00110100,11110100"
        encodeUsi("5e5f5e", Chushogi) must_== "00110100,11110101"
        encodeUsi("5e4f5e", Chushogi) must_== "00110100,11110110"
        encodeUsi("5e6e5e", Chushogi) must_== "00110100,11110111"
      }
    }
    "write many usis" in {
      "all games" in {
        forall(format.usi.Fixtures.prod500standard) { usisStr =>
          val bin = Binary.encode(Usi.readList(usisStr).get, Standard).toList
          bin.size must be_<=(usisStr.size)
        }
      }
    }
    "read single usi" in {
      "simple move" in {
        decodeUsi("00000000,00001001") must_== "1a1b"
        decodeUsi("00001001,00000000") must_== "1b1a"
        decodeUsi("00000000,00000001") must_== "1a2a"
        decodeUsi("01000111,01010000") must_== "9h9i"
        decodeUsi("00111100,00110011") must_== "7g7f"
      }
      "simple move with promotion" in {
        decodeUsi("01000110,10001010") must_== "8h2b+"
        decodeUsi("00000000,11010000") must_== "1a9i+"
      }
      "drop" in {
        decodeUsi("10000011,00011101") must_== "N*3d"
        decodeUsi("10000111,01010000") must_== "R*9i"
      }
      "simple minishogi" in {
        decodeUsi("00000000,00000101", Minishogi) must_== "1a1b"
        decodeUsi("00000000,00000001", Minishogi) must_== "1a2a"
        decodeUsi("00010111,00011000", Minishogi) must_== "4e5e"
        decodeUsi("00000100,00010100", Minishogi) must_== "5a1e"
        decodeUsi("10000110,00010001", Minishogi) must_== "B*3d"
      }
      "simple kyotoshogi" in {
        decodeUsi("00000000,00000101", Kyotoshogi) must_== "1a1b"
        decodeUsi("00000000,00000001", Kyotoshogi) must_== "1a2a"
        decodeUsi("00010111,00011000", Kyotoshogi) must_== "4e5e"
        decodeUsi("00000100,00010100", Kyotoshogi) must_== "5a1e"
        decodeUsi("10000000,00010001", Kyotoshogi) must_== "T*3d"
      }
      "chushogi move" in {
        decodeUsi("00000000,00000000", Chushogi) must_== "1a1a"
        decodeUsi("00000000,10001111", Chushogi) must_== "1a12l"
        decodeUsi("10001111,00000000", Chushogi) must_== "12l1a"
        decodeUsi("00001011,10000100", Chushogi) must_== "12a1l"
        decodeUsi("10000100,00001011", Chushogi) must_== "1l12a"
      }
      "lion move" in {
        decodeUsi("11111111,00110100,00000101", Chushogi) must_== "5e4e4f"
        decodeUsi("11111111,00110100,00001101", Chushogi) must_== "5e6d6e"
        decodeUsi("11111111,00110100,00010000", Chushogi) must_== "5e5d4d"
        decodeUsi("11111111,00110100,00011111", Chushogi) must_== "5e4d5d"
        decodeUsi("11111111,00110100,00100010", Chushogi) must_== "5e6f6e"
        decodeUsi("11111111,00110100,00101001", Chushogi) must_== "5e5f6e"
        decodeUsi("11111111,00110100,00110110", Chushogi) must_== "5e4f3g"
        decodeUsi("11111111,00110100,00111111", Chushogi) must_== "5e6e7e"
      }
      "igui/jitto" in {
        decodeUsi("00110100,11110000", Chushogi) must_== "5e4e5e"
        decodeUsi("00110100,11110001", Chushogi) must_== "5e6d5e"
        decodeUsi("00110100,11110010", Chushogi) must_== "5e5d5e"
        decodeUsi("00110100,11110011", Chushogi) must_== "5e4d5e"
        decodeUsi("00110100,11110100", Chushogi) must_== "5e6f5e"
        decodeUsi("00110100,11110101", Chushogi) must_== "5e5f5e"
        decodeUsi("00110100,11110110", Chushogi) must_== "5e4f5e"
        decodeUsi("00110100,11110111", Chushogi) must_== "5e6e5e"
      }
    }
    "be isomorphic" in {
      "for one" in {
        compareStrAndBin(format.usi.Fixtures.prod500standard.head, Standard)
        compareStrAndBin("1a12l 12d11d+ 12l1a 2i1i+ 1a5e+", Chushogi)
      }
      "for all" in {
        forall(format.usi.Fixtures.prod500standard)(compareStrAndBin(_, Standard))
      }
    }
    "for all move combinations (standard)" in {
      val allMoves = for {
        orig <- Standard.allPositions
        dest <- Standard.allPositions
      } yield Usi.Move(orig, dest, true, None)
      forall(allMoves.map(_.keys))(compareStrAndBin(_, Standard)) // no promotion
      forall(allMoves.map(_.usi))(compareStrAndBin(_, Standard))
    }
    "for all drop combinations (standard)" in {
      val allDrops = for {
        role <- Standard.handRoles
        pos  <- Standard.allPositions
      } yield Usi.Drop(role, pos)
      forall(allDrops.map(_.usi))(compareStrAndBin(_, Standard))
    }
    "for all moves chushogi" in {
      val promRanks = Chushogi.promotionRanks(Sente) ::: Chushogi.promotionRanks(Gote)
      val allMoves = for {
        orig <- Chushogi.allPositions
        dest <- Chushogi.allPositions
      } yield Usi.Move(orig, dest, false, None)
      // Based around 5E
      val allIguis = for {
        dir <- Pos.allDirections
      } yield Usi.Move(Pos.SQ5E, Pos.SQ5E, false, dir(Pos.SQ5E))
      val allLionMoves = for {
        dir1    <- Pos.allDirections
        dir2    <- Pos.allDirections
        midStep <- dir1(Pos.SQ5E)
        dest    <- dir2(midStep).filterNot(_ == Pos.SQ5E)
      } yield Usi.Move(Pos.SQ5E, dest, false, Some(midStep))
      forall(allMoves.map(_.usi))(compareStrAndBin(_, Chushogi))
      forall(
        allMoves
          .withFilter(m => promRanks.contains(m.orig.rank) || promRanks.contains(m.dest.rank))
          .map(m => s"${m.usi}+"),
      )(compareStrAndBin(_, Chushogi))
      forall(allIguis.map(_.usi))(compareStrAndBin(_, Chushogi))
      forall(allLionMoves.map(_.usi))(compareStrAndBin(_, Chushogi))
    }
  }

}

object BinaryTestUtils {

  def showByte(b: Byte): String =
    "%08d" format {
      b & 0xff
    }.toBinaryString.toInt

  def encodeUsi(m: String, variant: Variant = Standard): String =
    Binary.encode(List(Usi(m).get), variant) map showByte mkString ","

  def decodeUsi(m: String, variant: Variant = Standard): String =
    decodeUsis(m, variant).head

  def decodeUsis(m: String, variant: Variant = Standard): Seq[String] =
    Binary.decode(m.split(',').toList.map(parseBinary), variant, 600).map(_.usi)

  def parseBinary(s: String): Byte = {
    var i    = s.size - 1
    var sum  = 0
    var mult = 1
    while (i >= 0) {
      s.charAt(i) match {
        case '1' => sum += mult
        case '0' =>
        case x   => sys error s"invalid binary literal: $x in $s"
      }
      mult *= 2
      i -= 1
    }
    sum.toByte
  }
}
