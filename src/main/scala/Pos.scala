package shogi

import scala.math.abs
import scala.math.max
import scala.math.min

// Coordinate system starts at top right
// Directions are given from sente POV
final case class Pos private (index: Int) extends AnyVal {

  def xDist(other: Pos) = abs(file - other.file)
  def yDist(other: Pos) = abs(rank - other.rank)
  def dist(other: Pos)  = max(xDist(other), yDist(other))

  def down: Option[Pos]      = Pos.at(file.index, rank.index + 1)
  def left: Option[Pos]      = Pos.at(file.index + 1, rank.index)
  def downLeft: Option[Pos]  = Pos.at(file.index + 1, rank.index + 1)
  def downRight: Option[Pos] = Pos.at(file.index - 1, rank.index + 1)
  def up: Option[Pos]        = Pos.at(file.index, rank.index - 1)
  def right: Option[Pos]     = Pos.at(file.index - 1, rank.index)
  def upLeft: Option[Pos]    = Pos.at(file.index + 1, rank.index - 1)
  def upRight: Option[Pos]   = Pos.at(file.index - 1, rank.index - 1)

  def isLeftOf(other: Pos): Boolean  = file > other.file
  def isRightOf(other: Pos): Boolean = file < other.file
  def isBelow(other: Pos): Boolean   = rank > other.rank
  def isAbove(other: Pos): Boolean   = rank < other.rank

  def isSameFile(other: Pos): Boolean = file == other.file
  def isSameRank(other: Pos): Boolean = rank == other.rank

  def onSameDiagonal(other: Pos): Boolean =
    file.index - rank.index == other.file.index - other.rank.index || file.index + rank.index == other.file.index + other.rank.index
  def onSameLine(other: Pos): Boolean = isSameRank(other) || isSameFile(other)

  def touches(other: Pos): Boolean = dist(other) == 1

  def >|(stop: Pos => Boolean): List[Pos] = |<>|(stop, _.right)
  def |<(stop: Pos => Boolean): List[Pos] = |<>|(stop, _.left)

  @scala.annotation.tailrec
  def |<>|(stop: Pos => Boolean, dir: Direction, acc: List[Pos] = Nil): List[Pos] =
    dir(this) match {
      case Some(p) => if (stop(p)) (p :: acc).reverse else p.|<>|(stop, dir, p :: acc)
      case _       => acc.reverse
    }

  def <->(other: Pos): Seq[Pos] =
    min(file.index, other.file.index) to max(file.index, other.file.index) flatMap {
      Pos.at(_, rank.index)
    }

  // from down left corner to top right corner
  def upTo(other: Pos): Seq[Pos] =
    min(rank.index, other.rank.index) to max(rank.index, other.rank.index) flatMap {
      Pos.at(file.index, _)
    } flatMap { _ <-> other }

  @inline def file = File of this
  @inline def rank = Rank of this

  def key               = file.key + rank.key
  def kanjiKey          = file.key + rank.kanjiKey
  def kanjiFullWidthKey = file.kanjiFullWidthKey + rank.kanjiKey

  override def toString = key
}

object Pos {

  val MaxFiles = 12
  val MaxRanks = 12

  def apply(index: Int): Option[Pos] =
    if (0 <= index && index < MaxFiles * MaxRanks) Some(new Pos(index))
    else None

  def apply(file: File, rank: Rank): Pos =
    new Pos(file.index + MaxFiles * rank.index)

  def at(x: Int, y: Int): Option[Pos] =
    if (0 <= x && x < MaxFiles && 0 <= y && y < MaxRanks)
      Some(new Pos(y * MaxFiles + x))
    else None

  private[shogi] def fromUciMap(c: Char): Char =
    c match {
      case letter if letter >= 'a' && letter <= 'i' =>
        (-letter.toInt + 'i' + '1').toChar
      case digit if digit >= '1' && digit <= '9' =>
        (-digit.toInt + '9' + 'a').toChar
      case _ => c
    }

  def fromKey(key: String): Option[Pos] =
    allKeys.get(key) orElse allKanjiKeys.get(key) orElse allKanjiFullWidthKeys.get(key) orElse {
      allKeys get key.map(fromUciMap)
    }

  val all: List[Pos] = (0 until (MaxFiles * MaxRanks)).map(new Pos(_)).toList

  // 12a 11a 10a 9a 8a 7a 6a 5a 4a 3a 2a 1a
  // 12b 11b 10b 9b 8b 7b 6b 5b 4b 3b 2b 1b
  // 12c 11c 10c 9c 8c 7c 6c 5c 4c 3c 2c 1c
  // 12d 11d 10d 9d 8d 7d 6d 5d 4d 3d 2d 1d
  // 12e 11e 10e 9e 8e 7e 6e 5e 4e 3e 2e 1e
  // 12f 11f 10f 9f 8f 7f 6f 5f 4f 3f 2f 1f
  // 12g 11g 10g 9g 8g 7g 6g 5g 4g 3g 2g 1g
  // 12h 11h 10h 9h 8h 7h 6h 5h 4h 3h 2h 1h
  // 12i 11i 10i 9i 8i 7i 6i 5i 4i 3i 2i 1i
  // 12j 11j 10j 9j 8j 7j 6j 5j 4j 3j 2j 1j
  // 12k 11k 10k 9k 8k 7k 6k 5k 4k 3k 2k 1k
  // 12l 11l 10l 9l 8l 7l 6l 5l 4l 3l 2l 1l

  val SQ1A  = all(0)
  val SQ2A  = all(1)
  val SQ3A  = all(2)
  val SQ4A  = all(3)
  val SQ5A  = all(4)
  val SQ6A  = all(5)
  val SQ7A  = all(6)
  val SQ8A  = all(7)
  val SQ9A  = all(8)
  val SQ10A = all(9)
  val SQ11A = all(10)
  val SQ12A = all(11)

  val SQ1B  = all(12)
  val SQ2B  = all(13)
  val SQ3B  = all(14)
  val SQ4B  = all(15)
  val SQ5B  = all(16)
  val SQ6B  = all(17)
  val SQ7B  = all(18)
  val SQ8B  = all(19)
  val SQ9B  = all(20)
  val SQ10B = all(21)
  val SQ11B = all(22)
  val SQ12B = all(23)

  val SQ1C  = all(24)
  val SQ2C  = all(25)
  val SQ3C  = all(26)
  val SQ4C  = all(27)
  val SQ5C  = all(28)
  val SQ6C  = all(29)
  val SQ7C  = all(30)
  val SQ8C  = all(31)
  val SQ9C  = all(32)
  val SQ10C = all(33)
  val SQ11C = all(34)
  val SQ12C = all(35)

  val SQ1D  = all(36)
  val SQ2D  = all(37)
  val SQ3D  = all(38)
  val SQ4D  = all(39)
  val SQ5D  = all(40)
  val SQ6D  = all(41)
  val SQ7D  = all(42)
  val SQ8D  = all(43)
  val SQ9D  = all(44)
  val SQ10D = all(45)
  val SQ11D = all(46)
  val SQ12D = all(47)

  val SQ1E  = all(48)
  val SQ2E  = all(49)
  val SQ3E  = all(50)
  val SQ4E  = all(51)
  val SQ5E  = all(52)
  val SQ6E  = all(53)
  val SQ7E  = all(54)
  val SQ8E  = all(55)
  val SQ9E  = all(56)
  val SQ10E = all(57)
  val SQ11E = all(58)
  val SQ12E = all(59)

  val SQ1F  = all(60)
  val SQ2F  = all(61)
  val SQ3F  = all(62)
  val SQ4F  = all(63)
  val SQ5F  = all(64)
  val SQ6F  = all(65)
  val SQ7F  = all(66)
  val SQ8F  = all(67)
  val SQ9F  = all(68)
  val SQ10F = all(69)
  val SQ11F = all(70)
  val SQ12F = all(71)

  val SQ1G  = all(72)
  val SQ2G  = all(73)
  val SQ3G  = all(74)
  val SQ4G  = all(75)
  val SQ5G  = all(76)
  val SQ6G  = all(77)
  val SQ7G  = all(78)
  val SQ8G  = all(79)
  val SQ9G  = all(80)
  val SQ10G = all(81)
  val SQ11G = all(82)
  val SQ12G = all(83)

  val SQ1H  = all(84)
  val SQ2H  = all(85)
  val SQ3H  = all(86)
  val SQ4H  = all(87)
  val SQ5H  = all(88)
  val SQ6H  = all(89)
  val SQ7H  = all(90)
  val SQ8H  = all(91)
  val SQ9H  = all(92)
  val SQ10H = all(93)
  val SQ11H = all(94)
  val SQ12H = all(95)

  val SQ1I  = all(96)
  val SQ2I  = all(97)
  val SQ3I  = all(98)
  val SQ4I  = all(99)
  val SQ5I  = all(100)
  val SQ6I  = all(101)
  val SQ7I  = all(102)
  val SQ8I  = all(103)
  val SQ9I  = all(104)
  val SQ10I = all(105)
  val SQ11I = all(106)
  val SQ12I = all(107)

  val SQ1J  = all(108)
  val SQ2J  = all(109)
  val SQ3J  = all(110)
  val SQ4J  = all(111)
  val SQ5J  = all(112)
  val SQ6J  = all(113)
  val SQ7J  = all(114)
  val SQ8J  = all(115)
  val SQ9J  = all(116)
  val SQ10J = all(117)
  val SQ11J = all(118)
  val SQ12J = all(119)

  val SQ1K  = all(120)
  val SQ2K  = all(121)
  val SQ3K  = all(122)
  val SQ4K  = all(123)
  val SQ5K  = all(124)
  val SQ6K  = all(125)
  val SQ7K  = all(126)
  val SQ8K  = all(127)
  val SQ9K  = all(128)
  val SQ10K = all(129)
  val SQ11K = all(130)
  val SQ12K = all(131)

  val SQ1L  = all(132)
  val SQ2L  = all(133)
  val SQ3L  = all(134)
  val SQ4L  = all(135)
  val SQ5L  = all(136)
  val SQ6L  = all(137)
  val SQ7L  = all(138)
  val SQ8L  = all(139)
  val SQ9L  = all(140)
  val SQ10L = all(141)
  val SQ11L = all(142)
  val SQ12L = all(143)

  def findDirection(from: Pos, to: Pos): Option[Direction] = {
    if (to == from) None
    else if (to isSameFile from)
      Some(
        if (to isAbove from) (_.up) else (_.down),
      )
    else if (to isSameRank from)
      Some(
        if (to isLeftOf from) (_.left) else (_.right),
      )
    else if (to onSameDiagonal from)
      Some(
        if (to isAbove from) {
          if (to isLeftOf from) (_.upLeft) else (_.upRight)
        } else {
          if (to isLeftOf from) (_.downLeft) else (_.downRight)
        },
      )
    else None
  }

  val allDirections: Directions =
    List[Direction](_.up, _.down, _.left, _.right, _.upLeft, _.upRight, _.downLeft, _.downRight)

  val allKeys: Map[String, Pos] = all.map { pos =>
    pos.key -> pos
  }.toMap

  val allKanjiKeys: Map[String, Pos] = all.map { pos =>
    pos.kanjiKey -> pos
  }.toMap

  val allKanjiFullWidthKeys: Map[String, Pos] = all.map { pos =>
    pos.kanjiFullWidthKey -> pos
  }.toMap

}
