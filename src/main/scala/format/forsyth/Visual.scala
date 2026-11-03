package shogi
package format
package forsyth

import scala.util.chaining._

import shogi.variant._

// l n s g k g s n l
// . r . . . . .+B .
// p p p p p p . p p
// . . . . . . p . .
// . . . . . . . . .
// . . P . . . . . .
// P P . P P P P P P
// . . . . . . . R .
// L N S G K G S N L
// Hands:B
// Turn:Gote

// for debugging purposes
object Visual {

  def parse(source: String, variant: Variant): Option[Position] = {
    val clean = source.replaceAll("^\n|\n$| ", "")
    val lines = augmentString(clean).linesIterator.map(_.trim).filter(_.nonEmpty).toList
    val hands = lines.find(_.toLowerCase.startsWith("hands")).flatMap(_.split(':').lift(1)) | ""
    val turn  =
      if (lines.map(_.toLowerCase).exists(l => l.startsWith("turn") && l.contains("gote"))) "w"
      else "b"
    val sfen = lines
      .filterNot(_ contains ":")
      .map(line => line + "." * (variant.numberOfFiles - line.count(_ != '+')))
      .mkString("/")
      .foldLeft(List.empty[(Int, Char)]) {
        case (fp :: lp, cur) if fp._2 == cur && fp._2 == '.' =>
          (fp._1 + 1, cur) :: lp
        case (fp, cur) =>
          (1, cur) :: fp
      }
      .reverse
      .map(ic => s"${if (ic._2 == '.') ic._1 else ""}${ic._2}")
      .mkString("")
      .filter(_ != '.')
    val padStr =
      s"${variant.numberOfFiles}/" * (variant.numberOfRanks - sfen.count(_ == '/') - 1)
    val finalSfen =
      List[String](padStr + sfen, turn, hands)
        .mkString(" ")
    Sfen(finalSfen).toPosition(variant)
  }

  def render(position: Position, marks: Map[Iterable[Pos], Char] = Map.empty): String = {
    val markedPoss: Map[Pos, Char] = marks.foldLeft[Map[Pos, Char]](Map.empty) {
      case (marks, (poss, char)) =>
        marks ++ (poss.toList map { pos =>
          (pos, char)
        })
    }
    for (y <- 0 to (position.variant.numberOfRanks - 1)) yield {
      for (x <- (position.variant.numberOfFiles - 1) to 0 by -1) yield {
        "%2s" format (Pos.at(x, y).flatMap(markedPoss.get).map(_.toString) | (position
          .board(x, y)
          .flatMap(p => SfenUtils.toForsyth(p, position.variant))
          | "."))
      }
    } mkString
  } map (_.trim) mkString "\n" pipe { board =>
    List[String](
      board,
      if (position.variant.supportsDrops)
        s"Hands:${Sfen.handsToString(position.hands, position.variant).filterNot('-' ==)}"
      else "",
      s"Turn:${position.color.name.capitalize}",
    ).filter(_.nonEmpty).mkString("\n")
  }

  def addNewLines(str: String) = "\n" + str + "\n"
}
