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

  def parse(source: String, variant: Variant): Option[Situation] = {
    val clean = source.replaceAll("^\n|\n$| ", "")
    val lines = augmentString(clean).linesIterator.map(_.trim).filter(_.nonEmpty).toList
    val hands = lines.find(_.toLowerCase.startsWith("hands")).flatMap(_.split(':').lift(1)) | ""
    val turn =
      if (lines.map(_.toLowerCase).exists(l => l.startsWith("turn") && l.contains("gote"))) "w"
      else "b"
    val sfenReversed = lines
      .filterNot(_ contains ":")
      .mkString("/")
      .foldLeft(List.empty[(Int, Char)]) {
        case (fp :: lp, cur) if fp._2 == cur && fp._2 == '.' =>
          (fp._1 + 1, cur) :: lp
        case (fp, cur) =>
          (1, cur) :: fp
      }
      .map(ic => s"${if (ic._2 == '.') ic._1 else ""}${ic._2}")
      .mkString("")
      .filter(_ != '.')
    val padStr =
      s"${variant.numberOfFiles}/" * (variant.numberOfRanks - sfenReversed.count(_ == '/') - 1)
    val finalSfen =
      List[String](padStr + sfenReversed.reverse, turn, hands)
        .mkString(" ")
    Sfen(finalSfen).toSituation(variant)
  }

  def render(sit: Situation, marks: Map[Iterable[Pos], Char] = Map.empty): String = {
    val markedPoss: Map[Pos, Char] = marks.foldLeft[Map[Pos, Char]](Map.empty) {
      case (marks, (poss, char)) =>
        marks ++ (poss.toList map { pos =>
          (pos, char)
        })
    }
    for (y <- 0 to (sit.variant.numberOfRanks - 1)) yield {
      for (x <- (sit.variant.numberOfFiles - 1) to 0 by -1) yield {
        "%2s" format (Pos.at(x, y).flatMap(markedPoss.get).map(_.toString) | (sit
          .board(x, y)
          .flatMap(p => SfenUtils.toForsyth(p, sit.variant))
          | "."))
      }
    } mkString
  } map (_.trim) mkString "\n" pipe { board =>
    List[String](
      board,
      if (sit.variant.supportsDrops)
        s"Hands:${Sfen.handsToString(sit.hands, sit.variant).filterNot('-' ==)}"
      else "",
      s"Turn:${sit.color.name.capitalize}",
    ).filter(_.nonEmpty).mkString("\n")
  }

  def addNewLines(str: String) = "\n" + str + "\n"
}
