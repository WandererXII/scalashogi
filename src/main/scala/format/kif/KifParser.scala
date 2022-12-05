package shogi
package format
package kif

import scala.util.parsing.combinator._
import cats.data.Validated
import cats.data.Validated.{ invalid, valid, Invalid, Valid }
import cats.implicits._

import shogi.variant._
import shogi.format.forsyth.SfenUtils

// We are keeping the original interface of the parser
// Instead of being strict with what is a valid kif
// we are gonna try to be rather benevolent.
// Both half-width and full-width numbers are accepted (not in clock times)
// All reasonable position formats (1一, 1a, 11) will be accepted, but not positions like (111) for chushogi
// Pieces can either be kanji or english letters (sfen)

// https://gist.github.com/Marken-Foo/7694548af1f562ecd01fba6b60a9c96a
object KifParser {

  // Helper strings for regex, so we don't have to repeat ourselves that much
  val colorsS  = """▲|△|☗|☖"""
  val numbersS = """[1-9１-９一二三四五六七八九十百][0-9０-９一二三四五六七八九十百]*"""
  val filesS = s"""${(Pos.MaxFiles to 1 by -1)
      .map(i => s"$i|${KifUtils.intToFullWidth(i)}|${KifUtils.intToKanji(i)}")
      .toList
      .mkString("|")}"""
  val ranksS = s"""${(Pos.MaxRanks to 1 by -1)
      .map(i => s"$i|${KifUtils.intToFullWidth(i)}|${KifUtils.intToKanji(i)}|${('a' + i - 1).toChar}")
      .toList
      .mkString("|")}"""
  val positionS     = s"""(?:(?:(?:${filesS})(?:${ranksS}))|同|仝)"""
  val piecesJPS     = s"""${KifUtils.allKif mkString "|"}"""
  val handPiecesJPS = s"""${KifUtils.allKifDroppable mkString "|"}"""
  val piecesENGS    = s"""${SfenUtils.allForsyth.mkString("|").toUpperCase.replace("+", """\+""")}"""
  val handPiecesENGS =
    s"""${SfenUtils.allForsythDroppable.mkString("|").toUpperCase.replace("+", """\+""")}"""
  val promotionS = """不成|成|\+"""
  val dropS      = """打"""
  val parsS      = """\(|（|\)|）"""
  val lionMoveS  = """(?:一歩目|二歩目)\s?"""
  val iguiS      = raw"""(?:まで|(?:${parsS})居食い(?:${parsS}))\s?"""

  val moveOrDropRegex =
    raw"""(${colorsS})?(${positionS})\s?(${piecesJPS}|${piecesENGS})(((${promotionS})?\s?(${iguiS})?(${parsS})?←?(${positionS})(${parsS})?)|(${dropS}))""".r

  val moveOrDropLineRegex =
    raw"""(${numbersS}[\s\.。(?:手目)]{1,})?(${lionMoveS})?(${colorsS})?(${positionS})\s?(${piecesJPS}|${piecesENGS})(((${promotionS})?\s?(?:${iguiS})?(${parsS})?←?${positionS}(${parsS})?)|${dropS})""".r.unanchored

  val commentRegex =
    raw"""\*|＊""".r

  final case class StrMove(
      turnNumber: Option[Int],
      move: String,
      comments: List[String],
      secondLionMove: Boolean, // only for chushogi
      timeSpent: Option[Centis] = None,
      timeTotal: Option[Centis] = None
  )

  final case class StrVariation(
      variationStart: Int,
      moves: List[StrMove],
      variations: List[StrVariation]
  )

  def full(kif: String): Validated[String, ParsedNotation] =
    try {
      val preprocessed = augmentString(cleanKif(kif)).linesIterator
        .collect {
          case l if !commentRegex.matches(l.trim.take(1)) =>
            (l.split("#|&").headOption | "").trim
          case l => l.trim
        } // remove # or & comments, but keep them in * comments
        .filterNot(l => l.isEmpty || l.startsWith("まで") || commentRegex.matches(l))
        .mkString("\n")
      for {
        splitted <- splitKif(preprocessed)
        (headerStr, movesStr, variationStr) = splitted
        splitted2 <- splitMetaAndBoard(headerStr)
        (metaStr, boardStr) = splitted2
        preTags     <- TagParser(metaStr)
        parsedMoves <- MovesParser(movesStr)
        (strMoves, terminationOption) = parsedMoves
        init             <- getComments(headerStr)
        parsedVariations <- VariationParser(variationStr)
        variations = createVariations(parsedVariations)
        situation <- KifParserHelper.parseSituation(boardStr, preTags(_.Handicap), strMoves.map(_.move))
        tags = createTags(preTags, situation, strMoves.size, terminationOption)
        parsedMoves <- objMoves(strMoves, situation.variant, variations)
        _ <-
          if (kif.isEmpty || parsedMoves.value.nonEmpty || tags.knownTypes.value.nonEmpty)
            valid(true)
          else invalid("No moves, non-standard starting position or valid tags provided")
      } yield ParsedNotation(init, tags, parsedMoves)
    } catch {
      case _: StackOverflowError =>
        sys error "### StackOverflowError ### in KIF parser"
    }

  def objMoves(
      strMoves: List[StrMove],
      variant: Variant,
      variations: List[StrVariation],
      startDest: Option[Pos] = None,
      startNum: Int = 1
  ): Validated[String, ParsedMoves] = {
    // No need to store 0s that mean nothing
    val uselessTimes =
      strMoves.forall(m => m.timeSpent.fold(true)(_ == Centis(0)) && m.timeTotal.fold(true)(_ == Centis(0)))

    @scala.annotation.tailrec
    def mk(
        parsedMoves: List[ParsedMove],
        strMoves: List[StrMove],
        lastDest: Option[Pos],
        ply: Int
    ): Validated[String, List[ParsedMove]] =
      strMoves match {
        case Nil => valid(parsedMoves.reverse)
        case move :: rest =>
          move match {
            case StrMove(moveNumber, moveStr, comments, secondLionMove, timeSpent, timeTotal) => {
              MoveDropParser(
                moveStr,
                lastDest,
                if (variant.chushogi && secondLionMove)
                  parsedMoves.headOption.flatMap(_.positions.headOption)
                else None,
                variant
              ) map { m =>
                val m1 = m withComments comments withVariations {
                  variations
                    .withFilter(_.variationStart == (moveNumber | ply))
                    .map { v =>
                      objMoves(v.moves, variant, v.variations, lastDest, ply + 1) getOrElse ParsedMoves.empty
                    }
                    .filter(_.value.nonEmpty)
                }
                if (uselessTimes) m1 else m1 withTimeSpent timeSpent withTimeTotal timeTotal
              } match {
                case Valid(move) => {
                  val newMoves =
                    if (variant.chushogi && secondLionMove && parsedMoves.nonEmpty)
                      move :: parsedMoves.tail
                    else move :: parsedMoves
                  mk(newMoves, rest, move.positions.lastOption, ply + 1)
                }
                case Invalid(err) => invalid(err)
              }
            }
          }
      }

    mk(Nil, strMoves, startDest, startNum) map ParsedMoves.apply
  }

  def createVariations(vs: List[StrVariation]): List[StrVariation] = {
    def getChildren(parent: StrVariation, rest: List[StrVariation]): StrVariation = {
      // variationStart is increasing as long as it relates to the current variation
      val ch = rest
        .takeWhile(_.variationStart > parent.variationStart)
        .zipWithIndex
        .foldLeft[List[(StrVariation, Int)]](Nil) { case (acc, cur) =>
          if (acc.headOption.fold(false)(_._1.variationStart < cur._1.variationStart)) acc
          else cur :: acc
        }
        .reverse
      val res = ch.map { case (k, i) =>
        getChildren(k, rest.drop(i + 1))
      }
      StrVariation(parent.variationStart, parent.moves, res)
    }
    // Obtain first depth variations - that's what we want to use in objMoves
    val ch = vs.zipWithIndex
      .foldLeft[List[(StrVariation, Int)]](Nil) { case (acc, cur) =>
        if (
          acc.headOption.fold(false)(v =>
            (v._1.variationStart < cur._1.variationStart) && ((v._1.variationStart + v._1.moves.size) > cur._1.variationStart)
          )
        ) acc
        else cur :: acc
      }
      .reverse

    ch.map { case (k, i) =>
      getChildren(k, vs.drop(i + 1))
    }
  }

  def createTags(
      tags: Tags,
      sit: Situation,
      nbMoves: Int,
      moveTermTag: Option[Tag]
  ): Tags = {
    val sfenTag = sit.toSfen.some.collect {
      case sfen if !sit.variant.standard || sfen.truncate != sit.variant.initialSfen.truncate =>
        Tag(_.Sfen, sfen.truncate.value)
    }
    val termTag = (tags(_.Termination) orElse moveTermTag.map(_.value)).map(t => Tag(_.Termination, t))
    val resultTag = KifParserHelper
      .createResult(
        termTag,
        Color.fromSente((nbMoves + { if (sit.color.gote) 1 else 0 }) % 2 == 0)
      )

    val variantTag =
      if (!sit.variant.standard)
        Tag(_.Variant, sit.variant.name).some
      else None

    List(sfenTag, resultTag, termTag, variantTag).flatten.foldLeft(tags)(_ + _)
  }

  object VariationParser extends RegexParsers with Logging {

    override val whiteSpace = """(\s|\t|\r?\n)+""".r

    def apply(kifVariations: String): Validated[String, List[StrVariation]] = {
      parseAll(variations, kifVariations.trim) match {
        case Success(vars, _) => valid(vars)
        case _                => invalid("Cannot parse variations")
      }
    }

    def variations: Parser[List[StrVariation]] = rep(variation)

    def variation: Parser[StrVariation] =
      as("variation") {
        header ~ rep(move) ^^ { case h ~ m =>
          StrVariation(h, m, Nil)
        }
      }

    def header: Parser[Int] =
      """.+""".r ^^ { case num =>
        raw"""${numbersS}""".r.findFirstIn(num).map(KifUtils.kanjiToInt _) | 0
      }

    // todo - don't repeat this just use MovesParser
    def move: Parser[StrMove] =
      as("move") {
        (commentary *) ~>
          (opt(number) ~ opt(raw"""${lionMoveS}""".r) ~ moveOrDropRegex ~ opt(clock) ~ rep(commentary)) <~
          (moveExtras *) ^^ { case num ~ lionMove ~ moveStr ~ _ ~ comments =>
            StrMove(num, moveStr, cleanComments(comments), lionMove.exists(_.contains("二歩目")))
          }
      }

    def number: Parser[Int] = raw"""${numbersS}[\s\.。(?:手目)]{1,}""".r ^^ { case n =>
      KifUtils.kanjiToInt(n.filterNot(c => List('.', '。', '手', '目').contains(c)).trim)
    }

    def clock: Parser[String] =
      as("clock") {
        """[\(（][0-9０-９\/\s:／]{1,}[\)）]\+?""".r
      }

    def moveExtras: Parser[Unit] =
      as("moveExtras") {
        commentary.^^^(())
      }

    def commentary: Parser[String] =
      as("commentary") {
        commentRegex ~> """.+""".r
      }

  }

  trait Logging { self: Parsers =>
    protected val loggingEnabled = false
    protected def as[T](msg: String)(p: => Parser[T]): Parser[T] =
      if (loggingEnabled) log(p)(msg) else p
  }

  object MovesParser extends RegexParsers with Logging {

    override val whiteSpace = """(\s|\t|\r?\n)+""".r

    def apply(kifMoves: String): Validated[String, (List[StrMove], Option[Tag])] = {
      parseAll(strMoves, kifMoves) match {
        case Success((moves, termination), _) =>
          valid(
            (
              moves,
              termination map { r =>
                Tag(_.Termination, r)
              }
            )
          )
        case err => invalid("Cannot parse moves: %s\n%s".format(err.toString, kifMoves))
      }
    }

    def strMoves: Parser[(List[StrMove], Option[String])] =
      as("moves") {
        (strMove *) ~ (termination *) ~ (commentary *) ^^ { case parsedMoves ~ term ~ coms =>
          (updateLastComments(parsedMoves, coms), term.headOption)
        }
      }

    def strMove: Parser[StrMove] =
      as("move") {
        (commentary *) ~>
          (opt(number) ~ opt(raw"""${lionMoveS}""".r) ~ moveOrDropRegex ~ opt(clock) ~ rep(commentary)) <~
          (moveExtras *) ^^ { case num ~ lionMove ~ moveStr ~ clk ~ comments =>
            StrMove(
              num,
              moveStr,
              cleanComments(comments),
              lionMove.exists(_.contains("二歩目")),
              clk.flatMap(_._1),
              clk.flatMap(_._2)
            )
          }
      }

    def number: Parser[Int] = raw"""${numbersS}[\s\.。(?:手目)]{1,}""".r ^^ { case n =>
      KifUtils.kanjiToInt(n.filterNot(c => List('.', '。', '手', '目').contains(c)).trim)
    }

    private val clockMinuteSecondRegex     = """(\d++):(\d+(?:\.\d+)?)""".r
    private val clockHourMinuteSecondRegex = """(\d++):(\d++)[:\.](\d+(?:\.\d+)?)""".r

    private def readCentis(hours: String, minutes: String, seconds: String): Option[Centis] =
      for {
        h <- hours.toIntOption
        m <- minutes.toIntOption
        cs <- seconds.toDoubleOption match {
          case Some(s) => Some(BigDecimal(s * 100).setScale(0, BigDecimal.RoundingMode.HALF_UP).toInt)
          case _       => none
        }
      } yield Centis(h * 360000 + m * 6000 + cs)

    private def parseClock(str: String): Option[Centis] = {
      str match {
        case clockMinuteSecondRegex(minutes, seconds)            => readCentis("0", minutes, seconds)
        case clockHourMinuteSecondRegex(hours, minutes, seconds) => readCentis(hours, minutes, seconds)
        case _                                                   => None
      }
    }

    private def updateLastComments(moves: List[StrMove], comments: List[String]): List[StrMove] = {
      val index = moves.size - 1
      (moves lift index).fold(moves) { move =>
        moves.updated(index, move.copy(comments = move.comments ::: comments))
      }
    }

    def clock: Parser[(Option[Centis], Option[Centis])] =
      as("clock") {
        """[\(（]\s*""".r ~>
          clockMinuteSecondRegex ~ opt("/") ~ opt(clockHourMinuteSecondRegex) <~
          """\s*[\)）]\+?""".r ^^ { case spent ~ _ ~ total =>
            (parseClock(spent), total.flatMap(parseClock(_)))
          }
      }

    def moveExtras: Parser[Unit] =
      as("moveExtras") {
        commentary.^^^(())
      }

    def commentary: Parser[String] =
      as("commentary") {
        commentRegex ~> """.+""".r
      }

    def termination: Parser[String] =
      as("termination") {
        opt(number) ~ termValue ~ opt(clock) ^^ { case _ ~ term ~ _ =>
          term
        }
      }

    val termValue: Parser[String] = "中断" | "投了" | "持将棋" | "千日手" | "詰み" | "切れ負け" | "反則勝ち" | "入玉勝ち" | "Time-up"
  }

  object MoveDropParser extends RegexParsers with Logging {

    val MoveRegex =
      raw"""(?:${colorsS})?(${positionS})\s?(${piecesJPS}|${piecesENGS})(${promotionS})?\s?(?:${iguiS})?(?:(?:${parsS})?←?(${positionS})(?:${parsS})?)?""".r
    val DropRegex = raw"""(?:${colorsS})?(${positionS})\s?(${handPiecesJPS}|${handPiecesENGS})${dropS}""".r

    override def skipWhitespace = false

    def apply(
        str: String,
        lastDest: Option[Pos],
        firstLionOrig: Option[Pos], // for chushogi lion moves
        variant: Variant
    ): Validated[String, ParsedMove] = {
      str match {
        case MoveRegex(destS, roleS, promS, origS) =>
          for {
            roles <- KifUtils.anyToRole(
              roleS,
              variant
            ) toValid s"Unknown role in move: $str (variant - $variant)"
            destOpt =
              if (destS == "同" || destS == "仝") lastDest
              else
                Pos.fromKey(destS) orElse Pos.allHexKeys.get(augmentString(destS).map(KifUtils.toDigit _))
            dest <- destOpt toValid s"Cannot parse destination square in move: $str"
            orig <- Pos.fromKey(origS) toValid s"Cannot parse origin square in move: $str"
          } yield KifMove(
            dest = dest,
            roles = roles,
            orig = firstLionOrig | orig,
            midStep = if (firstLionOrig.isDefined) Some(orig) else None,
            promotion = promS == "成" || promS == "+",
            metas = Metas(
              comments = Nil,
              glyphs = Glyphs.empty,
              variations = Nil,
              timeSpent = None,
              timeTotal = None
            )
          )
        case DropRegex(posS, roleS) =>
          for {
            rolesBase <- KifUtils
              .anyToRole(roleS, variant)
              .map(_.toList) toValid s"Unknown role in drop: $str"
            role <- variant.handRoles.find(
              rolesBase contains _
            ) toValid s"${rolesBase mkString ","} can't be dropped in $variant variant"
            pos <- Pos.fromKey(posS) orElse Pos.allHexKeys.get(
              augmentString(posS).map(KifUtils.toDigit _)
            ) toValid s"Cannot parse destination square in drop: $str"
          } yield Drop(
            role = role,
            pos = pos,
            metas = Metas(
              comments = Nil,
              glyphs = Glyphs.empty,
              variations = Nil,
              timeSpent = None,
              timeTotal = None
            )
          )
        case _ => invalid("Cannot parse move/drop: %s\n".format(str))
      }
    }
  }

  object TagParser extends RegexParsers with Logging {

    def apply(kif: String): Validated[String, Tags] =
      parseAll(all, kif) match {
        case f: Failure       => invalid("Cannot parse KIF tags: %s\n%s".format(f.toString, kif))
        case Success(tags, _) => valid(Tags(tags.filter(_.value.nonEmpty)))
        case err              => invalid("Cannot parse KIF tags: %s\n%s".format(err.toString, kif))
      }

    def all: Parser[List[Tag]] =
      as("all") {
        rep(tag) <~ """(.|\n)*""".r
      }

    def tag: Parser[Tag] =
      """.+(:).*""".r ^^ { case line =>
        val s = line.split(":", 2).map(_.trim).toList
        Tag(normalizeKifName(s.head), s.lift(1) | "")
      }
  }
  def normalizeKifName(str: String): String =
    Tag.kifNameToTag.get(str).fold(str)(_.lowercase)

  private def cleanKif(kif: String): String =
    kif
      .replace("‑", "-")
      .replace("–", "-")
      .replace('　', ' ')
      .replace("：", ":")
      .replace(s"\ufeff", "")

  private def cleanComments(comments: List[String]) =
    comments.map(_.trim.take(2000)).filter(_.nonEmpty)

  private def getComments(kif: String): Validated[String, InitialPosition] =
    augmentString(kif).linesIterator.toList.map(_.trim).filter(_.nonEmpty) filter { line =>
      line.startsWith("*") || line.startsWith("＊")
    } match {
      case (comms) => valid(InitialPosition(comms.map(_.drop(1).trim)))
    }

  private def splitKif(kif: String): Validated[String, (String, String, String)] = {
    augmentString(kif).linesIterator.toList.map(_.trim).filter(_.nonEmpty) span { line =>
      !moveOrDropLineRegex.matches(line)
    } match {
      case (headerLines, rest) => {
        rest span { line =>
          !line.startsWith("変化")
        } match {
          case (moveLines, variationsLines) =>
            valid((headerLines.mkString("\n"), moveLines.mkString("\n"), variationsLines.mkString("\n")))
        }
      }
    }
  }

  private def splitMetaAndBoard(kif: String): Validated[String, (String, String)] =
    augmentString(kif).linesIterator.toList
      .map(_.trim)
      .filter(l => l.nonEmpty && !(l.startsWith("*") || l.startsWith("＊"))) partition { line =>
      (line contains ":") && !(line.tail startsWith "手の持駒")
    } match {
      case (metaLines, boardLines) => valid(metaLines.mkString("\n") -> boardLines.mkString("\n"))
    }
}
