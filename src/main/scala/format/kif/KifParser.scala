package shogi
package format
package kif

import java.time.Duration
import scala.util.parsing.combinator._

import cats.data.Validated
import cats.data.Validated.Invalid
import cats.data.Validated.Valid
import cats.data.Validated.invalid
import cats.data.Validated.valid
import cats.implicits._

import shogi.format.forsyth.Sfen
import shogi.format.forsyth.SfenUtils
import shogi.variant._

// We are keeping the original interface of the parser
// Instead of being strict with what is a valid kif
// we are gonna try to be rather benevolent.
// Both half-width and full-width numbers are accepted (not in clock times)
// All reasonable square formats (1一, 1a, 11) will be accepted, but not squares like (111) for chushogi
// Pieces can either be kanji or english letters (sfen)

// https://gist.github.com/Marken-Foo/7694548af1f562ecd01fba6b60a9c96a
object KifParser {

  // Helper strings for regex, so we don't have to repeat ourselves that much
  val colorsS  = """▲|△|☗|☖"""
  val numbersS = """[1-9１-９一二三四五六七八九十百][0-9０-９一二三四五六七八九十百]*"""
  val filesS   = s"""${(Square.MaxFiles to 1 by -1)
      .map(i => s"$i|${KifUtils.intToFullWidth(i)}|${KifUtils.intToKanji(i)}")
      .toList
      .mkString("|")}"""
  val ranksS = s"""${(Square.MaxRanks to 1 by -1)
      .map(i =>
        s"$i|${KifUtils.intToFullWidth(i)}|${KifUtils.intToKanji(i)}|${('a' + i - 1).toChar}",
      )
      .toList
      .mkString("|")}"""
  val squareS       = s"""(?:(?:(?:${filesS})(?:${ranksS}))|同|仝)"""
  val piecesJPS     = s"""${KifUtils.allKif mkString "|"}"""
  val handPiecesJPS = s"""${KifUtils.allKifDroppable mkString "|"}"""
  val piecesENGS = s"""${SfenUtils.allForsyth.mkString("|").toUpperCase.replace("+", """\+""")}"""
  val handPiecesENGS =
    s"""${SfenUtils.allForsythDroppable.mkString("|").toUpperCase.replace("+", """\+""")}"""
  val promotionS = """不成|成|\+"""
  val dropS      = """打"""
  val parsS      = """\(|（|\)|）"""
  val lionMoveS  = """(?:一歩目|二歩目)\s?"""
  val iguiS      = raw"""(?:まで|(?:${parsS})居食い(?:${parsS}))\s?"""

  val moveOrDropRegex =
    raw"""(${colorsS})?(${squareS})\s?(${piecesJPS}|${piecesENGS})(((${promotionS})?\s?(${iguiS})?(${parsS})?←?(${squareS})(${parsS})?)|(${dropS}))""".r

  val moveOrDropLineRegex =
    raw"""^\s*(${numbersS}[\s\.。(?:手目)]{1,})?(${lionMoveS})?(${colorsS})?(${squareS})\s?(${piecesJPS}|${piecesENGS})(((${promotionS})?\s?(?:${iguiS})?(${parsS})?←?${squareS}(${parsS})?)|${dropS})""".r.unanchored

  private val terminationRegex =
    """中断|投了|持将棋|千日手|引き分け|引分け|詰み|不詰|切れ負け|反則勝ち|反則負け|入玉勝ち|不戦勝|不戦敗|トライ勝ち|トライ負け|王手勝ち|Time-up""".r

  private val handicapLabels =
    List("上手:", "下手:", "上手の持駒:", "下手の持駒:", "上手番", "下手番")

  val terminationLineRegex =
    raw"""^\s*(?:${numbersS}[\s\.。(?:手目)]{1,})?(?:${terminationRegex.regex})(?=$$|[\s(（*＊])""".r.unanchored

  val commentRegex =
    raw"""\*|＊""".r

  private val inlineCommentRegex =
    raw"""\s+#.*$$""".r

  final case class StrStep(
      turnNumber: Option[Int],
      step: String,
      comments: List[String],
      secondLionMove: Boolean, // only for chushogi
      timeSpent: Option[Duration] = None,
      timeTotal: Option[Duration] = None,
  )

  final case class StrVariation(
      variationStart: Int,
      steps: List[StrStep],
      variations: List[StrVariation],
  )

  def full(kif: String): Validated[String, ParsedNotation] =
    try {
      val preprocessed = augmentString(cleanKif(kif)).linesIterator
        .collect {
          case l if !commentRegex.matches(l.trim.take(1)) =>
            val t = l.trim
            if (t.startsWith("#") || t.startsWith("&")) ""
            else inlineCommentRegex.replaceFirstIn(t, "")
          case l => l.trim
        } // remove # or & comment lines and trailing ' #' comments, but keep them in * comments
        .filterNot(l => l.isEmpty || l.startsWith("まで") || commentRegex.matches(l))
        .mkString("\n")
      for {
        splitted <- splitKif(preprocessed)
        (headerStr, stepsStr, variationStr) = splitted
        splitted2 <- splitMetaAndBoard(headerStr)
        (metaStr, boardStr) = splitted2
        preTags     <- TagParser(metaStr)
        parsedSteps <- StepsParser(stepsStr)
        (strSteps, terminationOption) = parsedSteps
        init             <- getComments(headerStr)
        parsedVariations <- VariationParser(variationStr)
        variations = createVariations(parsedVariations)
        position <- KifParserHelper.parsePosition(
          boardStr,
          preTags(_.Handicap),
          strSteps.map(_.step),
        )
        variant     = position.variant
        initialSfen = KifParserHelper
          .parseStartStep(boardStr)
          .fold(position.toSfen)(sn => Sfen(s"${position.toSfenWithoutMoveNumber} $sn"))
          .some
          .filterNot(s => s.truncate == variant.initialSfen.truncate && s.stepNumber.forall(_ <= 1))
        parsedSteps <- objSteps(strSteps, variant, variations)
        isHandicap = initialSfen.exists(Handicap.isHandicap(_, variant)) ||
          augmentString(headerStr).linesIterator
            .map(_.trim)
            .exists(l => handicapLabels.exists(l.startsWith))
        tags = createTags(
          preTags,
          position.color,
          parsedSteps.value.size,
          terminationOption,
          isHandicap,
        )
      } yield ParsedNotation(parsedSteps, initialSfen, variant, init, tags)
    } catch {
      case _: StackOverflowError =>
        invalid("Cannot parse KIF: parser stack limit exceeded")
    }

  private val maxVariationDepth = 100

  sealed private trait StepsError
  final private case class InvalidStep(error: String) extends StepsError
  private case object VariationTooDeep                extends StepsError

  def objSteps(
      strSteps: List[StrStep],
      variant: Variant,
      variations: List[StrVariation],
      startDest: Option[Square] = None,
      startNum: Int = 1,
  ): Validated[String, ParsedSteps] =
    objSteps(strSteps, variant, variations, startDest, startNum, depth = 0) leftMap {
      case InvalidStep(error) => error
      case VariationTooDeep   =>
        s"Cannot parse KIF: variation nesting exceeds $maxVariationDepth levels"
    }

  // A line at depth maxVariationDepth + 1 is parsed without its own variations,
  // only to know whether it would be kept
  private def objSteps(
      strSteps: List[StrStep],
      variant: Variant,
      variations: List[StrVariation],
      startDest: Option[Square],
      startNum: Int,
      depth: Int,
  ): Validated[StepsError, ParsedSteps] = {
    // No need to store 0s that mean nothing
    val uselessTimes =
      strSteps.forall(s =>
        s.timeSpent.fold(true)(_ == Duration.ZERO) && s.timeTotal.fold(true)(_ == Duration.ZERO),
      )

    // Each variation is used up by the first step it starts at, parsed or not.
    // Stops at the first too deep variation, since this line can't be kept anymore
    @scala.annotation.tailrec
    def parseVariations(
        candidates: List[StrVariation],
        lastDest: Option[Square],
        ply: Int,
        parsed: List[ParsedSteps] = Nil,
    ): (List[ParsedSteps], Boolean) =
      candidates match {
        case Nil       => (parsed.reverse, false)
        case v :: rest =>
          objSteps(v.steps, variant, v.variations, lastDest, ply, depth + 1) match {
            case Invalid(VariationTooDeep) => (Nil, true)
            case Valid(steps) if steps.value.nonEmpty =>
              parseVariations(rest, lastDest, ply, steps :: parsed)
            case _ => parseVariations(rest, lastDest, ply, parsed)
          }
      }

    @scala.annotation.tailrec
    def mk(
        parsedSteps: List[ParsedStep],
        strSteps: List[StrStep],
        pending: Map[Int, List[StrVariation]],
        lastDest: Option[Square],
        ply: Int,
        hasTooDeepVariation: Boolean,
    ): Validated[StepsError, List[ParsedStep]] =
      strSteps match {
        case Nil =>
          if (hasTooDeepVariation) invalid(VariationTooDeep) else valid(parsedSteps.reverse)
        case StrStep(stepNumber, stepStr, comments, secondLionMove, timeSpent, timeTotal) :: rest =>
          val lionContinuation = variant.chushogi && secondLionMove && parsedSteps.nonEmpty
          MoveDropParser(
            stepStr,
            lastDest,
            if (lionContinuation) parsedSteps.head.squares.headOption else None,
            variant,
          ) match {
            case Invalid(err) => invalid(InvalidStep(err))
            case Valid(s)     =>
              val start                                                  = stepNumber | ply
              val (stepVariations, nextPending, nextHasTooDeepVariation) =
                if (lionContinuation)
                  (parsedSteps.head.metas.variations, pending, hasTooDeepVariation)
                else if (hasTooDeepVariation)
                  // Only whether a later step is invalid still matters
                  (Nil, pending - start, true)
                else {
                  val (parsed, stepHasTooDeepVariation) =
                    parseVariations(pending.getOrElse(start, Nil), lastDest, ply)
                  (parsed, pending - start, hasTooDeepVariation || stepHasTooDeepVariation)
                }
              val s1         = s withComments comments withVariations stepVariations
              val parsedStep =
                if (uselessTimes) s1 else s1 withTimeSpent timeSpent withTimeTotal timeTotal
              val newSteps =
                if (lionContinuation) parsedStep :: parsedSteps.tail
                else parsedStep :: parsedSteps
              mk(
                newSteps,
                rest,
                nextPending,
                parsedStep.squares.lastOption,
                ply + 1,
                nextHasTooDeepVariation,
              )
          }
      }

    val pending =
      if (depth > maxVariationDepth) Map.empty[Int, List[StrVariation]]
      else variations.groupBy(_.variationStart)

    mk(Nil, strSteps, pending, startDest, startNum, hasTooDeepVariation = false) andThen { steps =>
      if (depth > maxVariationDepth && steps.nonEmpty) invalid(VariationTooDeep)
      else valid(ParsedSteps(steps))
    }
  }

  def createVariations(vs: List[StrVariation]): List[StrVariation] = {
    final class Frame(val variation: StrVariation) {
      var children: List[StrVariation] = Nil
      var result: StrVariation         = variation
    }

    var stack: List[Frame] = Nil
    var roots: List[Frame] = Nil
    var rootEnd            = 0

    def closeVariation(): Unit = {
      val frame = stack.head
      stack = stack.tail
      frame.result = frame.variation.copy(variations = frame.children.reverse)
      stack.headOption.foreach(parent => parent.children = frame.result :: parent.children)
    }

    vs.foreach { variation =>
      val start = variation.variationStart
      // A variation's descendants end at the next header with an equal or lower start.
      while (stack.headOption.exists(_.variation.variationStart >= start))
        closeVariation()

      val frame = new Frame(variation)
      stack = frame :: stack

      // Root selection is independent of nesting. A header can also be a root if
      // it falls outside the previous root's move range.
      if (
        roots.headOption.forall(root => start <= root.variation.variationStart || start >= rootEnd)
      ) {
        roots = frame :: roots
        rootEnd = start + variation.steps.size
      }
    }
    while (stack.nonEmpty)
      closeVariation()

    roots.reverse.map(_.result)
  }

  def createTags(
      tags: Tags,
      color: Color,
      nbSteps: Int,
      stepTermTag: Option[Tag],
      handicap: Boolean,
  ): Tags = {
    val termTag =
      (tags(_.Termination) orElse stepTermTag.map(_.value)).map(t => Tag(_.Termination, t))
    val resultTag = KifParserHelper
      .createResult(
        termTag,
        Color.fromSente((nbSteps + { if (color.gote) 1 else 0 }) % 2 == 0),
        handicap,
      )

    List[Option[Tag]](resultTag, termTag).flatten.foldLeft(tags)(_ + _)
  }

  object VariationParser extends RegexParsers with Logging {

    override val whiteSpace = fieldSeparator

    def apply(kifVariations: String): Validated[String, List[StrVariation]] = {
      parseAll(variations, kifVariations.trim) match {
        case Success(vars, _) => valid(vars)
        case _                => invalid("Cannot parse variations")
      }
    }

    def variations: Parser[List[StrVariation]] = rep(variation)

    def variation: Parser[StrVariation] =
      as("variation") {
        header ~ rep(step) <~ rep(termination) <~ rep(commentary) ^^ { case h ~ s =>
          StrVariation(h, s, Nil)
        }
      }

    def header: Parser[Int] =
      """変化.*""".r ^^ { case num =>
        raw"""${numbersS}""".r.findFirstIn(num).map(KifUtils.kanjiToInt _) | 0
      }

    private def termination: Parser[Unit] =
      (opt(number) ~ terminationRegex ~ opt(clock)).^^^(())

    // todo - don't repeat this just use StepsParser
    def step: Parser[StrStep] =
      as("step") {
        (commentary *) ~>
          (opt(number) ~ opt(raw"""${lionMoveS}""".r) ~ moveOrDropRegex ~ opt(clock) ~ rep(
            commentary,
          )) <~
          (stepExtras *) ^^ { case num ~ lionMove ~ stepStr ~ clk ~ comments =>
            StrStep(
              num,
              stepStr,
              cleanComments(comments),
              lionMove.exists(_.contains("二歩目")),
              clk.flatMap(_._1),
              clk.flatMap(_._2),
            )
          }
      }

    def number: Parser[Int] = raw"""${numbersS}[\s\.。(?:手目)]{1,}""".r ^^ { case n =>
      KifUtils.kanjiToInt(n.filterNot(c => List[Char]('.', '。', '手', '目').contains(c)).trim)
    }

    def clock: Parser[(Option[Duration], Option[Duration])] = Parser { input =>
      StepsParser.clock(input) match {
        case StepsParser.Success(value, next) => Success(value, next)
        case StepsParser.Failure(msg, next)   => Failure(msg, next)
        case StepsParser.Error(msg, next)     => Error(msg, next)
      }
    }

    def stepExtras: Parser[Unit] =
      as("stepExtras") {
        commentary.^^^(())
      }

    def commentary: Parser[String] =
      as("commentary") {
        commentRegex ~> """.+""".r
      }

  }

  trait Logging { self: Parsers =>
    protected val loggingEnabled =
      false
    protected def as[T](msg: String)(p: => Parser[T]): Parser[T] =
      if (loggingEnabled) log(p)(msg) else p
  }

  object StepsParser extends RegexParsers with Logging {

    override val whiteSpace = fieldSeparator

    def apply(kifSteps: String): Validated[String, (List[StrStep], Option[Tag])] = {
      parseAll(strSteps, kifSteps) match {
        case Success((steps, termination), _) =>
          valid(
            (
              steps,
              termination map { r =>
                Tag(_.Termination, r)
              },
            ),
          )
        case err => invalid("Cannot parse moves/drops: %s\n%s".format(err.toString, kifSteps))
      }
    }

    def strSteps: Parser[(List[StrStep], Option[String])] =
      as("steps") {
        (strStep *) ~ (termination *) ~ (commentary *) ^^ { case parsedSteps ~ term ~ coms =>
          (updateLastComments(parsedSteps, cleanComments(coms)), term.headOption)
        }
      }

    def strStep: Parser[StrStep] =
      as("step") {
        (commentary *) ~>
          (opt(number) ~ opt(raw"""${lionMoveS}""".r) ~ moveOrDropRegex ~ opt(clock) ~ rep(
            commentary,
          )) <~
          (stepExtras *) ^^ { case num ~ lionMove ~ stepStr ~ clk ~ comments =>
            StrStep(
              num,
              stepStr,
              cleanComments(comments),
              lionMove.exists(_.contains("二歩目")),
              clk.flatMap(_._1),
              clk.flatMap(_._2),
            )
          }
      }

    def number: Parser[Int] = raw"""${numbersS}[\s\.。(?:手目)]{1,}""".r ^^ { case n =>
      KifUtils.kanjiToInt(n.filterNot(c => List[Char]('.', '。', '手', '目').contains(c)).trim)
    }

    private val clockMinuteSecondRegex     = """(\d++):(\d+(?:\.\d+)?)""".r
    private val clockHourMinuteSecondRegex = """(\d++):(\d++)[:\.](\d+(?:\.\d+)?)""".r

    private def readDuration(hours: String, minutes: String, seconds: String): Option[Duration] =
      scala.util.Try {
        val total = new java.math.BigDecimal(hours)
          .multiply(java.math.BigDecimal.valueOf(3600000))
          .add(new java.math.BigDecimal(minutes).multiply(java.math.BigDecimal.valueOf(60000)))
          .add(new java.math.BigDecimal(seconds).multiply(java.math.BigDecimal.valueOf(1000)))
        Duration.ofMillis(total.setScale(0, java.math.RoundingMode.HALF_UP).longValueExact())
      }.toOption

    private def parseClock(str: String): Option[Duration] = {
      str match {
        case clockMinuteSecondRegex(minutes, seconds) => readDuration("0", minutes, seconds)
        case clockHourMinuteSecondRegex(hours, minutes, seconds) =>
          readDuration(hours, minutes, seconds)
        case _ => None
      }
    }

    private def updateLastComments(steps: List[StrStep], comments: List[String]): List[StrStep] = {
      val index = steps.size - 1
      (steps lift index).fold(steps) { step =>
        steps.updated(index, step.copy(comments = step.comments ::: comments))
      }
    }

    def clock: Parser[(Option[Duration], Option[Duration])] =
      as("clock") {
        """[\(（]\s*""".r ~>
          clockMinuteSecondRegex ~ opt("/") ~ opt(clockHourMinuteSecondRegex) <~
          """\s*[\)）]\+?""".r >> { case spent ~ _ ~ total =>
            (parseClock(spent), total.traverse(parseClock)) match {
              case (Some(s), Some(t)) => success((Some(s), t))
              case _                  => err("Clock exceeds the supported millisecond range")
            }
          }
      }

    def stepExtras: Parser[Unit] =
      as("stepExtras") {
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

    val termValue: Parser[String] = terminationRegex
  }

  object MoveDropParser extends RegexParsers with Logging {

    val MoveRegex =
      raw"""(?:${colorsS})?(${squareS})\s?(${piecesJPS}|${piecesENGS})(${promotionS})?\s?(?:${iguiS})?(?:(?:${parsS})?←?(${squareS})(?:${parsS})?)?""".r
    val DropRegex =
      raw"""(?:${colorsS})?(${squareS})\s?(${handPiecesJPS}|${handPiecesENGS})${dropS}""".r

    override def skipWhitespace = false

    def apply(
        str: String,
        lastDest: Option[Square],
        firstLionOrig: Option[Square], // for chushogi lion moves
        variant: Variant,
    ): Validated[String, ParsedStep] = {
      str match {
        case MoveRegex(destS, roleS, promS, origS) =>
          for {
            roles <- KifUtils.anyToRole(
              roleS,
              variant,
            ) toValid s"Unknown role in move: $str (variant - $variant)"
            destOpt =
              if (destS == "同" || destS == "仝") lastDest
              else KifUtils.parseKifSquare(destS)
            dest <- destOpt toValid s"Cannot parse destination square in move: $str"
            orig <- Option(origS).flatMap(KifUtils.parseKifSquare) toValid
              s"Cannot parse origin square in move: $str"
          } yield KifMove(
            dest = dest,
            roles = roles,
            orig = firstLionOrig | orig,
            midStep = Option.when(firstLionOrig.isDefined)(orig),
            promotion = promS == "成" || promS == "+",
            metas = Metas(
              comments = Nil,
              glyphs = Glyphs.empty,
              variations = Nil,
              timeSpent = None,
              timeTotal = None,
            ),
          )
        case DropRegex(destS, roleS) =>
          for {
            rolesBase <- KifUtils
              .anyToRole(roleS, variant)
              .map(_.toList) toValid s"Unknown role in drop: $str"
            role <- variant.dropRoles.find(
              rolesBase contains _,
            ) toValid s"${rolesBase mkString ","} can't be dropped in $variant variant"
            square <- KifUtils.parseKifSquare(
              destS,
            ) toValid s"Cannot parse destination square in drop: $str"
          } yield Drop(
            role = role,
            square = square,
            metas = Metas(
              comments = Nil,
              glyphs = Glyphs.empty,
              variations = Nil,
              timeSpent = None,
              timeTotal = None,
            ),
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
        rep(tag) <~ """(?s).*""".r
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
    valid(
      InitialPosition(
        augmentString(kif).linesIterator
          .map(_.trim)
          .filter(l => l.startsWith("*") || l.startsWith("＊"))
          .map(_.drop(1).trim)
          .toList,
      ),
    )

  private def splitKif(kif: String): Validated[String, (String, String, String)] = {
    augmentString(kif).linesIterator.map(_.trim).filter(_.nonEmpty) span { line =>
      !moveOrDropLineRegex.matches(line) && !terminationLineRegex.matches(line)
    } match {
      case (headerLines, rest) => {
        rest span { line =>
          !line.startsWith("変化")
        } match {
          case (stepLines, variationsLines) =>
            valid(
              (headerLines.mkString("\n"), stepLines.mkString("\n"), variationsLines.mkString("\n")),
            )
        }
      }
    }
  }

  private def splitMetaAndBoard(kif: String): Validated[String, (String, String)] =
    augmentString(kif).linesIterator
      .map(_.trim)
      .filter(l => l.nonEmpty && !(l.startsWith("*") || l.startsWith("＊"))) partition { line =>
      (line contains ":") && !(line.tail startsWith "手の持駒")
    } match {
      case (metaLines, boardLines) => valid(metaLines.mkString("\n") -> boardLines.mkString("\n"))
    }
}
