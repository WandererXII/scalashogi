package shogi
package format
package csa

import scala.util.parsing.combinator._
import cats.data.Validated
import cats.data.Validated.{ invalid, valid }
import cats.implicits._

import shogi.variant.Standard

// https://gist.github.com/Marken-Foo/b1047990ee0c65537582ebe591e2b6d7
object CsaParser {

  // Helper strings for regex, so we don't have to repeat ourselves that much
  val colorsS     = """\+|-"""
  val positionS   = """[1-9][1-9]"""
  val dropOriginS = """00"""
  val piecesS     = """OU|HI|RY|KA|UM|KI|GI|NG|KE|NK|KY|NY|FU|TO"""
  val handPiecesS = """HI|KA|KI|GI|KE|KY|FU"""

  val moveOrDropRegex =
    raw"""($colorsS)?($positionS|$dropOriginS)($positionS)($piecesS)""".r

  final case class StrStep(
      step: String,
      comments: List[String],
      timeSpent: Option[Centis] = None
  )

  def full(csa: String): Validated[String, ParsedNotation] =
    try {
      val preprocessed = augmentString(cleanCsa(csa)).linesIterator
        .collect {
          case l if !l.trim.startsWith("'") => l.replace(",", "\n").trim
          case l                            => l.trim // try to keep ',' in comments
        }
        .filterNot(l => l.isEmpty || l == "'" || l.startsWith("V")) // remove empty comments and version
        .mkString("\n")
      for {
        splitted <- splitHeaderAndSteps(preprocessed)
        (headerStr, stepsStr) = splitted
        splitted3 <- splitMetaAndBoard(headerStr)
        (metaStr, boardStr) = splitted3
        preTags     <- TagParser(metaStr)
        parsedSteps <- StepsParser(stepsStr)
        (strSteps, terminationOption) = parsedSteps
        init      <- getComments(headerStr)
        situation <- CsaParserHelper.parseSituation(boardStr)
        variant     = situation.variant
        initialSfen = situation.toSfen.some.filterNot(_.truncate == variant.initialSfen.truncate)
        tags        = createTags(preTags, situation.color, strSteps.size, terminationOption)
        parsedSteps <- objSteps(strSteps)
      } yield ParsedNotation(parsedSteps, initialSfen, variant, init, tags)
    } catch {
      case _: StackOverflowError =>
        sys error "### StackOverflowError ### in CSA parser"
    }

  def objSteps(strSteps: List[StrStep]): Validated[String, ParsedSteps] = {
    strSteps.map { case StrStep(stepStr, comments, timeSpent) =>
      (
        StepParser(stepStr) map { s =>
          s withComments comments withTimeSpent timeSpent
        }
      ): Validated[String, ParsedStep]
    }.sequence map { ParsedSteps.apply(_) }
  }

  def createTags(
      tags: Tags,
      color: Color,
      nbSteps: Int,
      stepTermTag: Option[Tag]
  ): Tags = {
    val termTag = (tags(_.Termination) orElse stepTermTag.map(_.value)).map(t => Tag(_.Termination, t))
    val resultTag = CsaParserHelper
      .createResult(
        termTag,
        Color.fromSente((nbSteps + { if (color.gote) 1 else 0 }) % 2 == 0)
      )

    List[Option[Tag]](resultTag, termTag).flatten.foldLeft(tags)(_ + _)
  }

  trait Logging { self: Parsers =>
    protected val loggingEnabled = false
    protected def as[T](msg: String)(p: => Parser[T]): Parser[T] =
      if (loggingEnabled) log(p)(msg) else p
  }

  object StepsParser extends RegexParsers with Logging {

    override val whiteSpace = """(\s|\t|\r?\n)+""".r

    def apply(csaSteps: String): Validated[String, (List[StrStep], Option[Tag])] = {
      parseAll(strSteps, csaSteps) match {
        case Success((steps, termination), _) =>
          valid(
            (
              steps,
              termination map { r =>
                Tag(_.Termination, r)
              }
            )
          )
        case err => invalid("Cannot parse moves/drops: %s\n%s".format(err.toString, csaSteps))
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
          (moveOrDropRegex ~ opt(clock) ~ rep(commentary)) <~
          (stepExtras *) ^^ { case step ~ clk ~ comments =>
            StrStep(step, cleanComments(comments), clk.flatten)
          }
      }

    private val clockSecondsRegex = """(\d++)""".r

    private def readCentis(seconds: String): Option[Centis] =
      seconds.toDoubleOption match {
        case Some(s) => Centis(BigDecimal(s * 100).setScale(0, BigDecimal.RoundingMode.HALF_UP).toInt).some
        case _       => none
      }

    private def parseClock(str: String): Option[Centis] = {
      str match {
        case clockSecondsRegex(seconds) => readCentis(seconds)
        case _                          => None
      }
    }

    private def updateLastComments(steps: List[StrStep], comments: List[String]): List[StrStep] = {
      val index = steps.size - 1
      (steps lift index).fold(steps) { step =>
        steps.updated(index, step.copy(comments = step.comments ::: comments))
      }
    }

    def clock: Parser[Option[Centis]] =
      as("clock") {
        """T""".r ~>
          clockSecondsRegex ^^ { case spent =>
            parseClock(spent)
          }
      }

    def stepExtras: Parser[Unit] =
      as("stepExtras") {
        commentary.^^^(())
      }

    def commentary: Parser[String] =
      as("commentary") {
        """'""" ~> """.+""".r
      }

    def termination: Parser[String] =
      as("termination") {
        "%" ~> termValue ~ opt(clock) ^^ { case term ~ _ =>
          term
        }
      }

    val termValue: Parser[String] =
      "CHUDAN" | "TORYO" | "JISHOGI" | "SENNICHITE" | "TSUMI" | "TIME_UP" | "ILLEGAL_MOVE" | "+ILLEGAL_ACTION" | "-ILLEGAL_ACTION" | "KACHI" | "HIKIWAKE" | "FUZUMI" | "MATTA" | "ERROR"
  }

  object StepParser extends RegexParsers with Logging {

    val MoveRegex =
      raw"""^(?:${colorsS})?($positionS)($positionS)($piecesS)""".r
    val DropRegex = raw"""^(?:${colorsS})?(?:$dropOriginS)($positionS)($handPiecesS)""".r

    override def skipWhitespace = false

    def apply(str: String): Validated[String, ParsedStep] = {
      str match {
        case MoveRegex(origS, destS, roleS) => {
          for {
            role <- CsaUtils.toRole(roleS) toValid s"Uknown role in move: $str"
            _ <-
              if (Standard.allRoles contains role) valid(role)
              else invalid(s"$role not supported in standard shogi")
            dest <- CsaUtils.parseCsaPos(destS) toValid s"Cannot parse destination sqaure in move: $str"
            orig <- CsaUtils.parseCsaPos(origS) toValid s"Cannot parse origin sqaure in move: $str"
          } yield CsaMove(
            dest = dest,
            role = role,
            orig = orig,
            metas = Metas(
              comments = Nil,
              glyphs = Glyphs.empty,
              variations = Nil,
              timeSpent = None,
              timeTotal = None
            )
          )
        }
        case DropRegex(posS, roleS) =>
          for {
            roleBase <- CsaUtils.toRole(roleS) toValid s"Uknown role in drop: $str"
            role <- Standard.handRoles.find(
              _ == roleBase
            ) toValid s"$roleBase can't be dropped in standard shogi"
            pos <- CsaUtils.parseCsaPos(posS) toValid s"Cannot parse destination sqaure in drop: $str"
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

    def apply(csa: String): Validated[String, Tags] =
      parseAll(all, csa) match {
        case f: Failure       => invalid("Cannot parse CSA tags: %s\n%s".format(f.toString, csa))
        case Success(tags, _) => valid(Tags(tags.filter(_.value.nonEmpty)))
        case err              => invalid("Cannot parse CSA tags: %s\n%s".format(err.toString, csa))
      }

    def all: Parser[List[Tag]] =
      as("all") {
        rep(tags) <~ """(.|\n)*""".r
      }

    def tags: Parser[Tag] = tag | playerTag

    def tag: Parser[Tag] =
      "$" ~>
        """\w+""".r ~ ":" ~ """.*""".r ^^ { case name ~ _ ~ value =>
          Tag(normalizeCsaName(name), value)
        }

    def playerTag: Parser[Tag] =
      """N""" ~>
        """[\+|-].*""".r ^^ { case line =>
          Tag(normalizeCsaName(line.slice(0, 1)), line.drop(1))
        }
  }

  private def cleanCsa(csa: String): String =
    csa
      .replace("‑", "-")
      .replace("–", "-")
      .replace('　', ' ')
      .replace("：", ":")
      .replace(s"\ufeff", "")

  private def cleanComments(comments: List[String]) =
    comments.map(_.trim.take(2000)).filter(_.nonEmpty)

  private def normalizeCsaName(str: String): String =
    Tag.csaNameToTag.get(str).fold(str)(_.lowercase)

  private def getComments(csa: String): Validated[String, InitialPosition] =
    valid(
      InitialPosition(
        augmentString(csa).linesIterator.map(_.trim).filter(_.startsWith("'")).map(_.drop(1).trim).toList
      )
    )

  private def splitHeaderAndSteps(csa: String): Validated[String, (String, String)] =
    augmentString(csa).linesIterator.map(_.trim).filter(_.nonEmpty) span { line =>
      !moveOrDropRegex.matches(line)
    } match {
      case (headerLines, stepLines) => valid(headerLines.mkString("\n") -> stepLines.mkString("\n"))
    }

  private def splitMetaAndBoard(csa: String): Validated[String, (String, String)] =
    augmentString(csa).linesIterator
      .map(_.trim)
      .filter(l => l.nonEmpty && !l.startsWith("'")) partition { line =>
      !((line startsWith "P") || (line == "+") || (line == "-"))
    } match {
      case (metaLines, boardLines) => valid(metaLines.mkString("\n") -> boardLines.mkString("\n"))
    }
}
