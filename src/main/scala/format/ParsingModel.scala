package shogi
package format

import java.time.Duration

import cats.data.NonEmptyList
import cats.data.Validated
import cats.data.Validated.valid

import shogi.format.forsyth.Sfen
import shogi.format.usi.Usi

final case class ParsedNotation(
    parsedSteps: ParsedSteps,
    initialSfen: Option[Sfen],
    variant: shogi.variant.Variant,
    initialPosition: InitialPosition,
    tags: Tags,
)

final case class ParsedSteps(value: List[ParsedStep]) extends AnyVal

object ParsedSteps {
  val empty = ParsedSteps(Nil)
}

sealed trait ParsedStep {

  def toUsi(position: Position): Validated[String, Usi]

  def squares: List[Square]

  def metas: Metas

  def withMetas(m: Metas): ParsedStep

  def withSuffixes(s: Suffixes): ParsedStep = withMetas(metas withSuffixes s)

  def withComments(s: List[String]): ParsedStep = withMetas(metas withComments s)

  def withVariations(s: List[ParsedSteps]): ParsedStep = withMetas(metas withVariations s)

  def withTimeSpent(ts: Option[Duration]): ParsedStep = withMetas(metas withTimeSpent ts)

  def withTimeTotal(tt: Option[Duration]): ParsedStep = withMetas(metas withTimeTotal tt)

  def mergeGlyphs(glyphs: Glyphs): ParsedStep =
    withMetas(
      metas.withGlyphs(metas.glyphs merge glyphs),
    )

}

final case class KifMove(
    dest: Square,
    orig: Square,
    roles: NonEmptyList[Role], // in chushogi some kanji map to many roles
    midStep: Option[Square] = None,
    promotion: Boolean = false,
    metas: Metas = Metas.empty,
) extends ParsedStep {

  def toUsi(position: Position) = {
    val mandatoryFlip =
      position.variant.kyotoshogi && position
        .board(orig)
        .exists(position.variant.forcePromote(_, dest))
    valid(Usi.Move(orig, dest, promotion || mandatoryFlip, midStep))
  }

  def withMetas(m: Metas) = copy(metas = m)

  def squares = midStep.fold(List(orig, dest))(ms => List(orig, ms, dest))

}

final case class CsaMove(
    dest: Square,
    orig: Square,
    role: Role,
    metas: Metas = Metas.empty,
) extends ParsedStep {

  def toUsi(position: Position): Validated[String, Usi] =
    Validated.fromOption(position.board(orig), s"No piece at $orig") map { p =>
      Usi.Move(orig, dest, position.variant.promote(p.role).contains(role), None)
    }

  def withMetas(m: Metas) = copy(metas = m)

  def squares = List(orig, dest)

}

// All notations can share drop
final case class Drop(
    role: DroppableRole,
    square: Square,
    metas: Metas = Metas.empty,
) extends ParsedStep {

  def toUsi(position: Position) =
    valid(Usi.Drop(role, square))

  def withMetas(m: Metas) = copy(metas = m)

  def squares = List(square)
}

final case class InitialPosition(
    comments: List[String],
)

final case class Metas(
    comments: List[String],
    glyphs: Glyphs,
    variations: List[ParsedSteps],
    timeSpent: Option[Duration],
    timeTotal: Option[Duration],
) {

  def withSuffixes(s: Suffixes) =
    copy(
      glyphs = s.glyphs,
    )

  def withGlyphs(g: Glyphs) = copy(glyphs = g)

  def withComments(c: List[String]) = copy(comments = c)

  def withVariations(v: List[ParsedSteps]) = copy(variations = v)

  def withTimeSpent(ts: Option[Duration]) = copy(timeSpent = ts)

  def withTimeTotal(tt: Option[Duration]) = copy(timeTotal = tt)
}

object Metas {
  val empty = Metas(Nil, Glyphs.empty, Nil, None, None)
}

final case class Suffixes(
    promotion: Boolean,
    glyphs: Glyphs,
)
