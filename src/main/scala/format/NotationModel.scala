package shogi
package format

import shogi.format.usi.Usi
import shogi.format.forsyth.Sfen

trait Notation {

  def steps: List[NotationStep]

  def initialSfen: Option[Sfen]

  def variant: shogi.variant.Variant

  def tags: Tags

  def withSteps(steps: List[NotationStep]): Notation

  def withTags(tags: Tags): Notation

  def updatePly(ply: Int, f: NotationStep => NotationStep) = {
    val index = ply - 1
    (steps lift index).fold(this) { step =>
      withSteps(steps.updated(index, f(step)))
    }
  }

  def nbPlies = steps.size

  def updateLastPly(f: NotationStep => NotationStep) = updatePly(nbPlies, f)

  def withEvent(title: String) =
    withTags(tags + Tag(_.Event, title))

  def render: String

  override def toString = render
}

final case class Initial(comments: List[String] = Nil)

object Initial {
  val empty = Initial(Nil)
}

final case class NotationStep(
    stepNumber: Int,
    usiWithRole: Usi.WithRole,
    comments: List[String] = Nil,
    glyphs: Glyphs = Glyphs.empty,
    result: Option[String] = None,
    variations: List[List[NotationStep]] = Nil,
    // time left for the user who made the move/drop, after he made it
    secondsSpent: Option[Int] = None,
    // total time spent playing so far
    secondsTotal: Option[Int] = None
)
