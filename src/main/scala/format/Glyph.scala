package shogi
package format

case class Glyph(id: Int, symbol: String, name: String) {

  override def toString = s"$symbol ($$$id $name)"
}

final case class Glyphs(
    moveOrDrop: Option[Glyph.MoveOrDropAssessment],
    position: Option[Glyph.PositionAssessment],
    observations: List[Glyph.Observation],
) {

  def isEmpty = this == Glyphs.empty

  def nonEmpty: Option[Glyphs] = if (isEmpty) None else Some(this)

  def toggle(glyph: Glyph) =
    glyph match {
      case g: Glyph.MoveOrDropAssessment => copy(moveOrDrop = !moveOrDrop.contains(g) option g)
      case g: Glyph.PositionAssessment   => copy(position = !position.contains(g) option g)
      case g: Glyph.Observation =>
        copy(observations =
          if (observations contains g) observations.filter(g !=)
          else g :: observations,
        )
      case _ => this
    }

  def merge(g: Glyphs) =
    if (isEmpty) g
    else if (g.isEmpty) this
    else
      Glyphs(
        g.moveOrDrop orElse moveOrDrop,
        g.position orElse position,
        (g.observations ::: observations).distinct,
      )

  def toList: List[Glyph] = moveOrDrop.toList ::: position.toList ::: observations
}

object Glyphs {
  val empty = Glyphs(None, None, Nil)

  def fromList(glyphs: List[Glyph]) =
    Glyphs(
      moveOrDrop = glyphs.collectFirst { case g: Glyph.MoveOrDropAssessment => g },
      position = glyphs.collectFirst { case g: Glyph.PositionAssessment => g },
      observations = glyphs.collect { case g: Glyph.Observation => g },
    )
}

object Glyph {

  sealed trait MoveOrDropAssessment extends Glyph

  object MoveOrDropAssessment {
    val good        = new Glyph(1, "!", "Good move/drop") with MoveOrDropAssessment
    val mistake     = new Glyph(2, "?", "Mistake") with MoveOrDropAssessment
    val brillant    = new Glyph(3, "!!", "Brillant move/drop") with MoveOrDropAssessment
    val blunder     = new Glyph(4, "??", "Blunder") with MoveOrDropAssessment
    val interesting = new Glyph(5, "!?", "Interesting move/drop") with MoveOrDropAssessment
    val dubious     = new Glyph(6, "?!", "Dubious move/drop") with MoveOrDropAssessment
    val only        = new Glyph(7, "□", "Only move/drop") with MoveOrDropAssessment
    val zugzwang    = new Glyph(22, "⨀", "Zugzwang") with MoveOrDropAssessment

    val all = List[Glyph](good, mistake, brillant, blunder, interesting, dubious, only, zugzwang)
    val byId: Map[Int, Glyph] = all.map { g =>
      g.id -> g
    }.toMap

    def display = all
  }

  sealed trait PositionAssessment extends Glyph

  object PositionAssessment {
    val equal               = new Glyph(10, "=", "Equal position") with PositionAssessment
    val unclear             = new Glyph(13, "∞", "Unclear position") with PositionAssessment
    val senteSlightlyBetter = new Glyph(14, "⩲", "Sente is slightly better") with PositionAssessment
    val goteSlightlyBetter  = new Glyph(15, "⩱", "Gote is slightly better") with PositionAssessment
    val senteQuiteBetter    = new Glyph(16, "±", "Sente is better") with PositionAssessment
    val goteQuiteBetter     = new Glyph(17, "∓", "Gote is better") with PositionAssessment
    val senteMuchBetter     = new Glyph(18, "+−", "Sente is winning") with PositionAssessment
    val goteMuchBetter      = new Glyph(19, "-+", "Gote is winning") with PositionAssessment

    val all = List[Glyph](
      equal,
      unclear,
      senteSlightlyBetter,
      goteSlightlyBetter,
      senteQuiteBetter,
      goteQuiteBetter,
      senteMuchBetter,
      goteMuchBetter,
    )
    val byId: Map[Int, Glyph] = all.map { g =>
      g.id -> g
    }.toMap

    def display = all
  }

  sealed trait Observation extends Glyph

  object Observation {
    val novelty      = new Glyph(146, "N", "Novelty") with Observation
    val development  = new Glyph(32, "↑↑", "Development") with Observation
    val initiative   = new Glyph(36, "↑", "Initiative") with Observation
    val attack       = new Glyph(40, "→", "Attack") with Observation
    val counterplay  = new Glyph(132, "⇆", "Counterplay") with Observation
    val timeTrouble  = new Glyph(138, "⊕", "Time trouble") with Observation
    val compensation = new Glyph(44, "=∞", "With compensation") with Observation
    val withIdea     = new Glyph(140, "∆", "With the idea") with Observation

    val all =
      List[Glyph](
        novelty,
        development,
        initiative,
        attack,
        counterplay,
        timeTrouble,
        compensation,
        withIdea,
      )
    val byId: Map[Int, Glyph] = all.map { g =>
      g.id -> g
    }.toMap

    def display = all
  }

  def find(id: Int): Option[Glyph] =
    MoveOrDropAssessment.byId.get(id) orElse
      PositionAssessment.byId.get(id) orElse
      Observation.byId.get(id)
}
