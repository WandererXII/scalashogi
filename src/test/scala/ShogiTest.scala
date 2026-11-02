package shogi

import cats.data.Validated
import cats.syntax.option._
import org.specs2.matcher.Matcher
import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

import shogi.format.forsyth.Sfen
import shogi.format.forsyth.Visual
import shogi.format.usi.Usi
import shogi.variant._

trait ShogiTest extends Specification with ValidatedMatchers {

  implicit def stringToSituation(str: String): Situation =
    Visual
      .parse(str, Standard)
      .orElse(Visual.parse(str, Chushogi))
      .orElse(Visual.parse(str, Minishogi))
      .orElse(Visual.parse(str, Annanshogi))
      .orElse(Visual.parse(str, Checkshogi))
      .orElse(Visual.parse(str, Kyotoshogi))
      .get

  implicit def colorChanger(str: String): AnyRef { def as(color: shogi.Color): shogi.Situation } =
    new {
      def as(color: Color): Situation = stringToSituation(str).copy(color = color)
    }

  case class RichActor(actor: MoveActor) {
    def threatens(to: Pos): Boolean =
      actor.piece.eyes(actor.pos, to) && {
        (actor.piece.projectionDirs.isEmpty) ||
        (actor.piece.directDirs.exists(_(actor.pos).contains(to))) ||
        Pos.findDirection(actor.pos, to).exists { dir =>
          actor.variant.longRangeThreatens(actor.board, actor.pos, dir, to)
        }
      }
  }

  implicit def richActor(actor: MoveActor): RichActor = RichActor(actor)

  case class RichState(state: Situation) {

    def as(color: Color): Situation = state.withColor(color)

    def playUsisStr(usisStr: Seq[String]): Validated[String, Situation] = {
      val validatedState = usisStr.foldLeft[Validated[String, Situation]](Validated.valid(state)) {
        case (validatedState, usiStr) =>
          validatedState.foreach { state =>
            val _ = state.moveActors.values.map { actor =>
              actor.destinations
            }
            val _ = state.dropActors.values.map { actor =>
              actor.destinations
            }
          }
          val nextState = validatedState flatMap { state =>
            state(Usi(usiStr).get)
          }
          nextState
      }
      validatedState
    }

    def playUsiStr(
        usiStr: String,
    ): Validated[String, Situation] =
      state.apply(Usi(usiStr).get)

  }

  implicit def richState(state: Situation): RichState = RichState(state)

  def sfenToState(sfen: Sfen, variant: Variant) =
    sfen.toSituation(variant) toValid "Could not construct situation from SFEN"

  def makeSituation(variant: Variant): Situation =
    Situation(variant)

  def makeSituationWithBoard(variant: Variant, color: Color, pieces: (Pos, Piece)*): Situation =
    makeSituation(variant).withColor(color).withBoard(Board(pieces))

  def makeEmptySituation(variant: Variant): Situation =
    Situation(variant).withBoard(Board.empty)

  def bePoss(poss: Pos*): Matcher[Option[Iterable[Pos]]] =
    beSome.like { case p =>
      p.toList must containTheSameElementsAs(poss.toList)
    }

  def makeState(variant: Variant): Situation =
    makeSituation(variant)

  def bePoss(situation: Situation, visual: String): Matcher[Option[Iterable[Pos]]] =
    beSome.like { case p =>
      Visual.addNewLines(Visual.render(situation, Map(p -> 'x'))) must_== visual
    }

  def beState(visual: String, variant: Variant): Matcher[Validated[String, Situation]] =
    beValid.like { case state =>
      state.visual must_== Visual.parse(visual, variant).get.visual
    }

  def pieceMoves(
      piece: Piece,
      pos: Pos,
      variant: Variant,
  ): Option[List[Pos]] = {
    val sit = makeEmptySituation(variant)
    sit
      .withColor(piece.color)
      .withBoard(sit.board.place(piece, pos).get)
      .moveActorAt(pos) map (_.destinations)
  }
}
