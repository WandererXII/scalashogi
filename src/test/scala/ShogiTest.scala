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
          actor.situation.variant.longRangeThreatens(actor.situation.board, actor.pos, dir, to)
        }
      }
  }

  implicit def richActor(actor: MoveActor): RichActor = RichActor(actor)

  case class RichGame(game: Game) {

    def as(color: Color): Game = game.withColor(color)

    def playUsisStr(usisStr: Seq[String]): Validated[String, Game] = {
      val vg = usisStr.foldLeft[Validated[String, Game]](Validated.valid(game)) {
        case (vg, usiStr) =>
          vg.foreach { g =>
            val _ = g.situation.moveActors.values.map { actor =>
              actor.destinations
            }
            val _ = g.situation.dropActors.values.map { actor =>
              actor.destinations
            }
          }
          val ng = vg flatMap { g =>
            g(Usi(usiStr).get)
          }
          ng
      }
      vg
    }

    def playUsiStr(
        usiStr: String,
    ): Validated[String, Game] =
      game.apply(Usi(usiStr).get)

    def withClock(c: Clock) = game.copy(clock = Some(c))
  }

  implicit def richGame(game: Game): RichGame = RichGame(game)

  def sfenToGame(sfen: Sfen, variant: Variant) =
    sfen.toSituation(variant) toValid "Could not construct situation from SFEN" map { sit =>
      Game(variant).copy(
        situation = sit,
      )
    }

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

  def makeGame(variant: Variant): Game =
    Game(makeSituation(variant))

  def bePoss(situation: Situation, visual: String): Matcher[Option[Iterable[Pos]]] =
    beSome.like { case p =>
      Visual.addNewLines(Visual.render(situation, Map(p -> 'x'))) must_== visual
    }

  def beGame(visual: String, variant: Variant): Matcher[Validated[String, Game]] =
    beValid.like { case g =>
      g.situation.visual must_== Visual.parse(visual, variant).get.visual
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
