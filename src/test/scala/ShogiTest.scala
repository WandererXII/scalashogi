package shogi

import cats.data.Validated
import cats.syntax.option._
import org.specs2.matcher.Matcher
import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

//import scala.annotation.nowarn

import format.forsyth.{ Sfen, Visual }
import format.usi.Usi
import variant._

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

    def playMoves(moves: (Pos, Pos, Boolean)*): Validated[String, Game] = playMoveList(moves)

    def playMoveList(moves: Seq[(Pos, Pos, Boolean)]): Validated[String, Game] = {
      val vg = moves.foldLeft[Validated[String, Game]](Validated.valid(game)) {
        case (vg, (orig, dest, prom)) =>
          vg.foreach { g =>
            val _ = g.situation.moveDestinations
          }
          val ng = vg flatMap { g =>
            g(Usi.Move(orig, dest, prom, None))
          }
          ng
      }
      vg
    }

    // todo - get rid of `playMoveList` and `playMoves`
    def playUsiMoveList(moves: Seq[Usi]): Validated[String, Game] = {
      val vg = moves.foldLeft[Validated[String, Game]](Validated.valid(game)) { case (vg, usi) =>
        vg.foreach { g =>
          val _ = g.situation.moveDestinations
          val _ = g.situation.dropDestinations
        }
        val ng = vg flatMap { g =>
          g(usi)
        }
        ng
      }
      vg
    }

    def playMove(
        orig: Pos,
        dest: Pos,
        promotion: Boolean = false,
        midStep: Option[Pos] = None
    ): Validated[String, Game] =
      game.apply(Usi.Move(orig, dest, promotion, midStep))

    def playDrop(
        role: DroppableRole,
        dest: Pos
    ): Validated[String, Game] =
      game.apply(Usi.Drop(role, dest))

    def withClock(c: Clock) = game.copy(clock = Some(c))
  }

  implicit def richGame(game: Game): RichGame = RichGame(game)

  def sfenToGame(sfen: Sfen, variant: Variant) =
    sfen.toSituation(variant) toValid "Could not construct situation from SFEN" map { sit =>
      Game(variant).copy(
        situation = sit
      )
    }

  def makeSituation(variant: Variant): Situation =
    Situation(variant)

  def makeSituationWithBoard(variant: Variant, pieces: (Pos, Piece)*): Situation =
    makeSituation(variant).withBoard(Board(pieces))

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
      variant: Variant
  ): Option[List[Pos]] = {
    val sit = makeEmptySituation(variant)
    sit.withBoard(sit.board.place(piece, pos).get).moveActorAt(pos) map (_.destinations)
  }
}
