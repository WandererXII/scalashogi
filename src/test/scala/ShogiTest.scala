package shogi

import cats.data.NonEmptyList
import cats.data.Validated
import cats.syntax.option._
import org.specs2.execute.Result
import org.specs2.matcher.Matcher
import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

import shogi.format.forsyth.Sfen
import shogi.format.forsyth.Visual
import shogi.format.usi.Usi
import shogi.variant._

trait ShogiTest extends Specification with ValidatedMatchers {

  implicit def stringToPosition(str: String): Position =
    Visual
      .parse(str, Standard)
      .orElse(Visual.parse(str, Chushogi))
      .orElse(Visual.parse(str, Minishogi))
      .orElse(Visual.parse(str, Annanshogi))
      .orElse(Visual.parse(str, Checkshogi))
      .orElse(Visual.parse(str, Kyotoshogi))
      .get

  implicit def colorChanger(str: String): AnyRef { def as(color: shogi.Color): shogi.Position } =
    new {
      def as(color: Color): Position = stringToPosition(str).copy(color = color)
    }

  case class RichActor(actor: MoveActor) {
    def threatens(to: Square): Boolean =
      actor.piece.eyes(actor.square, to) && {
        (actor.piece.projectionDirs.isEmpty) ||
        (actor.piece.directDirs.exists(_(actor.square).contains(to))) ||
        Square.findDirection(actor.square, to).exists { dir =>
          actor.variant.longRangeThreatens(actor.board, actor.square, dir, to)
        }
      }
  }

  implicit def richActor(actor: MoveActor): RichActor = RichActor(actor)

  case class RichPosition(position: Position) {

    def as(color: Color): Position = position.withColor(color)

    def playUsisStr(usisStr: Seq[String]): Validated[String, Position] = {
      val validatedPosition =
        usisStr.foldLeft[Validated[String, Position]](Validated.valid(position)) {
          case (validatedPosition, usiStr) =>
            validatedPosition.foreach { position =>
              val _ = position.moveActors.values.map { actor =>
                actor.destinations
              }
              val _ = position.dropActors.values.map { actor =>
                actor.destinations
              }
            }
            val nextPosition = validatedPosition flatMap { position =>
              position(Usi(usiStr).get)
            }
            nextPosition
        }
      validatedPosition
    }

    def playUsiStr(
        usiStr: String,
    ): Validated[String, Position] =
      position.apply(Usi(usiStr).get)

  }

  implicit def richPosition(position: Position): RichPosition = RichPosition(position)

  def sfenToPosition(sfen: Sfen, variant: Variant) =
    sfen.toPosition(variant) toValid "Could not construct position from SFEN"

  def makePosition(variant: Variant): Position =
    Position(variant)

  def makePositionWithBoard(variant: Variant, color: Color, pieces: (Square, Piece)*): Position =
    makePosition(variant).withColor(color).withBoard(Board(pieces))

  def makeEmptyPosition(variant: Variant): Position =
    Position(variant).withBoard(Board.empty)

  def completeReplay(usis: Seq[Usi]): NonEmptyList[Position] = {
    val (positions, error) = Replay.positionsWhileValid(usis, None, Standard)
    require(error.isEmpty, error.getOrElse(""))
    positions
  }

  def benchmark(nb: Int, iterations: Int)(run: => Unit): Result = {
    if (nb * iterations > 1) {
      println("warming up")
      run
    }
    println("running tests")
    val durations = for (_ <- 1 to iterations) yield {
      val start = System.nanoTime
      run
      val duration = System.nanoTime - start
      println(s"$nb games in ${duration / 1000000} ms")
      duration
    }
    val nanosPerGame = durations.sum.toDouble / (iterations * nb)
    println(f"Average = ${nanosPerGame / 1000}%.1f microseconds per game")
    println(f"          ${1e9 / nanosPerGame}%.0f games per second")
    success
  }

  def beSquares(squares: Square*): Matcher[Option[Iterable[Square]]] =
    beSome.like { case actual =>
      actual.toList must containTheSameElementsAs(squares.toList)
    }

  def beSquares(position: Position, visual: String): Matcher[Option[Iterable[Square]]] =
    beSome.like { case actual =>
      Visual.addNewLines(Visual.render(position, Map(actual -> 'x'))) must_== visual
    }

  def bePosition(visual: String, variant: Variant): Matcher[Validated[String, Position]] =
    beValid.like { case position =>
      position.visual must_== Visual.parse(visual, variant).get.visual
    }

  def pieceMoves(
      piece: Piece,
      square: Square,
      variant: Variant,
  ): Option[List[Square]] = {
    val position = makeEmptyPosition(variant)
    position
      .withColor(piece.color)
      .withBoard(position.board.place(piece, square).get)
      .moveActorAt(square) map (_.destinations)
  }
}
