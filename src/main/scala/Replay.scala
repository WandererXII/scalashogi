package shogi

import cats.data.NonEmptyList
import cats.data.Validated
import cats.data.Validated.Invalid
import cats.data.Validated.Valid
import cats.data.Validated.invalid
import cats.data.Validated.valid
import cats.implicits._

import shogi.format.Reader
import shogi.format.Tags
import shogi.format.forsyth.Sfen
import shogi.format.usi.Usi

final case class Replay(setup: Game, state: Game) {
  def apply(game: Game) = copy(state = game)
}

object Replay {

  def apply(game: Game) = new Replay(game, game)

  def apply(
      usis: Seq[Usi],
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant,
  ): Reader.Result =
    Reader.fromUsi(
      usis,
      initialSfen,
      variant,
      Tags.empty,
    )

  def replay(
      usis: Seq[Usi],
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant,
  ): Validated[String, Replay] =
    usis.foldLeft[Validated[String, Replay]](valid(Replay(Game(initialSfen, variant)))) {
      case (acc, usi) =>
        acc andThen { replay =>
          replay.state(usi) andThen { game =>
            valid(replay(game))
          }
        }
    }

  def gamesWhileValid(
      usis: Seq[Usi],
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant,
  ): (NonEmptyList[Game], Option[String]) = {

    @scala.annotation.tailrec
    def mk(games: NonEmptyList[Game], usis: List[Usi]): (NonEmptyList[Game], Option[String]) =
      usis match {
        case Nil => (games, None)
        case usi :: rest =>
          games.head(usi) match {
            case Valid(newGame) => mk(newGame :: games, rest)
            case Invalid(err)   => (games, err.some)
          }
      }

    mk(NonEmptyList.one(Game(initialSfen, variant)), usis.toList) match {
      case (games, err) => (games.reverse, err)
    }
  }

  def situations(
      usis: Seq[Usi],
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant,
  ): Validated[String, NonEmptyList[Situation]] = {
    val init = initialSfenToSituation(initialSfen, variant)
    usis.foldLeft[Validated[String, NonEmptyList[Situation]]](valid(NonEmptyList.one(init))) {
      case (acc, usi) =>
        acc andThen { sits =>
          sits.head(usi) andThen { sit =>
            valid(sit :: sits)
          }
        }
    } map (_.reverse)
  }

  def plyAtSfen(
      usis: Seq[Usi],
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant,
      atSfen: Sfen,
  ): Validated[String, Int] =
    if (atSfen.toSituation(variant).isEmpty) invalid(s"Invalid Sfen $atSfen")
    else {
      @scala.annotation.tailrec
      def recursivePlyAtSfen(sit: Situation, usis: List[Usi], ply: Int): Validated[String, Int] =
        usis match {
          case Nil => invalid(s"Can't find $atSfen, reached ply $ply")
          case usi :: rest =>
            sit(usi) match {
              case Valid(sitAfter) =>
                if (sitAfter.toSfen.truncate == atSfen.truncate) valid(ply)
                else recursivePlyAtSfen(sitAfter, rest, ply + 1)
              case Invalid(err) => invalid(s"Failed plyAtSfen with: $err")
            }
        }

      val sit = initialSfenToSituation(initialSfen, variant)
      recursivePlyAtSfen(sit, usis.toList, initialSfen.flatMap(_.stepNumber) | 1)
    }

  // Use for trusted usis
  // doesn't verify whether the usis are valid
  def usiWithRoleWhilePossible(
      usis: Seq[Usi],
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant,
  ): List[Usi.WithRole] = {

    @scala.annotation.tailrec
    def mk(
        roleMap: Map[Pos, Role],
        usis: List[Usi],
        roles: List[Usi.WithRole],
    ): List[Usi.WithRole] =
      usis match {
        case Nil => roles
        case usi :: rest =>
          usi match {
            case Usi.Move(orig, dest, prom, midStep) =>
              roleMap.get(orig) match {
                case Some(role) => {
                  val maybePromoted = variant.promote(role).filter(_ => prom) | role
                  val toRemove      = List[Option[Pos]](Some(orig), midStep).flatten
                  mk(
                    roleMap -- toRemove + (dest -> maybePromoted),
                    rest,
                    Usi.WithRole(usi, role) :: roles,
                  )
                }
                case None => roles
              }
            case Usi.Drop(role, pos) =>
              mk(roleMap + (pos -> role), rest, Usi.WithRole(usi, role) :: roles)
          }
      }

    val init = initialSfenToSituation(initialSfen, variant)
    mk(init.board.pieces.map { case (k, v) => k -> v.role }, usis.toList, Nil).reverse
  }

  private def initialSfenToSituation(
      initialSfen: Option[Sfen],
      variant: shogi.variant.Variant,
  ): Situation =
    initialSfen.flatMap(_.toSituation(variant)) | Situation(variant)

}
