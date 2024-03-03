package shogi

import cats.data.Validated
import cats.implicits._

import shogi.format.forsyth.Sfen
import shogi.format.ParsedMove
import shogi.format.usi.Usi
import shogi.variant.Variant

final case class Situation(
    board: Board,
    hands: Hands,
    color: Color,
    history: History,
    variant: Variant
) {

  def apply(usi: Usi): Validated[String, Situation] =
    usi match {
      case u: Usi.Move => variant.move(this, u)
      case u: Usi.Drop => variant.drop(this, u)
    }

  def apply(parsedMove: ParsedMove): Validated[String, Situation] =
    parsedMove.toUsi(this) andThen (apply _)

  // Moves

  lazy val moveActors: Map[Pos, MoveActor] = board.pieces map { case (pos, piece) =>
    (pos, MoveActor(piece, pos, this))
  }

  lazy val moveActorsOf: Color.Map[List[MoveActor]] = {
    val (s, g) = moveActors.values.toList.partition { _.color.sente }
    Color.Map(s, g)
  }

  def moveActorAt(at: Pos): Option[MoveActor] = moveActors get at

  lazy val moveDestinations: Map[Pos, List[Pos]] =
    moveActorsOf(color).collect {
      case actor if actor.destinations.nonEmpty => actor.pos -> actor.destinations
    }.toMap

  lazy val hasMoveDestinations: Boolean =
    moveActorsOf(color)
      .exists(_.destinations.nonEmpty)

  def moveDestsFrom(from: Pos): Option[List[Pos]] = moveActorAt(from) map (_.destinations)

  // Drops

  private def addOtherSide(roles: List[DroppableRole]): List[DroppableRole] =
    roles.flatMap { role =>
      variant
        .promote(role) match {
        case Some(dRole: DroppableRole) => List(role, dRole)
        case _                          => List(role)
      }
    }

  lazy val dropActors: Map[Piece, DropActor] = (if (variant.supportsDroppingEitherSide)
                                                  hands.rolesOf.map(addOtherSide)
                                                else hands.rolesOf)
    .reduce[List[(Piece, DropActor)]] { (sente, gote) =>
      sente.map(role => (Piece(Sente, role), DropActor(Sente, role, this))) :::
        gote.map(role => (Piece(Gote, role), DropActor(Gote, role, this)))
    }
    .toMap

  lazy val dropActorsOf: Color.Map[List[DropActor]] = {
    val (s, g) = dropActors.values.toList.partition { _.color.sente }
    Color.Map(s, g)
  }

  def dropActorOf(piece: Piece): Option[DropActor] = dropActors get piece

  lazy val dropDestinations: Map[Piece, List[Pos]] =
    dropActorsOf(color).collect {
      case actor if actor.destinations.nonEmpty => actor.piece -> actor.destinations
    }.toMap

  lazy val hasDropDestinations: Boolean =
    dropActorsOf(color)
      .exists(_.destinations.nonEmpty)

  def dropDestsOf(piece: Piece): Option[List[Pos]] = dropActorOf(piece) map (_.destinations)

  // King safety

  def check: Boolean = checkOf(color)

  private def checkOf(c: Color): Boolean = c.fold(checkSente, checkGote)

  lazy val checkSente = variant.check(board, Sente)
  lazy val checkGote  = variant.check(board, Gote)

  def checkSquares: List[Pos] = checkSquaresOf(color)

  private def checkSquaresOf(c: Color): List[Pos] = c.fold(checkSquaresSente, checkSquaresGote)

  lazy val checkSquaresSente = variant.checkSquares(board, Sente)
  lazy val checkSquaresGote  = variant.checkSquares(board, Gote)

  // Not taking into account specific drop rules
  lazy val possibleDropDests: List[Pos] =
    Some(checkSquares)
      .filter(_.size == 1)
      .fold(
        variant.allPositions.filterNot(board.pieces contains _)
      ) { royals =>
        DropActor.blockades(this, royals.head)
      }

  // Results

  def checkmate: Boolean = variant checkmate this

  def stalemate: Boolean = variant stalemate this

  def perpetualCheck: Boolean = variant perpetualCheck this

  def repetition: Boolean = variant repetition this

  def royalsLost: Boolean = variant royalsLost this

  def bareKing(color: Color): Boolean = variant.bareKing(this, color)

  def draw: Boolean = variant draw this

  def specialVariantEnd = variant specialVariantEnd this

  def impasse = variant impasse this

  def end(withImpasse: Boolean): Boolean =
    specialVariantEnd || checkmate || stalemate || perpetualCheck || repetition || draw || (withImpasse && impasse)

  def winner: Option[Color] = variant.winner(this)

  def materialImbalance: Int = variant.materialImbalance(this)

  def valid(strict: Boolean) = variant.valid(this, strict)

  def playable(strict: Boolean, withImpasse: Boolean): Boolean =
    valid(strict) && !end(withImpasse) && !copy(color = !color).check

  lazy val status: Option[Status] =
    if (specialVariantEnd) Status.SpecialVariantEnd.some
    else if (checkmate) Status.Mate.some
    else if (royalsLost) Status.RoyalsLost.some
    else if (bareKing(Sente) || bareKing(Gote)) Status.BareKing.some
    else if (stalemate) Status.Stalemate.some
    else if (impasse) Status.Impasse27.some
    else if (perpetualCheck) Status.PerpetualCheck.some
    else if (repetition) Status.Repetition.some
    else if (draw) Status.Draw.some
    else none

  // Util

  def withBoard(board: Board)                     = copy(board = board)
  def withHands(hands: Hands)                     = copy(hands = hands)
  def withHistory(history: History)               = copy(history = history)
  def withVariant(variant: shogi.variant.Variant) = copy(variant = variant)

  def switch = copy(color = !color)

  def visual: String = format.forsyth.Visual render this

  def toSfen: Sfen = Sfen(this)

  override def toString = s"${variant.name}\n$visual\nLast Move: ${history.lastUsi.fold("-")(_.usi)}\n"
}

object Situation {

  def apply(variant: shogi.variant.Variant): Situation =
    Situation(
      Board(variant),
      Hands(variant),
      Sente,
      History.empty,
      variant
    )

  def apply(board: Board, hands: Hands, color: Color, variant: Variant): Situation =
    Situation(board, hands, color, History.empty, variant)
}
