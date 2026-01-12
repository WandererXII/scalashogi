package shogi
package variant

import cats.syntax.option._

import shogi.Pos._
import shogi.format.forsyth.Sfen

case object Standard
    extends Variant(
      id = 1,
      key = "standard",
      name = "Standard",
    ) {

  val initialSfen = Sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1")

  val numberOfRanks = 9
  val numberOfFiles = 9

  val allPositions = (SQ9I upTo SQ1A).toList

  val pieces =
    Map(
      SQ9I -> Sente.lance,
      SQ8I -> Sente.knight,
      SQ7I -> Sente.silver,
      SQ6I -> Sente.gold,
      SQ5I -> Sente.king,
      SQ4I -> Sente.gold,
      SQ3I -> Sente.silver,
      SQ2I -> Sente.knight,
      SQ1I -> Sente.lance,
      SQ8H -> Sente.bishop,
      SQ2H -> Sente.rook,
      SQ9G -> Sente.pawn,
      SQ8G -> Sente.pawn,
      SQ7G -> Sente.pawn,
      SQ6G -> Sente.pawn,
      SQ5G -> Sente.pawn,
      SQ4G -> Sente.pawn,
      SQ3G -> Sente.pawn,
      SQ2G -> Sente.pawn,
      SQ1G -> Sente.pawn,
      SQ9C -> Gote.pawn,
      SQ8C -> Gote.pawn,
      SQ7C -> Gote.pawn,
      SQ6C -> Gote.pawn,
      SQ5C -> Gote.pawn,
      SQ4C -> Gote.pawn,
      SQ3C -> Gote.pawn,
      SQ2C -> Gote.pawn,
      SQ1C -> Gote.pawn,
      SQ8B -> Gote.rook,
      SQ2B -> Gote.bishop,
      SQ9A -> Gote.lance,
      SQ8A -> Gote.knight,
      SQ7A -> Gote.silver,
      SQ6A -> Gote.gold,
      SQ5A -> Gote.king,
      SQ4A -> Gote.gold,
      SQ3A -> Gote.silver,
      SQ2A -> Gote.knight,
      SQ1A -> Gote.lance,
    )

  val allRoles = List(
    Pawn,
    Lance,
    Knight,
    Silver,
    Gold,
    King,
    Bishop,
    Rook,
    PromotedLance,
    PromotedKnight,
    PromotedSilver,
    Dragon,
    Horse,
    Tokin,
  )

  val handRoles = List(
    Rook,
    Bishop,
    Gold,
    Silver,
    Knight,
    Lance,
    Pawn,
  )

  def promote(role: Role): Option[Role] =
    role match {
      case Pawn   => Some(Tokin)
      case Lance  => Some(PromotedLance)
      case Knight => Some(PromotedKnight)
      case Silver => Some(PromotedSilver)
      case Bishop => Some(Horse)
      case Rook   => Some(Dragon)
      case _      => None
    }

  def unpromote(role: Role): Option[Role] =
    role match {
      case Tokin          => Some(Pawn)
      case PromotedLance  => Some(Lance)
      case PromotedSilver => Some(Silver)
      case PromotedKnight => Some(Knight)
      case Horse          => Some(Bishop)
      case Dragon         => Some(Rook)
      case _              => None
    }

  def backrank(color: Color) =
    if (color.sente) Rank.A else Rank.I

  def promotionRanks(color: Color) =
    if (color.sente) List(Rank.A, Rank.B, Rank.C) else List(Rank.G, Rank.H, Rank.I)

  def valueOfRole(r: Role): Int =
    r match {
      case Pawn                                                           => 1
      case Lance                                                          => 3
      case Knight                                                         => 4
      case Silver                                                         => 5
      case Gold | Tokin | PromotedSilver | PromotedLance | PromotedKnight => 6
      case Bishop                                                         => 8
      case Rook                                                           => 10
      case Horse                                                          => 10
      case Dragon                                                         => 12
      case _                                                              => 0
    }

  private def impasseValueOf(r: Role): Int =
    r match {
      case Bishop | Rook | Horse | Dragon => 5
      case King                           => 0
      case _                              => 1
    }

  // In handicaps we give the value of the missing pieces to the handicap giver
  // Since this rule applies only to handicap games, only gote/uwate can be affected
  private def missingImpassePoints(sit: Situation): Int =
    sit.history.initialSfen
      .filter(Handicap.isHandicap(_, sit.variant))
      .flatMap(_.toSituation(sit.variant))
      .fold(0) { initSit =>
        math.max(
          0,
          54 -
            (initSit.board.pieces.values.map(p => impasseValueOf(p.role)).sum +
              initSit.hands(Sente).sum(impasseValueOf) + initSit.hands(Gote).sum(impasseValueOf)),
        )
      }

  protected def impasse(sit: Situation): Boolean = !sit.check && {
    val color        = sit.color
    val ranks        = sit.variant.promotionRanks(color)
    val enteredRoles = sit.board.pieces.collect {
      case (pos, piece) if (piece is color) && (ranks contains pos.rank) => piece.role
    }.toList
    def impassePoints: Int =
      enteredRoles.map(impasseValueOf).sum + sit
        .hands(color)
        .sum(impasseValueOf)

    // more than 10 - including the king
    enteredRoles.sizeIs > 10 && enteredRoles
      .contains(King) && impassePoints >= color.fold(28, 27 - missingImpassePoints(sit))
  }

  def status(sit: Situation): Option[Status] =
    if (!sit.hasDestinations) {
      if (sit.check) Status.Mate.some
      else Status.Stalemate.some
    } else if (impasse(sit)) Status.Impasse27.some
    else if (sit.history.fourfoldRepetition) {
      if (sit.history.perpetualCheckAttacker.isDefined) Status.PerpetualCheck.some
      else Status.Repetition.some
    } else if (isInsufficientMaterial(sit)) Status.Draw.some
    else none

  def winner(sit: Situation): Option[Color] =
    sit.status flatMap { status =>
      status match {
        case Status.Mate | Status.Stalemate => (!sit.color).some
        case Status.Impasse27               => (sit.color).some
        case Status.PerpetualCheck          => sit.history.perpetualCheckAttacker.map(!_)
        case _                              => none
      }
    }

}
