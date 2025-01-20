package shogi
package variant

import shogi.Pos._
import shogi.format.forsyth.Sfen

case object Kyotoshogi
    extends Variant(
      id = 5,
      key = "kyotoshogi",
      name = "Kyoto shogi",
      title = "Pieces alternate between promoted and demoted state after each time they are moved",
    ) {

  val initialSfen = Sfen("pgkst/5/5/5/TSKGP b - 1")

  val numberOfRanks = 5
  val numberOfFiles = 5

  val allPositions = (SQ5E upTo SQ1A).toList

  val pieces =
    Map(
      SQ5E -> Sente.tokin,
      SQ4E -> Sente.silver,
      SQ3E -> Sente.king,
      SQ2E -> Sente.gold,
      SQ1E -> Sente.pawn,
      SQ1A -> Gote.tokin,
      SQ2A -> Gote.silver,
      SQ3A -> Gote.king,
      SQ4A -> Gote.gold,
      SQ5A -> Gote.pawn,
    )

  val allRoles = List(
    Rook,
    Pawn,
    Silver,
    Bishop,
    Gold,
    Knight,
    Lance,
    Tokin,
    King,
  )

  val handRoles = List(
    Tokin,
    Gold,
    Silver,
    Pawn,
  )

  override def dropRoles = List(
    Tokin,
    Silver,
    Gold,
    Pawn,
    Lance,
    Bishop,
    Knight,
    Rook,
  )

  override def dropFilter(a: DropActor): List[Pos] = a.situation.possibleDropDests

  def promote(role: Role): Option[Role] =
    role match {
      case Rook   => Some(Pawn)
      case Pawn   => Some(Rook)
      case Silver => Some(Bishop)
      case Bishop => Some(Silver)
      case Gold   => Some(Knight)
      case Knight => Some(Gold)
      case Tokin  => Some(Lance)
      case Lance  => Some(Tokin)
      case _      => None
    }

  def unpromote(role: Role): Option[Role] =
    promote(role)

  def backrank(color: Color) =
    if (color.sente) Rank.A else Rank.E

  // we promote everywhere
  def promotionRanks(color: Color) = List(Rank.A, Rank.B, Rank.C, Rank.D, Rank.E)

  def valueOfRole(r: Role): Int =
    r match {
      case King                  => 0
      case Lance | Pawn | Knight => 1
      case _                     => 3
    }

  override def forcePromote(piece: Piece, to: Pos): Boolean =
    promote(piece.role).isDefined

  override def canPromote(piece: Piece, orig: Pos, dest: Pos, capture: Boolean): Boolean =
    forcePromote(piece, dest)

  override def supportsDroppingEitherSide = true

  override def perpetualCheck(sit: Situation): Boolean = false

  override def repetition(sit: Situation): Boolean = sit.history.threefoldRepetition

  override def hasUnmovablePieces(board: Board) = false

  override def hasDoublePawns(board: Board, color: Color) = false

}
