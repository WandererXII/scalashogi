package shogi
package variant

import shogi.Square._
import shogi.format.forsyth.Sfen

case object Annanshogi
    extends Variant(
      id = 4,
      key = "annanshogi",
      name = "Annan shogi",
    ) {

  val initialSfen = Sfen("lnsgkgsnl/1r5b1/p1ppppp1p/1p5p1/9/1P5P1/P1PPPPP1P/1B5R1/LNSGKGSNL b - 1")

  val numberOfRanks = 9
  val numberOfFiles = 9

  val allSquares = (SQ9I upTo SQ1A).toList

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
      SQ7G -> Sente.pawn,
      SQ6G -> Sente.pawn,
      SQ5G -> Sente.pawn,
      SQ4G -> Sente.pawn,
      SQ3G -> Sente.pawn,
      SQ1G -> Sente.pawn,
      SQ8F -> Sente.pawn,
      SQ2F -> Sente.pawn,
      SQ8D -> Gote.pawn,
      SQ2D -> Gote.pawn,
      SQ9C -> Gote.pawn,
      SQ7C -> Gote.pawn,
      SQ6C -> Gote.pawn,
      SQ5C -> Gote.pawn,
      SQ4C -> Gote.pawn,
      SQ3C -> Gote.pawn,
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

  def allRoles = Standard.allRoles

  def handRoles = Standard.handRoles

  def promote(role: Role)   = Standard.promote(role)
  def unpromote(role: Role) = Standard.unpromote(role)

  def backrank(color: Color) = Standard.backrank(color)

  def promotionRanks(color: Color) = Standard.promotionRanks(color)

  def valueOfRole(r: Role): Int = Standard.valueOfRole(r)

  override def forcePromote(piece: Piece, to: Square): Boolean = false

  private def directlyBehind(square: Square, color: Color): Option[Square] =
    color.fold(square.down, square.up)

  // Ignores filter - any move can change how the pieces around it move
  override def squareThreatened(
      board: Board,
      color: Color,
      square: Square,
      filter: Piece => Boolean = _ => true,
  ): Boolean =
    board.pieces exists {
      case (from, piece) if piece.color == color =>
        pieceThreatens(board, from, attackingPiece(piece, from, board), square)
      case _ => false
    }

  override def attackingPiece(piece: Piece, square: Square, board: Board): Piece =
    directlyBehind(square, piece.color)
      .flatMap(board.apply)
      .filter(_.color == piece.color)
      .getOrElse(piece)

  override def dropFilterPawnCheckmate(a: DropActor, d: Square): Boolean =
    (a.piece is Pawn) && a.position.hands.take(a.color, Pawn).exists { hands =>
      // A drop can change both the pawn's movement and that of the piece in front of it.
      val after = a.position
        .withBoard(a.board.forcePlace(a.piece, d))
        .withHands(hands)
        .switch
      after.check && !after.hasDestinations
    }

  override def hasUnmovablePieces(board: Board) = false

  override def hasDoublePawns(board: Board, color: Color) = false

}
