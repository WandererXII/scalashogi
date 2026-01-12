package shogi
package variant

import shogi.Pos._
import shogi.format.forsyth.Sfen

case object Annanshogi
    extends Variant(
      id = 4,
      key = "annanshogi",
      name = "Annan shogi",
      title = "Pieces move like the friendly piece behind them",
    ) {

  val initialSfen = Sfen("lnsgkgsnl/1r5b1/p1ppppp1p/1p5p1/9/1P5P1/P1PPPPP1P/1B5R1/LNSGKGSNL b - 1")

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

  override def forcePromote(piece: Piece, to: Pos): Boolean = false

  private def directlyBehind(pos: Pos, color: Color): Option[Pos] =
    color.fold(pos.down, pos.up)

  private def toAnnanAttackPieceMap(pieces: PieceMap): PieceMap =
    pieces.foldLeft(Map.empty[Pos, Piece]) { case (acc, (pos, piece)) =>
      acc.updated(
        pos,
        directlyBehind(pos, piece.color)
          .flatMap(pieces.get)
          .filter(_.color == piece.color)
          .getOrElse(piece),
      )
    }

  override def posThreatened(
      board: Board,
      color: Color,
      pos: Pos,
      filter: Piece => Boolean = _ => true,
  ): Boolean = {
    val updatedBoard = Board(toAnnanAttackPieceMap(board.pieces))
    super.posThreatened(updatedBoard, color, pos, _ => true)
  }

  override def attackingPiece(piece: Piece, pos: Pos, board: Board): Piece =
    directlyBehind(pos, piece.color)
      .flatMap(board.apply)
      .filter(_.color == piece.color)
      .getOrElse(piece)

  override def hasUnmovablePieces(board: Board) = false

  override def hasDoublePawns(board: Board, color: Color) = false

  def status(sit: Situation): Option[Status] = Standard.status(sit)

  def winner(sit: Situation): Option[Color] = Standard.winner(sit)

}
