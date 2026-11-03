package shogi

import scala.collection.mutable.ArrayBuffer

import shogi.format.usi.Usi

final case class MoveActor(
    piece: Piece,
    square: Square,
    position: Position,
) {

  def board   = position.board
  def color   = position.color
  def history = position.history
  def variant = position.variant

  def check = position.check

  lazy val attackingPiece = variant.attackingPiece(piece, square, board)

  lazy val destinations: List[Square] = variant.moveFilter(this)

  // Destinations where the piece can end up after the entire move
  // without move filter - e.g. not taking defending the king into account
  def unfilteredDestinations: List[Square] =
    shortUnfilteredDestinations ::: longRange(attackingPiece.projectionDirs)

  lazy val shortUnfilteredDestinations = shortRange(attackingPiece.directDirs)

  lazy val lionMoveDestinationsMap: Map[Square, List[Square]] =
    if (piece.role.hasLionPower)
      shortUnfilteredDestinations
        .withFilter(ms => ms.dist(square) == 1 && destinations.contains(ms))
        .map { ms =>
          (ms, square :: shogi.variant.Chushogi.lionMoveFilter(this, ms))
        }
        .toMap
    else Map.empty

  def toUsis: List[Usi.Move] = {
    val normalMoves = destinations
      .withFilter(!variant.forcePromote(piece, _))
      .map(Usi.Move(square, _, false, None))
    val promotedMoves = destinations
      .withFilter(d => variant.canPromote(piece, square, d, board(d).isDefined))
      .map(Usi.Move(square, _, true, None))
    val lionMoves = lionMoveDestinationsMap.flatMap { case (ms, dests) =>
      dests.map(d => Usi.Move(square, d, false, Some(ms)))
    }.toList

    normalMoves ::: promotedMoves ::: lionMoves
  }

  private def shortRange(dirs: Directions): List[Square] =
    dirs flatMap { _(square) } filter { to =>
      variant
        .isInsideBoard(to) && board.pieces.get(to).fold(true)(_.color != color)
    }

  private def longRange(dirs: Directions): List[Square] = {
    val buf = new ArrayBuffer[Square]
    @scala.annotation.tailrec
    def addAll(from: Square, dir: Direction): Unit = {
      dir(from) match {
        case Some(to) if variant.isInsideBoard(to) =>
          board.pieces.get(to) match {
            case None => {
              buf += to
              addAll(to, dir)
            }
            case Some(other) =>
              if (other.color != color)
                buf += to
          }
        case _ => ()
      }
    }

    dirs foreach { addAll(square, _) }
    buf.toList
  }
}
