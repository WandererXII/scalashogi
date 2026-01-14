package shogi

import scala.collection.mutable.ArrayBuffer

import shogi.format.usi.Usi

final case class MoveActor(
    piece: Piece,
    pos: Pos,
    situation: Situation,
) {

  def board   = situation.board
  def color   = situation.color
  def history = situation.history
  def variant = situation.variant

  def check = situation.check

  lazy val attackingPiece = variant.attackingPiece(piece, pos, board)

  lazy val destinations: List[Pos] = variant.moveFilter(this)

  // Destinations where the piece can end up after the entire move
  // without move filter - e.g. not taking defending the king into account
  def unfilteredDestinations: List[Pos] =
    shortUnfilteredDestinations ::: longRange(attackingPiece.projectionDirs)

  lazy val shortUnfilteredDestinations = shortRange(attackingPiece.directDirs)

  lazy val lionMoveDestinationsMap: Map[Pos, List[Pos]] =
    if (piece.role.hasLionPower)
      shortUnfilteredDestinations
        .withFilter(_.dist(pos) == 1)
        .map { ms =>
          (ms, pos :: shogi.variant.Chushogi.lionMoveFilter(this, ms))
        }
        .toMap
    else Map.empty

  def toUsis: List[Usi.Move] = {
    val normalMoves = destinations
      .withFilter(!variant.forcePromote(piece, _))
      .map(Usi.Move(pos, _, false, None))
    val promotedMoves = destinations
      .withFilter(d => variant.canPromote(piece, pos, d, board(d).isDefined))
      .map(Usi.Move(pos, _, true, None))
    val lionMoves = lionMoveDestinationsMap.flatMap { case (ms, dests) =>
      dests.map(d => Usi.Move(pos, d, false, Some(ms)))
    }.toList

    normalMoves ::: promotedMoves ::: lionMoves
  }

  private def shortRange(dirs: Directions): List[Pos] =
    dirs flatMap { _(pos) } filter { to =>
      variant
        .isInsideBoard(to) && board.pieces.get(to).fold(true)(_.color != color)
    }

  private def longRange(dirs: Directions): List[Pos] = {
    val buf = new ArrayBuffer[Pos]
    @scala.annotation.tailrec
    def addAll(p: Pos, dir: Direction): Unit = {
      dir(p) match {
        case Some(to) if variant.isInsideBoard(to) =>
          board.pieces.get(to) match {
            case None => {
              buf += to
              addAll(to, dir)
            }
            case Some(piece) =>
              if (piece.color != color)
                buf += to
          }
        case _ => ()
      }
    }

    dirs foreach { addAll(pos, _) }
    buf.toList
  }
}
