package shogi

import scala.collection.mutable.ArrayBuffer

import shogi.format.usi.Usi

final case class MoveActor(
    piece: Piece,
    pos: Pos,
    situation: Situation
) {

  lazy val attackingPiece = situation.variant.attackingPiece(piece, pos, situation.board)

  lazy val destinations: List[Pos] = situation.variant.moveFilter(this)

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
          (ms, pos :: situation.variant.lionMoveFilter(this, ms))
        }
        .toMap
    else Map.empty

  def toUsis: List[Usi.Move] = {
    val normalMoves = destinations
      .withFilter(!situation.variant.forcePromote(piece, _))
      .map(Usi.Move(pos, _, false, None))
    val promotedMoves = destinations
      .withFilter(d => situation.variant.canPromote(piece, pos, d, situation.board(d).isDefined))
      .map(Usi.Move(pos, _, true, None))
    val lionMoves = lionMoveDestinationsMap.flatMap { case (ms, dests) =>
      dests.map(d => Usi.Move(pos, d, false, Some(ms)))
    }.toList

    normalMoves ::: promotedMoves ::: lionMoves
  }

  def color        = piece.color
  def is(c: Color) = c == piece.color

  private def shortRange(dirs: Directions): List[Pos] =
    dirs flatMap { _(pos) } filter { to =>
      situation.variant
        .isInsideBoard(to) && situation.board.pieces.get(to).fold(true)(_.color != color)
    }

  private def longRange(dirs: Directions): List[Pos] = {
    val buf = new ArrayBuffer[Pos]
    @scala.annotation.tailrec
    def addAll(p: Pos, dir: Direction): Unit = {
      dir(p) match {
        case Some(to) if situation.variant.isInsideBoard(to) =>
          situation.board.pieces.get(to) match {
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
