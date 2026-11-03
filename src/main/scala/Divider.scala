package shogi

final case class Division(middle: Option[Int], end: Option[Int], plies: Int) {

  def openingSize: Int        = middle.orElse(end) | plies
  def middleSize: Option[Int] =
    middle.map { m =>
      (end | plies) - m
    }
  def endSize = end.map(plies -)

  def openingBounds = middle.orElse(end).map(0 -> _)
  def middleBounds  = middle.map(m => m -> (end | plies))
  def endBounds     = end.map(_ -> plies)
}

object Division {
  val empty = Division(None, None, 0)
}

sealed trait DividerData
final case class NotFound(senteInvadersInGotesCamp: Int, goteInvadersInSentesCamp: Int)
    extends DividerData
final case class Found(index: Int) extends DividerData

object Divider {

  def apply(positions: Seq[Position]): Division = {

    val indexedPositions = positions.zipWithIndex

    val midGame = indexedPositions.foldLeft[DividerData](NotFound(0, 0)) {
      case (found: Found, _) =>
        found
      case (NotFound(lastSenteInvaders, lastGoteInvaders), (position, index)) => {
        val curSenteInvaders = countInvaders(position, Sente)
        val curGoteInvaders  = countInvaders(position, Gote)
        if (
          // every piece except pawns and king
          majorsAndMinors(position) <= 17 ||
          isBackrankSparse(position) ||
          // if one piece invades the opposing camp and is not immediately captured
          (curSenteInvaders >= 1 && lastSenteInvaders >= 1) ||
          (curGoteInvaders >= 1 && lastGoteInvaders >= 1)
        ) Found(index)
        else
          // store the current # of Invaders
          NotFound(curSenteInvaders, curGoteInvaders)
      }
    }

    val midGameOption = midGame match {
      case Found(index) => Some(index)
      case _            => None
    }

    val endGame =
      if (midGameOption.isDefined) indexedPositions.foldLeft[DividerData](NotFound(0, 0)) {
        case (found: Found, _) =>
          found
        case (NotFound(lastSenteInvaders, lastGoteInvaders), (position, index)) => {
          val curSenteInvaders = countInvaders(position, Sente)
          val curGoteInvaders  = countInvaders(position, Gote)
          if (
            (majorsAndMinors(position) <= 3 && position.board.pieces.sizeIs <= 12) ||
            (curSenteInvaders >= 2 && lastSenteInvaders >= 2) ||
            (curGoteInvaders >= 2 && lastGoteInvaders >= 2)
          ) Found(index)
          else
            NotFound(curSenteInvaders, curGoteInvaders)
        }
      }
      else NotFound(0, 0)

    val endGameOption = endGame match {
      case Found(index) => Some(index)
      case _            => None
    }

    Division(
      midGameOption.filter(m => endGameOption.fold(true)(m <)),
      endGameOption,
      // The history includes the initial position, before any ply is played.
      (positions.size - 1).max(0),
    )
  }

  private def countInvaders(position: Position, color: Color): Int =
    position.board.pieces.count { case (square, piece) =>
      piece.color == color && position.variant.promotionRanks(color).contains(square.rank)
    }

  private def majorsAndMinors(position: Position): Int =
    position.board.pieces.values.count(p => p.role != Pawn && p.role != King)

  // Sparse back-rank indicates that pieces have been developed
  private def isBackrankSparse(position: Position): Boolean =
    position.board.pieces.count { case (square, piece) =>
      position.variant.backrank(!piece.color) == square.rank
    } < 4

}
