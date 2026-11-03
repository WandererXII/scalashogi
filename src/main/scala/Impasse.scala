package shogi

object Impasse {
  def apply(position: Position): Boolean =
    position.variant.allSquares.sizeIs == (9 * 9) && !position.check && {
      val color        = position.color
      val ranks        = position.variant.promotionRanks(color)
      val enteredRoles = position.board.pieces.collect {
        case (square, piece) if (piece is color) && (ranks contains square.rank) => piece.role
      }.toList
      def impassePoints: Int =
        enteredRoles.map(impasseValueOf).sum + position.hands(color).sum(impasseValueOf)

      enteredRoles.sizeIs > necessaryEnteredPieces && enteredRoles
        .contains(King) && impassePoints >= color.fold(
        necessarySenteScore,
        necessaryGoteScore - missingImpassePoints(position),
      )
    }

  // In handicaps we give the value of the missing pieces to the handicap giver
  // Since this rule applies only to handicap games, only gote/uwate can be affected
  private def missingImpassePoints(position: Position): Int =
    position.history.initialSfen
      .filter(Handicap.isHandicap(_, position.variant))
      .flatMap(_.toPosition(position.variant))
      .fold(0) { initPosition =>
        math.max(
          0,
          (necessaryGoteScore * 2) -
            (initPosition.board.pieces.values.map(p => impasseValueOf(p.role)).sum +
              initPosition.hands(Sente).sum(impasseValueOf) + initPosition
                .hands(Gote)
                .sum(impasseValueOf)),
        )
      }

  private def impasseValueOf(r: Role): Int =
    r match {
      case Bishop | Rook | Horse | Dragon => 5
      case King                           => 0
      case _                              => 1
    }

  private val necessaryEnteredPieces = 10
  private val necessarySenteScore    = 28
  private val necessaryGoteScore     = 27

}
