package shogi

object Impasse {
  def apply(situation: Situation): Boolean =
    situation.variant.allPositions.sizeIs == (9 * 9) && !situation.check && {
      val color        = situation.color
      val ranks        = situation.variant.promotionRanks(color)
      val enteredRoles = situation.board.pieces.collect {
        case (pos, piece) if (piece is color) && (ranks contains pos.rank) => piece.role
      }.toList
      def impassePoints: Int =
        enteredRoles.map(impasseValueOf).sum + situation.hands(color).sum(impasseValueOf)

      enteredRoles.sizeIs > necessaryEnteredPieces && enteredRoles
        .contains(King) && impassePoints >= color.fold(
        necessarySenteScore,
        necessaryGoteScore - missingImpassePoints(situation),
      )
    }

  // In handicaps we give the value of the missing pieces to the handicap giver
  // Since this rule applies only to handicap games, only gote/uwate can be affected
  private def missingImpassePoints(situation: Situation): Int =
    situation.history.initialSfen
      .filter(Handicap.isHandicap(_, situation.variant))
      .flatMap(_.toSituation(situation.variant))
      .fold(0) { initSit =>
        math.max(
          0,
          (necessaryGoteScore * 2) -
            (initSit.board.pieces.values.map(p => impasseValueOf(p.role)).sum +
              initSit.hands(Sente).sum(impasseValueOf) + initSit.hands(Gote).sum(impasseValueOf)),
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
