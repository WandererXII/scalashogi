package shogi
package pieces

import Pos._

class PawnTest extends ShogiTest {

  "a sente pawn" should {

    "move towards rank by 1 square" in {
      makeSituationWithBoard(
        shogi.variant.Standard,
        SQ9F -> Sente.pawn
      ) moveDestsFrom SQ9F must bePoss(SQ9E)
    }

    "not move to positions that are occupied by the same color" in {
      makeSituationWithBoard(
        shogi.variant.Standard,
        SQ9F -> Sente.pawn,
        SQ9E -> Sente.pawn
      ) moveDestsFrom SQ9F must bePoss()
    }

    "capture forward" in {
      makeSituationWithBoard(
        shogi.variant.Standard,
        SQ6F -> Sente.pawn,
        SQ6E -> Gote.pawn
      ) moveDestsFrom SQ6F must bePoss(SQ6E)
    }
  }

  "a gote pawn" should {

    "move towards rank by 1 square" in {
      makeSituationWithBoard(
        shogi.variant.Standard,
        SQ9E -> Gote.pawn
      ) moveDestsFrom SQ9E must bePoss(SQ9F)
    }

    "not move to positions that are occupied by the same color" in {
      makeSituationWithBoard(
        shogi.variant.Standard,
        SQ9F -> Gote.pawn,
        SQ9G -> Gote.pawn
      ) moveDestsFrom SQ9F must bePoss()
    }

    "capture forward" in {
      makeSituationWithBoard(
        shogi.variant.Standard,
        SQ6E -> Gote.pawn,
        SQ6F -> Sente.pawn
      ) moveDestsFrom SQ6E must bePoss(SQ6F)
    }
  }
}
