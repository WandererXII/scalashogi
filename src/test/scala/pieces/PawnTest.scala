package shogi
package pieces

import shogi.Square._

class PawnTest extends ShogiTest {

  "a sente pawn" should {

    "move towards rank by 1 square" in {
      makePositionWithBoard(
        shogi.variant.Standard,
        Sente,
        SQ9F -> Sente.pawn,
      ) moveDestsFrom SQ9F must beSquares(SQ9E)
    }

    "not move to squares that are occupied by the same color" in {
      makePositionWithBoard(
        shogi.variant.Standard,
        Sente,
        SQ9F -> Sente.pawn,
        SQ9E -> Sente.pawn,
      ) moveDestsFrom SQ9F must beSquares()
    }

    "capture forward" in {
      makePositionWithBoard(
        shogi.variant.Standard,
        Sente,
        SQ6F -> Sente.pawn,
        SQ6E -> Gote.pawn,
      ) moveDestsFrom SQ6F must beSquares(SQ6E)
    }
  }

  "a gote pawn" should {

    "move towards rank by 1 square" in {
      makePositionWithBoard(
        shogi.variant.Standard,
        Gote,
        SQ9E -> Gote.pawn,
      ) moveDestsFrom SQ9E must beSquares(SQ9F)
    }

    "not move to squares that are occupied by the same color" in {
      makePositionWithBoard(
        shogi.variant.Standard,
        Gote,
        SQ9F -> Gote.pawn,
        SQ9G -> Gote.pawn,
      ) moveDestsFrom SQ9F must beSquares()
    }

    "capture forward" in {
      makePositionWithBoard(
        shogi.variant.Standard,
        Gote,
        SQ6E -> Gote.pawn,
        SQ6F -> Sente.pawn,
      ) moveDestsFrom SQ6E must beSquares(SQ6F)
    }
  }
}
