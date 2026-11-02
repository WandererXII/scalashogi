package shogi

class PromotionTest extends ShogiTest {

  "pawn promotion" should {
    val situation: Situation = """
. . p . . . . . .
. . . . . . . . .
K . . . . . . . .
Hands:
Turn:Gote
"""
    val state                = situation
    "promote to a tokin" in {
      state.playUsiStr("7g7h+") must beState(
        """
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. .+p . . . . . .
K . . . . . . . .
Hands:
Turn:Sente
""",
        shogi.variant.Standard,
      )
    }
    "don't force promotion by default" in {
      state.playUsiStr("7g7h") must beState(
        """
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . p . . . . . .
K . . . . . . . .
Hands:
Turn:Sente
""",
        shogi.variant.Standard,
      )
    }
    "promotion by killing" in {
      stringToSituation(
        """
. . p . . . . . .
K . R . . . . . .
Turn:Gote""",
      ).playUsiStr("7h7i+") must beState(
        """
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
K .+p . . . . . .
Hands:r
Turn:Sente""",
        shogi.variant.Standard,
      )
    }
  }
}
