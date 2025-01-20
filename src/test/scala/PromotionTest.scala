package shogi

class PromotionTest extends ShogiTest {

  "pawn promotion" should {
    val situation = """
. . p . . . . . .
. . . . . . . . .
K . . . . . . . .
Hands:
Turn:Gote
"""
    val game      = Game(situation)
    "promote to a tokin" in {
      game.playUsiStr("7g7h+") must beGame(
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
      game.playUsiStr("7g7h") must beGame(
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
      Game(
        """
. . p . . . . . .
K . R . . . . . .
Turn:Gote""",
      ).playUsiStr("7h7i+") must beGame(
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
