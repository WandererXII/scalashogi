package shogi

class PromotionTest extends ShogiTest {

  "pawn promotion" should {
    val position: Position = """
. . p . . . . . .
. . . . . . . . .
K . . . . . . . .
Hands:
Turn:Gote
"""
    "promote to a tokin" in {
      position.playUsiStr("7g7h+") must bePosition(
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
      position.playUsiStr("7g7h") must bePosition(
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
      stringToPosition(
        """
. . p . . . . . .
K . R . . . . . .
Turn:Gote""",
      ).playUsiStr("7h7i+") must bePosition(
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
