package shogi

import format.forsyth.Visual.addNewLines

class PlayTest extends ShogiTest {

  "playing a game" should {
    "opening one" in {
      val game =
        makeGame(shogi.variant.Standard)
          .playUsisStr(
            List(
              "7g7f",
              "3c3d",
              "8h2b"
            )
          )
      "current game" in {
        game must beValid.like { case g =>
          addNewLines(g.situation.visual) must_== """
l n s g k g s n l
. r . . . . . B .
p p p p p p . p p
. . . . . . p . .
. . . . . . . . .
. . P . . . . . .
P P . P P P P P P
. . . . . . . R .
L N S G K G S N L
Hands:B
Turn:Gote
"""
        }
      }
      "after recapture" in {
        game flatMap { _.playUsiStr("3a2b") } must beValid.like { case g =>
          addNewLines(g.situation.visual) must_== """
l n s g k g . n l
. r . . . . . s .
p p p p p p . p p
. . . . . . p . .
. . . . . . . . .
. . P . . . . . .
P P . P P P P P P
. . . . . . . R .
L N S G K G S N L
Hands:Bb
Turn:Sente
"""
        }
      }
    }
  }
}
