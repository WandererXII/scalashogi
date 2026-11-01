package shogi

import shogi.format.forsyth.Visual.addNewLines

class PlayTest extends ShogiTest {

  "playing a game" should {
    "opening one" in {
      val state =
        makeState(shogi.variant.Standard)
          .playUsisStr(
            List(
              "7g7f",
              "3c3d",
              "8h2b",
            ),
          )
      "current game" in {
        state must beValid.like { case state =>
          addNewLines(state.situation.visual) must_== """
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
        state flatMap { _.playUsiStr("3a2b") } must beValid.like { case state =>
          addNewLines(state.situation.visual) must_== """
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
