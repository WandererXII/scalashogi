package shogi

class PlayOneMoveTest extends ShogiTest {

  "playing a move" should {
    "only process things once" in {
      makeGame(shogi.variant.Standard).playUsiStr("7g7f") must beValid
    }
  }
}
