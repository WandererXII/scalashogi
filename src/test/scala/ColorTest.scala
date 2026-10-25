package shogi

class ColorTest extends ShogiTest {

  "Color" should {
    "unary !" in {
      "sente" in { !Sente must_== Gote }
      "gote" in { !Gote must_== Sente }
    }
    "format shogi results and recover their winners" in {
      forall(List(Some(Sente), Some(Gote), None)) { winner =>
        Color.fromResult(Color.showResult(winner)) must_== winner
      }
    }
    "format a draw" in {
      Color.showResult(None) must_== "draw"
    }
  }
}
