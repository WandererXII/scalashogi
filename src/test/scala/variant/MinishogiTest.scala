package shogi
package variant

class MinishogiTest extends ShogiTest {

  def perft(game: Game, depth: Int) = Perft.perft(game, depth)

  "calculate minishogi perfts" should {
    val game = Game(shogi.variant.Minishogi)
    "1 depth" in {
      perft(game, 1) must be equalTo 14
    }
    "2 depth" in {
      perft(game, 2) must be equalTo 181
    }
    "3 depth" in {
      perft(game, 3) must be equalTo 2512
    }
    "4 depth" in {
      perft(game, 4) must be equalTo 35401
    }
    // "5 depth" in {
    //  perft(game, 5) must be equalTo 533203
    // }
  }

}
