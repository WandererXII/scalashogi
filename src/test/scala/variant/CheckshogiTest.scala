package shogi
package variant

import format.forsyth.Sfen
import format.usi.Usi

class CheckshogiTest extends ShogiTest {

  def perft(game: Game, depth: Int) = Perft.perft(game, depth)

  "starting position" should {
    val game = Game(shogi.variant.Checkshogi)
    "1 depth" in {
      perft(game, 1) must be equalTo 30
    }
    "2 depth" in {
      perft(game, 2) must be equalTo 900
    }
    "3 depth" in {
      perft(game, 3) must be equalTo 25470
    }
    // "4 depth" in {
    //   perft(game, 4) must be equalTo 719731
    // }
    // "5 depth" in {
    //  perft(game, 5) must be equalTo 19861490
    // }
  }

  "default positions" should {
    "be identical" in {
      Game(shogi.variant.Checkshogi).toSfen must_== shogi.variant.Checkshogi.initialSfen
    }
  }

  "win" should {
    "sente" in {
      val sit = Sfen("9/3gk4/9/2b6/9/6B2/9/4KG3/9 b").toSituation(shogi.variant.Checkshogi).get
      sit.check must beFalse
      sit.end(false) must beFalse
      sit.winner must beNone
      val sit2 = sit(Usi("3f2e").get).toOption.get
      sit2.check must beTrue
      sit2.end(false) must beTrue
      sit2.status must_== Some(Status.SpecialVariantEnd)
      sit2.winner must_== Some(Sente)
    }
    "gote" in {
      val sit = Sfen("9/3gk4/9/2b6/9/6B2/9/4KG3/9 w").toSituation(shogi.variant.Checkshogi).get
      sit.check must beFalse
      sit.end(false) must beFalse
      sit.winner must beNone
      val sit2 = sit(Usi("7d8e").get).toOption.get
      sit2.check must beTrue
      sit2.end(false) must beTrue
      sit2.status must_== Some(Status.SpecialVariantEnd)
      sit2.winner must_== Some(Gote)
    }
  }
}
