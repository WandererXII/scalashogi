package shogi
package variant

import shogi.format.forsyth.Sfen
import shogi.format.usi.Usi

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
    "4 depth" in {
      perft(game, 4) must be equalTo 719408
    }
    // "5 depth" in {
    //  perft(game, 5) must be equalTo 19839626
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
      sit.end must beFalse
      sit.winner must beNone
      val res = sit(Usi("3f2e").get).toOption.get
      res.check must beTrue
      res.end must beTrue
      res.status must_== Some(Status.SpecialVariantEnd)
      res.winner must_== Some(Sente)
    }
    "gote" in {
      val sit = Sfen("9/3gk4/9/2b6/9/6B2/9/4KG3/9 w").toSituation(shogi.variant.Checkshogi).get
      sit.check must beFalse
      sit.end must beFalse
      sit.winner must beNone
      val res = sit(Usi("7d8e").get).toOption.get
      res.check must beTrue
      res.end must beTrue
      res.status must_== Some(Status.SpecialVariantEnd)
      res.winner must_== Some(Gote)
    }
    "check end" in {
      val sit = Sfen("lnsgkgsnl/4r2b1/pppp1pppp/9/4r4/9/PPPP1PPPP/1BG1G4/LNS1K1SNL w Pp 12")
        .toSituation(shogi.variant.Checkshogi)
        .get
      sit.check must beFalse
      sit.end must beFalse
      sit.winner must beNone
      val res = sit(Usi("5e5h+").get).toOption.get
      res.check must beTrue
      res.end must beTrue
      res.status must_== Some(Status.SpecialVariantEnd)
      res.winner must_== Some(Gote)
    }
  }

  "pawn drops" should {
    "be allowed with checkmate" in {
      val sit =
        Sfen("3rkr3/9/8p/4N4/1B7/9/1SG6/1KS6/9 b LPp").toSituation(shogi.variant.Checkshogi).get
      sit.check must beFalse
      sit.end must beFalse
      sit.winner must beNone
      val usi = Usi.Drop("P*5b").get
      sit.dropActorOf(usi.role).get.destinations must contain(usi.pos)
      val pawnCheck = sit(usi).toOption.get
      pawnCheck.check must beTrue
      pawnCheck.end must beTrue
      pawnCheck.status must_== Some(Status.SpecialVariantEnd)
      pawnCheck.winner must_== Some(Sente)

      val usi2 = Usi.Drop("L*5b").get
      sit.dropActorOf(usi2.role).get.destinations must contain(usi2.pos)
      val lanceCheck = sit(usi2).toOption.get
      lanceCheck.check must beTrue
      lanceCheck.end must beTrue
      lanceCheck.status must_== Some(Status.SpecialVariantEnd)
      lanceCheck.winner must_== Some(Sente)
    }
    "be allowed with check alone" in {
      val sit =
        Sfen("3rk4/9/8p/4N4/1B7/9/1SG6/1KS6/9 b LPp 1").toSituation(shogi.variant.Checkshogi).get
      sit.check must beFalse
      sit.end must beFalse
      sit.winner must beNone
      val usi = Usi.Drop("P*5b").get
      sit
        .dropActorOf(usi.role)
        .get
        .destinations must contain(usi.pos)
      val pawnCheck = sit(usi).toOption.get
      pawnCheck.check must beTrue
      pawnCheck.end must beTrue
      pawnCheck.status must_== Some(Status.SpecialVariantEnd)
      pawnCheck.winner must_== Some(Sente)
    }
  }
}
