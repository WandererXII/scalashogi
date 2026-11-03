package shogi
package variant

import shogi.format.forsyth.Sfen
import shogi.format.usi.Usi

class CheckshogiTest extends ShogiTest {

  def perft(position: Position, depth: Int) = Perft.perft(position, depth)

  "starting position" should {
    val position = Position(shogi.variant.Checkshogi)
    "1 depth" in {
      perft(position, 1) must be equalTo 30
    }
    "2 depth" in {
      perft(position, 2) must be equalTo 900
    }
    "3 depth" in {
      perft(position, 3) must be equalTo 25470
    }
    "4 depth" in {
      perft(position, 4) must be equalTo 719408
    }
    // "5 depth" in {
    //  perft(position, 5) must be equalTo 19839626
    // }
  }

  "default positions" should {
    "be identical" in {
      Position(shogi.variant.Checkshogi).toSfen must_== shogi.variant.Checkshogi.initialSfen
    }
  }

  "win" should {
    "sente" in {
      val position = Sfen("9/3gk4/9/2b6/9/6B2/9/4KG3/9 b").toPosition(shogi.variant.Checkshogi).get
      position.check must beFalse
      position.end must beFalse
      position.winner must beNone
      val res = position(Usi("3f2e").get).toOption.get
      res.check must beTrue
      res.end must beTrue
      res.status must_== Some(Status.Check)
      res.winner must_== Some(Sente)
    }
    "gote" in {
      val position = Sfen("9/3gk4/9/2b6/9/6B2/9/4KG3/9 w").toPosition(shogi.variant.Checkshogi).get
      position.check must beFalse
      position.end must beFalse
      position.winner must beNone
      val res = position(Usi("7d8e").get).toOption.get
      res.check must beTrue
      res.end must beTrue
      res.status must_== Some(Status.Check)
      res.winner must_== Some(Gote)
    }
    "check end" in {
      val position = Sfen("lnsgkgsnl/4r2b1/pppp1pppp/9/4r4/9/PPPP1PPPP/1BG1G4/LNS1K1SNL w Pp 12")
        .toPosition(shogi.variant.Checkshogi)
        .get
      position.check must beFalse
      position.end must beFalse
      position.winner must beNone
      val res = position(Usi("5e5h+").get).toOption.get
      res.check must beTrue
      res.end must beTrue
      res.status must_== Some(Status.Check)
      res.winner must_== Some(Gote)
    }
  }

  "pawn drops" should {
    "be allowed with checkmate" in {
      val position =
        Sfen("3rkr3/9/8p/4N4/1B7/9/1SG6/1KS6/9 b LPp").toPosition(shogi.variant.Checkshogi).get
      position.check must beFalse
      position.end must beFalse
      position.winner must beNone
      val usi = Usi.Drop("P*5b").get
      position.dropActorOf(usi.role).get.destinations must contain(usi.square)
      val pawnCheck = position(usi).toOption.get
      pawnCheck.check must beTrue
      pawnCheck.end must beTrue
      pawnCheck.status must_== Some(Status.Check)
      pawnCheck.winner must_== Some(Sente)

      val usi2 = Usi.Drop("L*5b").get
      position.dropActorOf(usi2.role).get.destinations must contain(usi2.square)
      val lanceCheck = position(usi2).toOption.get
      lanceCheck.check must beTrue
      lanceCheck.end must beTrue
      lanceCheck.status must_== Some(Status.Check)
      lanceCheck.winner must_== Some(Sente)
    }
    "be allowed with check alone" in {
      val position =
        Sfen("3rk4/9/8p/4N4/1B7/9/1SG6/1KS6/9 b LPp 1").toPosition(shogi.variant.Checkshogi).get
      position.check must beFalse
      position.end must beFalse
      position.winner must beNone
      val usi = Usi.Drop("P*5b").get
      position
        .dropActorOf(usi.role)
        .get
        .destinations must contain(usi.square)
      val pawnCheck = position(usi).toOption.get
      pawnCheck.check must beTrue
      pawnCheck.end must beTrue
      pawnCheck.status must_== Some(Status.Check)
      pawnCheck.winner must_== Some(Sente)
    }
  }
}
