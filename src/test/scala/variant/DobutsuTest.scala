package shogi
package variant

import shogi.format.forsyth.Sfen
import shogi.format.usi.Usi

class DobutsuTest extends ShogiTest {

  def perft(game: Game, depth: Int, log: Boolean = false) = Perft.perft(game, depth, log)

  "calculate dobutsu perfts" should {
    val game = Game(shogi.variant.Dobutsu)
    "1 depth" in {
      perft(game, 1) must be equalTo 4
    }
    "2 depth" in {
      perft(game, 2) must be equalTo 17
    }
    "3 depth" in {
      perft(game, 3) must be equalTo 123
    }
    "4 depth" in {
      perft(game, 4) must be equalTo 976
    }
    "5 depth" in {
      perft(game, 5) must be equalTo 8122
    }
    "6 depth" in {
      perft(game, 6) must be equalTo 71677
    }
  }

  "moving into check and being captured" should {
    "allow king capture behavior" in {
      val sit = Sfen("3/1k1/3/1K1 b - 1").toSituation(shogi.variant.Dobutsu).get
      sit.moveActorAt(Pos.SQ2D).get.destinations.size must_== 5

      val move1 = Usi("2d2c").get
      sit(move1).isValid must beTrue
      val sit2 = sit(move1).toOption.get
      sit2.moveActorAt(Pos.SQ2B).get.destinations.size must_== 8

      val move2 = Usi("2b2c").get
      sit2(move2).isValid must beTrue
      val sitCaptureAfter = sit2(move2).toOption.get
      sitCaptureAfter.end must beTrue
      sitCaptureAfter.status must_== Some(Status.RoyalsLost)
      sitCaptureAfter.winner must_== Some(Color.Gote)

      val sitMissAfter = sit2(Usi("2b1b").get).toOption.get
      sitMissAfter.end must beFalse
    }
  }

  "try rule" should {
    "win by reaching other side" in {
      val sit = Sfen("k2/2K/3/3 b - 1").toSituation(shogi.variant.Dobutsu).get
      sit.moveActorAt(Pos.SQ1B).get.destinations.size must_== 5

      val move = Usi("1b1a").get
      sit(move).isValid must beTrue

      val afterTry = sit(move).toOption.get
      afterTry.end must beTrue
      afterTry.status must_== Some(Status.TryRule)
      afterTry.winner must_== Some(Color.Sente)
    }

    "try rule - in check" in {
      val sit = Sfen("1r1/bpK/k2/3 b -").toSituation(shogi.variant.Dobutsu).get
      sit.moveActorAt(Pos.SQ1B).get.destinations.size must_== 5

      val moveTry = Usi("1b1a").get
      sit(moveTry).isValid must beTrue
      val s1 = sit(moveTry).toOption.get
      s1.switch.check must beTrue
      s1.end must beFalse

      // opponent safe try rule
      val moveSafe = Usi("3c3d").get
      s1(moveSafe).isValid must beTrue
      val s2 = s1(moveSafe).toOption.get

      s2.end must beTrue
      s2.status must_== Some(Status.TryRule)
      s2.winner must_== Some(Color.Gote)

      val moveUnrelated = Usi("2b2c").get
      val s3            = s1(moveUnrelated).toOption.get
      s3.end must beFalse

      val moveClear = Usi("2a3a").get
      val s4        = s1(moveClear).toOption.get
      s4.end must beTrue
      s4.status must_== Some(Status.TryRule)
      s4.winner must_== Some(Color.Sente)
    }
  }

  "force promoting" should {
    "enforce promotion rules" in {
      val sit = Sfen("3/BRP/2k/K2 b P").toSituation(shogi.variant.Dobutsu).get
      sit(Usi("1b1a").get).isValid must beFalse
      sit(Usi("1b1a+").get).isValid must beTrue
      sit(Usi("3b2a+").get).isValid must beFalse
    }
  }

  "drops" should {
    "be legal" in {
      val sit  = Sfen("rkr/b1b/1P1/BKR b P").toSituation(shogi.variant.Dobutsu).get
      val drop = Usi("P*2b").get
      sit(drop).isValid must beTrue

      val afterDrop = sit(drop).toOption.get
      afterDrop.end must beFalse
    }
  }
}
