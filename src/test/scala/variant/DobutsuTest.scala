package shogi
package variant

import shogi.format.forsyth.Sfen
import shogi.format.usi.Usi

class DobutsuTest extends ShogiTest {

  def perft(position: Position, depth: Int, log: Boolean = false) =
    Perft.perft(position, depth, log)

  "calculate dobutsu perfts" should {
    val position = Position(shogi.variant.Dobutsu)
    "1 depth" in {
      perft(position, 1) must be equalTo 4
    }
    "2 depth" in {
      perft(position, 2) must be equalTo 17
    }
    "3 depth" in {
      perft(position, 3) must be equalTo 123
    }
    "4 depth" in {
      perft(position, 4) must be equalTo 976
    }
    "5 depth" in {
      perft(position, 5) must be equalTo 8122
    }
    "6 depth" in {
      perft(position, 6) must be equalTo 71677
    }
  }

  "moving into check and being captured" should {
    "allow king capture behavior" in {
      val position = Sfen("3/1k1/3/1K1 b - 1").toPosition(shogi.variant.Dobutsu).get
      position.moveActorAt(Square.SQ2D).get.destinations.size must_== 5

      val move1 = Usi("2d2c").get
      position(move1).isValid must beTrue
      val position2 = position(move1).toOption.get
      position2.moveActorAt(Square.SQ2B).get.destinations.size must_== 8

      val move2 = Usi("2b2c").get
      position2(move2).isValid must beTrue
      val positionCaptureAfter = position2(move2).toOption.get
      positionCaptureAfter.end must beTrue
      positionCaptureAfter.status must_== Some(Status.RoyalsLost)
      positionCaptureAfter.winner must_== Some(Color.Gote)

      val positionMissAfter = position2(Usi("2b1b").get).toOption.get
      positionMissAfter.end must beFalse
    }
  }

  "try rule" should {
    "win by reaching other side" in {
      val position = Sfen("k2/2K/3/3 b - 1").toPosition(shogi.variant.Dobutsu).get
      position.moveActorAt(Square.SQ1B).get.destinations.size must_== 5

      val move = Usi("1b1a").get
      position(move).isValid must beTrue

      val afterTry = position(move).toOption.get
      afterTry.end must beTrue
      afterTry.status must_== Some(Status.TryRule)
      afterTry.winner must_== Some(Color.Sente)
    }

    "work in check" in {
      val position = Sfen("1r1/bpK/k2/3 b -").toPosition(shogi.variant.Dobutsu).get
      position.moveActorAt(Square.SQ1B).get.destinations.size must_== 5

      val moveTry = Usi("1b1a").get
      position(moveTry).isValid must beTrue
      val s1 = position(moveTry).toOption.get
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

    "be draw" in {
      val position = Sfen("1K1/1r1/1R1/1k1 w BPbp 40").toPosition(shogi.variant.Dobutsu).get
      position.playable(strict = true) must beTrue
      val positionAfter = position(Usi("2b2c").get).toOption.get
      positionAfter.end must beTrue
      positionAfter.status must_== Some(Status.Draw)
      positionAfter.winner must_== None
    }
  }

  "force promoting" should {
    "enforce promotion rules" in {
      val position = Sfen("3/BRP/2k/K2 b P").toPosition(shogi.variant.Dobutsu).get
      position(Usi("1b1a").get).isValid must beFalse
      position(Usi("1b1a+").get).isValid must beTrue
      position(Usi("3b2a+").get).isValid must beFalse
    }
  }

  "drops" should {
    "allow pawns on the last rank" in {
      val position = Sfen("3/2k/K2/3 b P").toPosition(shogi.variant.Dobutsu).get
      position(Usi("P*1a").get).toOption.get.valid(strict = true) must beTrue
    }

    "be legal" in {
      val position = Sfen("rkr/b1b/1P1/BKR b P").toPosition(shogi.variant.Dobutsu).get
      val drop     = Usi("P*2b").get
      position(drop).isValid must beTrue

      val afterDrop = position(drop).toOption.get
      afterDrop.end must beFalse
    }
  }

  "playable" should {
    "be true" in {
      val positionB = Sfen("r2/2k/PKb/B2 b Pr").toPosition(shogi.variant.Dobutsu).get
      val positionW = Sfen("r2/2k/PKb/B2 w Pr").toPosition(shogi.variant.Dobutsu).get
      positionB.playable(strict = true) must beTrue
      positionW.playable(strict = true) must beTrue
    }
  }

  "double pawn" should {
    "work" in {
      val position = Sfen("rkb/2P/2P/BKR b - 1").toPosition(shogi.variant.Dobutsu).get
      position.playable(strict = true) must beTrue

      val positionPre = Sfen("rkb/3/2P/BKR b P 1").toPosition(shogi.variant.Dobutsu).get
      positionPre(Usi("P*1b").get).isValid must beTrue
    }
  }
}
