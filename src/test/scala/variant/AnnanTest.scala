package shogi
package variant

import format.forsyth.Sfen
import format.usi.Usi

class AnnanTest extends ShogiTest {

  def perft(game: Game, depth: Int) = Perft.perft(game, depth)

  "calculate annan perfts" should {
    val game = Game(shogi.variant.Annan)
    "1 depth" in {
      perft(game, 1) must be equalTo 28
    }
    "2 depth" in {
      perft(game, 2) must be equalTo 784
    }
  }

  val annanPerfts = List(
    ("4k4/4p4/9/4P4/4L4/4N4/9/9/9 b - 1", 10),
    ("2k6/9/9/9/9/1P1P1P1P1/1R3K1+B1/3R5/9 w - 1", 1)
  )

  "default positions" should {
    "be identical" in {
      Game(shogi.variant.Annan).toSfen must_== shogi.variant.Annan.initialSfen
    }
  }

  "annan positions" should {
    "forall" in {
      forall(annanPerfts) { line =>
        line match {
          case (sfenStr, d1) => {
            val game = Game(Some(shogi.variant.Annan), Some(Sfen(sfenStr)))
            perft(game, 1) must be equalTo d1.toInt
          }
        }
      }
    }
  }

  "random annan positions" should {
    "forall" in {
      forall(randomAnnanPerfts) { line =>
        line match {
          case (sfenStr, d1) => {
            val game = Game(Some(shogi.variant.Annan), Some(Sfen(sfenStr)))
            game.situation.end(false) must beFalse
            perft(game, 1) must be equalTo d1.toInt
          }
        }
      }
    }
  }

  "move legaility" should {
    "not checkmate" in {
      val sit = Sfen("4k4/4+R4/4L4/9/9/9/9/7GS/7GK w").toSituation(shogi.variant.Annan).get
      sit.check must beTrue
      sit.end(false) must beFalse
    }
    "checkmate" in {
      val sit = Sfen("4k4/4+R4/9/4L4/9/9/9/7GS/7GK w").toSituation(shogi.variant.Annan).get
      sit.end(false) must beTrue
    }
    "capture move giver to defend from check" in {
      val sit = Sfen("5k3/9/9/5l3/5p3/9/2B6/9/5K3 b").toSituation(shogi.variant.Annan).get
      sit(Usi("7g5e").get).isValid must beFalse
      sit(Usi("7g4d").get).isValid must beTrue
    }
    "capture move giver to defend from check with king" in {
      val sit = Sfen("9/8k/9/9/9/9/9/p+r7/K8 b").toSituation(shogi.variant.Annan).get
      sit(Usi("9i8e").get).isValid must beFalse
      sit(Usi("9i8h").get).isValid must beTrue
    }
    "king captures protected piece" in {
      val sit = Sfen("k8/9/9/5K3/6g2/6p2/9/9/9 b").toSituation(shogi.variant.Annan).get
      sit(Usi("4d3e").get).isValid must beTrue
    }
    "pawn checkmate not valid" in {
      val sit = Sfen("4k4/9/4G4/9/9/9/9/9/9 b P").toSituation(shogi.variant.Annan).get
      sit(Usi("P*4b").get).isValid must beTrue
      sit(Usi("P*5b").get).isValid must beFalse
    }
    "backrank drops" in {
      val sit = Sfen("9/9/9/k8/g8/G8/K8/9/9 b NLP").toSituation(shogi.variant.Annan).get
      sit(Usi("P*5a").get).isValid must beTrue
      sit(Usi("N*5a").get).isValid must beTrue
      sit(Usi("N*5b").get).isValid must beTrue
      sit(Usi("L*5a").get).isValid must beTrue
    }
    "don't force promotions" in {
      val sit = Sfen("9/3PL4/2P5N/k6N1/g8/G8/K8/9/5L3 b").toSituation(shogi.variant.Annan).get
      sit(Usi("2d3b").get).isValid must beTrue
      sit(Usi("2d3b+").get).isValid must beTrue
      sit(Usi("1c2a").get).isValid must beTrue
      sit(Usi("1c2a+").get).isValid must beTrue
      sit(Usi("5b5a").get).isValid must beTrue
      sit(Usi("5b5a+").get).isValid must beTrue
      sit(Usi("6b6a").get).isValid must beTrue
      sit(Usi("6b6a+").get).isValid must beTrue
      sit(Usi("4i4a").get).isValid must beTrue
      sit(Usi("4i4a+").get).isValid must beTrue
      Sfen("5L3/3PL4/2P5N/k6N1/g8/G8/K8/9/9 w").toSituation(shogi.variant.Annan).get.valid(true) must beTrue
      Sfen("5L3/3PL4/2P5N/k6N1/g8/G8/K8/9/9 b").toSituation(shogi.variant.Annan).get.valid(true) must beTrue
    }
    "double pawns" in {
      val sit = Sfen("9/9/9/9/9/k8/n1PPPPPP1/N2G2B2/K8 b 3P").toSituation(shogi.variant.Annan).get
      sit(Usi("6g7f").get).isValid must beTrue
      sit(Usi("P*8f").get).isValid must beTrue
      sit(Usi("P*7f").get).isValid must beFalse
      Sfen("9/9/9/9/9/k3P4/n1P1PPPP1/N2G2B2/K8 w 3P").toSituation(shogi.variant.Annan).get.valid(true) must beTrue
      Sfen("9/9/9/9/9/k3P4/n1P1PPPP1/N2G2B2/K8 b 3P").toSituation(shogi.variant.Annan).get.valid(true) must beTrue
    }
  }

  // format: off
  val randomAnnanPerfts: List[(String, String)] = List(
  )

}