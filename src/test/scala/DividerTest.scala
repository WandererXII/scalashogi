package shogi

import shogi.format.forsyth.Sfen
import shogi.format.usi.Usi
import shogi.variant.Standard

class DividerTest extends ShogiTest {
  val usis = Usi.readList(format.usi.Fixtures.fromProd2).get

  "the divider counts played plies" should {
    "handle an empty history" in {
      Divider(Seq.empty) must_== Division.empty
    }
    "count no plies for only the initial position" in {
      val positions = completeReplay(Nil)
      Divider(positions.toList) must_== Division.empty
    }
    "count one ply after one move" in {
      val positions = completeReplay(usis.take(1))
      val divided   = Divider(positions.toList)
      divided.plies must_== 1
      divided.openingSize must_== 1
    }
  }

  "division phase sizes and bounds" should {
    "handle an opening followed directly by endgame" in {
      val divided = Division(None, Some(4), 10)
      divided.openingSize must_== 4
      divided.openingBounds must beSome(0 -> 4)
      divided.middleSize must beNone
      divided.middleBounds must beNone
      divided.endSize must beSome(6)
      divided.endBounds must beSome(4 -> 10)
    }
    "handle a history that starts in endgame" in {
      val divided = Division(None, Some(0), 10)
      divided.openingSize must_== 0
      divided.openingBounds must beSome(0 -> 0)
      divided.endSize must beSome(10)
      divided.endBounds must beSome(0 -> 10)
    }
    "extend middlegame to the end of history when endgame is absent" in {
      val divided = Division(Some(4), None, 10)
      divided.middleSize must beSome(6)
      divided.middleBounds must beSome(4 -> 10)
      divided.endSize must beNone
      divided.endBounds must beNone
    }
    "preserve all three phases" in {
      val divided = Division(Some(3), Some(7), 10)
      divided.openingSize must_== 3
      divided.openingBounds must beSome(0 -> 3)
      divided.middleSize must beSome(4)
      divided.middleBounds must beSome(3 -> 7)
      divided.endSize must beSome(3)
      divided.endBounds must beSome(7 -> 10)
    }
    "preserve an opening-only history" in {
      val divided = Division(None, None, 10)
      divided.openingSize must_== 10
      divided.openingBounds must beNone
      divided.middleSize must beNone
      divided.middleBounds must beNone
      divided.endSize must beNone
      divided.endBounds must beNone
    }
    "preserve an empty history" in {
      Division.empty.openingSize must_== 0
      Division.empty.openingBounds must beNone
      Division.empty.middleBounds must beNone
      Division.empty.endBounds must beNone
    }
  }

  // more tests wanted
  "the divider finds middlegame and endgame" should {
    "start the middlegame when either side drops below four pieces on its back rank" in {
      val initial = Sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1BNSS1NRG/L3KG2L b - 1")
      val moves   = List(Usi("4i4h").get)
      val (positions, error) = Replay.positionsWhileValid(moves, Some(initial), Standard)

      require(error.isEmpty, error.getOrElse(""))
      Divider(positions) must_== Division(Some(1), None, 1)
    }

    "game1" in {
      val positions = completeReplay(usis)
      val divided   = Divider(positions.toList)
      println("Game 1 => " + divided)
      divided.plies must_== usis.size
      divided.middle must beSome.like { case x =>
        x must beBetween(10, 30)
      }
      divided.end must beNone
      divided.middleBounds must beSome.like { case (start, stop) =>
        Some(stop - start) must_== divided.middleSize
        stop must_== usis.size
      }
    }
  }
}
