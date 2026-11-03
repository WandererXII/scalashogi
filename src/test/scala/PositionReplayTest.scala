package shogi

import shogi.format.usi.Usi

class PositionReplayTest extends ShogiTest {
  val usis = format.usi.Fixtures.prod500standard.map(Usi.readList(_).get)

  "all 500 fixtures" should {
    "have no errors and correct size" in {
      forall(usis) { u =>
        val (positions, error) = Replay.positionsWhileValid(u, None, shogi.variant.Standard)
        error must beNone
        (positions.size - 1) must_== u.size
        forall(positions.toList.zipWithIndex) { case (position, index) =>
          position.usis must_== u.take(index).toVector
        }
      }
    }
  }

}
