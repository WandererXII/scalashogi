package shogi

import shogi.format.usi.Usi

class SituationReplayTest extends ShogiTest {
  val usis = format.usi.Fixtures.prod500standard.map(Usi.readList(_).get)

  "all 500 fixtures" should {
    "have no errors and correct size" in {
      usis forall { u =>
        val (sits, error) = Replay.statesWhileValid(u, None, shogi.variant.Standard)
        error must beNone
        (sits.size - 1) must_== u.size
        forall(sits.toList.zipWithIndex) { case (sit, index) =>
          sit.usis must_== u.take(index).toVector
        }
      }
    }
  }

}
