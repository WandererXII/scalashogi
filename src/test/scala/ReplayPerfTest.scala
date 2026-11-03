package shogi

import shogi.format.usi._

class ReplayPerfTest extends ShogiTest {

  // args(skipAll = true)

  val nb         = 100
  val usis       = format.usi.Fixtures.prod500standard.take(nb).map(Usi.readList(_).get)
  val iterations = 10

  def runOne(usis: List[Usi]) =
    Replay.positionsWhileValid(usis, None, shogi.variant.Standard)
  def run() = { usis foreach runOne }

  "playing a game" should {
    "many times" in {
      benchmark(nb, iterations)(run())
    }
  }
}
