package shogi

import shogi.format.usi._

class PositionReplayPerfTest extends ShogiTest {

  // args(skipAll = true)

  val nb         = 100
  val iterations = 10

  val usis = format.usi.Fixtures.prod500standard.take(nb).map(Usi.readList(_).get)
  def runOne(usis: List[Usi]) = {
    val positions = completeReplay(usis)
    require(
      positions.size == usis.size + 1,
      s"expected ${usis.size + 1} positions, got ${positions.size}",
    )
  }
  def run() = { usis foreach runOne }

  "playing a game" should {
    "many times" in {
      benchmark(nb, iterations)(run())
    }
  }
}
