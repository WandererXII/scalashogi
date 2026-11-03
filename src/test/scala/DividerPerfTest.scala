package shogi

class DividerPerfTest extends ShogiTest {
  // args(skipAll = true)

  val nb         = 500
  val iterations = 10
  // val nb = 1
  // val iterations = 1

  val usis        = shogi.format.usi.Usi.readList(format.usi.Fixtures.fromProd2).get
  val gameReplay  = completeReplay(usis)
  def runOne      = Divider(gameReplay.toList)
  def run(): Unit = { for (_ <- 1 to nb) runOne }

  "playing a game" should {
    "many times" in {
      benchmark(nb, iterations)(run())
    }
  }
}
