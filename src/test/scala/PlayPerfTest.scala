package shogi

class PlayPerfTest extends ShogiTest {

  // args(skipAll = true)

  val nb         = 100
  val iterations = 10
  // val nb = 1
  // val iterations = 1

  def runOne =
    makeGame(shogi.variant.Standard).playUsisStr(
      List(
        "7g7f",
        "8c8d",
        "7i6h",
        "3c3d",
        "6h7g",
        "7a6b",
        "2g2f",
        "3a4b",
        "3i4h",
        "4a3b",
        "6i7h",
        "5a4a",
      ),
    )
  def run(): Unit = { for (_ <- 1 to nb) runOne }

  "playing a game" should {
    "many times" in {
      runOne must beValid
      if (nb * iterations > 1) {
        println("warming up")
        run()
      }
      println("running tests")
      val durations = for (_ <- 1 to iterations) yield {
        val start = System.currentTimeMillis
        run()
        val duration = System.currentTimeMillis - start
        println(s"$nb games in $duration ms")
        duration
      }
      val nbGames       = iterations * nb
      val microsPerGame = (1000 * durations.sum) / nbGames
      println(s"Average = $microsPerGame microseconds per game")
      println(s"          ${1000000 / microsPerGame} games per second")
      true === true
    }
  }
}
