package shogi

class DividerPerfTest extends ShogiTest {

  // args(skipAll = true)

  val nb         = 500
  val iterations = 10
  // val nb = 1
  // val iterations = 1

  val usis        = shogi.format.usi.Usi.readList(format.usi.Fixtures.fromProd2).get
  val gameReplay  = Replay.situations(usis, None, variant.Standard).toOption.get
  def runOne      = Divider(gameReplay.toList)
  def run(): Unit = { for (_ <- 1 to nb) runOne }

  "playing a game" should {
    "many times" in {
      // runOne.end must beSome.like {
      //   case x => x must beBetween(65, 80)
      // }
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
