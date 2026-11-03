package shogi
package format
package usi

class UsiPerfTest extends ShogiTest {

  val usis       = format.usi.Fixtures.prod500standard
  val iterations = 15

  def runOne(usisStr: String) =
    Usi.readList(usisStr).get
  def run(): Unit = { usis foreach runOne }

  "reading a game" should {
    "many times" in {
      benchmark(usis.size, iterations)(run())
    }
  }
}
