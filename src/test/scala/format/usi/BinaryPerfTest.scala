package shogi
package format
package usi

class BinaryPerfTest extends ShogiTest {

  val usis       = format.usi.Fixtures.prod500standard.map(Usi.readList(_).get)
  val iterations = 15

  def runOne(usis: List[Usi]) =
    Binary.decode(Binary.encode(usis, variant.Standard).toVector, variant.Standard, 600)
  def run(): Unit = { usis foreach runOne }

  "reading a game" should {
    "many times" in {
      benchmark(usis.size, iterations)(run())
    }
  }
}
