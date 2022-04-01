package shogi
package format
package usi

import Usi._

class UsiCharPairTest extends ShogiTest {

  "char pair encoding" should {

    def conv(usi: Usi) = UsiCharPair(usi, variant.Standard).toString

    val allMoves = for {
      orig <- Pos.all
      dest <- Pos.all
    } yield Move(orig, dest, false)
    val allPairs = allMoves.map(conv(_))

    "unicity" in {
      allPairs.distinct.size must_== allMoves.size
    }
    "no void char" in {
      allPairs.count(_ contains UsiCharPair.voidChar) must_== 0
    }
  }
}
