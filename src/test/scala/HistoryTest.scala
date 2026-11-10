package shogi

class HistoryTest extends ShogiTest {

  def toHash(a: Int)                    = Array(a.toByte, 0.toByte, 0.toByte)
  def makeHistory(positions: List[Int]) =
    (positions map toHash).foldLeft(History.empty) { case (history, hash) =>
      history.copy(positionHashes = hash ++ history.positionHashes)
    }
  "fourfold repetition" should {
    "empty history" in {
      History.empty.fourfoldRepetition must beFalse
    }
    "not 4 same elements" in {
      val history = makeHistory(List(1, 2, 3, 4, 5, 2, 5, 6, 16, 2, 23, 55))
      history.fourfoldRepetition must beFalse
    }
    "not 4 elements same to the last one" in {
      val history = makeHistory(List(1, 2, 3, 4, 5, 2, 5, 6, 23, 2, 55, 2, 33))
      history.fourfoldRepetition must beFalse
    }
    "positive" in {
      val history = makeHistory(List(1, 2, 3, 4, 5, 6, 7, 2, 5, 6, 3, 2, 6, 2))
      history.fourfoldRepetition must beTrue
    }
    "two-ply cycles" in {
      makeHistory(List(1, 2, 1, 2, 1)).fourfoldRepetition must beFalse
      makeHistory(List(1, 2, 1, 2, 1, 2, 1)).fourfoldRepetition must beTrue
    }
    "ignore positions with the other side to play" in {
      makeHistory(List(1, 2, 1, 2, 1, 2, 2)).fourfoldRepetition must beFalse
    }
  }
  "threefold repetition" should {
    "two-ply cycles" in {
      makeHistory(List(1, 2, 1)).threefoldRepetition must beFalse
      makeHistory(List(1, 2, 1, 2, 1)).threefoldRepetition must beTrue
    }
  }
  "repetition distance" should {
    "no repetition" in {
      val history = makeHistory(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
      history.firstRepetitionDistance must_== None
    }
    "half" in {
      val history = makeHistory(List(0, 1, 2, 3, 4, 5, 12, 7, 8, 9, 10, 11, 12))
      history.firstRepetitionDistance must_== Some(3)
    }
    "last" in {
      val history = makeHistory(List(12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
      history.firstRepetitionDistance must_== Some(6)
    }
  }
  "equality" should {
    "hold for empty histories" in {
      (History.empty must_== History.empty) and
        (History.empty.hashCode must_== History.empty.hashCode)
    }
    "compare position hashes by content" in {
      val first  = makeHistory(List(1, 2, 3))
      val second = makeHistory(List(1, 2, 3))
      (first must_== second) and (first.hashCode must_== second.hashCode)
    }
    "distinguish different position hashes" in {
      makeHistory(List(1, 2, 3)) must_!= makeHistory(List(1, 2, 4))
    }
  }
}
