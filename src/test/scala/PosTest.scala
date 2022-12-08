package shogi

import Pos._

class PosTest extends ShogiTest {

  "A position" should {

    "boolean directions" in {
      "isLeftOf" in {
        SQ2A.isLeftOf(SQ1A) must beTrue
        SQ9A.isLeftOf(SQ1I) must beTrue
        SQ1A.isLeftOf(SQ2A) must beFalse
        SQ1I.isLeftOf(SQ9A) must beFalse
        SQ1A.isLeftOf(SQ1B) must beFalse
      }
      "isRightOf" in {
        SQ1A.isRightOf(SQ2A) must beTrue
        SQ1I.isRightOf(SQ9A) must beTrue
        SQ2A.isRightOf(SQ1A) must beFalse
        SQ9A.isRightOf(SQ1I) must beFalse
        SQ1A.isRightOf(SQ1B) must beFalse
      }
      "isAbove" in {
        SQ1A.isAbove(SQ1B) must beTrue
        SQ1A.isAbove(SQ9I) must beTrue
        SQ1B.isAbove(SQ1A) must beFalse
        SQ9I.isAbove(SQ1A) must beFalse
        SQ1A.isAbove(SQ2A) must beFalse
      }
      "isBelow" in {
        SQ1B.isBelow(SQ1A) must beTrue
        SQ9I.isBelow(SQ1A) must beTrue
        SQ1A.isBelow(SQ1B) must beFalse
        SQ1A.isBelow(SQ9I) must beFalse
        SQ2A.isBelow(SQ1A) must beFalse
      }
    }

    "project from to" in {
      Pos.findDirection(SQ5E, SQ5D) must beSome.like { dir => dir(SQ5E) must_== Some(SQ5D) }
      Pos.findDirection(SQ5E, SQ4D) must beSome.like { dir => dir(SQ5E) must_== Some(SQ4D) }
      Pos.findDirection(SQ5E, SQ4E) must beSome.like { dir => dir(SQ5E) must_== Some(SQ4E) }
      Pos.findDirection(SQ5E, SQ4F) must beSome.like { dir => dir(SQ5E) must_== Some(SQ4F) }
      Pos.findDirection(SQ5E, SQ5F) must beSome.like { dir => dir(SQ5E) must_== Some(SQ5F) }
      Pos.findDirection(SQ5E, SQ6F) must beSome.like { dir => dir(SQ5E) must_== Some(SQ6F) }
      Pos.findDirection(SQ5E, SQ6E) must beSome.like { dir => dir(SQ5E) must_== Some(SQ6E) }
      Pos.findDirection(SQ5E, SQ6D) must beSome.like { dir => dir(SQ5E) must_== Some(SQ6D) }
      Pos.findDirection(SQ5E, SQ2G) must beNone
    }

    "be used to derive a relative list of positions" in {
      "SQ6F >| false" in { SQ6F >| (_ => false) must contain(SQ5F, SQ4F, SQ3F, SQ2F, SQ1F) }
      "SQ6F |< false" in { SQ6F |< (_ => false) must contain(SQ7F, SQ8F, SQ9F) }
      "SQ6F >| (==SQ3F)" in { SQ6F >| (SQ3F ==) must contain(SQ5F, SQ4F, SQ3F) }
      "SQ6F |< (==SQ7F)" in { SQ6F |< (SQ7F ==) must contain(SQ7F) }
    }

    "be a string" in {
      "SQ6E" in { SQ6E.toString must_== "6e" }
    }

    "USI" in {
      "be correctly converted" in {
        SQ9I.key must_== "9i"
        SQ9A.key must_== "9a"
        SQ1A.key must_== "1a"
        SQ1I.key must_== "1i"
        SQ5E.key must_== "5e"
        SQ5G.key must_== "5g"
        SQ5C.key must_== "5c"
        SQ6E.key must_== "6e"
        SQ4E.key must_== "4e"

        SQ1A.kanjiKey must_== "1一"
        SQ10A.kanjiKey must_== "10一"
        SQ10L.kanjiKey must_== "10十二"

        SQ1A.kanjiFullWidthKey must_== "１一"
        SQ10A.kanjiFullWidthKey must_== "１０一"
        SQ10L.kanjiFullWidthKey must_== "１０十二"

      }
    }

    "all" in {
      Pos.all.map(_.key).map(k => Pos.fromKey(k).get) must_== Pos.all
      Pos.all.map(_.kanjiKey).map(k => Pos.fromKey(k).get) must_== Pos.all
      Pos.all.map(_.kanjiFullWidthKey).map(k => Pos.fromKey(k).get) must_== Pos.all
    }

    "board sizes" in {
      "12x12" in {
        Pos.all must have size 144
      }
      "9x9" in {
        (SQ9I upTo SQ1A).toList must have size 81
        (SQ9I upTo SQ1A).toList must contain(
          exactly(
            SQ9I,
            SQ8I,
            SQ7I,
            SQ6I,
            SQ5I,
            SQ4I,
            SQ3I,
            SQ2I,
            SQ1I,
            SQ9H,
            SQ8H,
            SQ7H,
            SQ6H,
            SQ5H,
            SQ4H,
            SQ3H,
            SQ2H,
            SQ1H,
            SQ9G,
            SQ8G,
            SQ7G,
            SQ6G,
            SQ5G,
            SQ4G,
            SQ3G,
            SQ2G,
            SQ1G,
            SQ9F,
            SQ8F,
            SQ7F,
            SQ6F,
            SQ5F,
            SQ4F,
            SQ3F,
            SQ2F,
            SQ1F,
            SQ9E,
            SQ8E,
            SQ7E,
            SQ6E,
            SQ5E,
            SQ4E,
            SQ3E,
            SQ2E,
            SQ1E,
            SQ9D,
            SQ8D,
            SQ7D,
            SQ6D,
            SQ5D,
            SQ4D,
            SQ3D,
            SQ2D,
            SQ1D,
            SQ9C,
            SQ8C,
            SQ7C,
            SQ6C,
            SQ5C,
            SQ4C,
            SQ3C,
            SQ2C,
            SQ1C,
            SQ9B,
            SQ8B,
            SQ7B,
            SQ6B,
            SQ5B,
            SQ4B,
            SQ3B,
            SQ2B,
            SQ1B,
            SQ9A,
            SQ8A,
            SQ7A,
            SQ6A,
            SQ5A,
            SQ4A,
            SQ3A,
            SQ2A,
            SQ1A
          )
        )
      }
      "5x5" in {
        (SQ5E upTo SQ1A).toList must have size 25
        (SQ5E upTo SQ1A).toList must contain(
          exactly(
            SQ5E,
            SQ4E,
            SQ3E,
            SQ2E,
            SQ1E,
            SQ5D,
            SQ4D,
            SQ3D,
            SQ2D,
            SQ1D,
            SQ5C,
            SQ4C,
            SQ3C,
            SQ2C,
            SQ1C,
            SQ5B,
            SQ4B,
            SQ3B,
            SQ2B,
            SQ1B,
            SQ5A,
            SQ4A,
            SQ3A,
            SQ2A,
            SQ1A
          )
        )
      }
    }
  }
}
