package shogi
package format

class TagsTest extends ShogiTest {

  "year" should {
    "accept dates and timestamps used by notation tags" in {
      forall(
        List(
          "2026.10.29",
          "2026/10/29",
          "2026-10-29",
          "2026/10/29 12:34:56",
          "2026-10-29T12:34:56.000Z",
          "2026.??.??",
        ),
      ) { date =>
        forall(List(Tag.Start, Tag.End, Tag.UTCDate)) { tagType =>
          Tags(List(Tag(tagType, date))).year must beSome(2026)
        }
      }
    }

    "accept the UTC date formatter's output" in {
      val date = Tag.UTCDate.format.print(0L)
      Tags(List(Tag(Tag.UTCDate, date))).year must beSome(1970)
    }

    "leave unknown or malformed years absent" in {
      forall(List("????.??.??", "", "2026", "prefix2026/10/29", "2026/10/29junk")) { date =>
        Tags(List(Tag(Tag.Start, date))).year must beNone
      }
    }
  }
}
