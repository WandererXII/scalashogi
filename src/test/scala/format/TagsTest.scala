package shogi
package format

import java.time.Instant
import java.time.OffsetDateTime

class TagsTest extends ShogiTest {

  "UTC formatters" should {
    "format instants at UTC midnight" in {
      Tag.UTCDate.format.format(Instant.EPOCH) must_== "1970/01/01"
      Tag.UTCTime.format.format(Instant.EPOCH) must_== "00:00:00"
    }

    "convert offset timestamps to UTC across date boundaries" in {
      val timestamp = OffsetDateTime.parse("2026-11-01T00:30:45+02:00")
      Tag.UTCDate.format.format(timestamp) must_== "2026/10/31"
      Tag.UTCTime.format.format(timestamp) must_== "22:30:45"
    }
  }

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
      val date = Tag.UTCDate.format.format(Instant.EPOCH)
      Tags(List(Tag(Tag.UTCDate, date))).year must beSome(1970)
    }

    "leave unknown or malformed years absent" in {
      forall(List("????.??.??", "", "2026", "prefix2026/10/29", "2026/10/29junk")) { date =>
        Tags(List(Tag(Tag.Start, date))).year must beNone
      }
    }
  }
}
