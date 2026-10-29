package shogi
package format
package usi

class UciToUsiTest extends ShogiTest {

  "read empty and whitespace-separated move lists" in {
    UciToUsi.readList("") must beSome(List.empty[Usi])
    UciToUsi.readList(" \t\n ") must beSome(List.empty[Usi])
    UciToUsi.readList(" a1a2  b2h8+\tP*e5\n") must_== UciToUsi.readList(
      Seq("a1a2", "b2h8+", "P*e5"),
    )
    UciToUsi.readList("a1a2 invalid b2h8+") must beNone
  }

  "Uci to usi" in {
    UciToUsi("a1a2") must beSome.like { case u =>
      u.usi must_== "9i9h"
    }
    UciToUsi("b2h8+") must beSome.like { case u =>
      u.usi must_== "8h2b+"
    }
    UciToUsi("P*e5") must beSome.like { case u =>
      u.usi must_== "P*5e"
    }
  }

}
