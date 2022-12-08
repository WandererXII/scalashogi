package shogi
package format
package usi

class UciToUsiTest extends ShogiTest {

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
