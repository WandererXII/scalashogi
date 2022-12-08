package shogi
package format
package usi

import Pos._

class UsiTest extends ShogiTest {

  "Usi" in {
    Usi("9i1a") must beSome.like { case u: Usi.Move =>
      u.orig must_== SQ9I
      u.dest must_== SQ1A
      u.promotion must beFalse
      u.usi must_== "9i1a"
    }
    // not really valid usi, but we allow it
    Usi("9i1a=") must beSome.like { case u: Usi.Move =>
      u.orig must_== SQ9I
      u.dest must_== SQ1A
      u.promotion must beFalse
      u.usi must_== "9i1a"
    }
    // Definitely not valid usi, but the plan is to use '?' for moves in puzzles, where we don't care about promotions
    Usi("9i1a?") must beSome.like { case u: Usi.Move =>
      u.orig must_== SQ9I
      u.dest must_== SQ1A
      u.promotion must beFalse
      u.usi must_== "9i1a"
    }
    Usi("8h2b+") must beSome.like { case u: Usi.Move =>
      u.orig must_== SQ8H
      u.dest must_== SQ2B
      u.promotion must beTrue
      u.usi must_== "8h2b+"
    }
    Usi("G*8b") must beSome.like { case u: Usi.Drop =>
      u.role must_== Gold
      u.pos must_== SQ8B
      u.usi must_== "G*8b"
    }
    Usi("H*8b") must beNone
    Usi("9a5e1a") must beSome.like { case u: Usi.Move =>
      u.orig must_== SQ9A
      u.dest must_== SQ1A
      u.midStep must_== Some(SQ5E)
      u.promotion must beFalse
      u.usi must_== "9a5e1a"
    }
  }

}
