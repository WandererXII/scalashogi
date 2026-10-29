package shogi
package format
package usi

import shogi.Pos._

class UsiTest extends ShogiTest {

  "reject an invalid intermediate square" in {
    Usi("7g13a7f") must beNone
    Usi.Move("7g0a7f") must beNone
    Usi.readList("7g13a7f") must beNone
  }

  "require the drop separator" in {
    Usi.Drop("P15e") must beNone
    Usi.Drop("P?5e") must beNone
    Usi.Drop("P5e") must beNone
    Usi.Drop("") must beNone
    Usi.Drop("P*5e").map(_.usi) must beSome("P*5e")
  }

  "read empty and whitespace-separated move lists" in {
    Usi.readList("") must beSome(List.empty[Usi])
    Usi.readList(" \t\n ") must beSome(List.empty[Usi])
    Usi.readList(" 7g7f  3c3d\tP*5e\n") must_== Usi.readList(
      Seq("7g7f", "3c3d", "P*5e"),
    )
    Usi.readList("7g7f invalid 3c3d") must beNone
  }

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
    Usi("6l5k") must beSome.like { case u: Usi.Move =>
      u.orig must_== SQ6L
      u.dest must_== SQ5K
      u.promotion must beFalse
      u.usi must_== "6l5k"
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
