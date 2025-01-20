package shogi
package format
package csa

class CsaParserTest extends ShogiTest {

  import shogi.format.csa.CsaFixtures._

  val parser                 = CsaParser.full _
  def parseStep(str: String) = CsaParser.StepParser(str)

  "drop" in {
    parseStep("-0077FU") must beValid.like { case d: Drop =>
      d.role must_== Pawn
      d.pos must_== Pos.SQ7G
    }
  }

  "move" in {
    parseStep("5948OU") must beValid.like { case m: CsaMove =>
      m.dest must_== Pos.SQ4H
      m.orig must_== Pos.SQ5I
      m.role must_== King
    }
  }

  "basic" should {
    "move" in {
      parser("PI,+5948OU") must beValid.like { case p =>
        p.parsedSteps.value.headOption must beSome.like { case m: CsaMove =>
          m.dest must_== Pos.SQ4H
          m.orig must_== Pos.SQ5I
          m.role must_== King
        }
      }
    }
    "drop" in {
      parser("PI,0077KI") must beValid.like { case p =>
        p.parsedSteps.value.headOption must beSome.like { case d: Drop =>
          d.role must_== Gold
          d.pos must_== Pos.SQ7G
        }
      }
    }
    "moves with numbers clock info" in {
      parser("""PI,
      +7776FU,T12
      -8384FU,T5
      """) must beValid.like { case p =>
        p.parsedSteps.value.lastOption must beSome.like { case m: CsaMove =>
          m.dest must_== Pos.SQ8D
          m.orig must_== Pos.SQ8C
          m.role must_== Pawn
          m.metas.timeSpent must beSome.like { case c: Centis =>
            c must_== Centis(500)
          }
        }
      }
    }
  }

  "tags" should {
    "one tag" in {
      parser("""PI,$SITE:KAZUSA ARC
      -8384FU,T5""") must beValid.like { case p =>
        p.tags.value.size must_== 1
        p.tags.value must contain { (tag: Tag) =>
          tag.name == Tag.Site && tag.value == """KAZUSA ARC"""
        }
      }
    }
    "name tag" in {
      parser("""PI,N-Me
      -8384FU,T5""") must beValid.like { case p =>
        p.tags.value must contain { (tag: Tag) =>
          tag.name == Tag.Gote && tag.value == """Me"""
        }
      }
    }
    "multiple tags" in {
      parser("""
        PI
        N-Me
        $SITE:lishogi
        N+Also me
        -8384FU
      """) must beValid.like { case p =>
        p.tags.value must contain { (tag: Tag) =>
          tag.name == Tag.Gote && tag.value == """Me"""
        }
        p.tags.value must contain { (tag: Tag) =>
          tag.name == Tag.Sente && tag.value == """Also me"""
        }
        p.tags.value must contain { (tag: Tag) =>
          tag.name == Tag.Site && tag.value == """lishogi"""
        }
      }
    }
    "empty tag is ignored" in {
      parser("""PI
      N-
      -8384FU""") must beValid.like { case p =>
        p.tags.value must not(contain { (tag: Tag) =>
          tag.name == Tag.Gote
        })
      }
    }
    "empty tag with another nonempty tag" in {
      parser("""
        PI,
        N-
        N+NOPE
        -8384FU
      """) must beValid.like { case p =>
        p.tags.value must contain { (tag: Tag) =>
          tag.name == Tag.Sente && tag.value == """NOPE"""
        }
      }
    }
  }

  "comments" should {
    "multiple comments" in {
      parser("""PI,+8483FU
      'such a neat comment
      ' one more, keep com,ma
      '
      ' drop P*5e""") must beValid.like {
        case ParsedNotation(ParsedSteps(List(step)), _, _, _, _) =>
          step.metas.comments must_== List(
            "such a neat comment",
            "one more, keep com,ma",
            "drop P*5e",
          )
      }
    }
    "termination comments" in {
      parser("""PI
      +8483FU
      'such a neat comment
      ' one more
      %TORYO,T3
      'comment on termination?""") must beValid.like {
        case ParsedNotation(ParsedSteps(List(step)), _, _, _, _) =>
          step.metas.comments must_== List(
            "such a neat comment",
            "one more",
            "comment on termination?",
          )
      }
    }
    "comments in header" in {
      parser("""'HEADER COMMENT
      'HEADER2
      $SITE:lishogi
      PI33FU
      ' H3
      +8483FU
      'Something comments
      +8483FU
      %CHUDAN""") must beValid.like {
        case ParsedNotation(_, initialSfen, _, InitialPosition(init), Tags(tags)) =>
          init must_== List("HEADER COMMENT", "HEADER2", "H3")
          initialSfen must beSome
          tags.size must_== 2
      }
    }
  }

  "from initial board" in {
    parser("""V2.2
N+鈴木大介 九段
N-深浦康市 九段
$EVENT:王座戦
$SITE:東京・将棋会館
$START:2017-03-22T01:00:00.000Z
$OPENING:中飛車
P1-KY-KE-GI-KI-OU-KI-GI-KE-KY
P2 * -HI *  *  *  *  * -KA * 
P3-FU-FU-FU-FU-FU-FU-FU-FU-FU
P4 *  *  *  *  *  *  *  *  * 
P5 *  *  *  *  *  *  *  *  * 
P6 *  *  *  *  *  *  *  *  * 
P7+FU+FU+FU+FU+FU+FU+FU+FU+FU
P8 * +KA *  *  *  *  * +HI * 
P9+KY+KE+GI+KI+OU+KI+GI+KE+KY
+
+7776FU
-8384FU
    """) must beValid.like { case ParsedNotation(_, initialSfen, _, _, Tags(tags)) =>
      tags.size must_== 6
      initialSfen must beNone
      tags must contain { (tag: Tag) =>
        tag.name == Tag.Sente && tag.value == "鈴木大介 九段"
      }
      tags must contain { (tag: Tag) =>
        tag.name == Tag.Gote && tag.value == "深浦康市 九段"
      }
      tags must contain { (tag: Tag) =>
        tag.name == Tag.Site && tag.value == "東京・将棋会館"
      }
    }
  }

  "csa fixture 1" in {
    parser(csa1) must beValid.like { case ParsedNotation(ParsedSteps(ps), _, _, _, Tags(tags)) =>
      ps.size must_== 111
      tags.size must_== 8
    }
  }

  "csa fixture 2" in {
    parser(csa2) must beValid.like { case ParsedNotation(ParsedSteps(ps), _, _, _, Tags(tags)) =>
      ps.size must_== 258
      tags.size must_== 4
    }
  }

}
