package shogi
package format
package kif

class KifParserTest extends ShogiTest {

  import shogi.format.kif.KifFixtures._

  val parser                                               = KifParser.full _
  def parseStep(str: String, lastDest: Option[Pos] = None) =
    KifParser.MoveDropParser(str, lastDest, None, shogi.variant.Standard)
  def parseChushogiMove(
      str: String,
      lastDest: Option[Pos] = None,
      firstLionOrig: Option[Pos] = None,
  ) =
    KifParser.MoveDropParser(str, lastDest, firstLionOrig, shogi.variant.Chushogi)

  "drop" in {
    parseStep("６四歩打") must beValid.like { case d: Drop =>
      d.role must_== Pawn
      d.pos must_== Pos.SQ6D
    }
  }

  "move" in {
    parseStep("７七金(78)") must beValid.like { case m: KifMove =>
      m.dest must_== Pos.SQ7G
      m.orig must_== Pos.SQ7H
      m.roles.head must_== Gold
      m.promotion must_== false
    }
  }

  "chushogi move" in {
    parseChushogiMove("2八龍馬 (←4十)") must beValid.like { case m: KifMove =>
      m.dest must_== Pos.SQ2H
      m.orig must_== Pos.SQ4J
      m.roles.head must_== Horse
      m.roles.tail must_== List(HorsePromoted)
      m.promotion must_== false
    }
  }

  "chushogi second lion move" in {
    parseChushogiMove("2八龍馬 (←4十)", None, Some(Pos.SQ5E)) must beValid.like { case m: KifMove =>
      m.orig must_== Pos.SQ5E
      m.midStep must_== Some(Pos.SQ4J)
      m.dest must_== Pos.SQ2H
      m.roles.head must_== Horse
      m.roles.tail must_== List(HorsePromoted)
      m.promotion must_== false
    }
  }

  "basic" should {
    "move" in {
      parser("☗７七金(78)") must beValid.like { case p =>
        p.parsedSteps.value.headOption must beSome.like { case m: KifMove =>
          m.dest must_== Pos.SQ7G
          m.orig must_== Pos.SQ7H
          m.roles.head must_== Gold
          m.promotion must_== false
        }
      }
    }
    "drop" in {
      parser("７四歩打") must beValid.like { case p =>
        p.parsedSteps.value.headOption must beSome.like { case d: Drop =>
          d.role must_== Pawn
          d.pos must_== Pos.SQ7D
        }
      }
    }
    "drop sfen" in {
      parser("７四P打") must beValid.like { case p =>
        p.parsedSteps.value.headOption must beSome.like { case d: Drop =>
          d.role must_== Pawn
          d.pos must_== Pos.SQ7D
        }
      }
    }
    "move with number" in {
      parser("1 ７七G(78)") must beValid.like { case p =>
        p.parsedSteps.value.headOption must beSome.like { case m: KifMove =>
          m.dest must_== Pos.SQ7G
          m.orig must_== Pos.SQ7H
          m.roles.head must_== Gold
          m.promotion must_== false
        }
      }
    }
    "move with number and a dot" in {
      parser("42. ７7金(78)") must beValid.like { case p =>
        p.parsedSteps.value.headOption must beSome.like { case m: KifMove =>
          m.dest must_== Pos.SQ7G
          m.orig must_== Pos.SQ7H
          m.roles.head must_== Gold
          m.promotion must_== false
        }
      }
    }
    "move with unconventional position string" in {
      parser("42. ７g金(7h)") must beValid.like { case p =>
        p.parsedSteps.value.headOption must beSome.like { case m: KifMove =>
          m.dest must_== Pos.SQ7G
          m.orig must_== Pos.SQ7H
          m.roles.head must_== Gold
          m.promotion must_== false
        }
      }
    }
    "moves with numbers clock info" in {
      parser("""
      1 ７六歩(77) (0:12/0:0:12)
      2 ７六飛(77) (0:12/)
      """) must beValid.like { case p =>
        p.parsedSteps.value.lastOption must beSome.like { case m: KifMove =>
          m.dest must_== Pos.SQ7F
          m.orig must_== Pos.SQ7G
          m.roles.head must_== Rook
          m.promotion must_== false
        }
      }
    }
  }

  "promotion" should {
    "as a true" in {
      parser("3 ２二角成(88) ") must beValid.like { case p =>
        p.parsedSteps.value.headOption must beSome.like { case m: KifMove =>
          m.promotion must_== true
        }
      }
    }
    "as a false" in {
      parser("3 ２二角不成(88) ") must beValid.like { case p =>
        p.parsedSteps.value.headOption must beSome.like { case m: KifMove =>
          m.promotion must_== false
        }
      }
    }
  }

  "同" should {
    "simple move with 同" in {
      parseStep("同 金(78)", Some(Pos.SQ7G)) must beValid.like { case m: KifMove =>
        m.dest must_== Pos.SQ7G
        m.orig must_== Pos.SQ7H
        m.roles.head must_== Gold
        m.promotion must_== false
      }
    }
    "from last move" in {
      parser("""
      1 ７六歩(77) (0:12/0:0:12)
      2 同 飛(77) (0:12/0:0:12)
      """) must beValid.like { case p =>
        p.parsedSteps.value.lastOption must beSome.like { case m: KifMove =>
          m.dest must_== Pos.SQ7F
          m.orig must_== Pos.SQ7G
          m.roles.head must_== Rook
          m.promotion must_== false
        }
      }
    }
    "consecutive moves" in {
      parser("""
      1 ７六歩(77) (0:12/0:0:12)
      2 同 飛(72) (0:12/0:0:12)
      3 同 角(55) (0:12/0:0:12)
      """) must beValid.like { case p =>
        p.parsedSteps.value must haveSize(3)
        p.parsedSteps.value.lastOption must beSome.like { case m: KifMove =>
          m.dest must_== Pos.SQ7F
          m.orig must_== Pos.SQ5E
          m.roles.head must_== Bishop
          m.promotion must_== false
        }
      }
    }
  }
  "tags" should {
    "one tag" in {
      parser("""後手：  Me
      1 ７六歩(77)""") must beValid.like { case p =>
        p.tags.value must contain { (tag: Tag) =>
          tag.name == Tag.Gote && tag.value == """Me"""
        }
      }
    }
    "multiple tags" in {
      parser("""
        後手：俺
        Sente: Also me
        1 ７六歩(77)
      """) must beValid.like { case p =>
        p.tags.value must contain { (tag: Tag) =>
          tag.name == Tag.Gote && tag.value == """俺"""
        }
        p.tags.value must contain { (tag: Tag) =>
          tag.name == Tag.Sente && tag.value == """Also me"""
        }
      }
    }
    "empty tag is ignored" in {
      parser("""後手：
      1 ７六歩(77)""") must beValid.like { case p =>
        p.tags.value.size must_== 0
      }
    }
    "empty tag with another nonempty tag" in {
      parser("""
        後手：
        Sente: NOPE
        1 ７六歩(77)
      """) must beValid.like { case p =>
        p.tags.value must contain { (tag: Tag) =>
          tag.name == Tag.Sente && tag.value == """NOPE"""
        }
      }
    }
  }

  "comments" should {
    "comment after step" in {
      parser("""７7金(78) *▲7八歩兵 - what a comment""") must beValid.like {
        case ParsedNotation(ParsedSteps(List(step)), _, _, _, Tags(tags)) =>
          step.metas.comments must_== List("▲7八歩兵 - what a comment")
          tags must beEmpty
      }
    }
    "multiple comments" in {
      parser("""７7金(78)
      *such a neat comment
      * one more
      *
      * drop P*5e""") must beValid.like {
        case ParsedNotation(ParsedSteps(List(step)), _, _, _, _) =>
          step.metas.comments must_== List("such a neat comment", "one more", "drop P*5e")
      }
    }
    "termination comments" in {
      parser("""1 ７7金(78)
      *such a neat comment
      * one more
      2 投了 ( 0:03/ )
      * comment on termination?""") must beValid.like {
        case ParsedNotation(ParsedSteps(List(step)), _, _, _, _) =>
          step.metas.comments must_== List(
            "such a neat comment",
            "one more",
            "comment on termination?",
          )
      }
    }
    "comments in header" in {
      parser("""*HEADER COMMENT
      後手：俺
      *HEADER2
      手合割：平手
      手数----指手---------消費時間--
      * H3
      1 ７六歩(77)
      * Something comments
      2 ３四歩(33)
      3 投了""") must beValid.like { case ParsedNotation(_, _, _, InitialPosition(init), _) =>
        init must_== List("HEADER COMMENT", "HEADER2", "H3")
      }
    }
    "multiple comments" in {
      parser("""７7金(78)
      * comment with hash # <- works
      *
      * or amp &, or something""") must beValid.like {
        case ParsedNotation(ParsedSteps(List(step)), _, _, _, _) =>
          step.metas.comments must_== List("comment with hash # <- works", "or amp &, or something")
      }
    }
  }

  "times" should {
    "both times" in {
      parser("""29 ４八玉(59) (2:25/0:8:23)""") must beValid.like {
        case ParsedNotation(ParsedSteps(List(step)), _, _, _, _) =>
          step.metas.timeSpent must_== Some(Centis(14500))
          step.metas.timeTotal must_== Some(Centis(50300))
      }
    }
    "both times with spaces" in {
      parser("""29 ４八玉(59) ( 2:25 / 0:8:23 )""") must beValid.like {
        case ParsedNotation(ParsedSteps(List(step)), _, _, _, _) =>
          step.metas.timeSpent must_== Some(Centis(14500))
          step.metas.timeTotal must_== Some(Centis(50300))
      }
    }
    "fractional time" in {
      parser("""29 ４八玉(59) (2:25.006/0:8:23.006)""") must beValid.like {
        case ParsedNotation(ParsedSteps(List(step)), _, _, _, _) =>
          step.metas.timeSpent must_== Some(Centis(14501))
          step.metas.timeTotal must_== Some(Centis(50301))
      }
    }
    "only step time" in {
      parser("""29 ４八玉(59) ( 2:25/)""") must beValid.like {
        case ParsedNotation(ParsedSteps(List(step)), _, _, _, _) =>
          step.metas.timeSpent must_== Some(Centis(14500))
          step.metas.timeTotal must_== None
      }
    }
    "no time" in {
      parser("""29 ４八玉(59)""") must beValid.like {
        case ParsedNotation(ParsedSteps(List(step)), _, _, _, _) =>
          step.metas.timeSpent must_== None
          step.metas.timeTotal must_== None
      }
    }
    "ignore + at the end?" in {
      parser("""29 ４八玉(59) (2:25/0:8:23)+""") must beValid.like {
        case ParsedNotation(ParsedSteps(List(step)), _, _, _, _) =>
          step.metas.timeSpent must_== Some(Centis(14500))
          step.metas.timeTotal must_== Some(Centis(50300))
      }
    }
    "ignore zero times" in {
      parser("""29 ４八玉(59) (0:00/0:0:00)""") must beValid.like {
        case ParsedNotation(ParsedSteps(List(step)), _, _, _, _) =>
          step.metas.timeSpent must_== None
          step.metas.timeTotal must_== None
      }
    }
  }

  "properly ignore comments, bookmarks and まで..." in {
    parser("""
      後手：
      # Ignore comments
      ## double comments
      # comments in # comments
      & and bookmarks # This is not necessary
      &
      #
##&
      先手：先手 # Mid-line
      1 ７六歩(77) (00:00/00:00:00)
      まで122手で中断
    """) must beValid.like { case p =>
      p.tags.value.size must_== 1
      p.tags.value must contain { (tag: Tag) =>
        tag.name == Tag.Sente && tag.value == "先手"
      }
    }
  }

  "from position board" in {
    parser("""
      後手：
      後手の持駒：金四　銀二　香三　歩十三
        ９ ８ ７ ６ ５ ４ ３ ２ １
      +---------------------------+
      | ・ ・ ・v桂 ・ ・ ・ ・ ・|一
      |v玉 角v歩 馬 ・ ・ ・ ・ ・|二
      | ・ ・ ・ ・ ・ ・ ・ ・ ・|三
      | 桂 ・ ・v歩 ・ ・ ・ ・ ・|四
      |vとv桂 ・ ・v歩 ・ ・ ・ ・|五
      | ・ ・ 飛 ・v全 ・ ・ ・ ・|六
      |v歩 桂 ・ ・ ・ ・ ・ ・ ・|七
      | ・ 香 ・ ・ ・ ・ ・ ・ ・|八
      | ・v銀 ・ ・ 龍 ・ ・ ・ ・|九
      +---------------------------+
      先手：先手
      先手の持駒：なし
    """) must beValid.like { case p =>
      p.tags.value.size must_== 1
      p.initialSfen must beSome.like { sfen =>
        sfen.value == "3n5/kBp+B5/9/N2p5/+pn2p4/2R1+s4/pN7/1L7/1s2+R4 b 4g2s3l13p"
      }
      p.tags.value must contain { (tag: Tag) =>
        tag.name == Tag.Sente && tag.value == "先手"
      }
    }
  }

  "from handicap" in {
    parser("""
      手合割：歩三兵
      先手：
      後手：
      手数----指手---------消費時間--
      1 ７六歩(77) (00:00/00:00:00)
    """) must beValid.like { case p =>
      p.initialSfen must beSome.like { sfen =>
        sfen.value == "4k4/9/9/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w 3p"
      }
    }
  }

  "from standard position" in {
    parser("""
      手合割：平手
      先手：
      後手：
      手数----指手---------消費時間--
      1 ７六歩(77) (00:00/00:00:00)
    """) must beValid.like { case p =>
      p.initialSfen must beNone
    }
  }

  "variations" should {
    "first step variation" in {
      parser("""
        後手：俺
        Sente: Also me

        手数----指手---------消費時間--
          1 ７六歩(77)        ( 0:00/00:00:00)
        変化：1手
          1 ２六歩(27)        ( 0:00/00:00:00)
      """) must beValid.like { case ParsedNotation(ParsedSteps(List(step)), _, _, _, _) =>
        step.metas.variations.headOption must beSome.like { case variation =>
          variation.value must haveSize(1)
        }
      }
    }
    "nested variations" in {
      parser("""
    手合割：平手
    先手：
    後手：
    手数----指手---------消費時間--
      1 ７六歩(77)        ( 0:00/00:00:00)
      2 ３四歩(33)        ( 0:00/00:00:00)
      3 ２二角成(88)       ( 0:00/00:00:00)
      4 同　銀(31)        ( 0:00/00:00:00)
      5 ２六歩(27)        ( 0:00/00:00:00)
      6 １四歩(13)        ( 0:00/00:00:00)
      7 １六歩(17)        ( 0:00/00:00:00)

    変化：6手
      6 ５四歩(53)        ( 0:00/00:00:00)
      7 ９六歩(97)        ( 0:00/00:00:00)

    変化：5手
      5 １六歩(17)        ( 0:00/00:00:00)

    変化：2手
      2 ８四歩(83)        ( 0:00/00:00:00)
      3 ８六歩(87)        ( 0:00/00:00:00)
      4 ４二玉(51)        ( 0:00/00:00:00)
      5 ４八玉(59)        ( 0:00/00:00:00)
      6 ４四歩(43)        ( 0:00/00:00:00)

    変化：6手
      6 １四歩(13)        ( 0:00/00:00:00)

    変化：4手
      4 ３四歩(33)        ( 0:00/00:00:00)

    変化：1手
      1 ２六歩(27)        ( 0:00/00:00:00)

    変化：1手
      1 ３六歩(37)        ( 0:00/00:00:00)
      """) must beValid.like { case ParsedNotation(ParsedSteps(parsedSteps), _, _, _, _) =>
        parsedSteps(5).metas.variations.headOption must beSome.like { case v =>
          v.value must haveSize(2)
        }
        parsedSteps(4).metas.variations.headOption must beSome.like { case v =>
          v.value must haveSize(1)
        }
        parsedSteps(1).metas.variations.headOption must beSome.like { case v =>
          v.value must haveSize(5)
          v.value(4).metas.variations.headOption must beSome.like { case vv =>
            vv.value must haveSize(1)
          }
          v.value(2).metas.variations.headOption must beSome.like { case vv =>
            vv.value must haveSize(1)
          }
        }
        parsedSteps(0).metas.variations must haveSize(2)
        parsedSteps(0).metas.variations.headOption must beSome.like { case v =>
          v.value must haveSize(1)
        }
        parsedSteps(0).metas.variations.lastOption must beSome.like { case v =>
          v.value must haveSize(1)
        }
      }
    }
  }

  "ending" should {
    "find end" in {
      parser("""
        # ---- Kifu for Windows95 V3.53 棋譜ファイル ----
        開始日時：1999/07/15(木) 19:07:12
        終了日時：1999/07/15(木) 19:07:17
        手合割：平手
        先手：先手の対局者名
        後手：後手の対局者名
        手数----指手---------消費時間-- # この行は、なくてもいい
        1 ７六歩(77) ( 0:16/ 00:00:00)
        2 ３四歩(33) ( 0:00/)
        3 投了 ( 0:03/ )
        4 中断 ( 0:03/ )
      """.trim) must beValid.like { case ParsedNotation(ParsedSteps(parsedSteps), _, _, _, tags) =>
        parsedSteps must haveSize(2)
        tags.value must contain { (tag: Tag) =>
          tag.name == Tag.Termination && tag.value == "投了"
        }
        tags.value must contain { (tag: Tag) =>
          tag.name == Tag.Result && tag.value == "0-1"
        }
      }
    }
  }

  "accept also reverse variation order 1 depth" in {
    parser("""
手合割：平手
先手：
後手：

手数----指手----消費時間--
   1 ３六歩(37)
   2 ４二玉(51)
   3 ３五歩(36)
   4 ３二玉(42)
   5 ３四歩(35)
   6 ４二銀(31)
   7 ３三歩成(34)
   8 同　桂(21)

変化：1手
   1 ７六歩(77)
   2 ４二玉(51)
   3 １六歩(17)

変化：3手
   3 ９六歩(97)
   4 ３二玉(42)

変化：5手
   5 ７六歩(77)

変化：7手
   7 ３三歩(34)
    """) must beValid.like { case ParsedNotation(ParsedSteps(parsedSteps), _, _, _, _) =>
      parsedSteps(0).metas.variations.headOption must beSome.like { case v =>
        v.value must haveSize(3)
        v.value(2).metas.variations.headOption must beSome.like { case vv =>
          vv.value must haveSize(2)
        }
      }
      parsedSteps(2).metas.variations.headOption must beNone
      parsedSteps(4).metas.variations.headOption must beSome.like { case v =>
        v.value must haveSize(1)
      }
      parsedSteps(6).metas.variations.headOption must beSome.like { case v =>
        v.value must haveSize(1)
      }
    }
  }

  "kif fixture 1" in {
    parser(kif1) must beValid.like { case ParsedNotation(ParsedSteps(ps), _, v, _, tags) =>
      ps.size must_== 111
      tags.value.size must_== 9
      v.standard must beTrue
    }
  }

  "kif fixture 2" in {
    parser(kif2) must beValid.like { case ParsedNotation(ParsedSteps(ps), _, v, _, tags) =>
      ps.size must_== 193
      tags.value.size must_== 8
      v.standard must beTrue
    }
  }

  "kif fixture 3" in {
    parser(kif3) must beValid.like {
      case ParsedNotation(ParsedSteps(ps), initialSfen, v, _, tags) =>
        ps.size must_== 117
        tags.value.size must_== 9
        initialSfen must beSome
        v.standard must beTrue
    }
  }

  "kif fixture 4" in {
    parser(kif4) must beValid.like { case ParsedNotation(ParsedSteps(ps), _, v, _, _) =>
      ps.size must_== 223
      v.standard must beTrue
    }
  }

  "kif fixture 8" in {
    parser(kif8) must beValid.like { case ParsedNotation(ParsedSteps(ps), _, v, _, _) =>
      ps.size must_== 168
      v.standard must beTrue
    }
  }

  "kif fixture 9" in {
    parser(kif9) must beValid.like { case ParsedNotation(ParsedSteps(ps), _, v, _, _) =>
      ps.size must_== 85
      v.standard must beTrue
    }
  }

  "kif fixture 12" in {
    parser(kif12) must beValid.like { case ParsedNotation(ParsedSteps(ps), _, v, _, _) =>
      ps.size must_== 121
      v.standard must beTrue
    }
  }

  "kif fixture 16" in {
    parser(kif16) must beValid.like { case ParsedNotation(ParsedSteps(ps), _, v, _, _) =>
      ps.size must_== 37
      v.standard must beTrue
    }
  }

  "kif fixture 17" in {
    parser(kif17) must beValid
  }

  "minishogi" in {
    parser("""
      先手：先手
      手合割：五々将棋
    """) must beValid.like { case p =>
      p.initialSfen must beNone
      p.variant.minishogi must beTrue
    }
  }

  "alternative minishogi handicap" in {
    parser("""
      先手：先手
      手合割：5五将棋
    """) must beValid.like { case p =>
      p.initialSfen must beNone
      p.variant.minishogi must beTrue
    }
  }

  "minishogi from position" in {
    parser("""
      後手：
      後手の持駒：なし
  ５ ４ ３ ２ １
+---------------+
|v飛v角v銀v金v玉|一
| ・ ・ ・ ・v歩|二
| 歩 ・ ・ ・ ・|三
| ・ ・ ・ ・ ・|四
| 玉 金 銀 角 飛|五
+---------------+
先手の持駒：なし
先手：先手
後手番
    """) must beValid.like { case p =>
      p.tags.value.size must_== 1
      p.tags.value must contain { (tag: Tag) =>
        tag.name == Tag.Sente && tag.value == "先手"
      }
      p.initialSfen must beSome.like { sfen =>
        sfen.value == "rbsgk/4p/P4/5/KGSBR w -"
      }
      p.variant.minishogi must beTrue
    }
  }

  "chushogi kif fixture 1" in {
    parser(chushogiKif1) must beValid.like { case p =>
      p.parsedSteps.value.size must_== 129
      p.variant.chushogi must beTrue
    }
  }

  "annanshogi" in {
    parser("""
      先手：先手
      手合割：安南将棋
    """) must beValid.like { case p =>
      p.initialSfen must beNone
      p.variant.annanshogi must beTrue
    }
  }

  "annanshogi with board" in {
    parser("""
      後手：
      手合割：安南将棋
      後手の持駒：金四　銀二　香三　歩十三
        ９ ８ ７ ６ ５ ４ ３ ２ １
      +---------------------------+
      | ・ ・ ・v桂 ・ ・ ・ ・ ・|一
      |v玉 角v歩 馬 ・ ・ ・ ・ ・|二
      | ・ ・ ・ ・ ・ ・ ・ ・ ・|三
      | 桂 ・ ・v歩 ・ ・ ・ ・ ・|四
      |vとv桂 ・ ・v歩 ・ ・ ・ ・|五
      | ・ ・ 飛 ・v全 ・ ・ ・ ・|六
      |v歩 桂 ・ ・ ・ ・ ・ ・ ・|七
      | ・ 香 ・ ・ ・ ・ ・ ・ ・|八
      | ・v銀 ・ ・ 龍 ・ ・ ・ ・|九
      +---------------------------+
      先手：先手
      先手の持駒：なし
    """) must beValid.like { case p =>
      p.initialSfen must beSome.like { sfen =>
        sfen.value == "3n5/kBp+B5/9/N2p5/+pn2p4/2R1+s4/pN7/1L7/1s2+R4 b 4g2s3l13p"
      }
      p.variant.annanshogi must beTrue
    }
  }

  "kyotoshogi" in {
    parser("""
手合割：京都将棋
先手：
後手：
手数----指手---------消費時間--
   1   ３四金(25) * even without 成 should be good
   2   １二と成(11)
   3   １四歩成(15)
   4   同　香成(12)
   5   ４四玉(35)
   6   ２二飛打
   7   同　桂成(34)
   8   ４三歩打
    """) must beValid.like { case p =>
      p.parsedSteps.value.size must_== 8
      p.initialSfen must beNone
      p.variant.kyotoshogi must beTrue
    }
  }

  "kyotoshogi with handicap" in {
    parser("""
手合割：京都将棋
上手の持駒：なし
  ５ ４ ３ ２ １
+---------------+
| ・v金v玉v銀 ・|一
| ・ ・ ・ ・ ・|二
| ・ ・ ・ ・ ・|三
| ・ ・ ・ ・ ・|四
| と 銀 玉 金 歩|五
+---------------+
下手の持駒：なし
上手番
下手：S
上手：G
手数----指手---------消費時間--""") must beValid.like { case p =>
      p.initialSfen must beSome.like { sfen =>
        sfen.truncate.value == "1gks1/5/5/5/TSKGP w -"
      }
      p.variant.kyotoshogi must beTrue
      p.tags.value must contain { (tag: Tag) =>
        tag.name == Tag.Sente && tag.value == """S"""
      }
      p.tags.value must contain { (tag: Tag) =>
        tag.name == Tag.Gote && tag.value == """G"""
      }
    }
  }

  "checkshogi" in {
    parser("""
      先手：先手
      手合割：王手将棋
    """) must beValid.like { case p =>
      p.initialSfen must beNone
      p.variant.checkshogi must beTrue
    }
  }

}
