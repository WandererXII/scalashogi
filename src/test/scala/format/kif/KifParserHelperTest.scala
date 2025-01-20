package shogi
package format
package kif

import shogi.format.forsyth.Sfen
import shogi.format.kif.KifParserHelper._

class KifParserHelperTest extends ShogiTest {

  def parseAndCompare(source: String, handicap: Option[String], resSfen: Sfen) =
    parseSituation(source, handicap, Nil) must beValid.like { case s =>
      s.toSfen.truncate must_== resSfen
    }

  "Handicap" in {
    parseAndCompare(
      "",
      Some("平手"),
      Sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b -"),
    )
    parseAndCompare(
      "",
      Some("二枚落ち"),
      Sfen("lnsgkgsnl/9/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w -"),
    )
  }

  "Board" in {
    parseAndCompare(
      """後手：
      手合割：平手
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
      先手の持駒：なし""",
      None,
      Sfen("3n5/kBp+B5/9/N2p5/+pn2p4/2R1+s4/pN7/1L7/1s2+R4 b 4g2s3l13p"),
    )
  }

  "Chushogi board" in {
    parseAndCompare(
      """ １２ １１ １０ ９  ８  ７  ６  ５  ４  ３  ２  １
+------------------------------------------------+
|  ・  ・  ・  ・  ・  ・  ・  ・  ・  ・  ・  ・|一
|  ・  ・  ・  ・  ・  ・  ・  ・  ・  獅  獅  ・|二
|  ・  ・  鷹  鷹  ・  ・  ・  ・  ・  ・  ・  ・|三
|  ・  ・  ・  ・  ・  ・  ・  ・  ・  ・  ・  ・|四
|  ・  ・  ・  ・  ・  ・  ・  ・  ・v成獅  ・  ・|五
|  ・  ・  ・  ・  ・  ・  ・  ・  ・  ・  ・  ・|六
|  ・  ・  ・  ・  ・  獅  ・  ・  ・  獅  ・  ・|七
|  ・  ・  ・  ・  ・ 成獅  ・  ・  ・  ・  ・  ・|八
|  ・  ・  ・  ・  ・  ・  ・  ・  ・  鷹  ・  ・|九
|  ・  ・  鷹  ・  ・  ・  ・  ・  ・  ・  ・  ・|十
|  ・  ・  鷹  ・  ・  ・  ・  ・  ・  鷹  ・  ・|十一
|  ・  ・  ・  ・  ・  ・  ・  ・  ・  ・  ・  ・|十二
+------------------------------------------------+""",
      None,
      Sfen("12/9NN1/2+H+H8/12/9+o2/12/5N3N2/5+O6/9+H2/2+H9/2+H6+H2/12 b -"),
    )
  }

  "Variants" in {
    parseAndCompare("", Some("5五将棋"), shogi.variant.Minishogi.initialSfen.truncate)
    parseAndCompare("", Some("安南将棋"), shogi.variant.Annanshogi.initialSfen.truncate)
    parseAndCompare("", Some("王手将棋"), shogi.variant.Checkshogi.initialSfen.truncate)
  }
}
