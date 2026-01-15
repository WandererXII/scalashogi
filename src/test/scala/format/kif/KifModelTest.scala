package shogi
package format
package kif

import shogi.format.forsyth.Sfen
import shogi.format.kif.Kif._
import shogi.format.usi._
import shogi.variant._

class KifModelTest extends ShogiTest {
  "render steps" in {
    Kif.renderStep(Usi.WithRole(Usi("7h7g").get, Gold), None, Standard) must_== "７七金(78)"
    Kif.renderStep(Usi.WithRole(Usi("P*6d").get, Pawn), None, Standard) must_== "６四歩打"
    Kif.renderStep(Usi.WithRole(Usi("4j2h").get, Horse), None, Chushogi) must_== "2八龍馬 （←4十）"
  }

  "render kif situation - board, hands, turn, from random sfen" in {
    renderSituation(
      Sfen("lnG6/2+P4+Sn/kp3+S3/2p6/1n7/9/9/7K1/9 w GS2r2b2gsn3l15p").toSituation(Standard).get,
    ) must_== """後手の持駒：飛二　角二　金二　銀　桂　香三　歩十五
  ９ ８ ７ ６ ５ ４ ３ ２ １
+---------------------------+
|v香v桂 金 ・ ・ ・ ・ ・ ・|一
| ・ ・ と ・ ・ ・ ・ 全v桂|二
|v玉v歩 ・ ・ ・ 全 ・ ・ ・|三
| ・ ・v歩 ・ ・ ・ ・ ・ ・|四
| ・v桂 ・ ・ ・ ・ ・ ・ ・|五
| ・ ・ ・ ・ ・ ・ ・ ・ ・|六
| ・ ・ ・ ・ ・ ・ ・ ・ ・|七
| ・ ・ ・ ・ ・ ・ ・ 玉 ・|八
| ・ ・ ・ ・ ・ ・ ・ ・ ・|九
+---------------------------+
先手の持駒：金　銀
後手番"""
  }

  "render kif situation - default minishogi" in {
    renderSetup(
      Some(Sfen("rbsgk/4p/5/P4/KGSBR b - 1")),
      Minishogi,
    ) must_== """手合割：5五将棋"""
  }

  "render kif situation - minishogi" in {
    renderSituation(
      Sfen("rbsgk/4p/P4/5/KGSBR w - 2").toSituation(Minishogi).get,
    ) must_== """手合割：5五将棋
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
後手番"""
  }

  "kif render" in {
    // random bs
    val steps = List(
      NotationStep(1, Usi.WithRole(Usi("7h7g").get, Gold)),
      NotationStep(2, Usi.WithRole(Usi("P*6d").get, Pawn)),
      NotationStep(3, Usi.WithRole(Usi("4i2h").get, Horse)),
    )
    Kif(
      steps,
      None,
      Standard,
    ).render must_== """手合割：平手
先手：
後手：
手数----指手---------消費時間--
   1   ７七金(78)
   2   ６四歩打
   3   ２八馬(49)"""
  }

  "chushogi kif render" in {
    // random bs
    val steps = List(
      NotationStep(1, Usi.WithRole(Usi("7h7g").get, Gold)),
      NotationStep(2, Usi.WithRole(Usi("4i2h").get, Pawn)),
      NotationStep(3, Usi.WithRole(Usi("5i2h1a").get, Horse)),
    )
    Kif(
      steps,
      None,
      Chushogi,
    ).render must_== """先手：
後手：
手数----指手---------消費時間--
   1手目   7七金将 （←7八）
   2手目   2八歩兵 （←4九）
   3手目一歩目 仝龍馬 （←5九）
   3手目二歩目 1一龍馬 （←2八）"""

    // not aware of any standard - so not sure about this...
    renderSituation(
      Sfen("12/9NN1/2+H+H8/12/9+o2/12/5N3N2/5+O6/9+H2/2+H9/2+H6+H2/12 b - 1")
        .toSituation(Chushogi)
        .get,
    ) must_== """ １２ １１ １０ ９  ８  ７  ６  ５  ４  ３  ２  １
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
+------------------------------------------------+"""

  }

  "render kif situation - default annanshogi" in {
    renderHeader(
      Some(Sfen("lnsgkgsnl/1r5b1/p1ppppp1p/1p5p1/9/1P5P1/P1PPPPP1P/1B5R1/LNSGKGSNL b - 1")),
      Annanshogi,
      Tags(
        List(
          Tag(_.Sente, "A"),
        ),
      ),
    ) must_== """手合割：安南将棋
先手：A
後手："""
  }

  "render kif situation with handicap - annanshogi" in {
    renderHeader(
      Some(Sfen("ln2k2nl/1r5b1/p1ppppp1p/1p5p1/9/1P5P1/P1PPPPP1P/1B5R1/LNSGKGSNL w - 1")),
      Annanshogi,
      Tags(
        List(
          Tag(_.Sente, "A"),
        ),
      ),
    ) must_== """手合割：安南将棋
上手の持駒：なし
  ９ ８ ７ ６ ５ ４ ３ ２ １
+---------------------------+
|v香v桂 ・ ・v玉 ・ ・v桂v香|一
| ・v飛 ・ ・ ・ ・ ・v角 ・|二
|v歩 ・v歩v歩v歩v歩v歩 ・v歩|三
| ・v歩 ・ ・ ・ ・ ・v歩 ・|四
| ・ ・ ・ ・ ・ ・ ・ ・ ・|五
| ・ 歩 ・ ・ ・ ・ ・ 歩 ・|六
| 歩 ・ 歩 歩 歩 歩 歩 ・ 歩|七
| ・ 角 ・ ・ ・ ・ ・ 飛 ・|八
| 香 桂 銀 金 玉 金 銀 桂 香|九
+---------------------------+
下手の持駒：なし
上手番
下手：A
上手："""
  }

  "render kif situation - annanshogi" in {
    renderHeader(
      Some(Sfen("3n5/kBp+B5/9/N2p5/+pn2p4/2R1+s4/pN7/1L7/1s2+R4 b 4g2s3l13p")),
      Annanshogi,
      Tags(
        List(
          Tag(_.Sente, "A"),
        ),
      ),
    ) must_== """手合割：安南将棋
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
先手の持駒：なし
先手：A
後手："""
  }

  "kif render kyoto" in {
    val steps = List(
      NotationStep(1, Usi.WithRole(Usi("2e3d+").get, Gold)),
      NotationStep(2, Usi.WithRole(Usi("1a1b+").get, Tokin)),
      NotationStep(3, Usi.WithRole(Usi("1e1d+").get, Pawn)),
      NotationStep(4, Usi.WithRole(Usi("1b1d+").get, Lance)),
      NotationStep(5, Usi.WithRole(Usi("3e4d").get, King)),
      NotationStep(6, Usi.WithRole(Usi("R*2b").get, Rook)),
      NotationStep(7, Usi.WithRole(Usi("3d2b+").get, Knight)),
      NotationStep(8, Usi.WithRole(Usi("P*4c").get, Pawn)),
    )
    Kif(
      steps,
      None,
      Kyotoshogi,
    ).render must_== """手合割：京都将棋
先手：
後手：
手数----指手---------消費時間--
   1   ３四金成(25)
   2   １二と成(11)
   3   １四歩成(15)
   4   同　香成(12)
   5   ４四玉(35)
   6   ２二飛打
   7   同　桂成(34)
   8   ４三歩打"""
  }

  "kif render kyoto - handicap" in {
    Kif(
      Nil,
      Some(Sfen("1gks1/5/5/5/TSKGP w - ")),
      Kyotoshogi,
    ).render must_== """手合割：京都将棋
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
下手：
上手：
手数----指手---------消費時間--"""
  }

  "kif render dobutsu" in {
    val steps = List(
      NotationStep(1, Usi.WithRole(Usi("2c2b").get, Pawn)),
      NotationStep(2, Usi.WithRole(Usi("1a2b").get, Bishop)),
      NotationStep(3, Usi.WithRole(Usi("2d2c").get, King)),
    )
    Kif(
      steps,
      None,
      Dobutsu,
    ).render must_== """手合割：どうぶつしょうぎ
先手：
後手：
手数----指手---------消費時間--
   1   ２二ひよこ(23)
   2   同　ぞう(11)
   3   ２三ライオン(24)"""
  }

  "render kif board - dobutsu" in {
    renderSituation(
      Dobutsu.initialSfen.toSituation(Dobutsu).get,
    ) must_== """手合割：どうぶつしょうぎ
後手の持駒：なし
  ３ ２ １
+---------+
|vきvラvぞ|一
| ・vひ ・|二
| ・ ひ ・|三
| ぞ ラ き|四
+---------+
先手の持駒：なし"""
  }

}
