package shogi
package format
package kif

import forsyth.Sfen
import variant._
import Kif._
import usi._

class KifModelTest extends ShogiTest {
  "render moves" in {
    Kif.renderMove(Usi.WithRole(Usi("7h7g").get, Gold), None, Standard) must_== "７七金(78)"
    Kif.renderMove(Usi.WithRole(Usi("P*6d").get, Pawn), None, Standard) must_== "６四歩打"
    Kif.renderMove(Usi.WithRole(Usi("4j2h").get, Horse), None, Chushogi) must_== "2八龍馬 （←4十）"
  }

  "render kif situation - board, hands, turn, from random sfen" in {
    renderSituation(
      Sfen("lnG6/2+P4+Sn/kp3+S3/2p6/1n7/9/9/7K1/9 w GS2r2b2gsn3l15p").toSituation(Standard).get
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
      Minishogi
    ) must_== """手合割：5五将棋"""
  }

  "render kif situation - minishogi" in {
    renderSituation(
      Sfen("rbsgk/4p/P4/5/KGSBR w - 2").toSituation(Minishogi).get
    ) must_== """後手の持駒：なし
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
    val moves = List(
      NotationMove(1, Usi.WithRole(Usi("7h7g").get, Gold)),
      NotationMove(2, Usi.WithRole(Usi("P*6d").get, Pawn)),
      NotationMove(3, Usi.WithRole(Usi("4i2h").get, Horse))
    )
    Kif(
      Tags.empty,
      moves
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
    val moves = List(
      NotationMove(1, Usi.WithRole(Usi("7h7g").get, Gold)),
      NotationMove(2, Usi.WithRole(Usi("4i2h").get, Pawn)),
      NotationMove(3, Usi.WithRole(Usi("5i2h1a").get, Horse))
    )
    Kif(
      Tags(
        List(
          Tag(_.Variant, "Chushogi")
        )
      ),
      moves
    ).render must_== """手合割：中将棋
先手：
後手：
手数----指手---------消費時間--
   1   7七金将 （←7八）
   2   2八歩兵 （←4九）
   3一歩目 仝龍馬 （←5九）
   3二歩目 1一龍馬 （←2八）"""

    // not aware of any standard - so not sure about this...
    renderSituation(
      Sfen("12/9NN1/2+H+H8/12/9+o2/12/5N3N2/5+O6/9+H2/2+H9/2+H6+H2/12 b - 1")
        .toSituation(Chushogi)
        .get
    ) must_== """  １２ １１ １０  ９   ８   ７   ６   ５   ４   ３   ２   １
+------------------------------------------------------------+
|  ・   ・   ・   ・   ・   ・   ・   ・   ・   ・   ・   ・ |一
|  ・   ・   ・   ・   ・   ・   ・   ・   ・   獅   獅   ・ |二
|  ・   ・   鷹   鷹   ・   ・   ・   ・   ・   ・   ・   ・ |三
|  ・   ・   ・   ・   ・   ・   ・   ・   ・   ・   ・   ・ |四
|  ・   ・   ・   ・   ・   ・   ・   ・   ・ v成獅   ・   ・ |五
|  ・   ・   ・   ・   ・   ・   ・   ・   ・   ・   ・   ・ |六
|  ・   ・   ・   ・   ・   獅   ・   ・   ・   獅   ・   ・ |七
|  ・   ・   ・   ・   ・  成獅   ・   ・   ・   ・   ・   ・ |八
|  ・   ・   ・   ・   ・   ・   ・   ・   ・   鷹   ・   ・ |九
|  ・   ・   鷹   ・   ・   ・   ・   ・   ・   ・   ・   ・ |十
|  ・   ・   鷹   ・   ・   ・   ・   ・   ・   鷹   ・   ・ |十一
|  ・   ・   ・   ・   ・   ・   ・   ・   ・   ・   ・   ・ |十二
+------------------------------------------------------------+"""

  }

}
