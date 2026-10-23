package shogi
package format
package csa

import shogi.format.csa.Csa._
import shogi.format.forsyth.Sfen
import shogi.variant.Standard

class CsaModelTest extends ShogiTest {

  "signed illegal-action terminations preserve the winner on import" in {
    forall(List(Status.PerpetualCheck, Status.Mate)) { status =>
      forall(List[Color](Sente, Gote)) { winner =>
        createTerminationStep(status, winnerTurn = true, Some(winner)) must beSome.like {
          case termination =>
            CsaParser.full(s"PI\n+\n+7776FU\n$termination") must beValid.like { case p =>
              p.tags(_.Result) must beSome(winner.fold("1-0", "0-1"))
            }
        }
      }
    }
  }

  "render csa situation - board, hands, turn, from random sfen" in {
    renderSituation(
      Sfen("lnG6/2+P4+Sn/kp3+S3/2p6/1n7/9/9/7K1/9 w GS2r2b2gsn3l15p").toSituation(Standard).get,
    ) must_== """P1-KY-KE+KI *  *  *  *  *  * 
P2 *  * +TO *  *  *  * +NG-KE
P3-OU-FU *  *  * +NG *  *  * 
P4 *  * -FU *  *  *  *  *  * 
P5 * -KE *  *  *  *  *  *  * 
P6 *  *  *  *  *  *  *  *  * 
P7 *  *  *  *  *  *  *  *  * 
P8 *  *  *  *  *  *  * +OU * 
P9 *  *  *  *  *  *  *  *  * 
P+00KI00GI
P-00HI00HI00KA00KA00KI00KI00GI00KE00KY00KY00KY00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU
-"""
  }

}
