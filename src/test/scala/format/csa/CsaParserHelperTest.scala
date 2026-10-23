package shogi
package format
package csa

import shogi.format.csa.CsaParserHelper._
import shogi.format.forsyth.Sfen

class CsaParserHelperTest extends ShogiTest {

  def parseAndCompare(source: String, resSfen: Sfen) =
    parseSituation(source) must beValid.like { case s =>
      s.toSfen.truncate must_== resSfen
    }

  "Handicap" in {
    parseAndCompare("PI", Sfen("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b -"))
    parseAndCompare(
      "PI82HI22KA,-",
      Sfen("lnsgkgsnl/9/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL w -"),
    )
  }

  "Board" in {
    parseAndCompare(
      """P1-KY-KE+KI *  *  *  *  *  * 
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
-""",
      Sfen("lnG6/2+P4+Sn/kp3+S3/2p6/1n7/9/9/7K1/9 w GS2r2b2gsn3l15p"),
    )
  }

  "00AL counts a tokin as a pawn already on the board" in {
    val board = (1 to 9)
      .map { rank =>
        s"P$rank" + (if (rank == 1) "+TO" else " * ") + " * " * 8
      }
      .mkString("\n")
    parseAndCompare(
      s"$board\nP+00AL",
      Sfen("+P8/9/9/9/9/9/9/9/9 b 2R2B4G4S4N4L17P"),
    )
  }

  "00AL subtracts promoted board pieces and the opponent's hand" in {
    val promotions = List(
      ("TO", "FU", Pawn, 18),
      ("NY", "KY", Lance, 4),
      ("NK", "KE", Knight, 4),
      ("NG", "GI", Silver, 4),
      ("UM", "KA", Bishop, 2),
      ("RY", "HI", Rook, 2),
    )
    val board = (1 to 9).map(rank => s"P$rank" + " * " * 9).mkString("\n")
    forall(promotions) { case (promoted, base, role, total) =>
      forall(List("+", "-")) { color =>
        val opponent = if (color == "+") "-" else "+"
        val source   = s"$board\nP${opponent}11$promoted\nP${opponent}00$base\nP${color}00AL"
        parseSituation(source) must beValid.like { case s =>
          s.hands(Color.fromSente(color == "+"))(role) must_== total - 2
        }
      }
    }
  }
}
