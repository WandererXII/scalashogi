package shogi
package format
package forsyth

import Pos._

class VisualTest extends ShogiTest {

  val f = Visual

  "The visual board formatter" should {
    "export new board" in {
      f.addNewLines(f render makeSituation) must_== newVisualFormat
    }

    "import and export is non destructive" in {
      forall(examples) { example =>
        f.addNewLines(f render ((f parse example).get)) must_== example
      }
    }

    "partial import" in {
      f.addNewLines(f render ((f parse partialSituationFormat).get)) must_== fullSituationFormat
    }

    "hand import" in {
      f.addNewLines(f render ((f parse handInBoard).get)) must_== fullHandInBoard
    }

    "bigger board" in {
      f.addNewLines(f render ((f.parse(chushogiBoard, shogi.variant.Chushogi)).get)) must_== chushogiBoard
      f.addNewLines(
        f render ((f.parse(chushogiPartial, shogi.variant.Chushogi)).get)
      ) must_== chushogiPartialFull
    }

    "export with special marks" in {
      val situation = Visual parse """
k . B . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
N . B . . . . . P
. . . . . . . . .
P P P P P P P P .
. . . . . . . . .
. N S G K G S N L
Hands:
Turn:Sente
"""
      val markedBoard =
        f render (situation.get, Map(Set(SQ8F, SQ6F, SQ8D, SQ6D, SQ9C, SQ5C, SQ4B, SQ3A) -> 'x'))
      f addNewLines markedBoard must_== """
k . B . . . x . .
. . . . . x . . .
x . . . x . . . .
. x . x . . . . .
N . B . . . . . P
. x . x . . . . .
P P P P P P P P .
. . . . . . . . .
. N S G K G S N L
Hands:
Turn:Sente
"""
    }
  }

  val newVisualFormat = """
l n s g k g s n l
. r . . . . . b .
p p p p p p p p p
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
P P P P P P P P P
. B . . . . . R .
L N S G K G S N L
Hands:
Turn:Sente
"""

  val partialSituationFormat = """
. . . . . . . . .
. . . . k . . . .
. . . . . . . . .
P P P P P P P P P
. B . . . . . R .
L N S G K G S N L
"""

  val fullSituationFormat = """
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . k . . . .
. . . . . . . . .
P P P P P P P P P
. B . . . . . R .
L N S G K G S N L
Hands:
Turn:Sente
"""

  val handInBoard = """
. . . . . . . . .
. . . . k . . . .
. . . . . . . . .
P P P P P P P P P
. B . . . . . R .
L N S G K G S N L
Hands:2lB
Turn:Sente
"""

  val fullHandInBoard = """
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . k . . . .
. . . . . . . . .
P P P P P P P P P
. B . . . . . R .
L N S G K G S N L
Hands:B2l
Turn:Sente
"""

  val chushogiBoard = """
l f c s g e k g s c f l
a . b . t x o t . b . a
m v r h d q n d h r v m
p p p p p p p p p p p p
. . . i . . . . i . . .
. . . . . . . . . . . .
. . . . . . . . . . . .
. . . I . . . . I . . .
P P P P P P P P P P P P
M V R H D N Q D H R V M
A . B . T O X T . B . A
L F C S G K E G S C F L
Turn:Sente
"""

  val chushogiPartial     = """
P P P P P P P P P P P P
L N . G+E . . . . . N L
"""
  val chushogiPartialFull = """
. . . . . . . . . . . .
. . . . . . . . . . . .
. . . . . . . . . . . .
. . . . . . . . . . . .
. . . . . . . . . . . .
. . . . . . . . . . . .
. . . . . . . . . . . .
. . . . . . . . . . . .
. . . . . . . . . . . .
. . . . . . . . . . . .
P P P P P P P P P P P P
L N . G+E . . . . . N L
Turn:Sente
"""

  val examples = Seq(
    newVisualFormat,
    """
l n s g k g s n l
. r . . . . . B .
p p p p p p . p p
. . . . . . p . .
. . . . . . . . .
. . P . . . . . .
P P . P P P P P P
. . . . . . . R .
L N S G K G S N L
Hands:
Turn:Sente
""",
    """
. . . . . . k . .
. . . . . . P . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
K . . . . . . . .
Hands:
Turn:Gote
""",
    """
l n s g k g s n l
. r . . . . . b .
p p p p p p p p p
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . .
L K . . . . . N L
Hands:
Turn:Sente
""",
    """
. . b g k g . n l
p . p p p . p p p
. r . . . . . . .
. . P . p . . . .
. . . . . . . . .
. . . . n B . . .
. P P . . N . . P
P . . . . P P P .
L N S . K . . . L
Hands:B2r10p
Turn:Sente
"""
  )
}
