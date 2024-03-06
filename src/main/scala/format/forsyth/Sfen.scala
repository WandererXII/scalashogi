package shogi
package format
package forsyth

import cats.implicits._

import shogi.variant.Variant

final case class Sfen(value: String) extends AnyVal {

  def toSituationPlus(variant: Variant): Option[Sfen.SituationPlus] =
    toSituation(variant) map { sit =>
      val sn = stepNumber map (_ max 1 min 9999)
      Sfen.SituationPlus(sit, sn | 1)
    }

  def toSituation(variant: Variant): Option[Situation] =
    for {
      board <- toBoard(variant)
      hands <- toHands(variant)
      history = toHistory(variant)
    } yield {
      val sit = Situation(board, hands, color | Sente, history, variant)
      if (color.isEmpty && sit.check) sit.switch else sit
    }

  def toBoard(variant: Variant): Option[Board] = {
    val positions = boardString | ""
    if (positions.count(_ == '/') == (variant.numberOfRanks - 1)) {
      Sfen.makePieceMapFromString(positions, variant) map Board.apply
    } else None
  }

  def toHands(variant: Variant): Option[Hands] =
    if (variant.supportsDrops)
      handsString.fold(Hands(variant).some)(Sfen.makeHandsFromString(_, variant))
    else Hands.empty.some

  def toHistory(variant: Variant): History =
    if (variant == shogi.variant.Chushogi)
      handsString.map(s => History.empty withLastLionCapture Pos.fromKey(s)).getOrElse(History.empty)
    else History.empty

  def boardString: Option[String] =
    value.split(' ').lift(0)

  def color: Option[Color] =
    value.split(' ').lift(1) flatMap (_.headOption) flatMap Color.apply

  def handsString: Option[String] =
    value.split(' ').lift(2)

  def stepNumber: Option[Int] =
    value.split(' ').lift(3) flatMap (_.toIntOption)

  def truncate = Sfen(value.split(' ') take 3 mkString " ")

  def initialOf(variant: Variant) = value == variant.initialSfen.value

  override def toString = value

}

object Sfen {

  def apply(game: Game): Sfen =
    apply(SituationPlus(game.situation, game.stepNumber))

  def apply(sp: SituationPlus): Sfen =
    Sfen(s"${situationToString(sp.situation)} ${sp.stepNumber}")

  def apply(sit: Situation): Sfen =
    Sfen(s"${situationToString(sit)}")

  final case class SituationPlus(situation: Situation, stepNumber: Int) {
    def plies        = stepNumber - (if ((stepNumber % 2 == 1) == situation.color.sente) 1 else 0)
    def toSfen: Sfen = apply(this)
  }

  def situationToString(sit: Situation): String =
    List[String](
      boardToString(sit.board, sit.variant),
      sit.color.letter.toString,
      if (sit.variant == shogi.variant.Chushogi) lastLionCaptureDestToString(sit.history)
      else handsToString(sit.hands, sit.variant)
    ) mkString " "

  def boardToString(board: Board, variant: Variant): String = {
    val sfen  = new scala.collection.mutable.StringBuilder(256)
    var empty = 0
    for (y <- 0 to (variant.numberOfRanks - 1)) {
      empty = 0
      for (x <- (variant.numberOfFiles - 1) to 0 by -1) {
        board(x, y).flatMap(p => SfenUtils.toForsyth(p, variant)) match {
          case None => empty = empty + 1
          case Some(forsyth) =>
            if (empty == 0) sfen append forsyth
            else {
              sfen append (empty.toString + forsyth)
              empty = 0
            }
        }
      }
      if (empty > 0) sfen append empty
      if (y < variant.numberOfRanks - 1) sfen append '/'
    }
    sfen.toString
  }

  private def lastLionCaptureDestToString(history: History): String =
    history.lastLionCapture.fold("-")(_.key)

  private[forsyth] def handToString(hand: Hand, variant: Variant): String =
    variant.handRoles map { r =>
      val cnt     = hand(r)
      val forsyth = SfenUtils.toForsyth(r, variant) | ""
      if (cnt == 1) forsyth
      else if (cnt > 1) cnt.toString + forsyth
      else ""
    } mkString ""

  def handsToString(hands: Hands, variant: Variant): String =
    List[String](
      handToString(hands.sente, variant).toUpperCase,
      handToString(hands.gote, variant)
    ).mkString("").some.filterNot(_.isEmpty) | "-"

  private def makePieceMapFromString(boardStr: String, variant: Variant): Option[PieceMap] = {

    @scala.annotation.tailrec
    def piecesListRec(
        pieces: List[(Pos, Piece)],
        chars: List[Char],
        x: Int,
        y: Int
    ): Option[List[(Pos, Piece)]] =
      chars match {
        case Nil => Some(pieces)
        case '/' :: rest if y < variant.numberOfRanks =>
          piecesListRec(pieces, rest, variant.numberOfFiles - 1, y + 1)
        case '1' :: c :: rest if c.isDigit && x >= 0 => piecesListRec(pieces, rest, x - (10 + c.asDigit), y)
        case c :: rest if c.isDigit && x >= 0        => piecesListRec(pieces, rest, x - c.asDigit, y)
        case '+' :: c :: rest =>
          (for {
            pos   <- Pos.at(x, y)
            _     <- Option.when(variant.isInsideBoard(pos))(())
            piece <- SfenUtils.toPiece("+" + c, variant)
          } yield pos -> piece :: pieces) match {
            case Some(ps) => piecesListRec(ps, rest, x - 1, y)
            case _        => None
          }
        case c :: rest => {
          (for {
            pos   <- Pos.at(x, y)
            _     <- Option.when(variant.isInsideBoard(pos))(())
            piece <- SfenUtils.toPiece(c.toString, variant)
          } yield pos -> piece :: pieces) match {
            case Some(ps) => piecesListRec(ps, rest, x - 1, y)
            case _        => None
          }
        }
      }

    piecesListRec(Nil, boardStr.toList, variant.numberOfFiles - 1, 0) map (_.toMap)
  }

  def makeHandsFromString(handsStr: String, variant: Variant): Option[Hands] = {

    @scala.annotation.tailrec
    def handsRec(hands: Hands, chars: List[Char], curCount: Option[Int]): Option[Hands] =
      chars match {
        case Nil      => Some(hands)
        case '-' :: _ => Some(Hands.empty)
        case d :: rest if d.isDigit =>
          handsRec(hands, rest, curCount.map(_ * 10 + d.asDigit) orElse d.asDigit.some)
        case p :: rest =>
          SfenUtils.toPiece(p.toString, variant).flatMap { p =>
            variant.handRoles.find(_ == p.role).map((p.color, _))
          } match {
            case Some((color, role)) =>
              handsRec(hands.store(color, role, curCount.fold(1)(math.min(_, 81))), rest, None)
            case _ => None
          }
      }

    handsRec(Hands.empty, handsStr.toList, None)
  }

  def clean(source: String): Sfen = Sfen(source.replace("_", " ").trim)

}
