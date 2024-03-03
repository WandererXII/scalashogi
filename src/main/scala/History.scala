package shogi

import shogi.format.usi.Usi
import shogi.format.forsyth.Sfen

final case class History(
    lastUsi: Option[Usi],
    lastLionCapture: Option[Pos],
    consecutiveAttacks: ConsecutiveAttacks,
    positionHashes: PositionHash,
    initialSfen: Option[Sfen]
) {

  // only hashes for positions with the same side to move
  private def currentTurnHashes = positionHashes.sliding(Hash.size, 2 * Hash.size).toList

  private def isRepetition(times: Int): Boolean =
    positionHashes.length > (times - 1) * 4 * Hash.size && {
      val positions = currentTurnHashes
      positions.headOption match {
        case Some(Array(x, y, z)) =>
          (positions count {
            case Array(x2, y2, z2) => x == x2 && y == y2 && z == z2
            case _                 => false
          }) >= times
        case _ => times <= 1
      }
    }

  // number of moves each player made since the repeated position first occured
  lazy val firstRepetitionDistance: Option[Int] = {
    val positions = currentTurnHashes
    positions.headOption match {
      case Some(Array(x, y, z)) =>
        Some(positions lastIndexWhere {
          case Array(x2, y2, z2) => x == x2 && y == y2 && z == z2
          case _                 => false
        }).filter(_ > 0)
      case _ => None
    }
  }

  lazy val threefoldRepetition = isRepetition(3)
  lazy val fourfoldRepetition  = isRepetition(4)

  def withLastUsi(u: Usi) = copy(lastUsi = Some(u))

  def withLastLionCapture(op: Option[Pos]) = copy(lastLionCapture = op)

  def withConsecutiveAttacks(ca: ConsecutiveAttacks) = copy(consecutiveAttacks = ca)

  def withPositionHashes(h: PositionHash) = copy(positionHashes = h)

  def withInitialSfen(s: Sfen) = copy(initialSfen = Some(s))

  override def toString = {
    val positions = (positionHashes grouped Hash.size).toList
    s"${lastUsi.fold("-")(_.usi)} ${lastLionCapture.fold("-")(_.key)} $consecutiveAttacks ${positions
        .map(Hash.debug)
        .mkString(" ")} ${initialSfen.fold("-")(_.value)}"
  }
}

object History {

  def empty: History = History(None, None, ConsecutiveAttacks.empty, Array.empty, None)

}

// attacks made in a row
final case class ConsecutiveAttacks(sente: Int, gote: Int) {

  def add(color: Color) =
    copy(
      sente = sente + color.fold(1, 0),
      gote = gote + color.fold(0, 1)
    )

  def reset(color: Color) =
    copy(
      sente = color.fold(0, sente),
      gote = color.fold(gote, 0)
    )

  def apply(color: Color) = color.fold(sente, gote)

  def nonEmpty = sente > 0 || gote > 0

  def empty = !nonEmpty

  override def toString =
    s"($sente, $gote)"
}

object ConsecutiveAttacks {
  def empty: ConsecutiveAttacks = ConsecutiveAttacks(0, 0)
}
