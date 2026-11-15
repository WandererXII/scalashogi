package shogi

import java.util.Arrays

import cats.syntax.option._

import shogi.format.forsyth.Sfen
import shogi.format.usi.Usi

final case class History(
    lastLionCapture: Option[Square],
    consecutiveAttacks: ConsecutiveAttacks,
    positionHashes: PositionHash,
    initialSfen: Option[Sfen],
    usis: Vector[Usi],
    startedAtPly: Int,
    stepShifted: Boolean, // true if the starting step number equals the starting ply
) {

  def plies: Int = startedAtPly + usis.size

  def startedAtStep: Int = startedAtPly + (if (stepShifted) 0 else 1)

  def lastUsi: Option[Usi] = usis.lastOption

  // only hashes for positions with the same side to play
  private def currentTurnHashes = positionHashes.sliding(Hash.size, 2 * Hash.size).toList

  private def isRepetition(times: Int): Boolean =
    positionHashes.length > (times - 1) * 2 * Hash.size && {
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

  // number of moves/drops each player made since the repeated position first occured
  private[shogi] def firstRepetitionDistance: Option[Int] = {
    val positions = currentTurnHashes
    positions.headOption match {
      case Some(Array(x, y, z)) =>
        (positions lastIndexWhere {
          case Array(x2, y2, z2) => x == x2 && y == y2 && z == z2
          case _                 => false
        }).some.filter(_ > 0)
      case _ => none
    }
  }

  lazy val perpetualCheckAttacker: Option[Color] =
    firstRepetitionDistance flatMap { dist =>
      val senteAttacks = consecutiveAttacks(Sente) >= dist
      val goteAttacks  = consecutiveAttacks(Gote) >= dist
      if (senteAttacks && goteAttacks) none
      else if (senteAttacks) Sente.some
      else if (goteAttacks) Gote.some
      else none
    }

  lazy val threefoldRepetition = isRepetition(3)
  lazy val fourfoldRepetition  = isRepetition(4)

  private[shogi] def record(usi: Usi): History =
    copy(usis = usis :+ usi)

  def withLastLionCapture(squareOpt: Option[Square]) = copy(lastLionCapture = squareOpt)

  def withConsecutiveAttacks(ca: ConsecutiveAttacks) = copy(consecutiveAttacks = ca)

  def withPositionHashes(h: PositionHash) = copy(positionHashes = h)

  def withInitialSfen(s: Sfen) = copy(initialSfen = s.some)

  override def equals(other: Any): Boolean = other match {
    case that: History =>
      lastLionCapture == that.lastLionCapture &&
      consecutiveAttacks == that.consecutiveAttacks &&
      initialSfen == that.initialSfen &&
      usis == that.usis &&
      startedAtPly == that.startedAtPly &&
      stepShifted == that.stepShifted &&
      Arrays.equals(positionHashes, that.positionHashes)
    case _ => false
  }

  override def hashCode(): Int =
    (
      lastLionCapture,
      consecutiveAttacks,
      initialSfen,
      usis,
      startedAtPly,
      stepShifted,
      Arrays.hashCode(positionHashes),
    ).hashCode()

  override def toString = {
    val positions = (positionHashes grouped Hash.size).toList
    s"${lastUsi.fold("-")(_.usi)} ${lastLionCapture.fold("-")(_.key)} $consecutiveAttacks ${positions
        .map(Hash.debug)
        .mkString(" ")} ${initialSfen.fold("-")(_.value)}"
  }
}

object History {

  def empty: History = History(
    lastLionCapture = none,
    consecutiveAttacks = ConsecutiveAttacks.empty,
    positionHashes = Array.empty,
    initialSfen = none,
    usis = Vector.empty,
    startedAtPly = 0,
    stepShifted = false,
  )

}

// attacks made in a row
final case class ConsecutiveAttacks(sente: Int, gote: Int) {

  def add(color: Color) =
    copy(
      sente = sente + color.fold(1, 0),
      gote = gote + color.fold(0, 1),
    )

  def reset(color: Color) =
    copy(
      sente = color.fold(0, sente),
      gote = color.fold(gote, 0),
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
