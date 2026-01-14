package shogi
package variant

import scala.annotation.unused

import cats.syntax.option._

import shogi.format.forsyth.Sfen
import shogi.format.usi.Usi

// Correctness depends on singletons for each variant ID
abstract class Variant private[variant] (
    val id: Int,
    val key: String,
    val name: String,
) {

  def initialSfen: Sfen

  def numberOfRanks: Int
  def numberOfFiles: Int

  def allPositions: List[Pos]

  def pieces: PieceMap
  def hands: HandsMap = Color.Map(Map.empty, Map.empty)

  // Roles available in the variant
  def allRoles: List[Role]

  // Only these roles can be stored in hand, order matters for export
  def handRoles: List[DroppableRole]

  // Promotions based on current variant, None for roles that do not promote
  def promote(role: Role): Option[Role]

  // Unpromotions based on current variant, for example for unpromoting pieces to hand
  def unpromote(role: Role): Option[Role]

  // Furthest rank for color
  def backrank(color: Color): Rank

  // Ranks where pieces of color can promote
  def promotionRanks(color: Color): List[Rank]

  def valueOfRole(role: Role): Int

  def forcePromote(piece: Piece, to: Pos): Boolean =
    piece.role match {
      case Pawn | Knight | Lance if backrank(piece.color) == to.rank            => true
      case Knight if math.abs(backrank(piece.color).index - to.rank.index) == 1 => true
      case _                                                                    => false
    }

  def canPromote(piece: Piece, orig: Pos, dest: Pos, @unused capture: Boolean): Boolean =
    promote(piece.role).isDefined &&
      promotionRanks(piece.color).exists(r => r == dest.rank || r == orig.rank)

  def supportsDrops = true

  def supportsDroppingEitherSide = false

  def isInsideBoard(pos: Pos): Boolean =
    pos.file.index < numberOfFiles && pos.rank.index < numberOfRanks

  // Optimized for performance
  // Exists a piece of color on board, which passes the filter and attacks pos
  def posThreatened(
      board: Board,
      color: Color,
      pos: Pos,
      filter: Piece => Boolean = _ => true,
  ): Boolean =
    board.pieces exists {
      case (from, piece) if piece.color == color && filter(piece) && piece.eyes(from, pos) =>
        piece.projectionDirs.isEmpty || piece.directDirs.exists(dir => dir(from).contains(pos)) ||
        Pos.findDirection(from, pos).exists { dir =>
          longRangeThreatens(board, from, dir, pos)
        }
      case _ => false
    }

  def attackingPiece(piece: Piece, @unused pos: Pos, @unused board: Board): Piece = piece

  // Filters out moves that would put the king in danger
  // Critical function - optimize for performance
  def moveFilter(a: MoveActor): List[Pos] = {
    // only long range roles, since you can only unpin a check from a role with projection
    val filter: Piece => Boolean =
      if ((a.piece is King) || a.check) _ => true else (_.projectionDirs.nonEmpty)
    val stableKingPos = if (a.piece is King) None else a.board.singleRoyalPosOf(a.color)
    a.unfilteredDestinations filterNot { dest =>
      (stableKingPos orElse Option.when(a.piece is King)(dest)) exists {
        posThreatened(
          a.board.forceMove(a.piece, a.pos, dest),
          !a.color,
          _,
          filter,
        )
      }
    }
  }

  def lionMoveFilter(a: MoveActor, midStep: Pos): List[Pos] =
    if (Role.allLions.contains(a.piece.role))
      a.shortUnfilteredDestinations filter { d =>
        d.dist(midStep) == 1 && a.board(d).filter(_.color != a.color).fold(true) { capture =>
          d.dist(a.pos) == 1 || !Role.allLions.contains(capture.role) ||
          a
            .board(midStep)
            .exists(midCapture => !(midCapture is Pawn) && !(midCapture is GoBetween)) ||
          (!posThreatened(
            a.board.forceTake(a.pos).forceTake(midStep),
            !a.color,
            d,
            _ => true,
          ) &&
            !posThreatened(
              a.board.forceTake(a.pos),
              !a.color,
              d,
              p => (p is Pawn) || (p is GoBetween),
            ))
        }
      }
    else {
      val dests = a.shortUnfilteredDestinations.filter(_.dist(midStep) == 1)
      a.history.lastLionCapture.fold(dests) { lionCaptureDest =>
        dests filterNot { d =>
          lionCaptureDest != d && a.board(d).exists(p => Role.allLions.contains(p.role))
        }
      }
    }

  def check(board: Board, color: Color): Boolean =
    board.royalPossOf(color).exists(posThreatened(board, !color, _))

  def checkSquares(board: Board, color: Color): List[Pos] =
    board.royalPossOf(color).filter(posThreatened(board, !color, _))

  def longRangeThreatens(board: Board, p: Pos, dir: Direction, to: Pos): Boolean =
    dir(p) exists { next =>
      next == to || (isInsideBoard(next) && !board.pieces
        .contains(next) && longRangeThreatens(board, next, dir, to))
    }

  def dropRoles = handRoles

  def dropFilterDoublePawn(a: DropActor, d: Pos): Boolean =
    (a.piece is Pawn) && (
      a.board.pieces.exists { case (pos, piece) =>
        a.piece == piece && pos.file == d.file
      }
    )

  def dropFilterPawnCheckmate(a: DropActor, d: Pos): Boolean =
    (a.piece is Pawn) && (
      a.board.singleRoyalPosOf(!a.color).fold(false) { kingPos =>
        a.piece.eyes(d, kingPos) && !(a.situation
          .withBoard(a.board.forcePlace(a.piece, d))
          .switch
          .hasDestinations)
      }
    )

  def dropFilter(a: DropActor): List[Pos] = {
    a.unfilteredDestinations.filterNot { d =>
      forcePromote(a.piece, d) || dropFilterDoublePawn(a, d) || dropFilterPawnCheckmate(a, d)
    }
  }

  // Could the position before the usi occur ever again
  def isIrreversible(
      @unused before: Board,
      @unused after: Board,
      @unused usi: Usi,
  ): Boolean = false

  // for perpetual check
  def isAttacking(after: Situation, @unused usi: Usi): Boolean =
    after.check

  def unpromoteRoleForHand(role: Role): Option[DroppableRole] =
    handRoles
      .find(_ == role)
      .orElse(unpromote(role) match {
        case Some(dRole: DroppableRole) if handRoles.contains(dRole) => dRole.some
        case _                                                       => none
      })

  def isInsufficientMaterial(board: Board, hands: Hands): Boolean =
    hands.isEmpty && board.pieces.sizeIs <= 2 &&
      board.pieces.forall { p => p._2 is King }

  def status(sit: Situation): Option[Status] =
    if (!sit.hasDestinations) {
      if (sit.check) Status.Mate.some
      else Status.Stalemate.some
    } else if (Impasse(sit)) Status.Impasse27.some
    else if (sit.history.fourfoldRepetition) {
      if (sit.history.perpetualCheckAttacker.isDefined) Status.PerpetualCheck.some
      else Status.Repetition.some
    } else if (isInsufficientMaterial(sit.board, sit.hands)) Status.Draw.some
    else none

  def winner(sit: Situation): Option[Color] =
    sit.status flatMap { status =>
      status match {
        case Status.Mate | Status.Stalemate | Status.Check => (!sit.color).some
        case Status.Impasse27                              => (sit.color).some
        case Status.PerpetualCheck => sit.history.perpetualCheckAttacker.map(!_)
        case _                     => none
      }
    }

  protected def hasUnmovablePieces(board: Board) =
    board.pieces.exists { case (pos, piece) =>
      forcePromote(piece, pos)
    }

  protected def hasDoublePawns(board: Board, color: Color) = {
    val pawnFiles = board.pieces.collect {
      case (pos, piece) if (piece is Pawn) && (piece is color) =>
        pos.file
    }.toList
    pawnFiles.distinct.size != pawnFiles.size
  }

  protected def validBoardSide(board: Board, strict: Boolean)(color: Color) = {
    val roles = board.piecesOf(color).map(_.role)
    roles.nonEmpty && roles.forall(allRoles contains _) &&
    roles.count(_ == King) <= 1 && roles.count(_ == Prince) <= 1 &&
    !hasUnmovablePieces(board) && !hasDoublePawns(board, color) &&
    (!strict || {
      roles.size <= pieces.size && (roles.count(_ == King) == 1 || roles.count(_ == Prince) == 1)
    })
  }

  protected def validHands(hands: Hands) =
    hands.roles.forall(handRoles contains _)

  def valid(board: Board, hands: Hands, strict: Boolean) =
    validHands(hands) && Color.all.forall(validBoardSide(board, strict) _)

  def standard   = this == Standard
  def minishogi  = this == Minishogi
  def chushogi   = this == Chushogi
  def annanshogi = this == Annanshogi
  def kyotoshogi = this == Kyotoshogi
  def checkshogi = this == Checkshogi

  override def toString = s"Variant($name)"

  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]

  override def hashCode: Int = id

}

object Variant {

  val all = List[Variant](
    Standard,
    Minishogi,
    Chushogi,
    Annanshogi,
    Kyotoshogi,
    Checkshogi,
  )

  val byId: Map[Int, Variant] = all map { v =>
    (v.id, v)
  } toMap

  val byKey: Map[String, Variant] = all map { v =>
    (v.key, v)
  } toMap

  val default: Variant = Standard

  def apply(id: Int): Option[Variant]     = byId get id
  def apply(key: String): Option[Variant] = byKey get key
  def orDefault(id: Int): Variant         = apply(id) | default
  def orDefault(key: String): Variant     = apply(key) | default

  def byName(name: String): Option[Variant] =
    all find (_.name.toLowerCase == name.toLowerCase)

  def exists(id: Int): Boolean = byId contains id

  val divisionSensibleVariants: Set[Variant] = Set(
    shogi.variant.Standard,
    shogi.variant.Annanshogi,
    shogi.variant.Checkshogi,
  )

}
