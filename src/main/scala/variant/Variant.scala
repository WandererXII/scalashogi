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

  def allSquares: List[Square]

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

  def forcePromote(piece: Piece, to: Square): Boolean =
    piece.role match {
      case Pawn | Knight | Lance if backrank(piece.color) == to.rank            => true
      case Knight if math.abs(backrank(piece.color).index - to.rank.index) == 1 => true
      case _                                                                    => false
    }

  def canPromote(piece: Piece, orig: Square, dest: Square, @unused capture: Boolean): Boolean =
    promote(piece.role).isDefined &&
      promotionRanks(piece.color).exists(r => r == dest.rank || r == orig.rank)

  def supportsDrops = true

  def supportsDroppingEitherSide = false

  def isInsideBoard(square: Square): Boolean =
    square.file.index < numberOfFiles && square.rank.index < numberOfRanks

  // Optimized for performance
  // Exists a piece of color on board, which passes the filter and attacks square
  def squareThreatened(
      board: Board,
      color: Color,
      square: Square,
      filter: Piece => Boolean = _ => true,
  ): Boolean =
    board.pieces exists {
      case (from, piece) if piece.color == color && filter(piece) && piece.eyes(from, square) =>
        piece.projectionDirs.isEmpty || piece.directDirs.exists(dir =>
          dir(from).contains(square),
        ) ||
        Square.findDirection(from, square).exists { dir =>
          longRangeThreatens(board, from, dir, square)
        }
      case _ => false
    }

  def attackingPiece(piece: Piece, @unused square: Square, @unused board: Board): Piece = piece

  // Filters out moves that would put the king in danger
  // Critical function - optimize for performance
  def moveFilter(a: MoveActor): List[Square] = {
    // only long range roles, since you can only unpin a check from a role with projection
    val filter: Piece => Boolean =
      if ((a.piece is King) || a.check) _ => true else (_.projectionDirs.nonEmpty)
    val stableKingSquare = if (a.piece is King) None else a.board.singleRoyalSquareOf(a.color)
    a.unfilteredDestinations filterNot { dest =>
      (stableKingSquare orElse Option.when(a.piece is King)(dest)) exists {
        squareThreatened(
          a.board.forceMove(a.piece, a.square, dest),
          !a.color,
          _,
          filter,
        )
      }
    }
  }

  def noOppositeCheck = true

  def check(board: Board, color: Color): Boolean =
    board.royalSquaresOf(color).exists(squareThreatened(board, !color, _))

  def checkSquares(board: Board, color: Color): List[Square] =
    board.royalSquaresOf(color).filter(squareThreatened(board, !color, _))

  def longRangeThreatens(board: Board, from: Square, dir: Direction, to: Square): Boolean =
    dir(from) exists { next =>
      next == to || (isInsideBoard(next) && !board.pieces
        .contains(next) && longRangeThreatens(board, next, dir, to))
    }

  def dropRoles = handRoles

  def dropFilterDoublePawn(a: DropActor, d: Square): Boolean =
    (a.piece is Pawn) && (
      a.board.pieces.exists { case (square, piece) =>
        a.piece == piece && square.file == d.file
      }
    )

  def dropFilterPawnCheckmate(a: DropActor, d: Square): Boolean =
    (a.piece is Pawn) && (
      a.board.singleRoyalSquareOf(!a.color).fold(false) { kingSquare =>
        a.piece.eyes(d, kingSquare) && !(a.position
          .withBoard(a.board.forcePlace(a.piece, d))
          .switch
          .hasDestinations)
      }
    )

  def dropFilter(a: DropActor): List[Square] = {
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
  def isAttacking(after: Position, @unused usi: Usi): Boolean =
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

  def status(position: Position): Option[Status] =
    if (!position.hasDestinations) {
      if (position.check) Status.Mate.some
      else Status.Stalemate.some
    } else if (Impasse(position)) Status.Impasse27.some
    else if (position.history.fourfoldRepetition) {
      if (position.history.perpetualCheckAttacker.isDefined) Status.PerpetualCheck.some
      else Status.Repetition.some
    } else if (isInsufficientMaterial(position.board, position.hands)) Status.Draw.some
    else none

  def winner(position: Position): Option[Color] =
    position.status flatMap { status =>
      status match {
        case Status.Mate | Status.Stalemate | Status.Check => (!position.color).some
        case Status.Impasse27                              => (position.color).some
        case Status.PerpetualCheck => position.history.perpetualCheckAttacker.map(!_)
        case _                     => none
      }
    }

  protected def hasUnmovablePieces(board: Board) =
    board.pieces.exists { case (square, piece) =>
      forcePromote(piece, square)
    }

  protected def hasDoublePawns(board: Board, color: Color) = {
    val pawnFiles = board.pieces.collect {
      case (square, piece) if (piece is Pawn) && (piece is color) =>
        square.file
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
    board.pieces.keys.forall(isInsideBoard) &&
      validHands(hands) && Color.all.forall(validBoardSide(board, strict) _)

  def standard   = this == Standard
  def minishogi  = this == Minishogi
  def chushogi   = this == Chushogi
  def annanshogi = this == Annanshogi
  def kyotoshogi = this == Kyotoshogi
  def checkshogi = this == Checkshogi
  def dobutsu    = this == Dobutsu

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
    Dobutsu,
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
