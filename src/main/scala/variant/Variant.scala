package shogi
package variant

import cats.data.Validated
import cats.syntax.option._
import scala.annotation.unused

import shogi.format.forsyth.Sfen
import shogi.format.usi.Usi

// Correctness depends on singletons for each variant ID
abstract class Variant private[variant] (
    val id: Int,
    val key: String,
    val name: String,
    val title: String
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

  // True if piece will never be able to move from pos
  // Used both for drops and moves without promotion
  def pieceInDeadZone(piece: Piece, pos: Pos): Boolean =
    piece.role match {
      case Pawn | Knight | Lance if backrank(piece.color) == pos.rank            => true
      case Knight if math.abs(backrank(piece.color).index - pos.rank.index) == 1 => true
      case _                                                                     => false
    }

  def canPromote(piece: Piece, orig: Pos, dest: Pos, @unused capture: Boolean): Boolean =
    promote(piece.role).isDefined &&
      promotionRanks(piece.color).exists(r => r == dest.rank || r == orig.rank)

  def autoPromote = false

  def supportsDrops = true

  def supportsDroppingEitherSide = false

  def isInsideBoard(pos: Pos): Boolean =
    pos.file.index < numberOfFiles && pos.rank.index < numberOfRanks

  // Optimized for performance
  // Exists a piece of color on board, which passes the filter and attacks pos
  def posThreatened(board: Board, color: Color, pos: Pos, filter: Piece => Boolean = _ => true): Boolean =
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
      if ((a.piece is King) || a.situation.check) _ => true else (_.projectionDirs.nonEmpty)
    val stableKingPos = if (a.piece is King) None else a.situation.board.singleRoyalPosOf(a.color)
    a.unfilteredDestinations filterNot { dest =>
      (stableKingPos orElse Option.when(a.piece is King)(dest)) exists {
        posThreatened(
          a.situation.board.forceMove(a.piece, a.pos, dest),
          !a.color,
          _,
          filter
        )
      }
    }
  }

  def lionMoveFilter(a: MoveActor, midStep: Pos): List[Pos] =
    if (Role.allLions.contains(a.piece.role))
      a.shortUnfilteredDestinations filter { d =>
        d.dist(midStep) == 1 && a.situation.board(d).filter(_.color != a.color).fold(true) { capture =>
          d.dist(a.pos) == 1 || !Role.allLions.contains(capture.role) ||
          a.situation
            .board(midStep)
            .exists(midCapture => !(midCapture is Pawn) && !(midCapture is GoBetween)) ||
          (!posThreatened(
            a.situation.board.forceTake(a.pos).forceTake(midStep),
            !a.color,
            d,
            _ => true
          ) &&
            !posThreatened(
              a.situation.board.forceTake(a.pos),
              !a.color,
              d,
              p => (p is Pawn) || (p is GoBetween)
            ))
        }
      }
    else a.shortUnfilteredDestinations.filter(_.dist(midStep) == 1)

  def check(board: Board, color: Color): Boolean =
    board.royalPossOf(color) exists {
      posThreatened(board, !color, _)
    }

  def checkSquares(board: Board, color: Color): List[Pos] =
    board.royalPossOf(color).filter(posThreatened(board, !color, _))

  def longRangeThreatens(board: Board, p: Pos, dir: Direction, to: Pos): Boolean =
    dir(p) exists { next =>
      next == to || (isInsideBoard(next) && !board.pieces
        .contains(next) && longRangeThreatens(board, next, dir, to))
    }

  def dropRoles = handRoles

  // For example, can't drop a pawn on a file with another pawn of the same color
  def dropFilter(a: DropActor): List[Pos] = {
    def illegalPawn(d: Pos) =
      (a.piece is Pawn) && (
        a.situation.board.pieces.exists { case (pos, piece) =>
          a.piece == piece && pos.file == d.file
        } ||
          a.situation.board.singleRoyalPosOf(!a.situation.color).fold(false) { kingPos =>
            a.piece.eyes(d, kingPos) && a.situation
              .withBoard(a.situation.board.forcePlace(a.piece, d))
              .switch
              .checkmate
          }
      )
    a.situation.possibleDropDests.filterNot { d =>
      pieceInDeadZone(a.piece, d) || illegalPawn(d)
    }
  }

  // Could the position before the move occur ever again
  def isIrreversible(@unused before: Situation, @unused after: Situation, @unused usi: Usi): Boolean = false

  // for perpetual check
  def isAttacked(@unused before: Situation, @unused after: Situation, @unused usi: Usi): Boolean =
    after.check

  protected def unpromoteRoleForHand(role: Role): Option[DroppableRole] =
    handRoles
      .find(_ == role)
      .orElse(unpromote(role) match {
        case Some(dRole: DroppableRole) if handRoles.contains(dRole) => dRole.some
        case _                                                       => none
      })

  // Finalizes situation after usi, used both for moves and drops
  protected def finalizeSituation(beforeSit: Situation, board: Board, hands: Hands, usi: Usi): Situation = {
    val newSit = beforeSit.copy(board = board, hands = hands).switch
    val h = beforeSit.history
      .withLastMove(usi)
      .withLastLionCapture {
        val roleOpt = usi.positions.headOption.flatMap(beforeSit.board(_).map(_.role))
        if (
          roleOpt.exists(!Role.allLions.contains(_)) &&
          usi.positions.lastOption
            .flatMap(beforeSit board _)
            .exists(p => (p is newSit.color) && Role.allLions.contains(p.role))
        )
          usi.positions.lastOption
        else None
      }
      .withConsecutiveAttacks {
        if (isAttacked(beforeSit, newSit, usi))
          beforeSit.history.consecutiveAttacks.add(!newSit.color)
        else beforeSit.history.consecutiveAttacks.reset(!newSit.color)
      }
      .withPositionHashes {
        val basePositionHashes =
          if (isIrreversible(beforeSit, newSit, usi)) Array.empty: PositionHash
          else if (beforeSit.history.positionHashes.isEmpty) Hash(beforeSit)
          else beforeSit.history.positionHashes
        Hash(newSit) ++ basePositionHashes
      }
    newSit.withHistory(h)
  }

  def move(sit: Situation, usi: Usi.Move): Validated[String, Situation] =
    for {
      actor <- sit.moveActorAt(usi.orig) toValid s"No piece on ${usi.orig}"
      _     <- Validated.cond(actor is sit.color, (), s"Not my piece on ${usi.orig}")
      capture = sit.board(usi.dest).filter(_.color != sit.color)
      _ <- Validated.cond(
        !usi.promotion || canPromote(actor.piece, usi.orig, usi.dest, capture.isDefined) || autoPromote,
        (),
        s"${actor.piece} cannot promote"
      )
      _ <- Validated.cond(
        usi.promotion || !pieceInDeadZone(actor.piece, usi.dest),
        (),
        s"${actor.piece} needs to promote"
      )
      _ <- Validated.cond(
        usi.midStep.fold(actor.destinations contains usi.dest) { ms =>
          actor.lionMoveDestinationsMap.get(ms).exists(_ contains usi.dest)
        },
        (),
        s"Piece on ${usi.orig} cannot move to ${usi.dest}${usi.midStep.fold("")(ms => s" via $ms")}"
      )
      unpromotedRoleCapture = capture.flatMap(p => unpromoteRoleForHand(p.role))
      hands =
        unpromotedRoleCapture
          .filter(_ => supportsDrops)
          .fold(sit.hands)(sit.hands.store(sit.color, _))
      board <-
        (if (usi.promotion || (autoPromote && promote(actor.piece.role).isDefined))
           sit.board.promote(usi.orig, usi.dest, promote)
         else
           sit.board.move(
             usi.orig,
             usi.dest
           )).map(b =>
          usi.midStep.fold(b)(b forceTake _)
        ) toValid s"Can't update board with ${usi.usi} in \n${sit.toSfen}"
    } yield finalizeSituation(sit, board, hands, usi)

  def drop(sit: Situation, usi: Usi.Drop): Validated[String, Situation] =
    for {
      _ <- Validated.cond(sit.variant.supportsDrops, (), "Variant doesn't support drops")
      _ <- Validated.cond(dropRoles contains usi.role, (), "Can't drop this role in this variant")
      piece = Piece(sit.color, usi.role)
      actor <- sit.dropActorOf(piece) toValid s"No actor of $piece"
      _     <- Validated.cond(actor.destinations.contains(usi.pos), (), s"Dropping $piece is not valid")
      roleToTake = if (supportsDroppingEitherSide) unpromoteRoleForHand(usi.role) else usi.role.some
      hands <- roleToTake.flatMap(
        sit.hands.take(sit.color, _)
      ) toValid s"No ${usi.role} to drop on ${usi.pos}"
      board <- sit.board.place(piece, usi.pos) toValid s"Can't drop ${usi.role} on ${usi.pos}, it's occupied"
    } yield finalizeSituation(sit, board, hands, usi)

  def impasse(@unused sit: Situation): Boolean = false

  def perpetualCheck(sit: Situation): Boolean =
    sit.history.fourfoldRepetition && sit.history.firstRepetitionDistance.exists { dist =>
      (dist + 1) <= sit.history.consecutiveAttacks(!sit.color)
    }

  def stalemate(sit: Situation): Boolean =
    !sit.check && !sit.hasMoveDestinations && !sit.hasDropDestinations

  def checkmate(sit: Situation): Boolean =
    sit.check && !sit.hasMoveDestinations && !sit.hasDropDestinations

  def bareKing(@unused sit: Situation, @unused color: Color): Boolean = false

  def royalsLost(@unused sit: Situation): Boolean = false

  def repetition(sit: Situation): Boolean = sit.history.fourfoldRepetition

  // Player wins or loses after their move
  def winner(sit: Situation): Option[Color] =
    if (sit.checkmate || sit.stalemate || sit.bareKing(sit.color) || sit.royalsLost) Some(!sit.color)
    else if (sit.bareKing(!sit.color) || sit.impasse || sit.perpetualCheck) Some(sit.color)
    else None

  // Returns the material imbalance in pawns
  def materialImbalance(sit: Situation): Int =
    sit.board.pieces.values.foldLeft(0) { case (acc, Piece(color, role)) =>
      acc + valueOfRole(role) * color.fold(1, -1)
    } + (sit.hands(Sente).sum(valueOfRole) - sit.hands(Gote).sum(valueOfRole))

  // Returns true if neither player can win. The game should end immediately.
  def isInsufficientMaterial(sit: Situation) =
    sit.hands.isEmpty && sit.board.pieces.size <= 2 &&
      sit.board.pieces.forall { p => p._2 is King }

  protected def hasUnmovablePieces(board: Board) =
    board.pieces.exists { case (pos, piece) =>
      pieceInDeadZone(piece, pos)
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

  def valid(sit: Situation, strict: Boolean) =
    validHands(sit.hands) && Color.all.forall(validBoardSide(sit.board, strict) _)

  def standard   = this == Standard
  def minishogi  = this == Minishogi
  def chushogi   = this == Chushogi
  def annanshogi = this == Annanshogi

  override def toString = s"Variant($name)"

  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]

  override def hashCode: Int = id

}

object Variant {

  val all = List[Variant](
    Standard,
    Minishogi,
    Chushogi,
    Annanshogi
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

  def byKeyOrName(keyOrName: String): Option[Variant] =
    byKey.get(keyOrName).orElse(byName(keyOrName))

  def exists(id: Int): Boolean = byId contains id

  val divisionSensibleVariants: Set[Variant] = Set(
    shogi.variant.Standard,
    shogi.variant.Annanshogi
  )

}
