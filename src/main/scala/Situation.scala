package shogi

import cats.data.Validated
import cats.implicits._

import shogi.format.ParsedStep
import shogi.format.forsyth.Sfen
import shogi.format.usi.Usi
import shogi.variant.Variant

final case class Situation(
    board: Board,
    hands: Hands,
    color: Color,
    history: History,
    variant: Variant,
) {

  def apply(usi: Usi): Validated[String, Situation] =
    usi match {
      case u: Usi.Move => move(u)
      case u: Usi.Drop => drop(u)
    }

  def apply(parsedStep: ParsedStep): Validated[String, Situation] =
    parsedStep.toUsi(this) flatMap (apply _)

  // Moves

  lazy val moveActors: Map[Pos, MoveActor] = board.pieces collect {
    case (pos, piece) if piece.color == color =>
      (pos, MoveActor(piece, pos, this))
  }

  def moveActorAt(at: Pos): Option[MoveActor] = moveActors get at

  lazy val hasMoveDestinations: Boolean =
    moveActors.values.exists(_.destinations.nonEmpty)

  def moveDestsFrom(from: Pos): Option[List[Pos]] = moveActorAt(from) map (_.destinations)

  def move(usi: Usi.Move): Validated[String, Situation] =
    for {
      actor <- moveActorAt(usi.orig) toValid s"No piece on ${usi.orig}"
      _     <- Validated.cond(actor.piece is color, (), s"Not my piece on ${usi.orig}")
      capture = board(usi.dest).filter(_.color != color)
      _ <- Validated.cond(
        !usi.promotion || variant.canPromote(actor.piece, usi.orig, usi.dest, capture.isDefined),
        (),
        s"${actor.piece} cannot promote",
      )
      _ <- Validated.cond(
        usi.promotion || !variant.forcePromote(actor.piece, usi.dest),
        (),
        s"${actor.piece} needs to promote",
      )
      _ <- Validated.cond(
        usi.midStep.fold(actor.destinations contains usi.dest) { ms =>
          actor.lionMoveDestinationsMap.get(ms).exists(_ contains usi.dest)
        },
        (),
        s"Piece on ${usi.orig} cannot move to ${usi.dest}${usi.midStep.fold("")(ms => s" via $ms")}",
      )
      unpromotedRoleCapture = capture.flatMap(p => variant.unpromoteRoleForHand(p.role))
      updatedHands          =
        unpromotedRoleCapture
          .filter(_ => variant.supportsDrops)
          .fold(hands)(hands.store(color, _))
      updatedBoard <-
        (if (usi.promotion)
           board.promote(usi.orig, usi.dest, variant.promote)
         else
           board.move(
             usi.orig,
             usi.dest,
           )).map(b =>
          usi.midStep.fold(b)(b forceTake _),
        ) toValid s"Can't update board with ${usi.usi} in \n${toSfen}"
    } yield finalizeAfterUsi(updatedBoard, updatedHands, usi)

  // Drops

  private def addOtherSide(roles: List[DroppableRole]): List[DroppableRole] =
    roles.flatMap { role =>
      variant
        .promote(role) match {
        case Some(dRole: DroppableRole) => List(role, dRole)
        case _                          => List(role)
      }
    }

  lazy val dropActors: Map[DroppableRole, DropActor] = {
    val rolesInHand    = hands(color).roles
    val allRolesInHand =
      if (variant.supportsDroppingEitherSide) addOtherSide(rolesInHand) else rolesInHand
    allRolesInHand.map(r => (r, DropActor(r, this))).toMap
  }

  def dropActorOf(role: DroppableRole): Option[DropActor] = dropActors get role

  lazy val hasDropDestinations: Boolean =
    dropActors.values
      .exists(_.destinations.nonEmpty)

  def drop(usi: Usi.Drop): Validated[String, Situation] =
    for {
      _ <- Validated.cond(variant.supportsDrops, (), "Variant doesn't support drops")
      _ <- Validated.cond(
        variant.dropRoles contains usi.role,
        (),
        "Can't drop this role in this variant",
      )
      piece = Piece(color, usi.role)
      actor <- dropActorOf(usi.role) toValid s"No actor of $piece"
      _ <- Validated.cond(actor.destinations.contains(usi.pos), (), s"Dropping $piece is not valid")
      roleToTake =
        if (variant.supportsDroppingEitherSide) variant.unpromoteRoleForHand(usi.role)
        else usi.role.some
      updatedHands <- roleToTake.flatMap(
        hands.take(color, _),
      ) toValid s"No ${usi.role} to drop on ${usi.pos}"
      updatedBoard <- board.place(
        piece,
        usi.pos,
      ) toValid s"Can't drop ${usi.role} on ${usi.pos}, it's occupied"
    } yield finalizeAfterUsi(updatedBoard, updatedHands, usi)

  // Moves and drops

  def hasDestinations: Boolean =
    hasDropDestinations || hasMoveDestinations

  private def finalizeAfterUsi(
      updatedBoard: Board,
      updatedHands: Hands,
      usi: Usi,
  ): Situation = {
    val newSit = copy(board = updatedBoard, hands = updatedHands).switch
    val h      = history
      .withLastUsi(usi)
      .withLastLionCapture {
        val roleOpt = usi.positions.headOption.flatMap(board(_).map(_.role))
        if (variant.chushogi && roleOpt.exists(!Role.allLions.contains(_)))
          usi.positions
            .drop(1) // drop orig
            .reverse // start from the final dest
            .find(pos =>
              board(pos)
                .exists(p => (p is newSit.color) && Role.allLions.contains(p.role)),
            )
        else None
      }
      .withConsecutiveAttacks {
        if (variant.isAttacking(newSit, usi))
          history.consecutiveAttacks.add(!newSit.color)
        else history.consecutiveAttacks.reset(!newSit.color)
      }
      .withPositionHashes {
        val basePositionHashes =
          if (variant.isIrreversible(board, newSit.board, usi)) Array.empty: PositionHash
          else if (history.positionHashes.isEmpty) Hash(this)
          else history.positionHashes
        Hash(newSit) ++ basePositionHashes
      }
    newSit.withHistory(h)
  }

  // King safety

  lazy val check: Boolean = variant.check(board, color)

  lazy val checkSquares: List[Pos] = variant.checkSquares(board, color)

  // Not taking into account specific drop rules, cached here
  lazy val possibleDropDests: List[Pos] =
    Some(checkSquares)
      .filter(_.size == 1)
      .fold(
        variant.allPositions.filterNot(board.pieces contains _),
      ) { royals =>
        DropActor.blockades(this, royals.head)
      }

  // Results

  lazy val status: Option[Status] = variant.status(this)

  def winner: Option[Color] = variant.winner(this)

  def end: Boolean =
    status.isDefined

  // Util

  def valid(strict: Boolean) = variant.valid(this.board, this.hands, strict)

  def playable(strict: Boolean): Boolean =
    valid(strict) && !end && !copy(color = !color).check

  def isInsufficientMaterial: Boolean =
    variant.isInsufficientMaterial(board, hands)

  def materialImbalance: Int =
    board.pieces.values.foldLeft(0) { case (acc, Piece(c, r)) =>
      acc + variant.valueOfRole(r) * c.fold(1, -1)
    } + (hands(Sente).sum(variant.valueOfRole) - hands(Gote).sum(variant.valueOfRole))

  def withBoard(board: Board)                     = copy(board = board)
  def withHands(hands: Hands)                     = copy(hands = hands)
  def withColor(color: Color)                     = copy(color = color)
  def withHistory(history: History)               = copy(history = history)
  def withVariant(variant: shogi.variant.Variant) = copy(variant = variant)

  def switch = copy(color = !color)

  def visual: String = format.forsyth.Visual render this

  def toSfen: Sfen = Sfen(this)

  override def toString =
    s"${variant.name}\n$visual\nLast USI: ${history.lastUsi.fold("-")(_.usi)}\n"
}

object Situation {

  def apply(variant: shogi.variant.Variant): Situation =
    Situation(
      Board(variant),
      Hands(variant),
      Sente,
      History.empty,
      variant,
    )

  def apply(board: Board, hands: Hands, color: Color, variant: Variant): Situation =
    Situation(board, hands, color, History.empty, variant)
}
