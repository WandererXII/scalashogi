package shogi
package format
package forsyth

import shogi.variant.{ Annanshogi, Chushogi, Kyotoshogi, Minishogi, Standard, Variant }

object SfenUtils {

  def toForsyth(role: Role, variant: Variant): Option[String] =
    variant match {
      case Standard | Annanshogi =>
        toForsythStandard get role
      case Minishogi =>
        toForsythMinishogi get role
      case Chushogi =>
        toForsythChushogi get role
      case Kyotoshogi =>
        toForsythKyotoshogi get role
    }

  def toRole(str: String, variant: Variant): Option[Role] =
    variant match {
      case Standard | Annanshogi =>
        toRoleStandard get str
      case Minishogi =>
        toRoleMinishogi get str
      case Chushogi =>
        toRoleChushogi get str
      case Kyotoshogi =>
        toRoleKyotoshogi get str
    }

  def toForsyth(piece: Piece, variant: Variant): Option[String] =
    toForsyth(piece.role, variant) map { f =>
      if (piece.color.sente) f.toUpperCase else f
    }

  def toPiece(str: String, variant: Variant): Option[Piece] = {
    val lower = str.toLowerCase
    toRole(lower, variant) map { r =>
      Piece(Color.fromSente(lower != str), r)
    }
  }

  private val toForsythStandard: Map[Role, String] = Map(
    King           -> "k",
    Pawn           -> "p",
    Lance          -> "l",
    Knight         -> "n",
    Silver         -> "s",
    Gold           -> "g",
    Bishop         -> "b",
    Rook           -> "r",
    Tokin          -> "+p",
    PromotedLance  -> "+l",
    PromotedKnight -> "+n",
    PromotedSilver -> "+s",
    Horse          -> "+b",
    Dragon         -> "+r"
  )

  private val toRoleStandard: Map[String, Role] = toForsythStandard map { case (k, v) => v -> k }

  private val toForsythMinishogi: Map[Role, String] =
    toForsythStandard filter { case (k, _) => Minishogi.allRoles contains k }

  private val toRoleMinishogi: Map[String, Role] =
    toForsythMinishogi map { case (k, v) => v -> k }

  private val toForsythChushogi: Map[Role, String] =
    (toForsythStandard filter { case (k, _) => Chushogi.allRoles contains k }) ++ Map(
      WhiteHorse            -> "+l",
      Leopard               -> "f",
      BishopPromoted        -> "+f",
      Copper                -> "c",
      SideMoverPromoted     -> "+c",
      VerticalMoverPromoted -> "+s",
      RookPromoted          -> "+g",
      Elephant              -> "e",
      Prince                -> "+e",
      Chariot               -> "a",
      Whale                 -> "+a",
      HorsePromoted         -> "+b",
      Tiger                 -> "t",
      Stag                  -> "+t",
      Kirin                 -> "o",
      LionPromoted          -> "+o",
      Phoenix               -> "x",
      QueenPromoted         -> "+x",
      SideMover             -> "m",
      Boar                  -> "+m",
      VerticalMover         -> "v",
      Ox                    -> "+v",
      DragonPromoted        -> "+r",
      Horse                 -> "h",
      Falcon                -> "+h",
      Dragon                -> "d",
      Eagle                 -> "+d",
      Lion                  -> "n",
      Queen                 -> "q",
      PromotedPawn          -> "+p",
      GoBetween             -> "i",
      ElephantPromoted      -> "+i"
    )

  private val toRoleChushogi: Map[String, Role] = toForsythChushogi map { case (k, v) => v -> k }

  private val toForsythKyotoshogi: Map[Role, String] = Map(
    King   -> "k",
    Pawn   -> "p",
    Lance  -> "l",
    Knight -> "n",
    Silver -> "s",
    Gold   -> "g",
    Bishop -> "b",
    Rook   -> "r",
    Tokin  -> "t"
  )

  private val toRoleKyotoshogi: Map[String, Role] = toForsythKyotoshogi map { case (k, v) => v -> k }

  val allForsyth: List[String] = (Role.all flatMap { r: Role =>
    Variant.all flatMap { v =>
      toForsyth(r, v)
    }
  }).distinct

  val allForsythDroppable: List[String] = (Role.allDroppable flatMap { r: Role =>
    Variant.all.filter(_.supportsDrops) flatMap { v =>
      toForsyth(r, v)
    }
  }).distinct

}
