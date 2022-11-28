package shogi
package format
package csa

object CsaUtils {
  def toPiece(s: String): Option[Piece] =
    toRole(s.drop(1)) map {
      Piece(Color.fromSente(s.take(1) == "+"), _)
    }

  def toCsa(p: Piece): Option[String] =
    toCsa(p.role) map { c =>
      if (p.color.sente) s"+$c" else s"-$c"
    }

  def toCsa(role: Role): Option[String] =
    toCsaStandard get role

  def toRole(str: String): Option[Role] =
    toRoleStandard get str

  private val toCsaStandard: Map[Role, String] = Map(
    King           -> "OU",
    Pawn           -> "FU",
    Lance          -> "KY",
    Knight         -> "KE",
    Silver         -> "GI",
    Gold           -> "KI",
    Bishop         -> "KA",
    Rook           -> "HI",
    Tokin          -> "TO",
    PromotedLance  -> "NY",
    PromotedKnight -> "NK",
    PromotedSilver -> "NG",
    Horse          -> "UM",
    Dragon         -> "RY"
  )

  private val toRoleStandard: Map[String, Role] = toCsaStandard map { case (k, v) => v -> k }

}
