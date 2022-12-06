package shogi

sealed trait Color {

  final def -(role: Role) = Piece(this, role)

  final def fold[A](s: => A, g: => A): A = if (sente) s else g

  def unary_! : Color

  val letter: Char
  val name: String
  val engName: String

  final def bishop                = this - Bishop
  final def bishopPromoted        = this - BishopPromoted
  final def boar                  = this - Boar
  final def chariot               = this - Chariot
  final def copper                = this - Copper
  final def dragon                = this - Dragon
  final def dragonPromoted        = this - DragonPromoted
  final def eagle                 = this - Eagle
  final def elephant              = this - Elephant
  final def elephantPromoted      = this - ElephantPromoted
  final def falcon                = this - Falcon
  final def goBetween             = this - GoBetween
  final def gold                  = this - Gold
  final def horse                 = this - Horse
  final def horsePromoted         = this - HorsePromoted
  final def king                  = this - King
  final def kirin                 = this - Kirin
  final def knight                = this - Knight
  final def lance                 = this - Lance
  final def leopard               = this - Leopard
  final def lion                  = this - Lion
  final def lionPromoted          = this - LionPromoted
  final def ox                    = this - Ox
  final def pawn                  = this - Pawn
  final def phoenix               = this - Phoenix
  final def prince                = this - Prince
  final def promotedKnight        = this - PromotedKnight
  final def promotedLance         = this - PromotedLance
  final def promotedPawn          = this - PromotedPawn
  final def promotedSilver        = this - PromotedSilver
  final def queen                 = this - Queen
  final def queenPromoted         = this - QueenPromoted
  final def rook                  = this - Rook
  final def rookPromoted          = this - RookPromoted
  final def sideMover             = this - SideMover
  final def sideMoverPromoted     = this - SideMoverPromoted
  final def silver                = this - Silver
  final def stag                  = this - Stag
  final def tiger                 = this - Tiger
  final def tokin                 = this - Tokin
  final def verticalMover         = this - VerticalMover
  final def verticalMoverPromoted = this - VerticalMoverPromoted
  final def whale                 = this - Whale
  final def whiteHorse            = this - WhiteHorse

  final val sente = this == Color.Sente
  final val gote  = this == Color.Gote
}

object Color {

  final case class Map[A](sente: A, gote: A) {
    def apply(color: Color) = if (color.sente) sente else gote

    def update(color: Color, f: A => A) = {
      if (color.sente) copy(sente = f(sente))
      else copy(gote = f(gote))
    }

    def map[B](fs: A => B, fg: A => B) = copy(sente = fs(sente), gote = fg(gote))

    def map[B](f: A => B): Map[B] = map(f, f)

    def all: Seq[A] = Seq(sente, gote)

    def reduce[B](f: (A, A) => B) = f(sente, gote)

    def forall(pred: A => Boolean) = pred(sente) && pred(gote)

    def exists(pred: A => Boolean) = pred(sente) || pred(gote)
  }

  object Map {
    def apply[A](f: Color => A): Map[A] = Map(sente = f(Sente), gote = f(Gote))
  }

  case object Sente extends Color {

    def unary_! = Gote

    val letter  = 'b'
    val engName = "black"
    val name    = "sente"

    override val hashCode = 1
  }

  case object Gote extends Color {

    def unary_! = Sente

    val letter  = 'w'
    val engName = "white"
    val name    = "gote"

    override val hashCode = 2
  }

  def fromPly(ply: Int) = fromSente((ply & 1) == 0)

  def fromSente(isSente: Boolean): Color = if (isSente) Sente else Gote

  def fromName(n: String): Option[Color] =
    if (n == "black" || n == "sente") Some(Sente)
    else if (n == "white" || n == "gote") Some(Gote)
    else None

  def apply(c: Char): Option[Color] =
    if (c == 'b') Some(Sente)
    else if (c == 'w') Some(Gote)
    else None

  val sente: Color = Sente
  val gote: Color  = Gote

  val all = List[Color](Sente, Gote)

  def showResult(color: Option[Color]) =
    color match {
      case Some(shogi.Sente) => "1-0"
      case Some(shogi.Gote)  => "0-1"
      case None              => "1/2-1/2"
    }

  def fromResult(result: String): Option[Color] =
    result match {
      case "1-0" => Some(shogi.Sente)
      case "0-1" => Some(shogi.Gote)
      case _     => None
    }
}
