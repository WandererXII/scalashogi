package shogi
package variant

import cats.syntax.option._

import shogi.Pos._
import shogi.format.forsyth.Sfen

case object Chushogi
    extends Variant(
      id = 3,
      key = "chushogi",
      name = "Chushogi",
      shortName = "Chushogi",
      title = "The most popular historical variant of modern shogi"
    ) {

  val initialSfen = Sfen("lfcsgekgscfl/a1b1txot1b1a/mvrhdqndhrvm/pppppppppppp/3i4i3/12/12/3I4I3/PPPPPPPPPPPP/MVRHDNQDHRVM/A1B1TOXT1B1A/LFCSGKEGSCFL b - 1")

  val numberOfRanks = 12
  val numberOfFiles = 12

  val allPositions = Pos.all

  val pieces =
    Map(
      SQ1A -> Gote.lance,
      SQ1B -> Gote.chariot,
      SQ1C -> Gote.sideMover,
      SQ1D -> Gote.pawn,
      SQ2A -> Gote.leopard,
      SQ2C -> Gote.verticalMover,
      SQ2D -> Gote.pawn,
      SQ3A -> Gote.copper,
      SQ3B -> Gote.bishop,
      SQ3C -> Gote.rook,
      SQ3D -> Gote.pawn,
      SQ4A -> Gote.silver,
      SQ4C -> Gote.horse,
      SQ4D -> Gote.pawn,
      SQ4E -> Gote.goBetween,
      SQ5A -> Gote.gold,
      SQ5B -> Gote.tiger,
      SQ5C -> Gote.dragon,
      SQ5D -> Gote.pawn,
      SQ6A -> Gote.king,
      SQ6B -> Gote.kirin,
      SQ6C -> Gote.lion,
      SQ6D -> Gote.pawn,
      SQ7A -> Gote.elephant,
      SQ7B -> Gote.phoenix,
      SQ7C -> Gote.queen,
      SQ7D -> Gote.pawn,
      SQ8A -> Gote.gold,
      SQ8B -> Gote.tiger,
      SQ8C -> Gote.dragon,
      SQ8D -> Gote.pawn,
      SQ9A -> Gote.silver,
      SQ9C -> Gote.horse,
      SQ9D -> Gote.pawn,
      SQ9E -> Gote.goBetween,
      SQ10A -> Gote.copper,
      SQ10B -> Gote.bishop,
      SQ10C -> Gote.rook,
      SQ10D -> Gote.pawn,
      SQ11A -> Gote.leopard,
      SQ11C -> Gote.verticalMover,
      SQ11D -> Gote.pawn,
      SQ11I -> Sente.pawn,
      SQ12A -> Gote.lance,
      SQ12B -> Gote.chariot,
      SQ12C -> Gote.sideMover,
      SQ12D -> Gote.pawn,
      SQ1I -> Sente.pawn,
      SQ1J -> Sente.sideMover,
      SQ1K -> Sente.chariot,
      SQ1L -> Sente.lance,
      SQ2I -> Sente.pawn,
      SQ2J -> Sente.verticalMover,
      SQ2L -> Sente.leopard,
      SQ3I -> Sente.pawn,
      SQ3J -> Sente.rook,
      SQ3K -> Sente.bishop,
      SQ3L -> Sente.copper,
      SQ4H -> Sente.goBetween,
      SQ4I -> Sente.pawn,
      SQ4J -> Sente.horse,
      SQ4L -> Sente.silver,
      SQ5I -> Sente.pawn,
      SQ5J -> Sente.dragon,
      SQ5K -> Sente.tiger,
      SQ5L -> Sente.gold,
      SQ6I -> Sente.pawn,
      SQ6J -> Sente.queen,
      SQ6K -> Sente.phoenix,
      SQ6L -> Sente.elephant,
      SQ7I -> Sente.pawn,
      SQ7J -> Sente.lion,
      SQ7K -> Sente.kirin,
      SQ7L -> Sente.king,
      SQ8I -> Sente.pawn,
      SQ8J -> Sente.dragon,
      SQ8K -> Sente.tiger,
      SQ8L -> Sente.gold,
      SQ9H -> Sente.goBetween,
      SQ9I -> Sente.pawn,
      SQ9J -> Sente.horse,
      SQ9L -> Sente.silver,
      SQ10I -> Sente.pawn,
      SQ10J -> Sente.rook,
      SQ10K -> Sente.bishop,
      SQ10L -> Sente.copper,
      SQ11J -> Sente.verticalMover,
      SQ11L -> Sente.leopard,
      SQ12I -> Sente.pawn,
      SQ12J -> Sente.sideMover,
      SQ12K -> Sente.chariot,
      SQ12L -> Sente.lance
    )

  val allRoles = List(
    Bishop,
    BishopPromoted,
    Boar,
    Chariot,
    Copper,
    Dragon,
    DragonPromoted,
    Eagle,
    Elephant,
    ElephantPromoted,
    Falcon,
    Gold,
    GoBetween,
    Horse,
    HorsePromoted,
    King,
    Kirin,
    Lance,
    Leopard,
    Lion,
    LionPromoted,
    Ox,
    Phoenix,
    Queen,
    QueenPromoted,
    Rook,
    RookPromoted,
    Pawn,
    Prince,
    PromotedPawn,
    SideMover,
    SideMoverPromoted,
    Silver,
    Stag,
    Tiger,
    VerticalMover,
    VerticalMoverPromoted,
    Whale,
    WhiteHorse
  )

  val handRoles = Nil

  def promote(role: Role): Option[Role] =
    role match {
      case Pawn => PromotedPawn.some;
      case GoBetween => ElephantPromoted.some;
      case SideMover => Boar.some;
      case VerticalMover => Ox.some;
      case Rook => DragonPromoted.some;
      case Bishop => HorsePromoted.some;
      case Dragon => Eagle.some;
      case Horse => Falcon.some;
      case Elephant => Prince.some;
      case Chariot => Whale.some;
      case Tiger => Stag.some;
      case Kirin => LionPromoted.some;
      case Phoenix => QueenPromoted.some;
      case Lance => WhiteHorse.some;
      case Leopard => BishopPromoted.some;
      case Copper => SideMoverPromoted.some;
      case Silver => VerticalMoverPromoted.some;
      case Gold => RookPromoted.some;
      case _ => none;
    }

  def unpromote(role: Role): Option[Role] =
    role match {
      case PromotedPawn => Pawn.some
      case ElephantPromoted => GoBetween.some
      case Boar => SideMover.some
      case Ox => VerticalMover.some
      case DragonPromoted => Rook.some
      case HorsePromoted => Bishop.some
      case Eagle => Dragon.some
      case Falcon => Horse.some
      case Prince => Elephant.some
      case Whale => Chariot.some
      case Stag => Tiger.some
      case LionPromoted => Kirin.some
      case QueenPromoted => Phoenix.some
      case WhiteHorse => Lance.some
      case BishopPromoted => Leopard.some
      case SideMoverPromoted => Copper.some
      case VerticalMoverPromoted => Silver.some
      case RookPromoted => Gold.some
      case _              => None
    }
  
  def backrank(color: Color) =
    if (color.sente) Rank.A else Rank.L

  def promotionRanks(color: Color) =
    if (color.sente) List(Rank.A, Rank.B, Rank.C, Rank.D) else List(Rank.I, Rank.J, Rank.K, Rank.L)

  override def perpetualCheck(sit: Situation): Boolean =
    sit.history.fourfoldRepetition && sit.history.firstRepetitionDistance.exists { dist =>
      (dist + 1) <= sit.history.consecutiveAttacks(!sit.color)
    }

  override def stalemate(sit: Situation): Boolean = !sit.hasMoveDestinations

  override def checkmate(sit: Situation): Boolean = false

}