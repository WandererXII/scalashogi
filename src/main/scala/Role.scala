package shogi

import scala.annotation.nowarn

sealed trait Role {
  lazy val name: String = toString.toLowerCase

  // For generating long-range moves, moves until stopped, e.g. bishop
  val senteProjectionDirs: Directions
  val goteProjectionDirs: Directions

  // For generating moves to adjacent squares or jumps, consumed after one use, e.g. pawn, knight
  val senteDirectDirs: Directions
  val goteDirectDirs: Directions

  // For generating second destinations of lion pieces after moving to `to`
  @nowarn
  def senteLionDirs(from: Pos, to: Pos): Directions = Nil
  @nowarn
  def goteLionDirs(from: Pos, to: Pos): Directions = Nil

  // Can move from `from` position to `to` position assuming empty max-sized board, for optimization
  def senteEyes(from: Pos, to: Pos): Boolean
  def goteEyes(from: Pos, to: Pos): Boolean
}

sealed trait DroppableRole extends Role

case object Bishop extends DroppableRole {
  val senteProjectionDirs = List(_.upLeft, _.upRight, _.downLeft, _.downRight)
  val goteProjectionDirs  = senteProjectionDirs

  val senteDirectDirs     = Nil
  val goteDirectDirs      = Nil

  def senteEyes(from: Pos, to: Pos) = (from onSameDiagonal to) && from != to
  def goteEyes(from: Pos, to: Pos) = senteEyes(from, to)
}

case object BishopPromoted extends Role {
  val senteProjectionDirs = Bishop.senteProjectionDirs
  val goteProjectionDirs  = Bishop.goteProjectionDirs

  val senteDirectDirs     = Bishop.senteDirectDirs
  val goteDirectDirs      = Bishop.goteDirectDirs

  def senteEyes(from: Pos, to: Pos) = Bishop.senteEyes(from, to)
  def goteEyes(from: Pos, to: Pos) = Bishop.goteEyes(from, to)
}

case object Boar extends Role {
  val senteProjectionDirs = List(_.upRight, _.right, _.downRight, _.downLeft, _.left, _.upLeft)
  val goteProjectionDirs  = senteProjectionDirs

  val senteDirectDirs     = Nil
  val goteDirectDirs      = Nil

  def senteEyes(from: Pos, to: Pos) = 
    ((from onSameDiagonal to) || (from isSameRank to)) && from != to
  def goteEyes(from: Pos, to: Pos) = senteEyes(from ,to)
}

case object Chariot extends Role {
  val senteProjectionDirs = List(_.up, _.down)
  val goteProjectionDirs  = senteProjectionDirs

  val senteDirectDirs     = Nil
  val goteDirectDirs      = Nil

  def senteEyes(from: Pos, to: Pos) = (from isSameFile to) && from != to
  def goteEyes(from: Pos, to: Pos)  = senteEyes(from, to)
}

case object Copper extends Role {
  val senteProjectionDirs = Nil
  val goteProjectionDirs  = Nil

  val senteDirectDirs     = List(_.up, _.upLeft, _.upRight, _.down)
  val goteDirectDirs      = List(_.up, _.down, _.downLeft, _.downRight)

  def senteEyes(from: Pos, to: Pos) = 
    (from touches to) && ((from isSameFile to) || (from isBelow to)) 
  def goteEyes(from: Pos, to: Pos) =
    (from touches to) && ((from isSameFile to) || (from isAbove to))
}

case object Dragon extends Role {
  val senteProjectionDirs = Rook.senteProjectionDirs
  val goteProjectionDirs  = Rook.goteProjectionDirs

  val senteDirectDirs     = Bishop.senteProjectionDirs
  val goteDirectDirs      = Bishop.goteProjectionDirs

  def senteEyes(from: Pos, to: Pos) = 
    (from touches to) || Rook.senteEyes(from, to)
  def goteEyes(from: Pos, to: Pos) = senteEyes(from, to)
}

case object DragonPromoted extends Role {
  val senteProjectionDirs = Dragon.senteProjectionDirs
  val goteProjectionDirs  = Dragon.goteProjectionDirs

  val senteDirectDirs     = Dragon.senteDirectDirs
  val goteDirectDirs      = Dragon.goteDirectDirs

  def senteEyes(from: Pos, to: Pos) = Dragon.senteEyes(from, to)
  def goteEyes(from: Pos, to: Pos) = Dragon.goteEyes(from, to)
}

case object Eagle extends Role {
  val senteProjectionDirs = List(_.up, _.right, _.downRight, _.down, _.downLeft, _.left)
  val goteProjectionDirs  = List(_.up, _.upRight, _.right, _.down, _.left, _.upLeft)

  val senteDirectDirs: Directions = List(
    _.upRight,
    p => Pos.at(p.file.index + 2, p.rank.index - 2),
    _.upLeft,
    p => Pos.at(p.file.index - 2, p.rank.index - 2)
  )
  val goteDirectDirs: Directions = List(
    _.downRight,
    p => Pos.at(p.file.index + 2, p.rank.index + 2),
    _.downLeft,
    p => Pos.at(p.file.index - 2, p.rank.index + 2)
  )

  override def senteLionDirs(from: Pos, to: Pos): Directions =
    if (from.upRight.exists(_==to)) List(_.upRight, _.downLeft)
    else if (from.upLeft.exists(_==to)) List(_.downLeft, _.upLeft)
    else Nil
  override def goteLionDirs(from: Pos, to: Pos): Directions =
    if (from.downRight.exists(_==to)) List(_.downRight, _.upLeft)
    else if (from.upLeft.exists(_==to)) List(_.upRight, _.downLeft)
    else Nil

  def senteEyes(from: Pos, to: Pos) =
    Rook.senteEyes(from, to) || ((from onSameDiagonal to) && ((from isAbove to) || (from xDist to) == 2 || (from xDist to) == 1))
  def goteEyes(from: Pos, to: Pos) =
    Rook.senteEyes(from, to) || ((from onSameDiagonal to) && ((from isBelow to) || (from xDist to) == 2 || (from xDist to) == 1))
}

case object Elephant extends Role {
  val senteProjectionDirs = Nil
  val goteProjectionDirs  = Nil

  val senteDirectDirs     = List(_.up, _.upLeft, _.upRight, _.left, _.right, _.downLeft, _.downRight)
  val goteDirectDirs      = List(_.down, _.downLeft, _.downRight, _.left, _.right, _.upRight, _.upLeft)

  def senteEyes(from: Pos, to: Pos) = 
    (from touches to) && (!(from isSameFile to) || (from isBelow to))
  def goteEyes(from: Pos, to: Pos) =
    (from touches to) && (!(from isSameFile to) || (from isAbove to))
}

case object ElephantPromoted extends Role {
  val senteProjectionDirs = Elephant.senteProjectionDirs
  val goteProjectionDirs  = Elephant.goteProjectionDirs

  val senteDirectDirs     = Elephant.senteDirectDirs
  val goteDirectDirs      = Elephant.goteDirectDirs

  def senteEyes(from: Pos, to: Pos) = Elephant.senteEyes(from, to)
  def goteEyes(from: Pos, to: Pos) = Elephant.goteEyes(from, to)
}

case object Falcon extends Role {
  val senteProjectionDirs = List(_.upRight, _.right, _.downRight, _.down, _.downLeft, _.left, _.upLeft)
  val goteProjectionDirs  = List(_.up, _.upRight, _.right, _.downRight, _.downLeft, _.left, _.upLeft)

  val senteDirectDirs     = List(
    _.up,
    p => Pos.at(p.file.index, p.rank.index - 2),
  )
  val goteDirectDirs      = List(
    _.down,
    p => Pos.at(p.file.index, p.rank.index + 2),
  )

  override def senteLionDirs(from: Pos, to: Pos): Directions =
    if (from.up.exists(_==to)) List(_.up, _.down)
    else Nil
  override def goteLionDirs(from: Pos, to: Pos): Directions =
    if (from.down.exists(_==to)) List(_.up, _.down)
    else Nil

  def senteEyes(from: Pos, to: Pos) =
    (Bishop.senteEyes(from, to) || (from isSameRank to) || ((from isSameFile to) && ((from isAbove to) || (from yDist to) <= 2))) && from != to
  def goteEyes(from: Pos, to: Pos) =
    (Bishop.senteEyes(from, to) || (from isSameRank to) || ((from isSameFile to) && ((from isBelow to) || (from yDist to) <= 2))) && from != to

}

case object GoBetween extends Role {
  val senteProjectionDirs = Nil
  val goteProjectionDirs  = Nil

  val senteDirectDirs     = List(_.up, _.down)
  val goteDirectDirs      = senteDirectDirs


  def senteEyes(from: Pos, to: Pos) = 
    (from isSameFile to) && ((from yDist to) == 1)
  def goteEyes(from: Pos, to: Pos) = senteEyes(from, to)
}

case object Gold extends DroppableRole {
  val senteProjectionDirs = Nil
  val goteProjectionDirs  = Nil

  val senteDirectDirs     = List(_.up, _.down, _.left, _.right, _.upLeft, _.upRight)
  val goteDirectDirs      = List(_.up, _.down, _.left, _.right, _.downLeft, _.downRight)

  def senteEyes(from: Pos, to: Pos) = 
    (from touches to) && ((to isAbove from) || (to onSameLine from))
  def goteEyes(from: Pos, to: Pos) =
    (from touches to) && ((to isBelow from) || (to onSameLine from))
}

case object Horse extends Role {
  val senteProjectionDirs = Bishop.senteProjectionDirs
  val goteProjectionDirs  = Bishop.goteProjectionDirs

  val senteDirectDirs     = Rook.senteProjectionDirs
  val goteDirectDirs      = Rook.goteProjectionDirs

  def senteEyes(from: Pos, to: Pos) = 
    (from touches to) || Bishop.senteEyes(from, to)
  def goteEyes(from: Pos, to: Pos) = senteEyes(from, to)
}

case object HorsePromoted extends Role {
  val senteProjectionDirs = Horse.senteProjectionDirs
  val goteProjectionDirs  = Horse.goteProjectionDirs

  val senteDirectDirs     = Horse.senteDirectDirs
  val goteDirectDirs      = Horse.goteDirectDirs

  def senteEyes(from: Pos, to: Pos) = Horse.senteEyes(from, to)
  def goteEyes(from: Pos, to: Pos) = Horse.goteEyes(from, to)
}

case object King extends Role {
  val senteProjectionDirs = Nil
  val goteProjectionDirs  = Nil

  val senteDirectDirs     = Rook.senteProjectionDirs ::: Bishop.senteProjectionDirs
  val goteDirectDirs      = senteDirectDirs

  def senteEyes(from: Pos, to: Pos) = from touches to
  def goteEyes(from: Pos, to: Pos) = senteEyes(from, to)
}

case object Kirin extends Role {
  val senteProjectionDirs = Nil
  val goteProjectionDirs  = Nil

  val senteDirectDirs: Directions = List[Direction](
    p => Pos.at(p.file.index, p.rank.index - 2),
    p => Pos.at(p.file.index + 2, p.rank.index),
    p => Pos.at(p.file.index, p.rank.index + 2),
    p => Pos.at(p.file.index - 2, p.rank.index)
  ) ::: Bishop.senteProjectionDirs
  val goteDirectDirs = senteDirectDirs

  def senteEyes(from: Pos, to: Pos) = 
    ((from touches to) && !(from onSameLine to)) || ((from onSameLine to) && (from dist to) == 2)
  def goteEyes(from: Pos, to: Pos) = senteEyes(from, to)
}

case object Knight extends DroppableRole {
  val senteProjectionDirs = Nil
  val goteProjectionDirs  = Nil

  val senteDirectDirs: Directions = List(
    p => Pos.at(p.file.index - 1, p.rank.index - 2),
    p => Pos.at(p.file.index + 1, p.rank.index - 2)
  )
  val goteDirectDirs: Directions = List(
    p => Pos.at(p.file.index - 1, p.rank.index + 2),
    p => Pos.at(p.file.index + 1, p.rank.index + 2)
  )

  def senteEyes(from: Pos, to: Pos) = 
    (from xDist to) == 1 && (from yDist to) == 2 && (to isAbove from)
  def goteEyes(from: Pos, to: Pos) =
    (from xDist to) == 1 && (from yDist to) == 2 && (to isBelow from)
}

case object Lance extends DroppableRole {
  val senteProjectionDirs = List(_.up)
  val goteProjectionDirs  = List(_.down)

  val senteDirectDirs     = Nil
  val goteDirectDirs      = Nil

  def senteEyes(from: Pos, to: Pos) = 
    (from isSameFile to) && (from isBelow to)
  def goteEyes(from: Pos, to: Pos) =
    (from isSameFile to) && (from isAbove to)
}

case object Leopard extends Role {
  val senteProjectionDirs = Nil
  val goteProjectionDirs  = Nil

  val senteDirectDirs     = List(_.up, _.upLeft, _.upRight, _.down, _.downLeft, _.downRight)
  val goteDirectDirs      = senteDirectDirs
  
  def senteEyes(from: Pos, to: Pos) = 
    (from touches to) && !(from isSameRank to)
  def goteEyes(from: Pos, to: Pos) = senteEyes(from, to)
}

case object Lion extends Role {
  val senteProjectionDirs = Nil
  val goteProjectionDirs  = Nil

  val senteDirectDirs: Directions =
    List[Direction](
      p => Pos.at(p.file.index, p.rank.index + 2),
      p => Pos.at(p.file.index + 1, p.rank.index + 2),
      p => Pos.at(p.file.index + 2, p.rank.index + 2),
      p => Pos.at(p.file.index + 2, p.rank.index + 1),
      p => Pos.at(p.file.index + 2, p.rank.index),
      p => Pos.at(p.file.index + 2, p.rank.index - 1),
      p => Pos.at(p.file.index + 2, p.rank.index - 2),
      p => Pos.at(p.file.index + 1, p.rank.index - 2),
      p => Pos.at(p.file.index, p.rank.index - 2),
      p => Pos.at(p.file.index - 1, p.rank.index - 2),
      p => Pos.at(p.file.index - 2, p.rank.index - 2),
      p => Pos.at(p.file.index - 2, p.rank.index - 1),
      p => Pos.at(p.file.index - 2, p.rank.index),
      p => Pos.at(p.file.index - 2, p.rank.index + 1),
      p => Pos.at(p.file.index - 2, p.rank.index + 2),
      p => Pos.at(p.file.index - 1, p.rank.index + 2)
    ) ::: King.senteDirectDirs
  val goteDirectDirs = senteDirectDirs

  override def senteLionDirs(from: Pos, to: Pos): Directions =
    if (from touches to) King.senteDirectDirs
    else Nil
  override def goteLionDirs(from: Pos, to: Pos): Directions =
    if (from touches to) King.goteDirectDirs
    else Nil

  def senteEyes(from: Pos, to: Pos) = (from dist to) <= 2 && from != to
  def goteEyes(from: Pos, to: Pos) = senteEyes(from, to)
}

case object LionPromoted extends Role {
  val senteProjectionDirs = Lion.senteProjectionDirs
  val goteProjectionDirs  = Lion.goteProjectionDirs

  val senteDirectDirs     = Lion.senteDirectDirs
  val goteDirectDirs      = Lion.goteDirectDirs

  override def senteLionDirs(from: Pos, to: Pos): Directions =
    Lion.senteLionDirs(from, to)
  override def goteLionDirs(from: Pos, to: Pos): Directions =
    Lion.goteLionDirs(from, to)

  def senteEyes(from: Pos, to: Pos) = Lion.senteEyes(from, to)
  def goteEyes(from: Pos, to: Pos) = Lion.goteEyes(from, to)
}

case object Ox extends Role {
  val senteProjectionDirs = List(_.up, _.upRight, _.downRight, _.down, _.downLeft, _.upLeft)
  val goteProjectionDirs  = senteProjectionDirs

  val senteDirectDirs     = Nil
  val goteDirectDirs      = Nil

  def senteEyes(from: Pos, to: Pos) = 
    ((from isSameFile to) || (from onSameDiagonal to)) && from != to
  def goteEyes(from: Pos, to: Pos) = senteEyes(from ,to)
}

case object Pawn extends DroppableRole {
  val senteProjectionDirs = Nil
  val goteProjectionDirs  = Nil

  val senteDirectDirs     = List(_.up)
  val goteDirectDirs      = List(_.down)

  def senteEyes(from: Pos, to: Pos) = from.up.exists(_ == to)
  def goteEyes(from: Pos, to: Pos) = from.down.exists(_ == to)
}

case object Phoenix extends Role {
  val senteProjectionDirs = Nil
  val goteProjectionDirs  = Nil

  val senteDirectDirs: Directions =
    List[Direction](
      p => Pos.at(p.file.index + 2, p.rank.index + 2),
      p => Pos.at(p.file.index + 2, p.rank.index - 2),
      p => Pos.at(p.file.index - 2, p.rank.index + 2),
      p => Pos.at(p.file.index - 2, p.rank.index - 2)
    ) ::: Rook.senteProjectionDirs
  val goteDirectDirs = senteDirectDirs

  def senteEyes(from: Pos, to: Pos) = 
    ((from touches to) && (from onSameLine to)) || ((from xDist to) == 2 && (from onSameDiagonal to))
  def goteEyes(from: Pos, to: Pos) = senteEyes(from, to)
}

case object Prince extends Role {
  val senteProjectionDirs = King.senteProjectionDirs
  val goteProjectionDirs  = King.goteProjectionDirs

  val senteDirectDirs     = King.senteDirectDirs
  val goteDirectDirs      = King.goteDirectDirs

  def senteEyes(from: Pos, to: Pos) = King.senteEyes(from, to)
  def goteEyes(from: Pos, to: Pos) = King.goteEyes(from, to)
}

case object PromotedKnight extends Role {
  val senteProjectionDirs = Gold.senteProjectionDirs
  val goteProjectionDirs  = Gold.goteProjectionDirs

  val senteDirectDirs     = Gold.senteDirectDirs
  val goteDirectDirs      = Gold.goteDirectDirs

  def senteEyes(from: Pos, to: Pos) = Gold.senteEyes(from, to)
  def goteEyes(from: Pos, to: Pos)  = Gold.goteEyes(from, to)
}

case object PromotedLance extends Role {
  val senteProjectionDirs = Gold.senteProjectionDirs
  val goteProjectionDirs  = Gold.goteProjectionDirs

  val senteDirectDirs     = Gold.senteDirectDirs
  val goteDirectDirs      = Gold.goteDirectDirs

  def senteEyes(from: Pos, to: Pos) = Gold.senteEyes(from, to)
  def goteEyes(from: Pos, to: Pos)  = Gold.goteEyes(from, to)
}

case object PromotedPawn extends Role {
  val senteProjectionDirs = Gold.senteProjectionDirs
  val goteProjectionDirs  = Gold.goteProjectionDirs

  val senteDirectDirs     = Gold.senteDirectDirs
  val goteDirectDirs      = Gold.goteDirectDirs

  def senteEyes(from: Pos, to: Pos) = Gold.senteEyes(from, to)
  def goteEyes(from: Pos, to: Pos) = Gold.goteEyes(from, to)
}

case object PromotedSilver extends Role {
  val senteProjectionDirs = Gold.senteProjectionDirs
  val goteProjectionDirs  = Gold.goteProjectionDirs

  val senteDirectDirs     = Gold.senteDirectDirs
  val goteDirectDirs      = Gold.goteDirectDirs

  def senteEyes(from: Pos, to: Pos) = Gold.senteEyes(from, to)
  def goteEyes(from: Pos, to: Pos)  = Gold.goteEyes(from, to)
}

case object Queen extends Role {
  val senteProjectionDirs = Rook.senteProjectionDirs ::: Bishop.senteProjectionDirs
  val goteProjectionDirs  = Rook.goteProjectionDirs ::: Bishop.goteProjectionDirs

  val senteDirectDirs     = Nil
  val goteDirectDirs      = Nil

  def senteEyes(from: Pos, to: Pos) = 
     Rook.senteEyes(from, to) || Bishop.senteEyes(from, to)
  def goteEyes(from: Pos, to: Pos) = senteEyes(from, to)
}

case object QueenPromoted extends Role {
  val senteProjectionDirs = Queen.senteProjectionDirs
  val goteProjectionDirs  = Queen.goteProjectionDirs

  val senteDirectDirs     = Queen.senteDirectDirs
  val goteDirectDirs      = Queen.goteDirectDirs

  def senteEyes(from: Pos, to: Pos) = Queen.senteEyes(from, to)
  def goteEyes(from: Pos, to: Pos) = Queen.goteEyes(from, to)
}

case object Rook extends DroppableRole {
  val senteProjectionDirs = List(_.up, _.down, _.left, _.right)
  val goteProjectionDirs  = senteProjectionDirs

  val senteDirectDirs     = Nil
  val goteDirectDirs      = Nil

  def senteEyes(from: Pos, to: Pos) = (from onSameLine to) && from != to
  def goteEyes(from: Pos, to: Pos) = senteEyes(from, to)
}

case object RookPromoted extends Role {
  val senteProjectionDirs = Rook.senteProjectionDirs
  val goteProjectionDirs  = Rook.goteProjectionDirs

  val senteDirectDirs     = Rook.senteDirectDirs
  val goteDirectDirs      = Rook.goteDirectDirs

  def senteEyes(from: Pos, to: Pos) = Rook.senteEyes(from, to)
  def goteEyes(from: Pos, to: Pos) = Rook.goteEyes(from, to)
}

case object SideMover extends Role {
  val senteProjectionDirs = List(_.left, _.right)
  val goteProjectionDirs  = senteProjectionDirs

  val senteDirectDirs     = List(_.up, _.down)
  val goteDirectDirs      = senteDirectDirs

  def senteEyes(from: Pos, to: Pos) = 
    ((from isSameRank to) && from != to) || GoBetween.senteEyes(from, to)
  def goteEyes(from: Pos, to: Pos) = senteEyes(from, to)
}

case object SideMoverPromoted extends Role {
  val senteProjectionDirs = SideMover.senteProjectionDirs
  val goteProjectionDirs  = SideMover.goteProjectionDirs

  val senteDirectDirs     = SideMover.senteDirectDirs
  val goteDirectDirs      = SideMover.goteDirectDirs

  def senteEyes(from: Pos, to: Pos) = SideMover.senteEyes(from, to)
  def goteEyes(from: Pos, to: Pos) = SideMover.goteEyes(from, to)
}

case object Silver extends DroppableRole {
  val senteProjectionDirs = Nil
  val goteProjectionDirs  = Nil

  val senteDirectDirs     = List(_.up, _.upLeft, _.upRight, _.downLeft, _.downRight)
  val goteDirectDirs      = List(_.down, _.upLeft, _.upRight, _.downLeft, _.downRight)

  def senteEyes(from: Pos, to: Pos) = 
    (from touches to) && !(from isSameRank to) && (!(from isSameFile to) || (to isAbove from))
  def goteEyes(from: Pos, to: Pos) =
    (from touches to) && !(from isSameRank to) && (!(from isSameFile to) || (to isBelow from))
}

case object Stag extends Role {
  val senteProjectionDirs = List(_.up, _.down)
  val goteProjectionDirs  = senteProjectionDirs

  val senteDirectDirs     = List(_.upRight, _.right, _.downRight, _.downLeft, _.left, _.upLeft)
  val goteDirectDirs      = senteDirectDirs

  def senteEyes(from: Pos, to: Pos) = 
    ((from isSameFile to) && from != to) || (from touches to)
  def goteEyes(from: Pos, to: Pos) = senteEyes(from, to)
}

case object Tiger extends Role {
  val senteProjectionDirs = Nil
  val goteProjectionDirs  = Nil

  val senteDirectDirs     = List(_.down, _.downLeft, _.downRight, _.left, _.right, _.upRight, _.upLeft)
  val goteDirectDirs      = List(_.up, _.upLeft, _.upRight, _.left, _.right, _.downLeft, _.downRight)

  def senteEyes(from: Pos, to: Pos) = Elephant.goteEyes(from, to)
  def goteEyes(from: Pos, to: Pos) = Elephant.senteEyes(from, to)
}

case object Tokin extends Role {
  val senteProjectionDirs = Gold.senteProjectionDirs
  val goteProjectionDirs  = Gold.goteProjectionDirs

  val senteDirectDirs     = Gold.senteDirectDirs
  val goteDirectDirs      = Gold.goteDirectDirs

  def senteEyes(from: Pos, to: Pos) = Gold.senteEyes(from, to)
  def goteEyes(from: Pos, to: Pos)  = Gold.goteEyes(from, to)
}

case object VerticalMover extends Role {
  val senteProjectionDirs = List(_.up, _.down)
  val goteProjectionDirs  = senteProjectionDirs

  val senteDirectDirs     = List(_.left, _.right)
  val goteDirectDirs      = senteDirectDirs

  def senteEyes(from: Pos, to: Pos) = 
     Chariot.senteEyes(from, to) || ((from isSameRank to) && ((from xDist to) == 1))
  def goteEyes(from: Pos, to: Pos) = senteEyes(from, to)
}

case object VerticalMoverPromoted extends Role {
  val senteProjectionDirs = VerticalMover.senteProjectionDirs
  val goteProjectionDirs  = VerticalMover.goteProjectionDirs

  val senteDirectDirs     = VerticalMover.senteDirectDirs
  val goteDirectDirs      = VerticalMover.goteDirectDirs

  def senteEyes(from: Pos, to: Pos) = VerticalMover.senteEyes(from, to)
  def goteEyes(from: Pos, to: Pos) = VerticalMover.goteEyes(from, to)
}

case object Whale extends Role {
  val senteProjectionDirs = List(_.down, _.downLeft, _.downRight, _.up)
  val goteProjectionDirs  = List(_.up, _.upLeft, _.upRight, _.down)

  val senteDirectDirs     = Nil
  val goteDirectDirs      = Nil

  def senteEyes(from: Pos, to: Pos) = WhiteHorse.goteEyes(from, to)
  def goteEyes(from: Pos, to: Pos) = WhiteHorse.senteEyes(from, to)
}

case object WhiteHorse extends Role {
  val senteProjectionDirs = List(_.up, _.upLeft, _.upRight, _.down)
  val goteProjectionDirs  = List(_.down, _.downLeft, _.downRight, _.up)

  val senteDirectDirs     = Nil
  val goteDirectDirs      = Nil

  def senteEyes(from: Pos, to: Pos) = 
    ((from isSameFile to) && from != to) || ((from onSameDiagonal to) && (from isBelow to))
  def goteEyes(from: Pos, to: Pos) =
    ((from isSameFile to) && from != to) || ((from onSameDiagonal to) && (from isAbove to))
}

object Role {

  val all: List[Role] = List(
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
    GoBetween,
    Gold,
    Horse,
    HorsePromoted,
    King,
    Kirin,
    Knight,
    Lance,
    Leopard,
    Lion,
    LionPromoted,
    Ox,
    Pawn,
    Phoenix,
    Prince,
    PromotedKnight,
    PromotedLance,
    PromotedPawn,
    PromotedSilver,
    Queen,
    QueenPromoted,
    Rook,
    RookPromoted,
    SideMover,
    SideMoverPromoted,
    Silver,
    Stag,
    Tiger,
    Tokin,
    VerticalMover,
    VerticalMoverPromoted,
    Whale,
    WhiteHorse,
  )

  val allDroppable: List[DroppableRole] = List(
    Bishop,
    Gold,
    Knight,
    Lance,
    Pawn,
    Rook,
    Silver
  )

  val allByName: Map[String, Role] = all map { r =>
    (r.name, r)
  } toMap

  def valueOf(r: Role): Int =
    r match {
      case Pawn                                                   => 1
      case Lance                                                  => 3
      case Knight                                                 => 4
      case Silver                                                 => 5
      case Gold | PromotedSilver | PromotedLance | PromotedKnight => 6
      case Tokin                                                  => 7
      case Bishop                                                 => 8
      case Rook                                                   => 10
      case Horse                                                  => 10
      case Dragon                                                 => 12
      case King                                                   => 0
      case _ => 0
    }
}
