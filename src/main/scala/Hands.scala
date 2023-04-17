package shogi

final case class Hands(sente: Hand, gote: Hand) {
  def apply(color: Color) = color.fold(sente, gote)

  def take(color: Color, role: DroppableRole, cnt: Int = 1): Option[Hands] =
    color.fold(
      sente.take(role, cnt) map { nh =>
        copy(sente = nh)
      },
      gote.take(role, cnt) map { nh =>
        copy(gote = nh)
      }
    )

  def store(color: Color, role: DroppableRole, cnt: Int = 1) =
    color.fold(
      copy(sente = sente.store(role, cnt)),
      copy(gote = gote.store(role, cnt))
    )

  def rolesOf: Color.Map[List[DroppableRole]] =
    Color.Map(sente.roles, gote.roles)

  def roles: List[DroppableRole] =
    (sente.roles ::: gote.roles).distinct

  def size: Int =
    sente.size + gote.size

  def isEmpty: Boolean =
    sente.isEmpty && gote.isEmpty

  def nonEmpty: Boolean =
    !isEmpty

}

object Hands {
  def apply(sente: Iterable[(DroppableRole, Int)], gote: Iterable[(DroppableRole, Int)]): Hands =
    new Hands(Hand(sente), Hand(gote))

  def apply(variant: shogi.variant.Variant): Hands =
    Hands(variant.hands.sente, variant.hands.gote)

  def empty: Hands = Hands(Nil, Nil)
}

final case class Hand(handMap: HandMap) extends AnyVal {

  def apply(role: DroppableRole): Int =
    handMap.getOrElse(role, 0)

  def take(role: DroppableRole, cnt: Int = 1) =
    handMap.get(role).filter(_ - cnt >= 0).map(cur => copy(handMap = handMap + (role -> (cur - cnt))))

  def store(role: DroppableRole, cnt: Int = 1) =
    copy(handMap = handMap + (role -> (apply(role) + cnt)))

  def roles: List[DroppableRole] =
    handMap.view.filter(_._2 > 0).map(_._1).toList

  def size: Int =
    handMap.values.sum

  def nonEmpty: Boolean =
    handMap.exists(_._2 > 0)

  def isEmpty: Boolean =
    !nonEmpty

  def sum(f: (DroppableRole) => Int): Int =
    handMap.foldLeft(0) { case (acc, (role, cnt)) =>
      acc + f(role) * cnt
    }

}

object Hand {

  def apply(hand: Iterable[(DroppableRole, Int)]): Hand =
    new Hand(hand.toMap)

  def empty: Hand = Hand(Nil)

}
