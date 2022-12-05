package shogi

final case class Rank private (val index: Int) extends AnyVal with Ordered[Rank] {
  @inline def -(that: Rank): Int           = index - that.index
  @inline override def compare(that: Rank) = this - that

  def offset(delta: Int): Option[Rank] =
    Rank(index + delta)

  def key: String    = (97 + index).toChar.toString
  def hexKey: String = (index + 1).toHexString

  def kanjiKey: String = Rank.kanjiKeys(index % Rank.kanjiKeys.size)

  override def toString = key
}

object Rank {
  def apply(index: Int): Option[Rank] =
    if (0 <= index && index < Pos.MaxRanks) Some(new Rank(index))
    else None

  @inline def of(pos: Pos): Rank = new Rank(pos.index / Pos.MaxFiles)

  private val kanjiKeys =
    Vector("一", "二", "三", "四", "五", "六", "七", "八", "九", "十", "十一", "十二")

  val A = new Rank(0)
  val B = new Rank(1)
  val C = new Rank(2)
  val D = new Rank(3)
  val E = new Rank(4)
  val F = new Rank(5)
  val G = new Rank(6)
  val H = new Rank(7)
  val I = new Rank(8)
  val J = new Rank(9)
  val K = new Rank(10)
  val L = new Rank(11)

  val all                     = List(A, B, C, D, E, F, G, H, I, J, K, L)
  val allReversed: List[Rank] = all.reverse
}
