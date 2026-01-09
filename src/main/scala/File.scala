package shogi

final case class File private (index: Int) extends AnyVal with Ordered[File] {
  @inline def -(that: File): Int           = index - that.index
  @inline override def compare(that: File) = this - that

  def key: String               = (index + 1).toString
  def kanjiFullWidthKey: String =
    key.map(c => (c + 65248).toChar)

  override def toString = key
}

object File {
  def apply(index: Int): Option[File] =
    if (0 <= index && index < Pos.MaxFiles) Some(new File(index))
    else None

  @inline def of(pos: Pos): File = new File(pos.index % Pos.MaxFiles)

  val First    = new File(0)
  val Second   = new File(1)
  val Third    = new File(2)
  val Forth    = new File(3)
  val Fifth    = new File(4)
  val Sixth    = new File(5)
  val Seventh  = new File(6)
  val Eighth   = new File(7)
  val Ninth    = new File(8)
  val Tenth    = new File(9)
  val Eleventh = new File(10)
  val Twelfth  = new File(11)

  val all =
    List[File](
      First,
      Second,
      Third,
      Forth,
      Fifth,
      Sixth,
      Seventh,
      Eighth,
      Ninth,
      Tenth,
      Eleventh,
      Twelfth,
    )
}
