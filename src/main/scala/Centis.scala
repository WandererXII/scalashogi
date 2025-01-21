package shogi

import scala.concurrent.duration._

import cats.Monoid

// maximum centis = Int.MaxValue / 100 / 60 / 60 / 24 = 248 days
final case class Centis(centis: Int) extends AnyVal with Ordered[Centis] {

  def roundTenths: Int =
    if (centis > 0) (centis + 5) / 10 else (centis - 4) / 10
  def roundSeconds: Int = math.round(centis * 0.01f)

  def toSeconds: BigDecimal = java.math.BigDecimal.valueOf(centis, 2)
  def millis: Long          = centis * 10L
  def toDuration            = FiniteDuration(millis, MILLISECONDS)

  def isPositive = centis > 0
  def isNegative = centis < 0

  def +(other: Centis)   = Centis(centis + other.centis)
  def -(other: Centis)   = Centis(centis - other.centis)
  def *(scalar: Int)     = Centis(scalar * centis)
  def *~(scalar: Float)  = Centis(scalar * centis)
  def *~(scalar: Double) = Centis(scalar * centis)
  def /(div: Int)        = Option.when(div != 0)(Centis(centis / div))
  def unary_-            = Centis(-centis)

  def avg(other: Centis) = Centis((centis + other.centis) >> 1)

  def compare(other: Centis) = Integer.compare(centis, other.centis)

  def atMost(o: Centis)  = Centis(math.min(centis, o.centis))
  def atLeast(o: Centis) = Centis(math.max(centis, o.centis))

  def nonNeg = Centis(math.max(centis, 0))
}

object Centis {
  implicit val CentisMonoid: Monoid[shogi.Centis] = new Monoid[Centis] {
    def combine(c1: Centis, c2: Centis) = c1 + c2
    final val empty: shogi.Centis       = Centis(0)
  }

  implicit final class CentisScalar(val scalar: Int) extends AnyVal {
    def *(o: Centis) = o * scalar
  }

  implicit final class CentisScalarF(val scalar: Float) extends AnyVal {
    def *~(o: Centis) = o *~ scalar
  }

  implicit final class CentisScalarD(val scalar: Double) extends AnyVal {
    def *~(o: Centis) = o *~ scalar
  }

  def apply(l: Long): Centis =
    Centis {
      if (l.toInt == l) l.toInt
      else if (l > 0) Integer.MAX_VALUE
      else Integer.MIN_VALUE
    }

  def apply(d: FiniteDuration): Centis =
    Centis.ofMillis {
      if (d.unit eq MILLISECONDS) d.length
      else d.toMillis
    }

  def apply(f: Float): Centis  = Centis(math.round(f))
  def apply(d: Double): Centis = Centis(math.round(d))

  def ofSeconds(s: Int) = Centis(100 * s)
  def ofMillis(i: Int)  = Centis((if (i > 0) i + 5 else i - 4) / 10)
  def ofMillis(l: Long) = Centis((if (l > 0) l + 5 else l - 4) / 10)
}
