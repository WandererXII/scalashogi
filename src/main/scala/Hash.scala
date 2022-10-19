package shogi

import java.security.MessageDigest

object Hash {

  val size = 3

  private def apply(str: String): PositionHash =
    MessageDigest getInstance "MD5" digest (str getBytes "UTF-8") take size

  def apply(sit: Situation): PositionHash = apply(sit.toSfen.value)

  def debug(hashes: PositionHash) = hashes.map(_.toInt).sum.toString

}