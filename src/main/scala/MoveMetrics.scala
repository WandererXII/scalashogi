package shogi

final case class LagMetrics(
    clientLag: Option[Centis] = None,
    clientMoveTime: Option[Centis] = None
) {

  // Calculate client reported lag given the server's duration for the move.
  def reportedLag(elapsed: Centis) =
    clientMoveTime.fold(clientLag)(mt => Some(elapsed - mt))
}

object LagMetrics {
  def empty = LagMetrics(None, None)
}
