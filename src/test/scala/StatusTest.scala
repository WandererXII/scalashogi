package shogi

class StatusTest extends ShogiTest {
  "Status id" should {
    "be unique" in {
      Status.all.length must_== Status.all.map(_.id).toSet.size
    }
  }
}
