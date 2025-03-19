package fixpoint.eqsat.hashCons

import org.junit.Test

class HelpersTest {
  @Test
  def cartesianProduct1(): Unit = {
    val input = Seq(Seq(1, 2), Seq(3, 4))
    val expected = Seq(Seq(1, 3), Seq(1, 4), Seq(2, 3), Seq(2, 4))
    assert(Helpers.cartesian(input) == expected)
  }

  @Test
  def cartesianProduct2(): Unit = {
    val input = Seq(Seq(1, 2), Seq(3, 4), Seq(5, 6))
    val expected = Seq(
      Seq(1, 3, 5), Seq(1, 3, 6), Seq(1, 4, 5), Seq(1, 4, 6),
      Seq(2, 3, 5), Seq(2, 3, 6), Seq(2, 4, 5), Seq(2, 4, 6)
    )
    assert(Helpers.cartesian(input) == expected)
  }

  @Test
  def cartesianProduct3(): Unit = {
    val input = Seq(Seq(1, 2), Seq(3), Seq(4, 5))
    val expected = Seq(
      Seq(1, 3, 4), Seq(1, 3, 5), Seq(2, 3, 4), Seq(2, 3, 5)
    )
    assert(Helpers.cartesian(input) == expected)
  }
}
