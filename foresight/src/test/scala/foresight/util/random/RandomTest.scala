package foresight.util.random

import org.junit.Assert._
import org.junit.Test

class RandomTest {

  @Test
  def nextLongShouldBeDeterministic(): Unit = {
    val rng1 = Random(42L)
    val rng2 = Random(42L)

    val (l1a, r1a) = rng1.nextLong
    val (l1b, r1b) = rng2.nextLong

    val (l2a, _) = r1a.nextLong
    val (l2b, _) = r1b.nextLong

    assertEquals(l1a, l1b)
    assertEquals(l2a, l2b)
  }

  @Test
  def nextIntShouldProduceSameLowerBitsAsNextLong(): Unit = {
    val rng = Random(123L)
    val (l, _) = rng.nextLong
    val (i, _) = rng.nextInt

    assertEquals(i, l.toInt)
  }

  @Test
  def nextDoubleShouldBeInRange(): Unit = {
    val rng = Random(999L)
    val (d, _) = rng.nextDouble

    assertTrue("double >= 0.0", d >= 0.0)
    assertTrue("double < 1.0", d < 1.0)
  }

  @Test
  def nextDoublesShouldReturnCorrectLengthAndRange(): Unit = {
    val rng = Random(2024L)
    val (doubles, _) = rng.nextDoubles(100)

    assertEquals(100, doubles.length)
    assertTrue(doubles.forall(d => d >= 0.0 && d < 1.0))
  }

  @Test
  def nextDoublesShouldBeDeterministic(): Unit = {
    val rng1 = Random(888L)
    val rng2 = Random(888L)

    val (ds1, _) = rng1.nextDoubles(10)
    val (ds2, _) = rng2.nextDoubles(10)

    assertEquals(ds1, ds2)
  }

  @Test
  def nextCallsShouldProduceDifferentValues(): Unit = {
    val rng = Random(1L)
    val (v1, r1) = rng.nextLong
    val (v2, _) = r1.nextLong

    assertNotEquals(v1, v2)
  }
}
