package foresight.eqsat.util

import org.junit.Test
import org.junit.Assert._
import java.util.Random
import scala.collection.mutable
import foresight.eqsat.util.random.Sample.withoutReplacement

class SampleTest {
  @Test
  def testSampleZeroElementsReturnsEmpty(): Unit = {
    val elems = Seq(("x", 1.0), ("y", 1.0))
    val result = withoutReplacement(elems, 0)
    assertTrue(result.isEmpty)
  }

  @Test
  def testSamplingAllElementsReturnsFullSet(): Unit = {
    val elems = Seq(("x", 1.0), ("y", 2.0), ("z", 3.0))
    val result = withoutReplacement(elems, 3)
    assertEquals(3, result.size)
    assertEquals(Set("x", "y", "z"), result.toSet)
  }

  @Test(expected = classOf[IllegalArgumentException])
  def testThrowsWhenSampleSizeTooLarge(): Unit = {
    val elems = Seq(("a", 1.0), ("b", 2.0))
    withoutReplacement(elems, 3)
  }

  @Test(expected = classOf[IllegalArgumentException])
  def testThrowsOnNonPositiveWeights(): Unit = {
    val elems = Seq(("a", 0.0), ("b", 2.0))
    withoutReplacement(elems, 1)
  }

  @Test
  def testWeightBiasAppearsStatistically(): Unit = {
    val elems = Seq(("a", 1.0), ("b", 3.0))
    val rng = new Random(123)
    val trials = 10000
    val counts = mutable.Map("a" -> 0, "b" -> 0)

    for (_ <- 1 to trials) {
      val selected = withoutReplacement(elems, 1, rng).head
      counts(selected) += 1
    }

    val freqA = counts("a").toDouble / trials
    val freqB = counts("b").toDouble / trials

    assertTrue(s"Expected b > a, but got a=$freqA b=$freqB", freqB > freqA)
    assertTrue("Frequencies should sum to ~1", math.abs(freqA + freqB - 1.0) < 0.01)
  }

  @Test
  def testWeightBiasAppearsStatisticallyInTwoOutOfThree(): Unit = {
    val elems = Seq(("a", 1.0), ("b", 2.0), ("c", 3.0))
    val rng = new Random(123)
    val trials = 10000

    // Track how often each element is included in the 2-element sample
    val counts = mutable.Map("a" -> 0, "b" -> 0, "c" -> 0)

    for (_ <- 1 to trials) {
      val selected = withoutReplacement(elems, 2, rng)
      selected.foreach { x => counts(x) += 1 }
    }

    // Convert to frequency
    val freqA = counts("a").toDouble / (2 * trials)
    val freqB = counts("b").toDouble / (2 * trials)
    val freqC = counts("c").toDouble / (2 * trials)

    // Expect: c > b > a
    assertTrue(s"Expected c > b, but got c=$freqC, b=$freqB", freqC > freqB)
    assertTrue(s"Expected b > a, but got b=$freqB, a=$freqA", freqB > freqA)
    assertTrue(
      s"Frequencies should sum to ~1. Got sum = ${freqA + freqB + freqC}",
      math.abs(freqA + freqB + freqC - 1.0) < 0.01
    )
  }
}
