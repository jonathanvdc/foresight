package foresight.util.random

import org.junit.Assert.*
import org.junit.Test

import scala.collection.mutable

class SampleTest {
  @Test
  def sampleZeroElementsReturnsEmpty(): Unit = {
    val elems = Seq(("x", 1.0), ("y", 1.0))
    val (result, _) = Sample.withoutReplacement(elems, 0, Random(0))
    assertTrue(result.isEmpty)
  }

  @Test
  def samplingAllElementsReturnsFullSet(): Unit = {
    val elems = Seq(("x", 1.0), ("y", 2.0), ("z", 3.0))
    val (result, _) = Sample.withoutReplacement(elems, 3, Random(0))
    assertEquals(3, result.size)
    assertEquals(Set("x", "y", "z"), result.toSet)
  }

  @Test(expected = classOf[IllegalArgumentException])
  def throwsWhenSampleSizeTooLarge(): Unit = {
    val elems = Seq(("a", 1.0), ("b", 2.0))
    Sample.withoutReplacement(elems, 3, Random(0))
  }

  @Test(expected = classOf[IllegalArgumentException])
  def throwsOnNonPositiveWeights(): Unit = {
    val elems = Seq(("a", 0.0), ("b", 2.0))
    Sample.withoutReplacement(elems, 1, Random(0))
  }

  @Test
  def weightBiasAppearsStatistically(): Unit = {
    val elems = Seq(("a", 1.0), ("b", 3.0))
    var rng = Random(123)
    val trials = 10000
    val counts = mutable.Map("a" -> 0, "b" -> 0)

    for (_ <- 1 to trials) {
      val (selected, newRng) = Sample.withoutReplacement(elems, 1, rng)
      rng = newRng
      counts(selected.head) += 1
    }

    val freqA = counts("a").toDouble / trials
    val freqB = counts("b").toDouble / trials

    assertTrue(s"Expected b > a, but got a=$freqA b=$freqB", freqB > freqA)
    assertTrue("Frequencies should sum to ~1", math.abs(freqA + freqB - 1.0) < 0.01)
  }

  @Test
  def weightBiasAppearsStatisticallyInTwoOutOfThree(): Unit = {
    val elems = Seq(("a", 1.0), ("b", 2.0), ("c", 3.0))
    var rng = Random(123)
    val trials = 10000

    // Track how often each element is included in the 2-element sample
    val counts = mutable.Map("a" -> 0, "b" -> 0, "c" -> 0)

    for (_ <- 1 to trials) {
      val (selected, newRng) = Sample.withoutReplacement(elems, 2, rng)
      selected.foreach { x => counts(x) += 1 }
      rng = newRng
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
