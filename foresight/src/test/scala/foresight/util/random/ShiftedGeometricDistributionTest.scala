package foresight.util.random

import org.junit.Assert.*
import org.junit.Test

class ShiftedGeometricDistributionTest {

  @Test
  def probabilityValues(): Unit = {
    val dist = ShiftedGeometricDistribution(0.5)

    assertEquals(0.5, dist(0), 1e-6)
    assertEquals(0.25, dist(1), 1e-6)
    assertEquals(0.125, dist(2), 1e-6)
  }

  @Test
  def probabilityNormalization(): Unit = {
    val dist = ShiftedGeometricDistribution(0.3)
    val probs = (0 until 100).map(dist(_))
    val sum = probs.sum

    assertEquals(1.0, sum, 1e-2)
  }

  @Test
  def prioritiesToProbabilitiesSinglePriorityGroup(): Unit = {
    val dist = ShiftedGeometricDistribution(0.5)
    val input = Seq("a" -> 1, "b" -> 1, "c" -> 1)
    val result = dist.prioritiesToProbabilities(input)

    assertEquals(3, result.size)
    val expectedProb = 1.0 / 3
    result.foreach { case (_, prob) => assertEquals(expectedProb, prob, 1e-6) }
  }

  @Test
  def prioritiesToProbabilitiesMultipleGroups(): Unit = {
    val dist = ShiftedGeometricDistribution(0.5)
    val input = Seq("a" -> 3, "b" -> 2, "c" -> 1)

    val result = dist.prioritiesToProbabilities(input)
    val probs = result.toMap

    assertTrue(probs("a") > probs("b"))
    assertTrue(probs("b") > probs("c"))
    assertEquals(1.0, probs.values.sum, 1e-6)
  }

  @Test
  def uniquePrioritiesProduceCorrectProbabilities(): Unit = {
    val dist = ShiftedGeometricDistribution(0.5)
    val input = Seq(25, 100, 50).map(x => x -> x)
    val result = dist.prioritiesToProbabilities(input)

    val probs = result.toMap
    val topUp = (1 - (0 until 3).map(dist(_)).sum) / 3

    assertEquals(0.5 + topUp, probs(100), 1e-6)
    assertEquals(0.25 + topUp, probs(50), 1e-6)
    assertEquals(0.125 + topUp, probs(25), 1e-6)
  }

  @Test
  def equalPrioritiesProduceUniformDistribution(): Unit = {
    val dist = ShiftedGeometricDistribution(0.5)
    val input = Seq(100, 100, 100).map(x => x -> x)
    val result = dist.prioritiesToProbabilities(input)

    val expected = 1.0 / 3
    result.foreach { case (_, prob) =>
      assertEquals(expected, prob, 1e-6)
    }
  }

  @Test
  def mixedPrioritiesProduceGroupedAveragedProbabilities(): Unit = {
    val dist = ShiftedGeometricDistribution(0.5)
    val input = Seq(200, 100, 100, 100, 50).map(x => x -> x)
    val result = dist.prioritiesToProbabilities(input)

    val probs = result.toMap
    val topUp = (1 - (0 until 5).map(dist(_)).sum) / 5

    assertEquals(0.5 + topUp, probs(200), 1e-6)

    val groupedMean = (0.25 + 0.125 + 0.0625) / 3
    assertEquals(groupedMean + topUp, probs(100), 1e-6)
    assertEquals(0.03125 + topUp, probs(50), 1e-6)
  }
}
