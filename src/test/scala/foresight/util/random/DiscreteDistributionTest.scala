package foresight.util.random

import org.junit.Assert.assertEquals
import org.junit.Test

class DiscreteDistributionTest {
  @Test
  def prioritiesToProbabilitiesTopUpCorrection(): Unit = {
    val dist = new DiscreteDistribution {
      override def apply(value: Int): Double = 0.0
    }

    val input = Seq("a" -> 1, "b" -> 2)
    val result = dist.prioritiesToProbabilities(input)

    result.foreach { case (_, prob) =>
      assertEquals(0.5, prob, 1e-6)
    }

    assertEquals(1.0, result.map(_._2).sum, 1e-6)
  }
}
