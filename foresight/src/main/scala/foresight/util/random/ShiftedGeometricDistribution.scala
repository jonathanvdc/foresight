package foresight.util.random

/**
 * Represents a shifted geometric distribution, a discrete probability distribution
 * modeling the number of failures before the first success in a sequence of Bernoulli trials,
 * with the support shifted so that values start at 0.
 *
 * The probability mass function (PMF) for a value `k` is:
 *   `P(X = k) = p * (1 - p)^k`
 * where:
 *   - `p` is the probability of success (0 < p <= 1)
 *   - `k` is a non-negative integer (k >= 0)
 *
 * See also: [[https://en.wikipedia.org/wiki/Geometric_distribution Geometric distribution (Wikipedia)]]
 *
 * @param p Probability of success in each trial.
 */
final case class ShiftedGeometricDistribution(p: Double) extends DiscreteDistribution {
  override def apply(value: Int): Double = math.pow(1 - p, value) * p
}
