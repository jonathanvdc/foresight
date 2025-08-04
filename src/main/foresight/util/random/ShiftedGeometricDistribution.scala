package foresight.util.random

/**
 * A shifted geometric distribution. The probability of a value k is `p * (1 - p)^k`.
 * @param p The probability of success.
 */
final case class ShiftedGeometricDistribution(p: Double) extends DiscreteDistribution {
  override def apply(value: Int): Double = math.pow(1 - p, value) * p
}
