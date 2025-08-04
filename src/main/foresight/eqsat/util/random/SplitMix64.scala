package foresight.eqsat.util.random

/**
 * A random number generator based on the SplitMix64 algorithm.
 *
 * This implementation is immutable and provides methods to generate random integers, longs, and doubles.
 * It uses a 64-bit state to produce pseudo-random numbers.
 */
private[random] final case class SplitMix64(state: Long) extends Random {
  def nextLong: (Long, SplitMix64) = {
    val z0 = state + 0x9E3779B97F4A7C15L
    val z1 = (z0 ^ (z0 >>> 30)) * 0xBF58476D1CE4E5B9L
    val z2 = (z1 ^ (z1 >>> 27)) * 0x94D049BB133111EBL
    val result = z2 ^ (z2 >>> 31)
    (result, SplitMix64(z0))
  }
}
