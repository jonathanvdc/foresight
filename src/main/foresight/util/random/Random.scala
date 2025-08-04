package foresight.util.random

/**
 * A trait that defines an immutable random number generator interface.
 *
 * This trait provides methods to generate random integers, longs, and doubles.
 * It is designed to be used with a specific type of random number generator,
 * which is specified by the type parameter `Random`.
 */
trait Random {
  /**
   * Generates a random long.
   *
   * @return A tuple containing the generated long and a new instance of the random number generator.
   */
  def nextLong: (Long, Random)

  /**
   * Generates a random integer.
   *
   * @return A tuple containing the generated integer and a new instance of the random number generator.
   */
  def nextInt: (Int, Random) = {
    val (l, nextRng) = nextLong
    (l.toInt, nextRng)
  }

  /**
   * Generates a random double in the range [0.0, 1.0).
   *
   * @return A tuple containing the generated double and a new instance of the random number generator.
   */
  def nextDouble: (Double, Random) = {
    val (l, nextRng) = nextLong
    val double = (l >>> 11) * (1.0 / (1L << 53)) // IEEE 53-bit double precision
    (double, nextRng)
  }

  /**
   * Generates a sequence of random doubles.
   *
   * @param n The number of random doubles to generate.
   * @return A tuple containing the sequence of generated doubles and a new instance of the random number generator.
   */
  final def nextDoubles(n: Int): (Seq[Double], Random) = {
    val (doubles, nextRng) = (0 until n).foldLeft((Seq.empty[Double], this)) {
      case ((acc, rng), _) =>
        val (d, newRng: Random) = rng.nextDouble
        (acc :+ d, newRng)
    }
    (doubles, nextRng)
  }
}

/**
 * A companion object for the [[Random]] trait that provides a factory method to create a new instance of
 * the SplitMix64 random number generator with a given seed.
 */
object Random {
  /**
   * Creates a new instance of the SplitMix64 random number generator with the given seed.
   *
   * @param seed The seed for the random number generator.
   * @return A new instance of SplitMix64 initialized with the given seed.
   */
  def apply(seed: Long): Random = SplitMix64(seed)
}
