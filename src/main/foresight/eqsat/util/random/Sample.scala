package foresight.eqsat.util.random

/**
 * Utility object for performing random sampling operations.
 */
object Sample {
  /**
   * Weighted random sampling without replacement using the Efraimidis–Spirakis algorithm.
   *
   * Each element is associated with a positive weight (priority), and the probability of an element being included in
   * the sample is proportional to its weight, adjusted over multiple draws without replacement.
   *
   * The algorithm works by generating a random key for each element using:
   *   key = pow(u, 1/w)
   * where u is a uniform random number in (0, 1), and w is the element's weight.
   * The N elements with the largest keys are selected.
   *
   * Reference:
   *   Efraimidis, Pavlos S., and Paul G. Spirakis.
   *   "Weighted random sampling with a reservoir."
   *   Information Processing Letters 97.5 (2006): 181-185.
   *
   * @param elements Sequence of elements paired with their weights (must be > 0)
   * @param n Number of elements to sample (must be ≤ elements.size)
   * @param rng Random number generator that extends `RandomLike` and `Random`
   * @tparam A Type of the elements to sample
   * @return A sequence of `n` elements sampled without replacement
   */
  def withoutReplacement[A](elements: Seq[(A, Double)],
                            n: Int,
                            rng: Random): (Seq[A], Random) = {
    require(n <= elements.size, "Cannot sample more elements than exist in the input")
    require(elements.forall(_._2 > 0), "All weights must be positive")

    if (n == 0) {
      return (Seq.empty[A], rng)
    } else if (n == elements.size) {
      return (elements.map(_._1), rng)
    }

    val (randomVals, newRng) = rng.nextDoubles(elements.size)

    val keyed = elements.zip(randomVals).map { case ((value, weight), u) =>
      val key = math.pow(u, 1.0 / weight)
      (value, key)
    }

    val selected = keyed
      .sortBy { case (_, key) => -key }
      .take(n)
      .map { case (value, _) => value }

    (selected, newRng)
  }
}
