package foresight.util.random

import foresight.util.collections.StrictMapOps.toStrictMapOps

import scala.annotation.tailrec

/**
 * A trait representing a discrete probability distribution over non-negative integer indices.
 *
 * Implementations define how probability mass is assigned to integer values (e.g., based on
 * geometric decay or custom heuristics). This abstraction supports flexible priority-based
 * decision-making or sampling strategies.
 *
 * The primary method [[apply]] returns the probability associated with a given index.
 * It is expected that probabilities are non-negative and sum to at most 1 across all values
 * (though this is not enforced at the trait level).
 *
 * The helper method [[prioritiesToProbabilities]] enables assigning probabilities to arbitrary keys
 * based on a provided priority mapping. Keys with equal priorities are grouped and assigned the mean
 * probability of their position-based indices, derived from the distribution. The resulting
 * probabilities are then normalized to ensure they sum to exactly 1.
 *
 * Use this trait to model discrete ranking or selection scenarios, such as:
 *   - Weighted random choice among prioritized options
 *   - Probabilistic rule selection in equality saturation
 *   - Discrete softmax-like prioritization heuristics
 */
trait DiscreteDistribution {
  /**
   * Returns the probability mass assigned to the given index.
   *
   * @param value A non-negative integer index.
   * @return The probability of this index according to the distribution.
   */
  def apply(value: Int): Double

  /**
   * Converts a sequence of (key, priority) pairs into a normalized sequence of (key, probability) pairs.
   *
   * Keys are grouped by their priority, then sorted by descending priority. Each group is assigned an average
   * probability based on the distribution's `apply` method, using the index range corresponding to that group.
   *
   * Finally, the resulting probabilities are normalized so that their sum is exactly 1 by distributing any
   * leftover mass uniformly.
   *
   * @param mapping A sequence of (key, priority) pairs.
   * @param ord The ordering used to sort priorities.
   * @tparam K The type of keys.
   * @tparam Priority The type of the priorities.
   * @return A sequence of (key, probability) pairs.
   */
  final def prioritiesToProbabilities[K, Priority](mapping: Seq[(K, Priority)])(implicit ord: Ordering[Priority]): Seq[(K, Double)] = {
    // Group keys by priority.
    val groupedByPriority = mapping.groupBy(_._2).mapValuesStrict(_.map(_._1)).toSeq

    // Sort grouped keys by descending priority.
    val sorted = groupedByPriority.sortBy(_._1).reverse.toList

    // Assign a weight to each key based on the mean weight of its priority group.
    @tailrec
    def processSorted(remaining: List[(Priority, Seq[K])], index: Int, results: Seq[(K, Double)]): Seq[(K, Double)] = {
      remaining match {
        case Nil => results
        case (_, keys) :: tail =>
          val nextIndex = index + keys.length
          val weights = Seq.range(index, nextIndex).map(apply)
          val meanWeight = if (weights.size == 1) weights.head else weights.sum / weights.length
          processSorted(tail, nextIndex, results ++ keys.map(key => key -> meanWeight))
      }
    }

    // Assign probabilities to keys. Then top up the probabilities to sum to 1.
    val probabilityAssignment = processSorted(sorted, 0, Seq.empty)
    val remainingProbability = 1 - probabilityAssignment.map(_._2).sum
    val topUp = remainingProbability / probabilityAssignment.size
    probabilityAssignment.map(pair => pair._1 -> (pair._2 + topUp))
  }
}
