package foresight.util.random

import scala.annotation.tailrec

/**
 * A discrete probability distribution.
 */
trait DiscreteDistribution {
  /**
   * Returns the probability of a given value.
   * @param value The value.
   * @return The probability of the value.
   */
  def apply(value: Int): Double

  /**
   * Converts a sequence of priorities to a sequence of probabilities. The probabilities are assigned based on the mean
   * weight of the priority group.
   * @param mapping The sequence of priorities to convert.
   * @tparam K The type of the keys.
   * @tparam Priority The type of the priorities.
   * @param ord The ordering of the priorities.
   * @return The sequence of keys with their probabilities.
   */
  final def prioritiesToProbabilities[K, Priority](mapping: Seq[(K, Priority)])(implicit ord: Ordering[Priority]): Seq[(K, Double)] = {
    // Group keys by priority.
    val groupedByPriority = mapping.groupBy(_._2).mapValues(_.map(_._1)).toSeq

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
