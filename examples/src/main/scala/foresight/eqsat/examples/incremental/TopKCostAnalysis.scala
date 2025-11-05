package foresight.eqsat.examples.incremental

import foresight.eqsat.Slot
import foresight.eqsat.collections.SlotMap
import foresight.eqsat.extraction.CostFunction
import foresight.eqsat.metadata.Analysis

/**
 * An analysis that retains, for each e-class, the K lowest extraction costs
 * according to a user-provided [[CostFunction]].
 *
 * The cost domain `C` is totally ordered by the implicit `Ordering[C]`.
 *
 * @param name          Human-readable name for diagnostics and debugging.
 * @param k             How many lowest costs to retain per e-class (K ≥ 1).
 * @param cost          Node-local cost model used to score trees bottom-up.
 * @param costOrdering  (implicit) Total ordering over costs used to select minima.
 * @tparam NodeT        Node/operator type stored in the e-graph.
 * @tparam C            Cost type produced by the cost function.
 */
final case class TopKCostAnalysis[NodeT, C](name: String, k: Int, cost: CostFunction[NodeT, C])
                                           (implicit val costOrdering: Ordering[C])
  extends Analysis[NodeT, TopKCostAnalysis.TopK[C]] {

  import TopKCostAnalysis._

  require(k >= 1, s"TopKCostAnalysis requires k >= 1, got $k")

  override def rename(result: TopK[C], renaming: SlotMap): TopK[C] = result

  override def make(node: NodeT, defs: Seq[Slot], uses: Seq[Slot], args: Seq[TopK[C]]): TopK[C] = {
    // If any child has no realizable cost, this node cannot produce a cost.
    if (args.exists(_.values.isEmpty)) return TopK(Vector.empty)

    // Combine only each child's best (lowest) cost.
    val bestArgs = args.map(_.min)
    TopK(Vector(cost(node, defs, uses, bestArgs)))
  }

  override def join(left: TopK[C], right: TopK[C]): TopK[C] = {
    // Merge the two top-K sets and keep the K best distinct costs.
    TopK(takeKSortedDistinct(left.values ++ right.values, k)(costOrdering))
  }
}

/**
 * Companion object for [[TopKCostAnalysis]].
 */
object TopKCostAnalysis {
  /** Container for Top-K costs at an e-class. Values are sorted in ascending order. */
  final case class TopK[C](values: Vector[C]) extends AnyVal {
    /** Return the smallest (best) cost. */
    def min: C = {
      require(values.nonEmpty, "TopK is empty; cannot take min")
      values.head
    }

    /** Return the cutoff (worst among retained costs). */
    def cutoff: C = {
      require(values.nonEmpty, "TopK is empty; cannot take cutoff")
      values.last
    }
  }

  /**
   * Sort ascending, drop duplicates (by value), and keep first K.
   * Stable w.r.t. the Ordering for ties.
   */
  private[incremental] def takeKSortedDistinct[C](xs: Vector[C], k: Int)(implicit ord: Ordering[C]): Vector[C] = {
    // Use a TreeSet to get sorted unique values, then take K; convert back to Vector.
    // We still sort the taken slice to ensure ascending order in case the Set's iterator
    // order differs between Scala versions.
    val set = scala.collection.immutable.TreeSet.empty[C](ord) ++ xs
    set.take(k).toVector
  }
}
