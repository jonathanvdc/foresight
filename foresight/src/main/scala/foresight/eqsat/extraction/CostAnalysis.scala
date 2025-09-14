package foresight.eqsat.extraction

import foresight.eqsat._
import foresight.eqsat.metadata.{Analysis, EGraphWithMetadata}

/**
 * An analysis that derives the cost of a minimal-cost extraction results for each e-class.
 *
 * This analysis computes a user-provided [[CostFunction]] at every e-class using an
 * implicit ordering. The resulting value encodes the cost of a minimal-cost tree
 * extraction.
 *
 * The cost domain `C` is totally ordered by the implicit `Ordering[C]`.
 *
 * Immutability: Foresight e-graphs are immutable. Running this analysis does not mutate the input;
 * it produces results attached to the analysis view of the graph.
 *
 * @param name          Human-readable name for diagnostics and debugging.
 * @param cost          Node-local cost model used to score trees bottom-up.
 * @param costOrdering  (implicit) Total ordering over costs used to select minima.
 * @tparam NodeT        Node/operator type stored in the e-graph.
 * @tparam C            Cost type produced by the cost function.
 */
final case class CostAnalysis[NodeT, C](name: String, cost: CostFunction[NodeT, C])
                                       (implicit val costOrdering: Ordering[C])
  extends Analysis[NodeT, C] {

  override def rename(result: C, renaming: SlotMap): C = {
    result
  }

  override def make(node: NodeT, defs: Seq[Slot], uses: Seq[Slot], args: Seq[C]): C = {
    cost(node, defs, uses, args)
  }

  override def join(left: C, right: C): C = {
    costOrdering.min(left, right)
  }
}

/**
 * Constructors and presets for common extraction analyses.
 */
object CostAnalysis {

  /**
   * An analysis that minimizes total node count (tree size).
   *
   * Cost model: `size(node) = 1 + sum(child sizes)`.
   *
   * @param nodeOrdering Implicit ordering to deterministically break cost ties.
   * @tparam NodeT       Node/operator type.
   * @return An extraction analysis that prefers the fewest nodes.
   *
   * @example {{{
   * implicit val order: Ordering[Op] = ...
   * val smallest = ExtractionAnalysis.smallest[Op]
   * }}}
   */
  def smallest[NodeT](implicit nodeOrdering: Ordering[NodeT]): ExtractionAnalysis[NodeT, Int] = {
    ExtractionAnalysis("SmallestExtractionAnalysis", CostFunction.size)
  }

  /**
   * An analysis that minimizes tree height (maximum depth).
   *
   * Cost model: `height(node) = 1 + max(child heights)`; leaf height is 1.
   *
   * @param nodeOrdering Implicit ordering to deterministically break cost ties.
   * @tparam NodeT       Node/operator type.
   * @return An extraction analysis that prefers shallow trees.
   *
   * @example {{{
   * implicit val order: Ordering[Op] = ...
   * val shallowest = ExtractionAnalysis.shallowest[Op]
   * }}}
   */
  def shallowest[NodeT](implicit nodeOrdering: Ordering[NodeT]): ExtractionAnalysis[NodeT, Int] = {
    ExtractionAnalysis("ShallowestExtractionAnalysis", CostFunction.depth)
  }
}
