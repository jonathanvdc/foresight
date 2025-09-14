package foresight.eqsat.extraction

import foresight.eqsat.metadata.{Analysis, EGraphWithMetadata}
import foresight.eqsat._

/**
 * An analysis that derives minimal-cost extraction results for each e-class.
 *
 * This analysis computes an [[ExtractionTreeCall]] at every e-class using a user-provided
 * [[CostFunction]] and implicit orderings. The resulting value encodes both the chosen
 * minimal-cost tree and the slot renaming needed to apply it in context.
 *
 * The cost domain `C` is totally ordered by the implicit `Ordering[C]`. When multiple candidates
 * have the same minimal cost, ties are broken deterministically using the implicit `Ordering[NodeT]`
 * via [[ExtractionTreeOrdering]].
 *
 * Immutability: Foresight e-graphs are immutable. Running this analysis does not mutate the input;
 * it produces results attached to the analysis view of the graph.
 *
 * @param name          Human-readable name for diagnostics and debugging.
 * @param cost          Node-local cost model used to score trees bottom-up.
 * @param costOrdering  (implicit) Total ordering over costs used to select minima.
 * @param nodeOrdering  (implicit) Tie-breaker over node types for deterministic selection.
 * @tparam NodeT        Node/operator type stored in the e-graph.
 * @tparam C            Cost type produced by the cost function.
 */
final case class ExtractionAnalysis[NodeT, C](name: String,
                                              cost: CostFunction[NodeT, C])
                                             (implicit costOrdering: Ordering[C],
                                              nodeOrdering: Ordering[NodeT])
  extends Analysis[NodeT, ExtractionTreeCall[NodeT, C]] {

  /**
   * Builds an [[Extractor]] that projects the analysis result at a given e-class into a concrete [[Tree]].
   *
   * The extractor reads this analysis's results from an [[EGraphWithMetadata]] and converts the chosen
   * [[ExtractionTreeCall]] into a concrete tree (applying any accumulated renamings).
   *
   * @tparam Repr Concrete immutable e-graph representation.
   * @return An extractor that uses this analysis's results.
   *
   * @example {{{
   * val analysis = ExtractionAnalysis.smallest[Op]
   * val egm: EGraphWithMetadata[Op, MyEGraph] = ...
   * val ext = analysis.extractor[MyEGraph]
   * val tree: Tree[Op] = ext(call, egm)  // materializes the chosen minimal tree
   * }}}
   */
  def extractor[Repr <: EGraphLike[NodeT, Repr] with EGraph[NodeT]]: Extractor[NodeT, EGraphWithMetadata[NodeT, Repr]] = {
    new Extractor[NodeT, EGraphWithMetadata[NodeT, Repr]] {
      override def apply(call: EClassCall, egraph: EGraphWithMetadata[NodeT, Repr]): Tree[NodeT] = {
        val extractionTree = get(egraph)(call, egraph)
        extractionTree.applied.toTree
      }
    }
  }

  /**
   * Renames the slots in an analysis result without changing its chosen structure or cost.
   *
   * This composes the result's existing renaming with the provided `renaming`. Composition is
   * performed so that slots already mapped in the result are retained where appropriate.
   *
   * @param result   The analysis result to rebind.
   * @param renaming A mapping from current slot IDs to their new IDs in the caller's context.
   * @return A result equivalent up to alpha-renaming.
   */
  override def rename(result: ExtractionTreeCall[NodeT, C], renaming: SlotMap): ExtractionTreeCall[NodeT, C] = {
    ExtractionTreeCall(result.tree, result.renaming.composeRetain(renaming))
  }

  /**
   * Constructs the analysis value for a node given already-computed child results.
   *
   * Computes the node's cost via the provided [[CostFunction]], builds an [[ExtractionTree]]
   * carrying that cost and structure, asserts slot uniqueness, and wraps it as an
   * [[ExtractionTreeCall]] with an identity renaming on the tree's slot set.
   *
   * @param node The operator/type at this node.
   * @param defs Slots defined locally by this node.
   * @param uses Slots used by this node but defined elsewhere.
   * @param args Child analysis results (already scored and well-typed).
   * @return The composed analysis result for the subtree.
   */
  override def make(node: NodeT, defs: Seq[Slot], uses: Seq[Slot], args: Seq[ExtractionTreeCall[NodeT, C]]): ExtractionTreeCall[NodeT, C] = {
    val treeCost = cost(node, defs, uses, args.map(_.cost))
    val tree = ExtractionTree(treeCost, node, defs, uses, args)
    assert(tree.slotSet.forall(_.isUnique))
    // assert(node.slots.toSet.subsetOf(tree.slotSet))
    ExtractionTreeCall(
      tree,
      SlotMap.identity(tree.slotSet))
  }

  /**
   * Joins two competing results by choosing the minimal one under the analysis ordering.
   *
   * Primary key: `costOrdering`. Tie-break: `nodeOrdering` (via [[ExtractionTreeOrdering]]).
   *
   * @param left  Candidate A.
   * @param right Candidate B.
   * @return The chosen minimal candidate.
   */
  override def join(left: ExtractionTreeCall[NodeT, C],
                    right: ExtractionTreeCall[NodeT, C]): ExtractionTreeCall[NodeT, C] = {
    ExtractionTreeOrdering[NodeT, C]().callOrdering.min(left, right)
  }
}

/**
 * Constructors and presets for common extraction analyses.
 */
object ExtractionAnalysis {

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
