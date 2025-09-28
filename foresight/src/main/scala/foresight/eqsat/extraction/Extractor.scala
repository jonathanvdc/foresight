package foresight.eqsat.extraction

import foresight.eqsat.{EClassCall, MixedTree, Tree}
import foresight.eqsat.immutable.{EGraph, EGraphLike}
import foresight.eqsat.readonly

/**
 * An extractor that converts e-graph references (e-class calls) into concrete expression trees.
 * Implementations define the extraction policy (e.g., cost-based or heuristic).
 *
 * Determinism depends on the extractor and the e-graph contents. If multiple trees are equally
 * optimal under the policy, the chosen tree may be implementation-defined.
 *
 * @tparam NodeT The node type stored in the e-graph and produced in the resulting [[Tree]].
 * @tparam Repr  The concrete e-graph type, which must implement both [[EGraphLike]] and [[EGraph]].
 */
trait Extractor[NodeT, -Repr <: readonly.EGraph[NodeT]] {

  /**
   * Extracts a concrete expression tree that realizes the given e-class call, according to this
   * extractor's policy.
   *
   * @param call   The e-class call to extract from.
   * @param egraph The e-graph that contains `call` and its equivalences.
   * @return A concrete [[Tree]] composed of nodes of type `NodeT`.
   *
   * @example
   *   {{{
   *   val tree: Tree[NodeT] = extractor(call, egraph)
   *   }}}
   */
  def apply(call: EClassCall, egraph: Repr): Tree[NodeT]

  /**
   * Extracts a concrete expression tree from a [[MixedTree]] by first materializing it into an
   * e-class call on a derived e-graph and then extracting from that call.
   *
   * The original e-graph is not mutated. In Foresight, e-graphs are immutable; `egraph.add(tree)`
   * returns a new e-graph along with the created call. This method uses that derived e-graph for
   * extraction but does not expose it to the caller.
   *
   * @param tree   The mixed tree to materialize prior to extraction.
   * @param egraph The original e-graph; remains unchanged.
   * @return A concrete [[Tree]] extracted from the materialized call.
   *
   * @example
   *   {{{
   *   val result: Tree[NodeT] = extractor(mixedTree, egraph) // `egraph` is unchanged
   *   }}}
   */
  final def apply[
    EGraphT <: Repr with EGraph[NodeT] with EGraphLike[NodeT, EGraphT]
  ](tree: MixedTree[NodeT, EClassCall], egraph: EGraphT): Tree[NodeT] = {
    val (call, newGraph) = egraph.add(tree)
    apply(call, newGraph)
  }
}
