package foresight.eqsat.lang

import foresight.eqsat.EClassCall
import foresight.eqsat.immutable
import foresight.eqsat.readonly.ReadOnlyEGraph

/**
 * An extractor that converts e-graph references (e-class calls) into surface language expressions.
 * Implementations define the extraction policy (e.g., cost-based or heuristic).
 *
 * Determinism depends on the extractor and the e-graph contents. If multiple trees are equally
 * optimal under the policy, the chosen tree may be implementation-defined.
 *
 * @tparam E The surface expression type stored in the e-graph and produced in the resulting expression.
 * @tparam Repr  The concrete e-graph type, which must implement both [[EGraphLike]] and [[EGraph]].
 */
trait LanguageExtractor[E, -Repr <: ReadOnlyEGraph[LanguageOp[E]]](using L: Language[E]) {

  /**
   * Extracts a concrete expression tree that realizes the given e-class call, according to this
   * extractor's policy.
   *
   * @param call   The e-class call to extract from.
   * @param egraph The e-graph that contains `call` and its equivalences.
   * @return A concrete surface expression of type `E`.
   *
   * @example
   *   {{{
   *   val expr: E = extractor(call, egraph)
   *   }}}
   */
  def apply(call: EClassCall, egraph: Repr): E

  /**
   * Extracts a concrete expression tree from an expression by first materializing it into an
   * e-class call on a derived e-graph and then extracting from that call.
   *
   * The original e-graph is not mutated. In Foresight, e-graphs are immutable; `egraph.add(tree)`
   * returns a new e-graph along with the created call. This method uses that derived e-graph for
   * extraction but does not expose it to the caller.
   *
   * @param tree   An expression that potentially includes e-classes.
   * @param egraph The original e-graph; remains unchanged.
   * @param enc Encoder for the expression type `E` to the e-graph's `EClassCall`.
   * @return A concrete `E` extracted from the materialized call.
   *
   * @example
   *   {{{
   *   val result: E = extractor(mixedTree, egraph) // `egraph` is unchanged
   *   }}}
   */
  final def apply[
    EGraphT <: Repr with immutable.EGraph[LanguageOp[E]] with immutable.EGraphLike[LanguageOp[E], EGraphT]
  ](tree: E, egraph: EGraphT)(using enc: AtomEncoder[E, EClassCall]): E = {
    val (call, newGraph) = egraph.add(tree)
    apply(call, newGraph)
  }
}
