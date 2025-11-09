package foresight.eqsat.readonly

import foresight.eqsat.{CallTree, EClassCall, EClassRef, MixedTree}
import foresight.eqsat.metadata.Analysis

/**
 * Incremental, queryable results of running an [[Analysis]] over an e-graph.
 *
 * This is a read-only interface that provides access to analysis results for e-classes.
 *
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam A     The type of the analysis results.
 */
trait AnalysisMetadata[NodeT, A] {
  /** The analysis associated with this metadata. */
  def analysis: Analysis[NodeT, A]

  /**
   * Gets the stored analysis result for the given e-class reference.
   * @param call The e-class reference to get the analysis result for.
   * @return The analysis result for the given e-class reference.
   */
  def apply(call: EClassRef): A

  /**
   * Compute the analysis result for an e-class application.
   *
   * Canonicalizes `call` against `egraph` and then delegates to [[applyPrecanonicalized]].
   *
   * @param call   The (possibly non-canonical) e-class application.
   * @param egraph The e-graph to canonicalize within.
   * @return The analysis result for `call`.
   */
  final def apply(call: EClassCall, egraph: EGraph[NodeT]): A = applyPrecanonicalized(egraph.canonicalize(call))

  /**
   * Evaluate a mixed tree whose leaves are either e-class applications or concrete nodes.
   *
   * For a call leaf, this delegates to [[apply(EClassCall,EGraph)]]. For a node leaf, it first
   * computes the results of all argument subtrees and then invokes the analysis transfer function
   * [[Analysis.make]] using the node’s definitions and uses provided by the tree.
   *
   * @param tree   The mixed tree to evaluate.
   * @param egraph The e-graph to resolve calls and canonicalization.
   * @return The analysis result for the whole tree.
   */
  final def apply(tree: MixedTree[NodeT, EClassCall],
                  egraph: EGraph[NodeT]): A = {
    tree match {
      case MixedTree.Atom(call: EClassCall) => apply(call, egraph)

      case MixedTree.Node(node, defs, uses, args) =>
        val argsResults = args.map(apply(_, egraph))
        analysis.make(node, defs, uses, argsResults)
    }
  }

  /**
   * Evaluate a call tree whose leaves are either e-class applications or concrete nodes.
   *
   * For a call leaf, this delegates to [[apply(EClassCall,EGraph)]]. For a node leaf, it first
   * computes the results of all argument subtrees and then invokes the analysis transfer function
   * [[Analysis.make]] using the node’s definitions and uses provided by the tree.
   *
   * @param tree   The call tree to evaluate.
   * @param egraph The e-graph to resolve calls and canonicalization.
   * @return The analysis result for the whole tree.
   */
  final def apply(tree: CallTree[NodeT],
                  egraph: EGraph[NodeT]): A = {
    tree match {
      case CallTree.Node(node, defs, uses, args) =>
        val argsResults = args.map(apply(_, egraph))
        analysis.make(node, defs, uses, argsResults)

      case call: EClassCall => apply(call, egraph)
    }
  }

  /**
   * Compute the analysis result for an already-canonicalized e-class application.
   *
   * The stored class result is parameterized over *generic* slots; we specialize it to this call
   * by alpha-renaming according to the call’s argument mapping.
   *
   * @param call Canonical e-class application.
   * @return The specialized analysis result.
   */
  protected def applyPrecanonicalized(call: EClassCall): A =
    analysis.rename(apply(call.ref), call.args)
}
