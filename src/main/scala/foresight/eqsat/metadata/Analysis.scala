package foresight.eqsat.metadata

import foresight.eqsat.{EGraph, ENode, Slot, SlotMap}

/**
 * An analysis that can be performed on an e-graph.
 *
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam A The type of the analysis result.
 */
trait Analysis[NodeT, A] {
  /**
   * The name of the analysis.
   */
  def name: String

  /**
   * Renames the slots in an analysis result.
   * @param result The analysis result to rename.
   * @param renaming The renaming to apply to the slots. The keys of the map are the slots as they appear in the result,
   *                 and the values are the slots to which they are renamed.
   * @return The analysis result with the slots renamed.
   */
  def rename(result: A, renaming: SlotMap): A

  /**
   * Makes an analysis result for a node.
   * @param node The type of node to make the analysis result for.
   * @param defs The slots that are defined by the node.
   * @param uses The slots that are used by the node and are defined elsewhere.
   * @param args The analysis results for the arguments to the node.
   * @return The analysis result for the node.
   */
  def make(node: NodeT, defs: Seq[Slot], uses: Seq[Slot], args: Seq[A]): A

  /**
   * Makes an analysis result for an e-node.
   * @param node The e-node to make the analysis result for.
   * @param args The analysis results for the arguments to the e-node.
   * @return The analysis result for the e-node.
   */
  final def make(node: ENode[NodeT], args: Seq[A]): A = {
    make(node.nodeType, node.definitions, node.uses, args)
  }

  /**
   * Joins two analysis results.
   * @param left The left analysis result.
   * @param right The right analysis result.
   * @return The joined analysis result.
   */
  def join(left: A, right: A): A

  /**
   * Analyzes an e-graph.
   * @param egraph The e-graph to analyze.
   * @return The analysis metadata for the e-graph.
   */
  final def apply(egraph: EGraph[NodeT]): AnalysisMetadata[NodeT, A] = {
    val updater = new AnalysisUpdater(this, egraph, Map.empty)

    // First apply the analysis to all e-nodes that have no arguments.
    for (c <- egraph.classes) {
      for (node <- egraph.nodes(egraph.canonicalize(c))) {
        if (node.args.isEmpty) {
          updater.update(c, make(node, Seq.empty))
        }
      }
    }

    // Process the worklist by applying the analysis to the rest of the e-graph. This will eventually touch all e-nodes.
    updater.processPending(initialized = false)

    AnalysisMetadata(this, updater.results)
  }

  /**
   * Gets the analysis metadata from an e-graph.
   * @param egraph The e-graph to get the metadata from.
   * @return The analysis metadata.
   */
  final def get(egraph: EGraphWithMetadata[NodeT, _]): AnalysisMetadata[NodeT, A] = {
    egraph.getMetadata(name)
  }
}
