package foresight.eqsat

import foresight.eqsat.hashCons.HashConsEGraph

/**
 * An immutable e-graph. An e-graph is a data structure that represents a set of expressions. Each expression is
 * represented by an e-node, which is a node in the e-graph. E-nodes are grouped into e-classes, which are sets of
 * equivalent e-nodes.
 *
 * @tparam NodeT The node type of the expressions that the e-graph represents.
 */
trait EGraph[NodeT] extends EGraphLike[NodeT, EGraph[NodeT]]

/**
 * A companion object for the immutable e-graph trait.
 */
object EGraph {
  /**
   * Creates a new empty e-graph.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @return An empty e-graph.
   */
  def empty[NodeT]: EGraph[NodeT] = HashConsEGraph.empty[NodeT]
}
