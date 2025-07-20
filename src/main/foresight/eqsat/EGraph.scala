package foresight.eqsat

import foresight.eqsat.hashCons.HashConsEGraph

/**
 * An immutable e-graph structure that provides a core API for working with e-classes and e-nodes.
 *
 * E-graphs are data structures used for representing and manipulating equivalence classes of expressions, enabling
 * efficient equality saturation and term rewriting. This trait defines the essential operations for querying, adding,
 * and merging e-classes and e-nodes, as well as for traversing and transforming the e-graph in a functional, immutable
 * style.
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
