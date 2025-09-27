package foresight.eqsat.mutable

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.{AddNodeResult, EClassCall, ReadOnlyEGraph}

/**
 * A mutable e-graph that supports adding e-nodes and merging e-classes.
 *
 * @tparam NodeT The type of the nodes in the e-graph.
 */
trait EGraph[NodeT] extends ReadOnlyEGraph[NodeT] {
  /**
   * Adds many e-nodes in one pass.
   *
   * For each input:
   *   - If the e-node already exists, the corresponding result is [[AddNodeResult.AlreadyThere]] with its e-class.
   *   - Otherwise, the e-node is inserted into a fresh e-class and the result is [[AddNodeResult.Added]].
   *
   * This method mutates the e-graph in place.
   *
   * @param nodes        The e-nodes to add.
   * @param parallelize  Strategy used for any parallel work within the addition.
   * @return Per-node results in input order.
   */
  def tryAddMany(nodes: Seq[NodeT], parallelize: ParallelMap): Seq[AddNodeResult]

  /**
   * Unions (merges) pairs of e-classes.
   *
   * Merging combines the member e-nodes of each pair, possibly triggering upward merging and
   * additional equivalences. The operation returns the updated e-graph and a partition of all
   * e-classes that became newly equivalent because of the unions.
   *
   * This method mutates the e-graph in place.
   *
   * @param pairs       Pairs of e-class applications to union.
   * @param parallelize Parallel strategy used during merging/rebuild work.
   * @return Sets of newly equivalent classes.
   */
  def unionMany(pairs: Seq[(EClassCall, EClassCall)], parallelize: ParallelMap): Set[Set[EClassCall]]

  /**
   * Creates an empty e-graph of the same concrete type and configuration (e.g., registered metadata),
   * without all current e-classes and e-nodes.
   *
   * @return An empty e-graph with the same configuration. Does not mutate the current e-graph.
   */
  def emptied: EGraph[NodeT]
}
