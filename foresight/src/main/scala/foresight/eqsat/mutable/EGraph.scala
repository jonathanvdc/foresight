package foresight.eqsat.mutable

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.immutable
import foresight.eqsat.readonly.ReadOnlyEGraph
import foresight.eqsat.{AddNodeResult, EClassCall, ENode, MixedTree, Tree}

/**
 * A mutable e-graph that supports adding e-nodes and merging e-classes.
 *
 * @tparam NodeT The type of the nodes in the e-graph.
 */
trait EGraph[NodeT] extends ReadOnlyEGraph[NodeT] {
  // Core API:

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
  def tryAddMany(nodes: Seq[ENode[NodeT]], parallelize: ParallelMap): Seq[AddNodeResult]

  /**
   * Unions (merges) pairs of e-classes.
   *
   * Merging combines the member e-nodes of each pair, possibly triggering upward merging and
   * additional equivalences. The operation returns a partition of all
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
  def emptied: this.type

  // Helper methods:

  /**
   * Adds a single e-node.
   *
   * If the e-node already exists, returns its e-class; otherwise inserts it into a fresh e-class.
   * Updates the current e-graph in place.
   *
   * @param node The e-node to add.
   * @return E-class of `node`.
   */
  final def add(node: ENode[NodeT]): EClassCall = {
    tryAddMany(Seq(node), ParallelMap.sequential) match {
      case Seq(AddNodeResult.Added(call)) => call
      case Seq(AddNodeResult.AlreadyThere(call)) => call
      case _ => throw new IllegalStateException("Unexpected result from tryAddMany")
    }
  }

  /**
   * Adds a mixed tree to the e-graph.
   *
   * Child calls (if any) are added/resolved first; then the root e-node is added or found.
   * Updates the current e-graph in place.
   *
   * @param tree The mixed tree to add.
   * @return E-class of the root.
   */
  final def add(tree: MixedTree[NodeT, EClassCall]): EClassCall = {
    tree match {
      case MixedTree.Node(t, defs, uses, args) =>
        val newArgs = args.map(add)
        add(ENode(t, defs, uses, newArgs))

      case MixedTree.Atom(call) => call
    }
  }

  /**
   * Adds a pure tree to the e-graph.
   *
   * Child subtrees are added/resolved first; then the root e-node is added or found.
   * Updates the current e-graph in place.
   *
   * @param tree The pure tree to add.
   * @return E-class of the root.
   */
  final def add(tree: Tree[NodeT]): EClassCall = {
    add(MixedTree.fromTree(tree))
  }

  /**
   * Unions (merges) pairs of e-classes using the default parallel strategy.
   * Updates the current e-graph in place.
   *
   * @param pairs Pairs of e-class applications to union.
   * @return Sets of newly equivalent classes.
   */
  final def unionMany(pairs: Seq[(EClassCall, EClassCall)]): Set[Set[EClassCall]] = {
    unionMany(pairs, ParallelMap.default)
  }

  /**
   * Wraps this e-graph with metadata support.
   *
   * The returned wrapper delegates all core operations to this e-graph, while managing
   * metadata instances that can track additional information about the e-graph.
   *
   * @return A new e-graph wrapper with metadata support.
   */
  final def withMetadata: EGraphWithMetadata[NodeT, this.type] = {
    EGraphWithMetadata(this)
  }
}

/**
 * A companion object for the [[EGraph]] trait.
 */
object EGraph {
  /**
   * Creates a new mutable e-graph.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @return An empty mutable e-graph.
   */
  def empty[NodeT]: EGraph[NodeT] = FreezableEGraph(immutable.EGraph.empty[NodeT])

  /**
   * Builds a new e-graph from a single tree, returning the e-class of its root.
   *
   * This is equivalent to:
   * {{{
   * val g = EGraph.empty[NodeT]
   * g.add(tree)
   * }}}
   *
   * The input is a `MixedTree[NodeT, Nothing]`, i.e., a tree that does not contain pre-existing
   * `EClassCall`s. All children are inserted (or found) first, then the root e-node is inserted (or
   * found), yielding the root e-class.
   *
   * @param tree The tree to insert into a fresh e-graph.
   * @tparam NodeT The node type of the tree and resulting e-graph.
   * @return (Root e-class of `tree`, new e-graph containing exactly the nodes added by `tree`.)
   */
  def from[NodeT](tree: MixedTree[NodeT, Nothing]): (EClassCall, EGraph[NodeT]) = {
    val egraph = empty[NodeT]
    (egraph.add(tree), egraph)
  }
}
