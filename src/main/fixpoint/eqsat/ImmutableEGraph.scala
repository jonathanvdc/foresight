package fixpoint.eqsat

import fixpoint.eqsat.hashCons.HashConsEGraph

/**
 * An immutable e-graph. An e-graph is a data structure that represents a set of expressions. Each expression is
 * represented by an e-node, which is a node in the e-graph. E-nodes are grouped into e-classes, which are sets of
 * equivalent e-nodes.
 *
 * @tparam NodeT The node type of the expressions that the e-graph represents.
 */
trait ImmutableEGraph[NodeT] {
  // Core API:

  /**
   * Gets the current canonical reference to an e-class, if the e-class is defined in this e-graph; otherwise, returns
   * None.
   * @param ref The reference to canonicalize.
   * @return The canonical reference to the e-class pointed to by ref, if the e-class is defined in this e-graph;
   *         otherwise, None.
   */
  def tryCanonicalize(ref: EClassRef): Option[EClassRef]

  /**
   * Enumerates all unique e-classes in this e-graph.
   * @return All unique e-classes in the e-graph, represented as their canonical references.
   */
  def classes: Iterable[EClassRef]

  /**
   * The set of nodes of a given e-class.
   * @param ref The e-class whose nodes to find.
   * @return All nodes in the e-class pointed to by ref.
   */
  def nodes(ref: EClassRef): Set[ENode[NodeT]]

  /**
   * Finds the e-class of a given e-node.
   * @param node The e-node to find the e-class of.
   * @return The e-class of the e-node, if it is defined in this e-graph; otherwise, None.
   */
  def find(node: ENode[NodeT]): Option[EClassRef]

  /**
   * Adds an e-node to this e-graph If it is already present, then the e-node is not added, and the e-class reference of
   * the existing e-node is returned. Otherwise, the e-node is added to a unique e-class, whose reference is returned.
   * @param node The e-node to add to the e-graph.
   * @return The e-class reference of the e-node in the e-graph, and the new e-graph with the e-node added.
   */
  def add(node: ENode[NodeT]): (EClassRef, ImmutableEGraph[NodeT])

  /**
   * Unions many e-classes in this e-graph. The resulting e-classes contain all e-nodes from the e-classes being unioned.
   * This operation builds a new e-graph with the e-classes unioned. Upward merging may produce further unions.
   * Both the updated e-graph and a set of all newly-equivalent e-classes are returned.
   * @param pairs The pairs of e-classes to union.
   * @return The e-classes resulting from the unions, and the new e-graph with the e-classes unioned.
   */
  def unionMany(pairs: Seq[(EClassRef, EClassRef)]): (Set[Set[EClassRef]], ImmutableEGraph[NodeT])

  // Helper methods:

  /**
   * Gets the current canonical reference to an e-class.
   * @param ref The reference to canonicalize.
   * @return The canonical reference to the e-class pointed to by ref.
   */
  final def canonicalize(ref: EClassRef): EClassRef = tryCanonicalize(ref).get

  /**
   * Canonicalizes an e-node.
   * @param node The e-node to canonicalize.
   * @return The canonicalized e-node.
   */
  final def canonicalize(node: ENode[NodeT]): ENode[NodeT] = ENode(node.nodeType, node.args.map(canonicalize))

  /**
   * Determines whether the e-graph contains a given e-class.
   * @param ref The e-class to check for.
   * @return True if the e-graph contains the e-class; otherwise, false.
   */
  final def contains(ref: EClassRef): Boolean = tryCanonicalize(ref).isDefined

  /**
   * Determines whether the e-graph contains a given e-node.
   * @param node The e-node to check for.
   * @return True if the e-graph contains the e-node; otherwise, false.
   */
  final def contains(node: ENode[NodeT]): Boolean = find(node).isDefined

  /**
   * Adds a tree to the e-graph.
   * @param tree The tree to add.
   * @return The e-class reference of the tree's root in the e-graph, and the new e-graph with the tree added.
   */
  final def add(tree: Tree[NodeT]): (EClassRef, ImmutableEGraph[NodeT]) = {
    val (args, graphWithArgs) = tree.args.foldLeft((Seq.empty[EClassRef], this))((acc, arg) => {
      val (node, egraph) = acc._2.add(arg)
      (acc._1 :+ node, egraph)
    })
    graphWithArgs.add(ENode(tree.nodeType, args))
  }

  /**
   * Unions two e-classes in this e-graph. The resulting e-class contains all e-nodes from both e-classes.
   * The effects of this operation are deferred until the e-graph is rebuilt.
   *
   * @param left The reference to the first e-class to union.
   * @param right The reference to the second e-class to union.
   * @return The e-class reference of the resulting e-class, and the new e-graph with the e-classes unioned.
   */
  final def union(left: EClassRef, right: EClassRef): EGraphWithPendingUnions[NodeT] =
    EGraphWithPendingUnions(this, List.empty).union(left, right)

  // Debugging and testing:

  /**
   * Checks the invariants of the e-graph.
   */
  private[eqsat] def checkInvariants(): Unit = ()
}

/**
 * A companion object for the immutable e-graph trait.
 */
object ImmutableEGraph {
  /**
   * Creates a new empty e-graph.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @return An empty e-graph.
   */
  def empty[NodeT]: ImmutableEGraph[NodeT] = HashConsEGraph.empty[NodeT]
}
