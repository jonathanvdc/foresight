package fixpoint.eqsat

import fixpoint.eqsat.metadata.EGraphWithMetadata
import fixpoint.eqsat.rewriting.PortableMatch
import fixpoint.eqsat.saturation.EGraphWithRecordedApplications

/**
 * An immutable e-graph. An e-graph is a data structure that represents a set of expressions. Each expression is
 * represented by an e-node, which is a node in the e-graph. E-nodes are grouped into e-classes, which are sets of
 * equivalent e-nodes.
 *
 * @tparam NodeT The node type of the expressions that the e-graph represents.
 */
trait EGraphLike[NodeT, +This <: EGraphLike[NodeT, This] with EGraph[NodeT]] {
  // Core API:

  /**
   * Gets the current canonical reference to an e-class, if the e-class is defined in this e-graph; otherwise, returns
   * None.
   * @param ref The reference to canonicalize.
   * @return The canonical reference to the e-class pointed to by ref, if the e-class is defined in this e-graph;
   *         otherwise, None.
   */
  def tryCanonicalize(ref: EClassRef): Option[EClassCall]

  /**
   * Canonicalizes an e-node, canonicalizing its arguments and decomposing it into a canonical shape and a renaming of
   * the shape's parameter slots to argument slots.
   * @param node The e-node to canonicalize.
   * @return The canonicalized e-node.
   */
  def canonicalize(node: ENode[NodeT]): ShapeCall[NodeT]

  /**
   * Enumerates all unique e-classes in this e-graph.
   * @return All unique e-classes in the e-graph, represented as their canonical references.
   */
  def classes: Iterable[EClassRef]

  /**
   * The set of nodes of a given e-class application.
   * @param call The e-class whose nodes to find.
   * @return All nodes in the e-class pointed to by app.
   */
  def nodes(call: EClassCall): Set[ENode[NodeT]]

  /**
   * The set of all e-nodes that refer to an e-class.
   * @param ref The e-class whose uses to find.
   * @return All e-classes that point to ref through their e-nodes.
   */
  def users(ref: EClassRef): Set[ENode[NodeT]]

  /**
   * Finds the e-class of a given e-node.
   * @param node The e-node to find the e-class of.
   * @return The e-class of the e-node, if it is defined in this e-graph; otherwise, None.
   */
  def find(node: ENode[NodeT]): Option[EClassCall]

  /**
   * Determines whether two e-classes are the same. Both classes are assumed to be in the e-graph.
   * @param first The first e-class to compare.
   * @param second The second e-class to compare.
   * @return True if the e-classes are the same; otherwise, false.
   */
  def areSame(first: EClassCall, second: EClassCall): Boolean

  /**
   * Adds an e-node to this e-graph If it is already present, then the e-node is not added, and the e-class reference of
   * the existing e-node is returned. Otherwise, the e-node is added to a unique e-class, whose reference is returned.
   * @param node The e-node to add to the e-graph.
   * @return The e-class reference of the e-node in the e-graph, and the new e-graph with the e-node added.
   */
  def add(node: ENode[NodeT]): (EClassCall, This)

  /**
   * Unions many e-classes in this e-graph. The resulting e-classes contain all e-nodes from the e-classes being unioned.
   * This operation builds a new e-graph with the e-classes unioned. Upward merging may produce further unions.
   * Both the updated e-graph and a set of all newly-equivalent e-classes are returned.
   * @param pairs The pairs of e-classes to union.
   * @return The e-classes resulting from the unions, and the new e-graph with the e-classes unioned.
   */
  def unionMany(pairs: Seq[(EClassCall, EClassCall)]): (Set[Set[EClassCall]], This)

  // Helper methods:

  /**
   * Gets the current canonical reference to an e-class.
   * @param ref The reference to canonicalize.
   * @return The canonical reference to the e-class pointed to by ref.
   */
  final def canonicalize(ref: EClassRef): EClassCall = tryCanonicalize(ref).get

  /**
   * Canonicalizes an e-class application.
   * @param call The e-class application to canonicalize.
   * @return The canonicalized e-class application.
   */
  final def canonicalize(call: EClassCall): EClassCall = {
    canonicalize(call.ref).rename(call.args)
  }

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
   * Adds a mixed tree to the e-graph.
   * @param tree The tree to add.
   * @return The e-class reference of the tree's root in the e-graph, and the new e-graph with the tree added.
   */
  final def add(tree: MixedTree[NodeT, EClassCall]): (EClassCall, This) = {
    tree match {
      case MixedTree.Node(t, defs, uses, args) =>
        val (newArgs, graphWithArgs) = args.foldLeft((Seq.empty[EClassCall], this.asInstanceOf[This]))((acc, arg) => {
          val (node, egraph) = acc._2.add(arg)
          (acc._1 :+ node, egraph)
        })
        graphWithArgs.add(ENode(t, defs, uses, newArgs))

      case MixedTree.Call(call) =>
        (call, this.asInstanceOf[This])
    }
  }

  /**
   * Adds a tree to the e-graph.
   * @param tree The tree to add.
   * @return The e-class reference of the tree's root in the e-graph, and the new e-graph with the tree added.
   */
  final def add(tree: Tree[NodeT]): (EClassCall, This) = {
    val (args, graphWithArgs) = tree.args.foldLeft((Seq.empty[EClassCall], this))((acc, arg) => {
      val (node, egraph) = acc._2.add(arg)
      (acc._1 :+ node, egraph)
    })
    graphWithArgs.add(ENode(tree.nodeType, tree.definitions, tree.uses, args))
  }

  /**
   * Adds a node to the e-graph, returning the new e-graph if the node was not already present.
   * @param node The node to add.
   * @return The e-class reference of the node in the e-graph, and the new e-graph with the node added.
   *         If the node was already present, then None is returned.
   */
  final def tryAdd(node: ENode[NodeT]): (EClassCall, Option[This]) = {
    find(node) match {
      case Some(call) => (call, None)
      case None =>
        val (call, egraph) = add(node)
        (call, Some(egraph))
    }
  }

  /**
   * Unions two e-classes in this e-graph. The resulting e-class contains all e-nodes from both e-classes.
   * The effects of this operation are deferred until the e-graph is rebuilt.
   *
   * @param left The reference to the first e-class to union.
   * @param right The reference to the second e-class to union.
   * @return The e-class reference of the resulting e-class, and the new e-graph with the e-classes unioned.
   */
  final def union(left: EClassCall, right: EClassCall): EGraphWithPendingUnions[This] =
    EGraphWithPendingUnions(this.asInstanceOf[This]).union(left, right)

  /**
   * Enhances this e-graph with the ability to store metadata.
   * @return The e-graph with metadata.
   */
  final def withMetadata: EGraphWithMetadata[NodeT, This] = EGraphWithMetadata(this.asInstanceOf[This])
}
