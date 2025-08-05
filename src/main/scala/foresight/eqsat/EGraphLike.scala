package foresight.eqsat

import foresight.eqsat.metadata.EGraphWithMetadata
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.rewriting.PortableMatch
import foresight.eqsat.saturation.EGraphWithRecordedApplications

/**
 * An immutable e-graph structure that provides a core API for working with e-classes and e-nodes.
 *
 * E-graphs are data structures used for representing and manipulating equivalence classes of expressions, enabling
 * efficient equality saturation and term rewriting. This trait defines the essential operations for querying, adding,
 * and merging e-classes and e-nodes, as well as for traversing and transforming the e-graph in a functional, immutable
 * style.
 *
 * The split between [[EGraphLike]] and [[EGraph]] allows [[EGraph]] to be a simpler trait parameterized only by
 * `NodeT`, while [[EGraphLike]] is parameterized by both `NodeT` and the most derived type (`This`). This design
 * ensures that methods in [[EGraphLike]] can always return the most precise type of the implementing e-graph.
 *
 * @tparam NodeT The type of the nodes described by the e-nodes in the e-graph.
 * @tparam This The type of the e-graph that this trait is mixed into, which must be a subtype of [[EGraphLike]].
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
   * Adds many e-nodes to this e-graph. If any of the e-nodes are already present, then they are not added, and the
   * corresponding result is an e-class application that contains the e-node. Otherwise, the e-nodes are added to unique
   * e-classes, whose references are returned along with the new e-graph.
   * @param nodes The e-nodes to add to the e-graph.
   * @param parallelize The parallelization strategy to use for parallel tasks in the addition operation.
   * @return The results of adding each e-node, and the new e-graph with the e-nodes added.
   */
  def tryAddMany(nodes: Seq[ENode[NodeT]], parallelize: ParallelMap): (Seq[AddNodeResult], This)

  /**
   * Unions many e-classes in this e-graph. The resulting e-classes contain all e-nodes from the e-classes being unioned.
   * This operation builds a new e-graph with the e-classes unioned. Upward merging may produce further unions.
   * Both the updated e-graph and a set of all newly-equivalent e-classes are returned.
   * @param pairs The pairs of e-classes to union.
   * @param parallelize The parallel map to use for parallel tasks in the union operation.
   * @return The e-classes resulting from the unions, and the new e-graph with the e-classes unioned.
   */
  def unionMany(pairs: Seq[(EClassCall, EClassCall)], parallelize: ParallelMap): (Set[Set[EClassCall]], This)

  /**
   * Creates a new e-graph of this type that is empty, i.e., it contains no e-classes or e-nodes. While the contents
   * of the e-graph are discard, its configuration (e.g., registered metadata) is preserved.
   * @return An empty e-graph of this type.
   */
  def emptied: This

  // Helper methods:

  /**
   * Gets the number of unique e-classes in this e-graph.
   * @return The number of unique e-classes in the e-graph.
   */
  final def classCount: Int = {
    classes.size
  }

  /**
   * Gets the number of unique e-nodes in this e-graph.
   * @return The number of e-nodes in the e-graph.
   */
  final def nodeCount: Int = {
    classes.toSeq.map(ref => nodes(canonicalize(ref)).size).sum
  }

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
    canonicalize(call.ref).renamePartial(call.args)
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
   * Determines whether the e-graph contains a given mixed tree.
   * @param tree The mixed tree to check for.
   * @return True if the e-graph contains the tree; otherwise, false.
   */
  final def contains(tree: MixedTree[NodeT, EClassCall]): Boolean = find(tree).isDefined

  /**
   * Determines whether the e-graph contains a given tree.
   * @param tree The tree to check for.
   * @return True if the e-graph contains the tree; otherwise, false.
   */
  final def contains(tree: Tree[NodeT]): Boolean = find(tree).isDefined

  /**
   * Finds the e-class corresponding to the root of a mixed tree.
   * @param tree The mixed tree to find in the e-graph.
   * @return The e-class of the tree's root, if it is defined in this e-graph; otherwise, None.
   */
  final def find(tree: MixedTree[NodeT, EClassCall]): Option[EClassCall] = {
    tree match {
      case MixedTree.Node(t, defs, uses, args) =>
        val newArgs = args.map(find).collect { case Some(call) => call }
        if (newArgs.size == args.size) {
          find(ENode(t, defs, uses, newArgs))
        } else {
          None
        }

      case MixedTree.Call(call) => Some(call)
    }
  }

  /**
   * Finds the e-class corresponding to the root of a tree.
   * @param tree The tree to find in the e-graph.
   * @return The e-class of the tree's root, if it is defined in this e-graph; otherwise, None.
   */
  final def find(tree: Tree[NodeT]): Option[EClassCall] = {
    val args = tree.args.map(find).collect { case Some(call) => call }
    if (args.size == tree.args.size) {
      find(ENode(tree.nodeType, tree.definitions, tree.uses, args))
    } else {
      None
    }
  }

  /**
   * Adds an e-node to this e-graph If it is already present, then the e-node is not added, and the e-class reference of
   * the existing e-node is returned. Otherwise, the e-node is added to a unique e-class, whose reference is returned.
   * @param node The e-node to add to the e-graph.
   * @return The e-class reference of the e-node in the e-graph, and the new e-graph with the e-node added.
   */
  final def add(node: ENode[NodeT]): (EClassCall, This) = {
    tryAddMany(Seq(node), ParallelMap.sequential) match {
      case (Seq(AddNodeResult.Added(call)), egraph) => (call, egraph)
      case (Seq(AddNodeResult.AlreadyThere(call)), egraph) => (call, egraph)
      case _ => throw new IllegalStateException("Unexpected result from tryAddMany")
    }
  }

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
   * Unions many e-classes in this e-graph. The resulting e-classes contain all e-nodes from the e-classes being unioned.
   * This operation builds a new e-graph with the e-classes unioned. Upward merging may produce further unions.
   * Both the updated e-graph and a set of all newly-equivalent e-classes are returned.
   * @param pairs The pairs of e-classes to union.
   * @return The e-classes resulting from the unions, and the new e-graph with the e-classes unioned.
   */
  final def unionMany(pairs: Seq[(EClassCall, EClassCall)]): (Set[Set[EClassCall]], This) = {
    unionMany(pairs, ParallelMap.default)
  }

  /**
   * Unions two e-classes in this e-graph. The resulting e-class contains all e-nodes from both e-classes.
   * The effects of this operation are deferred until the e-graph is rebuilt.
   *
   * @param left The reference to the first e-class to union.
   * @param right The reference to the second e-class to union.
   * @return The e-class reference of the resulting e-class, and the new e-graph with the e-classes unioned.
   */
  final def union(left: EClassCall, right: EClassCall): EGraphWithPendingUnions[NodeT, This] =
    EGraphWithPendingUnions(this.asInstanceOf[This]).union(left, right)

  /**
   * Enhances this e-graph with the ability to store metadata.
   * @return The e-graph with metadata.
   */
  final def withMetadata: EGraphWithMetadata[NodeT, This] = EGraphWithMetadata(this.asInstanceOf[This])
}
