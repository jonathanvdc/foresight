package foresight.eqsat

/**
 * A read-only interface to an e-graph, supporting queries about e-classes and e-nodes.
 *
 * This interface does not support any mutation operations.
 *
 * @tparam NodeT The type of the e-nodes stored in the e-graph.
 */
trait ReadOnlyEGraph[NodeT] {
  // Core API:

  /**
   * Returns the current canonical reference of an e-class if it exists in this e-graph.
   *
   * Canonicalization follows all pending/recorded unions so that structurally equal or merged
   * e-classes map to a stable representative.
   *
   * @param ref A possibly non-canonical e-class reference.
   * @return The canonical reference if `ref` exists in this e-graph; otherwise, `ref`.
   */
  def canonicalizeOrNull(ref: EClassRef): EClassCall

  /**
   * Canonicalizes an e-node: its argument calls are canonicalized, then the node is decomposed
   * into a canonical shape with a slot renaming that maps the shape's parameters to the arguments.
   *
   * @param node The e-node to canonicalize.
   * @return A canonicalized shape application for `node`.
   */
  def canonicalize(node: ENode[NodeT]): ShapeCall[NodeT]

  /**
   * Enumerates all unique (canonical) e-classes in the e-graph.
   *
   * @return Each distinct e-class, represented by its canonical reference.
   */
  def classes: Iterable[EClassRef]

  /**
   * Returns the set of e-nodes contained in a given e-class.
   *
   * @param call The (possibly non-canonical) e-class application to inspect.
   * @return All e-nodes stored in the canonical e-class targeted by `call`.
   */
  def nodes(call: EClassCall): Set[ENode[NodeT]]

  /**
   * Returns the set of e-nodes that refer to (use) the given e-class.
   *
   * This is an inverse-usage query: it finds all e-nodes whose arguments include `ref`.
   *
   * @param ref The e-class whose users to retrieve.
   * @return All e-nodes in the graph that reference `ref`.
   */
  def users(ref: EClassRef): Set[ENode[NodeT]]

  /**
   * Finds the e-class containing the given e-node, if present.
   *
   * This variant returns null if the e-node is not found, for Java interoperability.
   *
   * @param node The e-node to look up.
   * @return The e-class call if the e-node exists in this e-graph; otherwise, null.
   */
  def findOrNull(node: ENode[NodeT]): EClassCall

  /**
   * Tests whether two e-class applications refer to the same canonical e-class.
   *
   * Both inputs are assumed to refer to e-classes in this e-graph.
   *
   * @param first  First e-class application.
   * @param second Second e-class application.
   * @return true if they canonicalize to the same e-class; false otherwise.
   */
  def areSame(first: EClassCall, second: EClassCall): Boolean

  // Helper methods:

  /**
   * Counts distinct e-classes in this e-graph.
   * @return The number of distinct (canonical) e-classes in this e-graph.
   */
  final def classCount: Int = {
    classes.size
  }

  /**
   * Counts the total number of e-nodes across all e-classes in this e-graph.
   * @return The total number of e-nodes across all e-classes.
   */
  final def nodeCount: Int = {
    classes.toSeq.map(ref => nodes(canonicalize(ref)).size).sum
  }

  /**
   * Returns the current canonical reference of an e-class if it exists in this e-graph.
   *
   * Canonicalization follows all pending/recorded unions so that structurally equal or merged
   * e-classes map to a stable representative.
   *
   * @param ref A possibly non-canonical e-class reference.
   * @return Some(canonical reference) if `ref` exists in this e-graph; otherwise, None.
   */
  final def tryCanonicalize(ref: EClassRef): Option[EClassCall] = Option(canonicalizeOrNull(ref))

  /**
   * Canonicalizes an e-class reference.
   *
   * @throws NoSuchElementException if the e-class does not exist in this e-graph.
   * @param ref The e-class to canonicalize.
   * @return The canonical e-class application.
   */
  final def canonicalize(ref: EClassRef): EClassCall = {
    val call = canonicalizeOrNull(ref)
    if (call == null) {
      throw new NoSuchElementException(s"EClassRef $ref does not exist in this e-graph")
    } else {
      call
    }
  }

  /**
   * Canonicalizes an e-class application by canonicalizing its reference and then reapplying
   * the original argument renaming.
   *
   * @param call The e-class application to canonicalize.
   * @return The canonicalized e-class application.
   */
  final def canonicalize(call: EClassCall): EClassCall = {
    canonicalize(call.ref).renamePartial(call.args)
  }

  /**
   * Finds the e-class containing the given e-node, if present.
   *
   * @param node The e-node to look up.
   * @return Some(e-class call) if the e-node exists in this e-graph; otherwise, None.
   */
  final def find(node: ENode[NodeT]): Option[EClassCall] = Option(findOrNull(node))

  /**
   * Checks whether the e-graph contains the given e-class reference.
   *
   * @param ref The e-class reference to check.
   * @return true if the reference resolves to a canonical e-class; false otherwise.
   */
  final def contains(ref: EClassRef): Boolean = tryCanonicalize(ref).isDefined

  /**
   * Checks whether the e-graph contains the given e-node.
   *
   * @param node The e-node to check.
   * @return true if present; false otherwise.
   */
  final def contains(node: ENode[NodeT]): Boolean = find(node).isDefined

  /**
   * Checks whether the e-graph contains the given mixed tree (all subcalls must exist).
   *
   * @param tree The mixed tree to check.
   * @return true if the root resolves to an existing e-class; false otherwise.
   */
  final def contains(tree: MixedTree[NodeT, EClassCall]): Boolean = find(tree).isDefined

  /**
   * Checks whether the e-graph contains the given pure tree (all subtrees must exist).
   *
   * @param tree The tree to check.
   * @return true if the root resolves to an existing e-class; false otherwise.
   */
  final def contains(tree: Tree[NodeT]): Boolean = find(tree).isDefined

  /**
   * Resolves the e-class corresponding to the root of a mixed tree, if all argument calls exist.
   *
   * @param tree The mixed tree to resolve.
   * @return Some(e-class call) if all children exist and the root e-node exists; otherwise, None.
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

      case MixedTree.Atom(call) => Some(call)
    }
  }

  /**
   * Resolves the e-class corresponding to the root of a pure tree, if all subtrees exist.
   *
   * @param tree The pure tree to resolve.
   * @return Some(e-class call) if all children exist and the root e-node exists; otherwise, None.
   */
  final def find(tree: Tree[NodeT]): Option[EClassCall] = {
    val args = tree.args.map(find).collect { case Some(call) => call }
    if (args.size == tree.args.size) {
      find(ENode(tree.nodeType, tree.definitions, tree.uses, args))
    } else {
      None
    }
  }
}
