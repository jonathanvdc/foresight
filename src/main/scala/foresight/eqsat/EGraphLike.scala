package foresight.eqsat

import foresight.eqsat.metadata.EGraphWithMetadata
import foresight.eqsat.parallel.ParallelMap

/**
 * An e-graph*is a data structure for representing and maintaining equivalence classes of expressions.
 * E-graphs support equality saturation, a powerful technique for exploring all equivalent forms of a term
 * by exhaustively applying rewrite rules while avoiding redundant work by compacting equivalent subterms
 * into shared e-classes.
 *
 * In Foresight, an e-graph contains:
 *  - **E-classes** ([[EClassRef]]/[[EClassCall]]): partitions of semantically equivalent expressions.
 *  - **E-nodes** ([[ENode]]): operator/operand tuples representing a single application of a function or constructor.
 *  - A **canonicalization invariant**: every e-class has a unique, canonical representative, and all queries
 *    and updates are expressed in terms of these canonical forms.
 *
 * ## Purpose of this trait
 * [[EGraphLike]] defines the core API contract for e-graph implementations, parameterized by:
 *  - The node payload type `NodeT` (the domain-specific symbol type your e-nodes use).
 *  - The self-type `This`, so all operations that conceptually return "the same kind of e-graph"
 *    return your most-derived type, preserving fluent, type-safe chaining.
 *
 * This trait is deliberately minimal but powerful:
 *  - **Query methods**: enumerate classes, retrieve nodes, check membership, canonicalize, resolve usage.
 *  - **Construction methods**: add nodes and trees, bulk-add, merge e-classes (union), or create an empty graph.
 *  - **Traversal methods**: find the e-class containing a given node or tree.
 *
 * Concrete e-graph variants (e.g., with metadata, recorded rule applications, or deferred unions) are built by
 * wrapping or extending a base [[EGraphLike]] implementation.
 *
 * ## Immutability and functional style
 * All e-graph operations return a **new** e-graph instance rather than mutating the receiver. This makes
 * e-graphs:
 *   - **Thread-safe by default** for read operations
 *   - Naturally suited to speculative or branch-and-bound rewriting, where multiple alternate states can
 *     be explored in parallel without interfering with each other
 *
 * As a consequence:
 *   - Adding nodes or merging classes returns a *new* `This` instance with the updated structure.
 *   - Query methods never modify the e-graph.
 *
 * ## Canonicalization
 * Canonicalization is central to e-graph correctness and performance:
 *   - Every e-class is identified by a canonical [[EClassRef]] (chosen representative).
 *   - When adding or merging, all inputs are canonicalized so that the same equivalence is not represented twice.
 *   - Queries that take non-canonical inputs will internally canonicalize them.
 *
 * ## Related types
 *   - [[EGraph]] — a simpler, single-parameter variant of this trait.
 *   - [[foresight.eqsat.metadata.EGraphWithMetadata]] — an [[EGraphLike]] augmented with per-class metadata storage.
 *   - [[EGraphWithPendingUnions]] — a deferred-union wrapper for batching merges before a rebuild.
 *   - [[ENode]], [[EClassRef]], [[EClassCall]], [[ShapeCall]] — fundamental building blocks.
 *
 * @tparam NodeT The type of the domain-specific symbol or operator stored in each e-node.
 * @tparam This  The concrete e-graph type mixing in this trait, enabling precise return types for fluent APIs.
 * @example Typical usage
 * {{{
 * val (classA, g1) = egraph.add(nodeA)
 * val (classB, g2) = g1.add(nodeB)
 * val g3 = g2.union(classA, classB).rebuilt // merge equivalences
 * val allClasses: Iterable[EClassRef] = g3.classes
 * }}}
 */
trait EGraphLike[NodeT, +This <: EGraphLike[NodeT, This] with EGraph[NodeT]] {
  // Core API:

  /**
   * Returns the current canonical reference of an e-class if it exists in this e-graph.
   *
   * Canonicalization follows all pending/recorded unions so that structurally equal or merged
   * e-classes map to a stable representative.
   *
   * @param ref A possibly non-canonical e-class reference.
   * @return Some(canonical reference) if `ref` exists in this e-graph; otherwise, None.
   */
  def tryCanonicalize(ref: EClassRef): Option[EClassCall]

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
   * @param node The e-node to look up.
   * @return Some(e-class call) if the e-node exists in this e-graph; otherwise, None.
   */
  def find(node: ENode[NodeT]): Option[EClassCall]

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

  /**
   * Adds many e-nodes in one pass.
   *
   * For each input:
   *   - If the e-node already exists, the corresponding result is [[AddNodeResult.AlreadyThere]] with its e-class.
   *   - Otherwise, the e-node is inserted into a fresh e-class and the result is [[AddNodeResult.Added]].
   *
   * The e-graph is immutable: this method returns a new e-graph reflecting all insertions.
   *
   * @param nodes        The e-nodes to add.
   * @param parallelize  Strategy used for any parallel work within the addition.
   * @return (Per-node results in input order, new e-graph containing the additions).
   */
  def tryAddMany(nodes: Seq[ENode[NodeT]], parallelize: ParallelMap): (Seq[AddNodeResult], This)

  /**
   * Unions (merges) multiple pairs of e-classes.
   *
   * Merging combines the member e-nodes of each pair, possibly triggering upward merging and
   * additional equivalences. The operation returns the updated e-graph and a partition of all
   * e-classes that became newly equivalent because of the unions.
   *
   * The e-graph is immutable: this method returns a new e-graph with the merges applied.
   *
   * @param pairs        Pairs of e-class applications to union.
   * @param parallelize  Parallel strategy used during merging/rebuild work.
   * @return (Sets of newly equivalent classes, new e-graph after unions).
   */
  def unionMany(pairs: Seq[(EClassCall, EClassCall)], parallelize: ParallelMap): (Set[Set[EClassCall]], This)

  /**
   * Creates an empty e-graph of the same concrete type and configuration (e.g., registered metadata),
   * discarding all current e-classes and e-nodes.
   *
   * @return An empty e-graph with the same configuration.
   */
  def emptied: This

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
   * Canonicalizes an e-class reference.
   *
   * @throws NoSuchElementException if the e-class does not exist in this e-graph.
   * @param ref The e-class to canonicalize.
   * @return The canonical e-class application.
   */
  final def canonicalize(ref: EClassRef): EClassCall = tryCanonicalize(ref).get

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

      case MixedTree.Call(call) => Some(call)
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

  /**
   * Adds a single e-node.
   *
   * If the e-node already exists, returns its e-class; otherwise inserts it into a fresh e-class.
   * The e-graph is immutable: returns a new e-graph containing the result.
   *
   * @param node The e-node to add.
   * @return (E-class of `node`, new e-graph).
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
   *
   * Child calls (if any) are added/resolved first; then the root e-node is added or found.
   * Returns a new e-graph containing the result.
   *
   * @param tree The mixed tree to add.
   * @return (E-class of the root, new e-graph).
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
   * Adds a pure tree to the e-graph.
   *
   * Child subtrees are added/resolved first; then the root e-node is added or found.
   * Returns a new e-graph containing the result.
   *
   * @param tree The pure tree to add.
   * @return (E-class of the root, new e-graph).
   */
  final def add(tree: Tree[NodeT]): (EClassCall, This) = {
    val (args, graphWithArgs) = tree.args.foldLeft((Seq.empty[EClassCall], this))((acc, arg) => {
      val (node, egraph) = acc._2.add(arg)
      (acc._1 :+ node, egraph)
    })
    graphWithArgs.add(ENode(tree.nodeType, tree.definitions, tree.uses, args))
  }

  /**
   * Unions (merges) multiple pairs of e-classes using the default parallel strategy.
   *
   * @param pairs Pairs of e-class applications to union.
   * @return (Sets of newly equivalent classes, new e-graph after unions).
   */
  final def unionMany(pairs: Seq[(EClassCall, EClassCall)]): (Set[Set[EClassCall]], This) = {
    unionMany(pairs, ParallelMap.default)
  }

  /**
   * Defers a union between two e-classes.
   *
   * This does not immediately mutate the e-graph. Instead it returns an [[EGraphWithPendingUnions]]
   * that records the request. You can chain more unions on the result and apply them together later
   * via [[EGraphWithPendingUnions.rebuild]] / [[EGraphWithPendingUnions.rebuilt]].
   *
   * Internally, deferred unions are eventually passed to [[unionMany]], which performs a full rebuild.
   * Deferral can be substantially faster when many unions are expected, by avoiding repeated rebuilds.
   *
   * @param left  First e-class.
   * @param right Second e-class.
   * @return An [[EGraphWithPendingUnions]] wrapping this e-graph and the deferred union.
   */
  final def union(left: EClassCall, right: EClassCall): EGraphWithPendingUnions[NodeT, This] =
    EGraphWithPendingUnions[NodeT, This](this.asInstanceOf[This]).union(left, right)

  /**
   * Returns a view of this e-graph that can store and maintain per-e-class (and related) metadata.
   *
   * The returned instance preserves immutability: operations that would change metadata yield a new
   * e-graph-with-metadata instance.
   *
   * @return An [[EGraphWithMetadata]] that wraps this e-graph.
   */
  final def withMetadata: EGraphWithMetadata[NodeT, This] = EGraphWithMetadata(this.asInstanceOf[This])
}