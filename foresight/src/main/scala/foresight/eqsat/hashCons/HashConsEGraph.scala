package foresight.eqsat.hashCons

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat._

/**
 * An e-graph that uses hash-consing to map e-nodes to e-classes.
 *
 * @param unionFind The disjoint set data structure that maintains the union-find information of the e-classes.
 * @param hashCons The hash-consing map that maps e-nodes to e-classes.
 * @param classData The data of each e-class in the e-graph.
 * @tparam NodeT The type of the nodes described by the e-nodes in the e-graph.
 */
private[eqsat] final case class HashConsEGraph[NodeT] private[hashCons](private val unionFind: SlottedUnionFind,
                                                                        private val hashCons: Map[ENode[NodeT], EClassRef],
                                                                        private val classData: Map[EClassRef, EClassData[NodeT]])
  extends EGraph[NodeT] with EGraphLike[NodeT, HashConsEGraph[NodeT]] with ReadOnlyHashConsEGraph[NodeT] {

  override def emptied: HashConsEGraph[NodeT] = HashConsEGraph.empty

  // We guarantee the following invariants:
  //   1. All nodes in hashCons and classData are kept canonical with regard to the current state of unionFind.
  //   2. The e-nodes in the hash-cons map are always kept in sync with the e-nodes in classData. That is,
  //      hashCons(node) == ref if and only if classData(ref).nodes contains node.
  //   3. The parents set of an e-class in classData is always kept in sync with the e-class arguments of the e-nodes
  //      in the e-class. That is, classData(ref).parents contains parent if and only if there exists an e-node in
  //      classData(ref) such that parent is in the e-node's arguments.

  private def toMutable: MutableHashConsEGraph[NodeT] = {
    new MutableHashConsEGraph(new MutableSlottedUnionFind(unionFind.parents), hashCons, classData)
  }

  override def classes: Iterable[EClassRef] = classData.keys

  override def canonicalizeOrNull(ref: EClassRef): EClassCall = {
    unionFind.findOrNull(ref)
  }

  override def nodeToRefOrElse(node: ENode[NodeT], default: => EClassRef): EClassRef = {
    hashCons.getOrElse(node, default)
  }

  override def isCanonical(ref: EClassRef): Boolean = {
    unionFind.isCanonical(ref)
  }

  override def dataForClass(ref: EClassRef): EClassData[NodeT] = {
    classData(ref)
  }

  override def tryAddMany(nodes: Seq[ENode[NodeT]],
                          parallelize: ParallelMap): (Seq[AddNodeResult], HashConsEGraph[NodeT]) = {
    // Adding independent e-nodes is fundamentally a sequential operation, but the most expensive part of adding nodes
    // is canonicalizing them and looking them up in the e-graph. Canonicalization can be parallelized since adding a
    // node will never change the canonical form of other nodes - only union operations can do that.
    //
    // Node lookups are partially parallelizable, but this is not worth the overhead of separating them into groups
    // of nodes that can safely be looked up in parallel. Instead, we just parallelize the canonicalization step and
    // then perform the lookups and additions sequentially.

    val p = parallelize.child("add nodes")

    val mutable = toMutable
    val canonicalized = p(nodes, canonicalize)
    val results = p.run {
      canonicalized.map { node =>
        mutable.tryAddUnsafe(node)
      }
    }
    (results.toSeq, mutable.toImmutable)
  }

  override def unionMany(pairs: Seq[(EClassCall, EClassCall)],
                         parallelize: ParallelMap): (Set[Set[EClassCall]], HashConsEGraph[NodeT]) = {
    require(
      pairs.forall { case (first, second) => first.isWellFormed(this) && second.isWellFormed(this) },
      "All e-class applications must be well-formed.")

    parallelize.child("union").run {
      val mutable = toMutable
      val equivalences = mutable.unionMany(pairs)
      (equivalences, mutable.toImmutable)
    }
  }

  /**
   * Checks that the invariants of the hash-consed e-graph are satisfied.
   */
  def checkInvariants(): Unit = {
    // Check that hashCons is canonicalized.
    for ((node, ref) <- hashCons) {
      assert(canonicalize(node).shape == node)
      assert(canonicalize(ref).ref == ref)
    }

    // Check that classData is canonicalized.
    for ((ref, data) <- classData) {
      assert(canonicalize(ref).ref == ref)
      for ((node, _) <- data.nodes) {
        assert(node.isShape)
        assert(canonicalize(node).shape == node)
      }
      for (user <- data.users) {
        assert(canonicalize(user).shape == user)
      }
    }

    // Check that the hash-cons map is in sync with the class data.
    for ((node, ref) <- hashCons) {
      assert(classData(ref).nodes.contains(node))
    }

    // Check that the users set of each e-class is in sync with the e-class arguments of the e-nodes in the e-class.
    for ((ref, data) <- classData) {
      for (user <- data.users) {
        assert(user.isShape)

        val userClass = hashCons(user)
        val userClassData = classData(userClass)
        assert(userClassData.nodes.keys.exists(_.args.map(_.ref).contains(ref)))
      }
    }
  }
}

/**
 * A companion object for the hash-consed e-graph.
 */
private[eqsat] object HashConsEGraph {
  /**
   * Creates a new hash-consed e-graph with no e-nodes.
   * @tparam NodeT The type of the nodes described by the e-nodes in the e-graph.
   * @return An empty hash-consed e-graph.
   */
  def empty[NodeT]: HashConsEGraph[NodeT] = HashConsEGraph(SlottedUnionFind.empty, Map.empty, Map.empty)
}
