package fixpoint.eqsat.hashCons

import fixpoint.eqsat.{AppliedENode, AppliedRef, EClassData, EClassRef, EGraph, EGraphLike, ENode}

/**
 * An e-graph that uses hash-consing to map e-nodes to e-classes.
 *
 * @param unionFind The disjoint set data structure that maintains the union-find information of the e-classes.
 * @param hashCons The hash-consing map that maps e-nodes to e-classes.
 * @param classData The data of each e-class in the e-graph.
 * @tparam NodeT The type of the nodes described by the e-nodes in the e-graph.
 */
private[eqsat] final case class HashConsEGraph[NodeT] private[eqsat](private val unionFind: SlottedUnionFind,
                                                                     private val hashCons: Map[ENode[NodeT], EClassRef],
                                                                     private val classData: Map[EClassRef, EClassData[NodeT]])
  extends EGraph[NodeT] with EGraphLike[NodeT, HashConsEGraph[NodeT]] {

  // We guarantee the following invariants:
  //   1. All nodes in hashCons and classData are kept canonical with regard to the current state of unionFind.
  //   2. The e-nodes in the hash-cons map are always kept in sync with the e-nodes in classData. That is,
  //      hashCons(node) == ref if and only if classData(ref).nodes contains node.
  //   3. The parents set of an e-class in classData is always kept in sync with the e-class arguments of the e-nodes
  //      in the e-class. That is, classData(ref).parents contains parent if and only if there exists an e-node in
  //      classData(ref) such that parent is in the e-node's arguments.

  private def toMutable: MutableHashConsEGraph[NodeT] = {
    new MutableHashConsEGraph(new MutableSlottedUnionFind(unionFind), hashCons, classData)
  }

  override def classes: Iterable[EClassRef] = classData.keys

  override def tryCanonicalize(ref: EClassRef): Option[AppliedRef] = {
    unionFind.tryFind(ref)
  }

  override def canonicalize(node: ENode[NodeT]): AppliedENode[NodeT] = {
    toMutable.canonicalize(node)
  }

  override def nodes(app: AppliedRef): Set[ENode[NodeT]] = {
    val canonicalApp = canonicalize(app)
    classData(canonicalApp.ref).appliedNodes.map(_.rename(canonicalApp.args).applied)
  }

  override def users(ref: EClassRef): Set[ENode[NodeT]] = {
    val canonicalApp = canonicalize(ref)
    classData(canonicalApp.ref).users.map(node => {
      val c = hashCons(node)
      val mapping = classData(c).nodes(node)
      AppliedENode(mapping, node).applied
    })
  }

  override def find(node: ENode[NodeT]): Option[AppliedRef] = {
    toMutable.find(node)
  }

  override def add(node: ENode[NodeT]): (AppliedRef, HashConsEGraph[NodeT]) = {
    val mutable = toMutable
    val ref = mutable.add(node)
    (ref, mutable.toImmutable)
  }

  override def unionMany(pairs: Seq[(AppliedRef, AppliedRef)]): (Set[Set[AppliedRef]], HashConsEGraph[NodeT]) = {
    val mutable = toMutable
    val equivalences = mutable.unionMany(pairs)
    (equivalences, mutable.toImmutable)
  }

  /**
   * Checks that the invariants of the hash-consed e-graph are satisfied.
   */
  def checkInvariants(): Unit = {
    // Check that hashCons is canonicalized.
    for ((node, ref) <- hashCons) {
      assert(canonicalize(node).node == node)
      assert(canonicalize(ref).ref == ref)
    }

    // Check that classData is canonicalized.
    for ((ref, data) <- classData) {
      assert(canonicalize(ref).ref == ref)
      for ((node, _) <- data.nodes) {
        assert(node.isShape)
        assert(canonicalize(node).node == node)
      }
      for (user <- data.users) {
        assert(canonicalize(user).node == user)
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
object HashConsEGraph {
  /**
   * Creates a new hash-consed e-graph with no e-nodes.
   * @tparam NodeT The type of the nodes described by the e-nodes in the e-graph.
   * @return An empty hash-consed e-graph.
   */
  def empty[NodeT]: HashConsEGraph[NodeT] = HashConsEGraph(SlottedUnionFind.empty, Map.empty, Map.empty)
}
