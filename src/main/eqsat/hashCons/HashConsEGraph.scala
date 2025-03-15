package eqsat.hashCons

import eqsat.{DisjointSet, EClassRef, ENode, ImmutableEGraph}

/**
 * An e-graph that uses hash-consing to map e-nodes to e-classes.
 * @param unionFind The disjoint set data structure that maintains the union-find information of the e-classes.
 * @param hashCons The hash-consing map that maps e-nodes to e-classes.
 * @param classData The data of each e-class in the e-graph.
 * @param classRepairWorklist The set of e-classes in the e-graph that may contain nodes pointing to non-canonical
 *                            e-classes. The e-classes in this worklist may themselves be non-canonical.
 * @tparam NodeT The type of the nodes described by the e-nodes in the e-graph.
 */
private[eqsat] final case class HashConsEGraph[NodeT] private(unionFind: DisjointSet[EClassRef],
                                                              hashCons: Map[ENode[NodeT], EClassRef],
                                                              classData: Map[EClassRef, HashConsEClassData[NodeT]],
                                                              classRepairWorklist: Seq[EClassRef]) extends ImmutableEGraph[NodeT] {

  // We guarantee the following invariants:
  //   1. All non-canonical EClassRefs in the e-graph are referred to by the e-nodes of the e-classes in the repair
  //      worklist.
  //   2. The e-nodes in the hash-cons map are always kept in sync with the e-nodes in classData. That is,
  //      hashCons(node) == ref if and only if classData(ref).nodes contains node.

  override def classes: Seq[EClassRef] = classData.keys.toSeq

  override def tryCanonicalize(ref: EClassRef): Option[EClassRef] = {
    val canonical = unionFind.find(ref)
    if (canonical == ref) Some(canonical).filter(classData.contains) else Some(canonical)
  }

  override def canonicalize(ref: EClassRef): EClassRef = unionFind.find(ref)

  override def nodes(ref: EClassRef): Set[ENode[NodeT]] = classData(ref).nodes

  override def find(node: ENode[NodeT]): Option[EClassRef] = {
    hashCons.get(canonicalize(node)).map(canonicalize)
  }

  override def add(node: ENode[NodeT]): (EClassRef, ImmutableEGraph[NodeT]) = {
    val canonicalNode = canonicalize(node)
    find(canonicalNode) match {
      case Some(ref) => (ref, this)
      case None =>
        val ref = new EClassRef()
        val newClassData = HashConsEClassData(Set(canonicalNode), Set.empty)
        val newHashCons = hashCons + (canonicalNode -> ref)
        val classDataWithNode = classData + (ref -> newClassData)
        val classDataWithUpdatedChildren = classDataWithNode ++ canonicalNode.args.distinct.map(c =>
          c -> classData(c).copy(parents = classData(c).parents + ref))

        (ref, HashConsEGraph(unionFind, newHashCons, classDataWithUpdatedChildren, classRepairWorklist))
    }
  }

  override def union(left: EClassRef, right: EClassRef): (EClassRef, ImmutableEGraph[NodeT]) = {
    val leftRoot = canonicalize(left)
    val rightRoot = canonicalize(right)
    if (leftRoot == rightRoot) {
      (leftRoot, this)
    } else {
      // Union the two classes and figure out which one is the dominant class.
      val newUnionFind = unionFind.union(leftRoot, rightRoot)
      val domRoot = unionFind.find(leftRoot)
      val subRoot = if (domRoot == leftRoot) rightRoot else leftRoot

      // Merge the nodes and parents of the dominant and subordinate classes.
      val domData = classData(domRoot)
      val subData = classData(subRoot)
      val newNodes = domData.nodes ++ subData.nodes
      val newParents = domData.parents ++ subData.parents
      val newClassData = HashConsEClassData(newNodes, newParents)
      val newClassDataMap = classData + (domRoot -> newClassData) - subRoot

      // Update the hash-cons so that all nodes from the subordinate class now point to the dominant class. Make no
      // attempt at canonicalizing the nodes, as we will perform this operation in the rebuilding logic.
      val newHashCons = hashCons ++ subData.nodes.map(_ -> domRoot)

      // The merge we just performed may have broken the invariant that all EClassRefs in the e-graph are canonicalized.
      // Specifically, the subordinate class may still be referred to by other e-nodes, which are captured by the
      // subordinate class' parents set.
      // To eventually repair the invariant, we add the subordinate class' parents set to the repair worklist.
      val newClassRepairWorklist = classRepairWorklist ++ subData.parents

      (leftRoot, HashConsEGraph(newUnionFind, newHashCons, newClassDataMap, newClassRepairWorklist))
    }
  }

  override def requiresRebuild: Boolean = classRepairWorklist.nonEmpty

  override def rebuilt: ImmutableEGraph[NodeT] = ???
}
