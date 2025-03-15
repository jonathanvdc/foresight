package eqsat.hashCons

import eqsat.{DisjointSet, EClassRef, ENode, ImmutableEGraph}

/**
 * An e-graph that uses hash-consing to map e-nodes to e-classes.
 * @param unionFind The disjoint set data structure that maintains the union-find information of the e-classes.
 * @param hashCons The hash-consing map that maps e-nodes to e-classes.
 * @param classData The data of each e-class in the e-graph.
 * @param classRepairWorklist The set of e-classes in the e-graph that may contain nodes pointing to non-canonical
 *                            e-classes. The e-classes in this worklist may themselves be non-canonical.
 * @tparam ExprT The type of the expression that the e-graph represents.
 */
private[eqsat] final case class HashConsEGraph[ExprT] private(unionFind: DisjointSet[EClassRef],
                                                              hashCons: Map[ENode[ExprT], EClassRef],
                                                              classData: Map[EClassRef, HashConsEClassData[ExprT]],
                                                              classRepairWorklist: Seq[EClassRef]) extends ImmutableEGraph[ExprT] {

  // When the e-graph does not require repair (i.e., when the repair worklist is empty), we guarantee the following
  // invariant: except for the union-find data structure, all EClassRefs in the e-graph are canonicalized.
  // When the repair worklist is not empty, all non-canonical EClassRefs in the e-graph are referred to by the e-nodes
  // of the e-classes in the repair worklist.

  override def classes: Seq[EClassRef] = classData.keys.toSeq

  override def tryCanonicalize(ref: EClassRef): Option[EClassRef] = {
    val canonical = unionFind.find(ref)
    if (canonical == ref) Some(canonical).filter(classData.contains) else Some(canonical)
  }

  override def canonicalize(ref: EClassRef): EClassRef = unionFind.find(ref)

  override def nodes(ref: EClassRef): Set[ENode[ExprT]] = classData(ref).nodes

  override def find(node: ENode[ExprT]): Option[EClassRef] = {
    hashCons.get(canonicalize(node)).map(canonicalize)
  }

  override def add(node: ENode[ExprT]): (EClassRef, ImmutableEGraph[ExprT]) = {
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

  override def union(left: EClassRef, right: EClassRef): (EClassRef, ImmutableEGraph[ExprT]) = {
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

  override def rebuilt: ImmutableEGraph[ExprT] = ???
}
