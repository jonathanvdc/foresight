package eqsat.hashCons

import eqsat.{DisjointSet, EClassRef, ENode, ImmutableEGraph}

/**
 * An e-graph that uses hash-consing to map e-nodes to e-classes.
 * @param unionFind The disjoint set data structure that maintains the union-find information of the e-classes.
 * @param hashCons The hash-consing map that maps e-nodes to e-classes.
 * @param classData The data of each e-class in the e-graph.
 * @param unionWorklist The worklist of e-classes that need to be unioned.
 * @tparam NodeT The type of the nodes described by the e-nodes in the e-graph.
 */
final case class HashConsEGraph[NodeT] private(unionFind: DisjointSet[EClassRef],
                                               hashCons: Map[ENode[NodeT], EClassRef],
                                               classData: Map[EClassRef, HashConsEClassData[NodeT]],
                                               unionWorklist: List[(EClassRef, EClassRef)]) extends ImmutableEGraph[NodeT] {

  // We guarantee the following invariants:
  //   1. All nodes in hashCons and classData are kept canonical with regard to the current state of unionFind.
  //   2. The e-nodes in the hash-cons map are always kept in sync with the e-nodes in classData. That is,
  //      hashCons(node) == ref if and only if classData(ref).nodes contains node.
  //   3. The parents set of an e-class in classData is always kept in sync with the e-class arguments of the e-nodes
  //      in the e-class. That is, classData(ref).parents contains parent if and only if there exists an e-node in
  //      classData(ref) such that parent is in the e-node's arguments.

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

  override def add(node: ENode[NodeT]): (EClassRef, HashConsEGraph[NodeT]) = {
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

        (ref, HashConsEGraph(unionFind, newHashCons, classDataWithUpdatedChildren, unionWorklist))
    }
  }

  override def union(left: EClassRef, right: EClassRef): HashConsEGraph[NodeT] = {
    val canonicalLeft = canonicalize(left)
    val canonicalRight = canonicalize(right)
    if (canonicalLeft == canonicalRight) {
      this
    } else {
      HashConsEGraph(unionFind, hashCons, classData, (canonicalLeft, canonicalRight) :: unionWorklist)
    }
  }

  override def requiresRebuild: Boolean = unionWorklist.nonEmpty

  override def rebuilt: HashConsEGraph[NodeT] = {
    var unionFind = this.unionFind
    var hashCons = this.hashCons
    var classData = this.classData
    var unionWorklist = this.unionWorklist

    // The nodes repair set contains all e-classes containing nodes that might refer to non-canonical e-classes.
    var nodesRepairWorklist = Set.empty[EClassRef]

    // The parents repair set contains all e-classes whose parents set might contain non-canonical e-classes.
    var parentsRepairWorklist = Set.empty[EClassRef]

    def unify(left: EClassRef, right: EClassRef): EClassRef = {
      val leftRoot = unionFind.find(left)
      val rightRoot = unionFind.find(right)
      if (leftRoot == rightRoot) {
        leftRoot
      } else {
        // Union the two classes and figure out which one is the dominant class.
        unionFind = unionFind.union(leftRoot, rightRoot)
        val domRoot = unionFind.find(leftRoot)
        val subRoot = if (domRoot == leftRoot) rightRoot else leftRoot

        // Merge the nodes and parents of the dominant and subordinate classes.
        val domData = classData(domRoot)
        val subData = classData(subRoot)
        val newNodes = domData.nodes ++ subData.nodes
        val newParents = domData.parents ++ subData.parents
        val newClassData = HashConsEClassData(newNodes, newParents)
        classData = (classData - subRoot) + (domRoot -> newClassData)

        // Update the hash-cons so that all nodes from the subordinate class now point to the dominant class. Make no
        // attempt at canonicalizing the nodes, as we will perform this operation in the rebuilding logic.
        hashCons = hashCons ++ subData.nodes.map(_ -> domRoot)

        // The merge we just performed may have broken the invariant that all EClassRefs in the e-graph are canonicalized.
        // Specifically, the subordinate class may still be referred to by other e-nodes, either in the form of a direct
        // reference, captured by the subordinate class' parents set, or in the form of a child-to-parent reference,
        // the inverse of which is captured by the e-class arguments of the nodes in the subordinate class.
        // To repair the invariant, we add the subordinate class' parents set to the repair worklist.
        nodesRepairWorklist = nodesRepairWorklist ++ subData.parents
        parentsRepairWorklist = parentsRepairWorklist ++ subData.nodes.flatMap(_.args)

        domRoot
      }
    }

    def repairNodes(ref: EClassRef): Unit = {
      // Repairing the nodes of an e-class consists of canonicalizing all nodes in the e-class.
      // Once canonicalized, there are three possibilities:
      //   1. The canonicalized node is exactly the same as the original node. In this case, we do nothing.
      //   2. The canonicalized node is different from the original node, but the canonicalized node is already in the
      //      hash-cons map. In this case, we union the original node with the canonicalized node.
      //   3. The canonicalized node is different from the original node, and the canonicalized node is not in the
      //      hash-cons map. In this case, we add the canonicalized node to the hash-cons and queue its arguments
      //      for parent set repair.
      var data = classData(ref)
      for (node <- data.nodes) {
        val canonicalNode = canonicalize(node)
        if (canonicalNode.args != node.args) {
          hashCons.get(canonicalNode) match {
            case Some(other) =>
              unionWorklist = (ref, other) :: unionWorklist

            case None =>
              data = HashConsEClassData(data.nodes - node + canonicalNode, data.parents)
              hashCons = hashCons + (canonicalNode -> ref)
              parentsRepairWorklist = parentsRepairWorklist ++ canonicalNode.args
          }
        }
      }

      classData = classData + (ref -> data)
    }

    def repairParents(ref: EClassRef): Unit = {
      // Repairing the parents of an e-class consists of canonicalizing all parents of the e-class.
      val data = classData(ref)
      val canonicalParents = data.parents.map(canonicalize)
      if (canonicalParents != data.parents) {
        classData = classData + (ref -> HashConsEClassData(data.nodes, canonicalParents))
      }
    }

    while (unionWorklist.nonEmpty || nodesRepairWorklist.nonEmpty || parentsRepairWorklist.nonEmpty) {
      // Process all unions in the worklist. The unify operation may add new e-classes to the repair worklists,
      // but will not add elements to its own union worklist.
      for ((left, right) <- unionWorklist) {
        unify(left, right)
      }
      unionWorklist = List.empty

      // Process the node repair worklist. The repairNodes operation may add new e-classes to the union worklist,
      // but will not add elements to its own repair worklist.
      for (ref <- nodesRepairWorklist.map(canonicalize)) {
        repairNodes(ref)
      }
      nodesRepairWorklist = Set.empty

      // Process the parents repair worklist. The repairParents operation will not add elements to any worklist.
      for (ref <- parentsRepairWorklist.map(canonicalize)) {
        repairParents(ref)
      }
      parentsRepairWorklist = Set.empty
    }

    HashConsEGraph(unionFind, hashCons, classData, List.empty)
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
  def empty[NodeT]: HashConsEGraph[NodeT] = HashConsEGraph(DisjointSet.empty, Map.empty, Map.empty, List.empty)
}
