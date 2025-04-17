package foresight.eqsat.hashCons

import foresight.eqsat.{AddNodeResult, EClassCall, EClassRef, ENode, ShapeCall, Slot, SlotMap}

private final class MutableHashConsEGraph[NodeT](private val unionFind: MutableSlottedUnionFind,
                                                 private var hashCons: Map[ENode[NodeT], EClassRef],
                                                 private var classData: Map[EClassRef, EClassData[NodeT]]) {

  def toImmutable: HashConsEGraph[NodeT] = {
    new HashConsEGraph(unionFind.set, hashCons, classData)
  }

  def slots(ref: EClassRef): Set[Slot] = {
    classData(ref).slots
  }

  def tryCanonicalize(ref: EClassRef): Option[EClassCall] = {
    unionFind.tryFindAndCompress(ref)
  }

  def canonicalize(ref: EClassRef): EClassCall = {
    unionFind.findAndCompress(ref)
  }

  def canonicalize(app: EClassCall): EClassCall = {
    canonicalize(app.ref).rename(app.args)
  }

  def canonicalize(node: ENode[NodeT]): ShapeCall[NodeT] = {
    import scala.math.Ordering.Implicits.seqDerivedOrdering

    val canonicalizedArgs = node.copy(args = node.args.map(canonicalize))
    groupCompatibleVariants(canonicalizedArgs).toSeq
      .map(_.asShapeCall)
      .minBy(_.shape.slots)
  }

  private def groupCompatibleVariants(node: ENode[NodeT]): Set[ENode[NodeT]] = {
    val groups = node.args.map({
      case EClassCall(ref, renaming) =>
        val data = classData(ref)
        data.permutations.allPerms.map(perm => EClassCall(ref, perm.compose(renaming)))
    })

    Helpers.cartesian(groups).map(args => node.copy(args = args)).toSet
  }

  def find(node: ENode[NodeT]): Option[EClassCall] = {
    findUnsafe(canonicalize(node))
  }

  /**
   * Finds the e-class of a given e-node. The e-node must be canonical.
   * @param renamedShape The canonicalized e-node to find the e-class of.
   * @return The e-class of the e-node, if it is defined in this e-graph; otherwise, None.
   */
  private def findUnsafe(renamedShape: ShapeCall[NodeT]): Option[EClassCall] = {
    if (MutableHashConsEGraph.debug) {
      assert(renamedShape.shape.isShape)
    }

    hashCons.get(renamedShape.shape).map { ref =>
      val data = classData(ref)
      val classToNode = data.nodes(renamedShape.shape)
      val out = classToNode.inverse.compose(renamedShape.renaming)
      EClassCall(ref, out)
    }
  }

  /**
   * Creates an empty e-class with the given slots. The e-class is added to the union-find and the class data map.
   * @param slots The slots of the e-class.
   * @return The reference to the new e-class.
   */
  private def createEmptyClass(slots: Set[Slot]): EClassRef = {
    val ref = new EClassRef()
    unionFind.add(ref, slots)

    val data = EClassData[NodeT](slots, Map.empty, PermutationGroup.identity(SlotMap.identity(slots)), Set.empty)
    classData = classData + (ref -> data)

    ref
  }

  /**
   * Adds a node to an e-class. The node is added to the hash cons, the class data, and the argument e-classes' users.
   * @param ref The reference to the e-class.
   * @param node The node to add.
   */
  private def addNodeToClass(ref: EClassRef, node: ShapeCall[NodeT]): Unit = {
    // Set the node in the hash cons, update the class data and add the node to the argument e-classes' users.
    val data = classData(ref)
    hashCons = hashCons + (node.shape -> ref)
    classData = classData + (ref -> data.copy(nodes = data.nodes + (node.shape -> node.renaming)))
    classData = classData ++ node.shape.args.map(_.ref).distinct.map(c => {
      val argData = classData(c)
      c -> argData.copy(users = argData.users + node.shape)
    })
  }

  /**
   * Removes a node from an e-class. The node is removed from the hash cons, the class data, and the argument e-classes'
   * users.
   * @param ref The reference to the e-class.
   * @param shape The node to remove.
   */
  private def removeNodeFromClass(ref: EClassRef, shape: ENode[NodeT]): Unit = {
    if (MutableHashConsEGraph.debug) {
      assert(shape.isShape)
    }

    // Remove the node from the hash cons, update the class data and remove the node from the argument e-classes' users.
    val data = classData(ref)
    hashCons = hashCons - shape
    classData = classData + (ref -> data.copy(nodes = data.nodes - shape))
    classData = classData ++ shape.args.map(_.ref).distinct.map(c => {
      val argData = classData(c)
      c -> argData.copy(users = argData.users - shape)
    })
  }

  /**
   * Adds a new node to the e-graph.
   * @param canonicalNode A pre-canonicalized node to add to the e-graph.
   * @return The e-class call of the added node.
   */
  private def addNewUnsafe(canonicalNode: ShapeCall[NodeT]): EClassCall = {
    // Generate slots for the e-class.
    val shape = canonicalNode.shape
    val nodeSlotsToClassSlots = SlotMap.bijectionFromSetToFresh(shape.slotSet)
    val slots = (shape.slotSet -- shape.definitions).map(nodeSlotsToClassSlots.apply)

    // Set up an empty e-class with the slots.
    val ref = createEmptyClass(slots)

    // Add the node to the empty e-class, populating it.
    val shapeCall = ShapeCall(shape, nodeSlotsToClassSlots)
    addNodeToClass(ref, shapeCall)

    // Propagate symmetries from the node to the e-class.
    propagatePermutations(ref, shapeCall)

    // Construct an e-class call with the node slots to e-class slots bijection.
    val publicRenaming = SlotMap(
      canonicalNode.renaming.iterator.filter(p => !shape.definitions.contains(p._1)).toMap)
    EClassCall(ref, nodeSlotsToClassSlots.inverse.composePartial(publicRenaming))
  }

  /**
   * Adds a new node to the e-graph. The node is added to the hash cons, the class data, and the argument e-classes'
   * users. If the e-node is already in the e-graph, the e-class reference of the existing e-node is returned.
   * Otherwise, the e-node is added to a unique e-class, whose reference is returned.
   *
   * @param canonicalNode A pre-canonicalized node to add to the e-graph.
   * @return The e-class reference of the e-node in the e-graph.
   */
  def tryAddUnsafe(canonicalNode: ShapeCall[NodeT]): AddNodeResult = {
    findUnsafe(canonicalNode) match {
      case Some(ref) => AddNodeResult.AlreadyThere(ref)
      case None =>
        val ref = addNewUnsafe(canonicalNode)
        AddNodeResult.Added(ref)
    }
  }

  /**
   * Infers e-class slot permutations from a shape call.
   * @param shape The shape call to infer permutations from.
   * @return A set of slot permutations for the shape call. These permutations may contain redundant slots.
   */
  private def inferPermutations(shape: ShapeCall[NodeT]): Set[SlotMap] = {
    groupCompatibleVariants(shape.asNode).flatMap(variant => {
      val variantShape = variant.asShapeCall
      if (shape.shape == variantShape.shape) {
        val permutation = shape.renaming.inverse.compose(variantShape.renaming)
        Some(permutation)
      } else {
        None
      }
    })
  }

  /**
   * Propagates slot permutations from an e-node to an e-class.
   * @param ref The e-class reference that contains the e-node.
   * @param shape The e-node from which permutations are propagated.
   * @return True if the e-class was modified; otherwise, false.
   */
  private def propagatePermutations(ref: EClassRef, shape: ShapeCall[NodeT]): Boolean = {
    // First, infer the permutations from the shape call.
    val inferred = inferPermutations(shape)
    if (inferred.isEmpty) {
      return false
    }

    // Drop redundant slots from the permutations.
    val data = classData(ref)
    val nonRedundantPermutations = inferred.map { p =>
      val nonRedundant = p.filterKeys(data.slots)
      assert(nonRedundant.isPermutation)
      nonRedundant
    }

    data.permutations.tryAddSet(nonRedundantPermutations) match {
      case Some(newPerms) =>
        classData = classData + (ref -> data.copy(permutations = newPerms))
        true

      case None =>
        false
    }
  }

  private def isWellFormed(call: EClassCall): Boolean = {
    val slots = unionFind.set.findAndCompress(call.ref)._1.args.valueSet
    slots.subsetOf(call.args.keySet)
  }

  def unionMany(pairs: Seq[(EClassCall, EClassCall)]): Set[Set[EClassCall]] = {
    val oldClassData = classData

    // The pairs of e-classes that were unified.
    var unifiedPairs = List.empty[(EClassCall, EClassCall)]

    // The nodes repair set contains all e-nodes that may no longer be canonical.
    // The invariant maintained throughout the unification algorithm is that the elements of the node repair set
    // are in the hash cons.
    var nodesRepairWorklist = Set.empty[ENode[NodeT]]

    def touchedClass(ref: EClassRef): Unit = {
      nodesRepairWorklist = nodesRepairWorklist ++ classData(ref).users
    }

    def shrinkSlots(ref: EClassRef, slots: Set[Slot]): Unit = {
      val data = classData(ref)

      // We first determine the set of slots that are redundant in the e-class. These are the slots that are not in the
      // new set of slots. Additionally, if a slot is redundant, all slots that are in the same orbit as the redundant
      // slot are also redundant. We remove both categories of redundant slots from the e-class.
      val redundantSlots = data.slots -- slots
      val inferredRedundantSlots = redundantSlots.flatMap(data.permutations.orbit)
      val finalSlots = slots -- inferredRedundantSlots

      // We now restrict the elements of the permutation group to the new slots.
      val generators = data.permutations.generators.map(g =>
        SlotMap.fromPairs(g.iterator.filter(p => finalSlots.contains(p._1)).toSeq))
      val identity = SlotMap.identity(finalSlots)

      // We update the e-class data with the new slots and permutations.
      val newData = data.copy(slots = finalSlots, permutations = PermutationGroup(identity, generators))
      classData = classData + (ref -> newData)

      // We update the union-find with the new slots. Since the e-class is a root in the union-find, its set of slots in
      // the AppliedRef can simply be set to an identity bijection of the new slots. unionFind.add will take care of
      // constructing that bijection.
      if (MutableHashConsEGraph.debug) {
        assert(finalSlots.subsetOf(data.slots))
      }
      unionFind.add(ref, finalSlots)

      // Reducing the slots of an e-class may decanonicalize the e-class' users. Add the potentially affected nodes to
      // the repair worklist.
      touchedClass(ref)
    }

    def shrinkAppliedSlots(ref: EClassCall, slots: Set[Slot]): Unit = {
      val appSlotsToClassSlots = ref.args.inverse
      shrinkSlots(ref.ref, slots.map(appSlotsToClassSlots.apply))
    }

    def renamePermutation(permutation: SlotMap, renaming: SlotMap): SlotMap = {
      SlotMap(permutation.iterator.collect {
        case (k, v) if renaming.contains(k) && renaming.contains(v) => (renaming(k), renaming(v))
      }.toMap)
    }

    def mergeInto(subRoot: EClassCall, domRoot: EClassCall): Unit = {
      // Construct a mapping of the slots of the dominant e-class to the slots of the subordinate e-class.
      val map = domRoot.args.compose(subRoot.args.inverse)
      if (MutableHashConsEGraph.debug) {
        assert(map.keySet == slots(domRoot.ref))
        assert(map.valueSet == slots(subRoot.ref))
      }

      // Update the union-find and record the union in unifiedPairs.
      unionFind.update(subRoot.ref, EClassCall(domRoot.ref, map))
      unifiedPairs = (subRoot, domRoot) :: unifiedPairs

      // Translate the subordinate class' nodes to the dominant class' slots. We use composeFresh to cover potential
      // redundant slots in the subordinate class' nodes.
      val invMap = map.inverse
      val subData = classData(subRoot.ref)
      for ((node, bijection) <- subData.nodes) {
        removeNodeFromClass(subRoot.ref, node)
        addNodeToClass(domRoot.ref, ShapeCall(node, bijection.composeFresh(invMap)))
        nodesRepairWorklist = nodesRepairWorklist + node
      }

      // Merge permutations of subordinate class into dominant class.
      val subToDom = subRoot.args.compose(domRoot.args.inverse)
      val subPermutations = subData.permutations.generators.map(renamePermutation(_, subToDom))

      val domData = classData(domRoot.ref)
      domData.permutations.tryAddSet(subPermutations) match {
        case Some(newPermutations) =>
          // If the new permutations are not already in the set of permutations, we update the e-class data.
          classData = classData + (domRoot.ref -> domData.copy(permutations = newPermutations))
          touchedClass(domRoot.ref)

        case None =>
      }

      if (MutableHashConsEGraph.debug) {
        // Check that all nodes have been removed from the subordinate class.
        assert(classData(subRoot.ref).nodes.isEmpty)
      }

      // Queue all users of the subordinate class for repair.
      touchedClass(subRoot.ref)
    }

    def unify(left: EClassCall, right: EClassCall): Unit = {
      val leftRoot = unionFind.findAndCompress(left)
      val rightRoot = unionFind.findAndCompress(right)
      if (leftRoot != rightRoot) {
        unifyRoots(leftRoot, rightRoot)
      }
    }

    def unifyRoots(leftRoot: EClassCall, rightRoot: EClassCall): Unit = {
      // We first determine the set of slots that are common to both e-classes. If this set is smaller than either
      // of the e-classes' slot sets, we shrink the e-classes' slots to the common set. This operation will update
      // the union-find, so we recurse in case of shrinkage.
      val slots = leftRoot.slotSet intersect rightRoot.slotSet
      if (slots != leftRoot.slotSet) {
        shrinkAppliedSlots(leftRoot, slots)
        unify(leftRoot, rightRoot)
      } else if (slots != rightRoot.slotSet) {
        shrinkAppliedSlots(rightRoot, slots)
        unify(leftRoot, rightRoot)
      } else if (leftRoot.ref == rightRoot.ref) {
        // If the two e-classes are the same but their arguments are different, we update the permutation groups.
        val ref = leftRoot.ref
        val data = classData(ref)

        // We first construct the new permutation and make sure it is not already in the set of permutations.
        val perm = leftRoot.args.compose(rightRoot.args.inverse)
        val group = data.permutations
        if (group.contains(perm)) {
          return
        }

        // We add the new permutation to the e-class data.
        val newData = data.copy(permutations = group.add(perm))
        classData = classData + (ref -> newData)

        // We add the e-class's nodes and users to the repair worklist, as the e-class now has a new permutation.
        touchedClass(ref)
      } else if (classData(leftRoot.ref).nodes.size > classData(rightRoot.ref).nodes.size) {
        mergeInto(rightRoot, leftRoot)
      } else {
        mergeInto(leftRoot, rightRoot)
      }
    }

    def repairNode(node: ENode[NodeT]): Unit = {
      // The first step to repairing a hashcons entry is to canonicalize the node.
      // Once canonicalized, there are three possibilities:
      //   1. The canonicalized node is exactly the same as the original node. In this case, we do nothing.
      //   2. The canonicalized node is different from the original node, but the canonicalized node is already in the
      //      hash-cons map. In this case, we union the original node with the canonicalized node.
      //   3. The canonicalized node is different from the original node, and the canonicalized node is not in the
      //      hash-cons map. In this case, we add the canonicalized node to the hash-cons and queue its arguments
      //      for parent set repair.
      val ref = hashCons(node)
      val data = classData(ref)
      val canonicalNode = canonicalize(node)

      // oldRenaming : old node slot -> old e-class slot.
      val oldRenaming = data.nodes(node)

      // newRenaming : canonical node slot -> old e-class slot,
      // is obtained by composing canonicalNode.renaming (canonical node slot -> old node slot) with oldRenaming.
      // We use composePartial here because the canonical node may have fewer slots than the original node due to
      // slot shrinking in one of the canonical node's argument e-classes. We'll check that the old renaming's
      // keys is a superset of the values of the canonical node renaming.
      if (MutableHashConsEGraph.debug) {
        assert(canonicalNode.renaming.valueSet.subsetOf(oldRenaming.keySet))
      }
      val newRenaming = canonicalNode.renaming.composePartial(oldRenaming)

      // Shrink e-class slots if the canonical node has fewer slots
      if (!data.slots.subsetOf(newRenaming.valueSet)) {
        shrinkSlots(ref, data.slots.intersect(newRenaming.valueSet))
        repairNode(node)
        return
      }

      if (canonicalNode.shape != node) {
        hashCons.get(canonicalNode.shape) match {
          case Some(other) =>
            // canonicalNodeRenaming : canonical node slot -> canonical e-class slot.
            val canonicalNodeRenaming = classData(other).nodes(canonicalNode.shape)

            // Union the original node with the canonicalized node in the class data. Remove the old node from the
            // hash-cons.
            removeNodeFromClass(ref, node)

            // Construct two calls that we append to the union worklist. The first is a call to the original old e-class
            // and takes an oldRenaming.inverse : old e-class slot -> old node slot.
            // The second is a call to the canonicalized e-class and takes an argument map
            // canonicalCallArgs : canonical e-class slot -> old node slot.
            val canonicalCallArgs = canonicalNodeRenaming.inverse.compose(canonicalNode.renaming)
            val leftCall = EClassCall(ref, oldRenaming.inverse)
            val rightCall = EClassCall(other, canonicalCallArgs)
            if (MutableHashConsEGraph.debug) {
              assert(isWellFormed(leftCall), "Left call is not well-formed.")
              assert(isWellFormed(rightCall), "Right call is not well-formed.")
            }
            unify(leftCall, rightCall)
            return

          case None =>
            // Eliminate the old node from the e-class and add the canonicalized node.
            removeNodeFromClass(ref, node)
            addNodeToClass(ref, ShapeCall(canonicalNode.shape, newRenaming))
        }
      } else if (oldRenaming != newRenaming) {
        removeNodeFromClass(ref, node)
        addNodeToClass(ref, ShapeCall(canonicalNode.shape, newRenaming))
      }

      // Infer symmetries from the canonicalized node.
      if (propagatePermutations(ref, canonicalNode.shape.rename(newRenaming).asShapeCall)) {
        touchedClass(ref)
      }
    }

    // Unify the input pairs.
    for ((left, right) <- pairs) {
      unify(left, right)
    }

    // Repair all nodes in the repair worklist.
    while (nodesRepairWorklist.nonEmpty) {
      val node = nodesRepairWorklist.head
      nodesRepairWorklist = nodesRepairWorklist - node
      repairNode(node)
    }

    // Unlink all emptied e-classes from the class data.
    classData = classData.filterNot(_._2.nodes.isEmpty)

    val touched = unifiedPairs.flatMap(p => Seq(p._1, p._2)).map(_.ref).toSet
    touched.map(c => (canonicalize(c), c)).groupBy(_._1.ref).values.map(_.map {
      case (canonical, original) =>
        EClassCall(original, SlotMap.identity(oldClassData(original).slots).composeFresh(canonical.args.inverse))
    }).toSet
  }
}

private object MutableHashConsEGraph {
  /**
   * Tells if debug mode is enabled.
   */
  final val debug = false
}