package foresight.eqsat.hashCons

import foresight.eqsat.{AddNodeResult, EClassCall, EClassRef, ENode, ShapeCall, Slot}
import foresight.eqsat.collections.{SlotMap, SlotSet}
import foresight.eqsat.mutable.EGraph
import foresight.eqsat.parallel.ParallelMap
import foresight.util.Debug

import scala.collection.mutable.HashMap

/**
 * A mutable hash-consed e-graph. This class provides methods for adding nodes, unifying e-classes, and maintaining
 * the integrity of the e-graph. It uses a mutable union-find data structure to manage e-class representatives and
 * supports slot management and permutation propagation.
 *
 * The concrete implementations of this class must provide the underlying union-find structure and methods for
 * managing e-class data, including creating empty e-classes, adding and removing nodes from e-classes, and unlinking
 * empty e-classes.
 *
 * @tparam NodeT The type of the nodes in the e-graph.
 */
private[hashCons] abstract class AbstractMutableHashConsEGraph[NodeT]
  extends ReadOnlyHashConsEGraph[NodeT] with EGraph[NodeT] {

  /**
   * The underlying mutable union-find data structure used to manage e-class representatives.
   */
  protected val unionFind: AbstractMutableSlottedUnionFind

  /**
   * Updates the slots and permutations of an e-class.
   * @param ref The e-class reference.
   * @param slots The new slots for the e-class.
   * @param permutations The new permutations for the e-class.
   */
  protected def updateClassSlotsAndPermutations(ref: EClassRef, slots: SlotSet, permutations: PermutationGroup[SlotMap]): Unit

  /**
   * Updates the permutations of an e-class.
   * @param ref The e-class reference.
   * @param permutations The new permutations for the e-class.
   */
  protected def updateClassPermutations(ref: EClassRef, permutations: PermutationGroup[SlotMap]): Unit

  /**
   * Creates an empty e-class with the given slots. The e-class is added to the union-find and the class data map.
   * @param slots The slots of the e-class.
   * @return The reference to the new e-class.
   */
  protected def createEmptyClass(slots: SlotSet): EClassRef

  /**
   * Adds a node to an e-class. The node is added to the hash cons, the class data, and the argument e-classes' users.
   * @param ref The reference to the e-class.
   * @param node The node to add.
   */
  protected def addNodeToClass(ref: EClassRef, node: ShapeCall[NodeT]): Unit

  /**
   * Removes a node from an e-class. The node is removed from the hash cons, the class data, and the argument e-classes'
   * users.
   * @param ref The reference to the e-class.
   * @param shape The node to remove.
   */
  protected def removeNodeFromClass(ref: EClassRef, shape: ENode[NodeT]): Unit

  /**
   * Unlinks all empty e-classes from the class data map. An e-class is considered empty if it has no nodes.
   */
  protected def unlinkEmptyClasses(): Unit

  override def tryAddMany(nodes: Seq[ENode[NodeT]],
                          parallelize: ParallelMap): Seq[AddNodeResult] = {
    // Adding independent e-nodes is fundamentally a sequential operation, but the most expensive part of adding nodes
    // is canonicalizing them and looking them up in the e-graph. Canonicalization can be parallelized since adding a
    // node will never change the canonical form of other nodes - only union operations can do that.
    //
    // Node lookups are partially parallelizable, but this is not worth the overhead of separating them into groups
    // of nodes that can safely be looked up in parallel. Instead, we just parallelize the canonicalization step and
    // then perform the lookups and additions sequentially.

    val p = parallelize.child("add nodes")

    val canonicalized = p(nodes, canonicalize)
    val results = p.run {
      canonicalized.map { node =>
        tryAddUnsafe(node)
      }
    }
    results.toSeq
  }

  override def unionMany(pairs: Seq[(EClassCall, EClassCall)], parallelize: ParallelMap): Set[Set[EClassCall]] = {
    if (pairs.isEmpty) {
      Set.empty
    } else {
      require(
        pairs.forall { case (first, second) => first.isWellFormed(this) && second.isWellFormed(this) },
        "All e-class applications must be well-formed.")

      val unionParallelize = parallelize.child("union")
      unionParallelize.run {
        unionManyImpl(pairs)
      }
    }
  }

  /**
   * Query the hash cons for the given node. Returns null if the node is not in the hash cons.
   *
   * @param node The node to query.
   * @return The e-class reference of the node, or null if the node is not in the hash cons.
   */
  private def nodeToClassOrNull(node: ENode[NodeT]): EClassRef = {
    nodeToRefOrElse(node, null)
  }

  private def slots(ref: EClassRef): SlotSet = dataForClass(ref).slots

  /**
   * Adds a new node to the e-graph. Assumes that the node is not already in the e-graph.
   * The node is added to a new unique e-class, whose reference is returned.
   * @param canonicalNode A pre-canonicalized node to add to the e-graph.
   * @return The e-class call of the added node.
   */
  private def addNewUnsafe(canonicalNode: ShapeCall[NodeT]): EClassCall = {
    val shape = canonicalNode.shape

    // Fast path for nodes without slots.
    if (!shape.hasSlots) {
      val newRef = createEmptyClass(SlotSet.empty)
      addNodeToClass(newRef, canonicalNode)
      return newRef.callWithoutSlots
    }

    // Generate slots for the e-class.
    val nodeSlotsToClassSlots = SlotMap.bijectionFromSetToFresh(shape.slotSet)
    val slots = shape.slotSet.diff(SlotSet.from(shape.definitions)).map(nodeSlotsToClassSlots.apply(_))

    // Set up an empty e-class with the slots.
    val ref = createEmptyClass(slots)

    // Add the node to the empty e-class, populating it.
    val shapeCall = ShapeCall(shape, nodeSlotsToClassSlots)
    addNodeToClass(ref, shapeCall)

    // Propagate symmetries from the node to the e-class.
    propagatePermutations(ref, shapeCall)

    // Construct an e-class call with the node slots to e-class slots bijection.
    val publicRenaming = canonicalNode.renaming.filterKeys(!shape.definitions.contains(_))
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
    val resultOrNull = findUnsafe(canonicalNode)
    if (resultOrNull == null) {
      val ref = addNewUnsafe(canonicalNode)
      AddNodeResult.Added(ref)
    } else {
      AddNodeResult.AlreadyThere(resultOrNull)
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
   * Propagates slot permutations to an e-class.
   * @param ref The e-class reference to propagate permutations to.
   * @param permutations The set of slot permutations to propagate.
   * @return True if the e-class was modified; otherwise, false.
   */
  private def propagatePermutations(ref: EClassRef, permutations: Set[SlotMap]): Boolean = {
    if (permutations.isEmpty) {
      return false
    }

    // Drop redundant slots from the permutations.
    val data = dataForClass(ref)
    val nonRedundantPermutations = permutations.map { p =>
      val nonRedundant = p.filterKeys(data.slots)
      if (Debug.isEnabled) assert(nonRedundant.isPermutation)
      nonRedundant
    }

    data.permutations.tryAddSet(nonRedundantPermutations) match {
      case Some(newPerms) =>
        updateClassPermutations(ref, newPerms)
        true

      case None =>
        false
    }
  }

  /**
   * Propagates slot permutations from an e-node to an e-class.
   * @param ref The e-class reference that contains the e-node.
   * @param shape The e-node from which permutations are propagated.
   * @return True if the e-class was modified; otherwise, false.
   */
  private def propagatePermutations(ref: EClassRef, shape: ShapeCall[NodeT]): Boolean = {
    propagatePermutations(ref, inferPermutations(shape))
  }

  private def isWellFormed(call: EClassCall): Boolean = {
    val slots = unionFind.find(call.ref).args.valueSet
    slots.subsetOf(call.args.keySet)
  }

  private def unionManyImpl(pairs: Seq[(EClassCall, EClassCall)]): Set[Set[EClassCall]] = {
    // The pairs of e-classes that were unified.
    var unifiedPairs = List.empty[(EClassCall, EClassCall)]

    // A map from e-class references to their old slots before unification. This is used to construct the return value.
    val oldSlots = HashMap.empty[EClassRef, SlotSet]

    // The nodes repair set contains all e-nodes that may no longer be canonical.
    // The invariant maintained throughout the unification algorithm is that the elements of the node repair set
    // are in the hash cons.
    var nodesRepairWorklist = Set.empty[ENode[NodeT]]

    def touchedClass(ref: EClassRef): Unit = {
      nodesRepairWorklist = nodesRepairWorklist ++ dataForClass(ref).users
    }

    def shrinkSlots(ref: EClassRef, slots: SlotSet): Unit = {
      val data = dataForClass(ref)

      // We first determine the set of slots that are redundant in the e-class. These are the slots that are not in the
      // new set of slots. Additionally, if a slot is redundant, all slots that are in the same orbit as the redundant
      // slot are also redundant. We remove both categories of redundant slots from the e-class.
      val redundantSlots = data.slots -- slots
      val inferredRedundantSlots = redundantSlots.flatMap(data.permutations.orbit)
      val finalSlots = slots.diff(inferredRedundantSlots)

      // We now restrict the elements of the permutation group to the new slots.
      val generators = data.permutations.generators.map(g =>
        SlotMap.fromPairs(g.iterator.filter(p => finalSlots.contains(p._1)).toSeq))
      val identity = SlotMap.identity(finalSlots)

      // We update the e-class data with the new slots and permutations.
      updateClassSlotsAndPermutations(ref, finalSlots, PermutationGroup(identity, generators))

      // We update the union-find with the new slots. Since the e-class is a root in the union-find, its set of slots in
      // the AppliedRef can simply be set to an identity bijection of the new slots. unionFind.add will take care of
      // constructing that bijection.
      if (Debug.isEnabled) {
        assert(finalSlots.subsetOf(data.slots))
      }
      unionFind.add(ref, finalSlots)

      // Reducing the slots of an e-class may decanonicalize the e-class' users. Add the potentially affected nodes to
      // the repair worklist.
      touchedClass(ref)
    }

    def shrinkAppliedSlots(ref: EClassCall, slots: SlotSet): Unit = {
      val appSlotsToClassSlots = ref.args.inverse
      shrinkSlots(ref.ref, slots.map(appSlotsToClassSlots.apply(_)))
    }

    def mergeInto(subRoot: EClassCall, domRoot: EClassCall): Unit = {
      // Construct a mapping of the slots of the dominant e-class to the slots of the subordinate e-class.
      val map = domRoot.args.compose(subRoot.args.inverse)
      if (Debug.isEnabled) {
        assert(map.keySet == slots(domRoot.ref))
        assert(map.valueSet == slots(subRoot.ref))
      }

      // Update the union-find and record the union in unifiedPairs.
      unionFind.update(subRoot.ref, EClassCall(domRoot.ref, map))
      oldSlots(subRoot.ref) = slots(subRoot.ref)
      oldSlots(domRoot.ref) = slots(domRoot.ref)
      unifiedPairs = (subRoot, domRoot) :: unifiedPairs

      // Translate the subordinate class' nodes to the dominant class' slots. We use composeFresh to cover potential
      // redundant slots in the subordinate class' nodes.
      val invMap = map.inverse
      val subData = dataForClass(subRoot.ref)
      for ((node, bijection) <- subData.nodes) {
        removeNodeFromClass(subRoot.ref, node)
        addNodeToClass(domRoot.ref, ShapeCall(node, bijection.composeFresh(invMap)))
        nodesRepairWorklist = nodesRepairWorklist + node
      }

      // Merge permutations of subordinate class into dominant class.
      val subToDom = subRoot.args.compose(domRoot.args.inverse)
      val subPermutations = subData.permutations.generators.map(_.rename(subToDom))

      val domData = dataForClass(domRoot.ref)
      domData.permutations.tryAddSet(subPermutations) match {
        case Some(newPermutations) =>
          // If the new permutations are not already in the set of permutations, we update the e-class data.
          updateClassPermutations(domRoot.ref, newPermutations)
          touchedClass(domRoot.ref)

        case None =>
      }

      if (Debug.isEnabled) {
        // Check that all nodes have been removed from the subordinate class.
        assert(dataForClass(subRoot.ref).nodes.isEmpty)
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
      val slots = leftRoot.slotSet.intersect(rightRoot.slotSet)
      if (slots != leftRoot.slotSet) {
        shrinkAppliedSlots(leftRoot, slots)
        unify(leftRoot, rightRoot)
      } else if (slots != rightRoot.slotSet) {
        shrinkAppliedSlots(rightRoot, slots)
        unify(leftRoot, rightRoot)
      } else if (leftRoot.ref == rightRoot.ref) {
        // If the two e-classes are the same but their arguments are different, we update the permutation groups.
        val ref = leftRoot.ref
        val data = dataForClass(ref)

        // We first construct the new permutation and make sure it is not already in the set of permutations.
        val perm = leftRoot.args.compose(rightRoot.args.inverse)
        val group = data.permutations
        if (group.contains(perm)) {
          return
        }

        // We add the new permutation to the e-class data.
        updateClassPermutations(ref, group.add(perm))

        // We add the e-class's nodes and users to the repair worklist, as the e-class now has a new permutation.
        touchedClass(ref)
      } else if (dataForClass(leftRoot.ref).nodes.size > dataForClass(rightRoot.ref).nodes.size) {
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
      val ref = nodeToClassOrNull(node)
      if (Debug.isEnabled) {
        assert(ref != null, "The node to repair must be in the hash-cons.")
      }

      val data = dataForClass(ref)
      val canonicalNode = canonicalize(node)

      // oldRenaming : old node slot -> old e-class slot.
      val oldRenaming = data.nodes(node)

      // newRenaming : canonical node slot -> old e-class slot,
      // is obtained by composing canonicalNode.renaming (canonical node slot -> old node slot) with oldRenaming.
      // We use composePartial here because the canonical node may have fewer slots than the original node due to
      // slot shrinking in one of the canonical node's argument e-classes. We'll check that the old renaming's
      // keys is a superset of the values of the canonical node renaming.
      if (Debug.isEnabled) {
        assert(canonicalNode.renaming.valueSet.subsetOf(oldRenaming.keySet))
      }
      val newRenaming = canonicalNode.renaming.composePartial(oldRenaming)

      // Infer permutations from the canonicalized node.
      val canonicalShapeCall = ShapeCall(canonicalNode.shape, newRenaming)
      val inferredPermutations = inferPermutations(canonicalShapeCall)

      // Infer redundant slots from permutations. The idea is that if an e-class slot finds itself in the orbit of a
      // redundant slot in any permutation, the slot is redundant.
      val redundantFromPermutations = inferredPermutations.flatMap { permutation =>
        val group = PermutationGroup(SlotMap.identity(permutation.keySet), Set(permutation))
        permutation.keySet.filterNot(data.slots).flatMap(group.orbit).filter(data.slots)
      }

      // Shrink e-class slots if the canonical node has fewer slots
      if (!data.slots.subsetOf(newRenaming.valueSet) || redundantFromPermutations.nonEmpty) {
        shrinkSlots(ref, data.slots.intersect(newRenaming.valueSet).diff(redundantFromPermutations))
        repairNode(node)
        return
      }

      if (canonicalNode.shape != node) {
        nodeToClassOrNull(canonicalNode.shape) match {
          case null =>
            // Eliminate the old node from the e-class and add the canonicalized node.
            removeNodeFromClass(ref, node)
            addNodeToClass(ref, canonicalShapeCall)

          case other =>
            // canonicalNodeRenaming : canonical node slot -> canonical e-class slot.
            val canonicalNodeRenaming = dataForClass(other).nodes(canonicalNode.shape)

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
            if (Debug.isEnabled) {
              assert(isWellFormed(leftCall), "Left call is not well-formed.")
              assert(isWellFormed(rightCall), "Right call is not well-formed.")
            }
            unify(leftCall, rightCall)
            return
        }
      } else if (oldRenaming != newRenaming) {
        removeNodeFromClass(ref, node)
        addNodeToClass(ref, canonicalShapeCall)
      }

      // Infer symmetries from the canonicalized node.
      if (propagatePermutations(ref, inferredPermutations)) {
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
    unlinkEmptyClasses()

    // Check that all nodes in the class data map are canonicalized.
    if (Debug.isEnabled) {
      assert(classes.forall(k => canonicalize(k).ref == k), "All e-class references must be canonicalized.")
      assert(nodeCount >= classCount,
        "The number of nodes in the class data must be greater than or equal to the number of e-classes.")
    }

    val touched = unifiedPairs.flatMap(p => Seq(p._1, p._2)).map(_.ref).toSet
    touched.map(c => (canonicalize(c), c)).groupBy(_._1.ref).values.map(_.map {
      case (canonical, original) =>
        EClassCall(original, SlotMap.identity(oldSlots(original)).composeFresh(canonical.args.inverse))
    }).toSet
  }
}
