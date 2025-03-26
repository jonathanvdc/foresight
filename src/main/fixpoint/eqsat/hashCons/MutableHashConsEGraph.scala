package fixpoint.eqsat.hashCons

import fixpoint.eqsat.{EClassCall, EClassRef, ENode, ShapeCall, Slot, SlotMap}

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
    findImpl(canonicalize(node))
  }

  private def findImpl(renamedShape: ShapeCall[NodeT]): Option[EClassCall] = {
    assert(renamedShape.shape.isShape)

    hashCons.get(renamedShape.shape).map { ref =>
      val data = classData(ref)
      val classToNode = data.nodes(renamedShape.shape)
      val out = classToNode.inverse.compose(renamedShape.renaming)
      EClassCall(ref, out)
    }
  }

  def add(node: ENode[NodeT]): EClassCall = {
    val canonicalNode = canonicalize(node)
    findImpl(canonicalNode) match {
      case Some(ref) => ref
      case None =>
        // Generate a new e-class reference for a brand new e-class.
        val ref = new EClassRef()

        // Generate slots for the e-class and use them to construct the e-class's data.
        val shape = canonicalNode.shape
        val nodeSlotsToClassSlots = SlotMap.bijectionFromSetToFresh(shape.slots.toSet)
        val slots = (shape.slots.toSet -- shape.definitions).map(nodeSlotsToClassSlots.apply)
        val newClassData = EClassData(
          slots,
          Map(shape -> nodeSlotsToClassSlots),
          PermutationGroup.identity(SlotMap.identity(slots)),
          Set.empty[ENode[NodeT]])

        // Add the new class to the union-find.
        unionFind.add(ref, slots)

        // Update the hash-cons and class data map.
        hashCons = hashCons + (shape -> ref)
        classData = classData + (ref -> newClassData)

        // Add the new node to the argument e-classes's users.
        classData = classData ++ canonicalNode.args.map(_.ref).distinct.map(c =>
          c -> classData(c).copy(users = classData(c).users + shape))

        val publicRenaming = SlotMap(
          canonicalNode.renaming.iterator.filter(p => !shape.definitions.contains(p._1)).toMap)
        EClassCall(ref, nodeSlotsToClassSlots.inverse.composePartial(publicRenaming))
    }
  }

  def unionMany(pairs: Seq[(EClassCall, EClassCall)]): Set[Set[EClassCall]] = {
    val oldClassData = classData

    var unionWorklist = pairs.toList

    // The pairs of e-classes that were unified.
    var unifiedPairs = List.empty[(EClassCall, EClassCall)]

    // The nodes repair set contains all e-classes containing nodes that might refer to non-canonical e-classes.
    var nodesRepairWorklist = Set.empty[ENode[NodeT]]

    // The parents repair set contains all e-classes whose parents set might contain non-canonical e-classes.
    var usersRepairWorklist = Set.empty[EClassRef]

    def touchedNodes(data: EClassData[NodeT]): Unit = {
      nodesRepairWorklist = nodesRepairWorklist ++ data.nodes.keys ++ data.users
    }

    def touchedUsers(data: EClassData[NodeT]): Unit = {
      usersRepairWorklist = usersRepairWorklist ++ data.nodes.flatMap(_._1.args.map(_.ref))
    }

    def shrinkSlots(ref: EClassRef, slots: Set[Slot]): Unit = {
      val data = classData(ref)

      // We first determine the set of slots that are redundant in the e-class. These are the slots that are not in the
      // new set of slots. Additionally, if a slot is redundant, all slots that are in the same orbit as the redundant
      // slot are also redundant. We remove both categories of redundant slots from the e-class.
      // TODO: test removing slots that are inferred to be redundant due to them being in the orbit of a redundant slot.
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
      unionFind.add(ref, finalSlots)

      // We add the e-class's nodes and users to the repair worklist, as they now include redundant slots.
      touchedNodes(newData)
    }

    def shrinkAppliedSlots(ref: EClassCall, slots: Set[Slot]): Unit = {
      val appSlotsToClassSlots = ref.args.inverse
      shrinkSlots(ref.ref, slots.map(appSlotsToClassSlots.apply))
    }

    def mergeInto(subRoot: EClassCall, domRoot: EClassCall): Unit = {
      // Construct a mapping of the slots of the dominant e-class to the slots of the subordinate e-class.
      val map = domRoot.args.compose(subRoot.args.inverse)
      assert(map.keys == slots(domRoot.ref))
      assert(map.values == slots(subRoot.ref))

      // Update the union-find and record the union in unifiedPairs.
      unionFind.update(subRoot.ref, EClassCall(domRoot.ref, map))
      unifiedPairs = (subRoot, domRoot) :: unifiedPairs

      // Merge the nodes and parents of the dominant and subordinate classes.
      val domData = classData(domRoot.ref)
      val subData = classData(subRoot.ref)

      // Translate the subordinate class' nodes to the dominant class' slots. We use composeFresh to cover potential
      // redundant slots in the subordinate class' nodes.
      val invMap = map.inverse
      val newNodes = domData.nodes ++ subData.nodes.mapValues(_.composeFresh(invMap))

      // Merge the parents of the dominant and subordinate classes.
      val newParents = domData.users ++ subData.users

      // Merge permutations of subordinate class into dominant class.
      val subToDom = subRoot.args.compose(domRoot.args.inverse)
      val subPermutations = subData.permutations.generators.map(slotMap =>
        SlotMap(slotMap.iterator.collect {
          case (k, v) if subToDom.contains(k) && subToDom.contains(v) => (subToDom(k), subToDom(v))
        }.toMap))

      val newPermutations = domData.permutations.tryAddSet(subPermutations) match {
        case Some(newPerms) =>
          // If the new permutations are not already in the set of permutations, we update the e-class data.
          // We add the e-class's nodes and users to the repair worklist, as the e-class now has new permutations.
          touchedNodes(domData)
          newPerms

        case None => domData.permutations
      }

      // Construct the new e-class data and update the class data map.
      val newClassData = EClassData(domData.slots, newNodes, newPermutations, newParents)
      classData = (classData - subRoot.ref) + (domRoot.ref -> newClassData)

      // Update the hash-cons so that all nodes from the subordinate class now point to the dominant class. Make no
      // attempt at canonicalizing the nodes, as we will perform this operation in the rebuilding logic.
      hashCons = hashCons ++ subData.nodes.keys.map(_ -> domRoot.ref)

      // The merge we just performed may have broken the invariant that all EClassRefs in the e-graph are canonicalized.
      // Specifically, the subordinate class may still be referred to by other e-nodes, either in the form of a direct
      // reference, captured by the subordinate class' parents set, or in the form of a child-to-parent reference,
      // the inverse of which is captured by the e-class arguments of the nodes in the subordinate class.
      // To repair the invariant, we add the subordinate class' parents set to the repair worklist.
      touchedNodes(subData)
      touchedUsers(subData)
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
      val slots = leftRoot.slots intersect rightRoot.slots
      if (slots != leftRoot.slots) {
        shrinkAppliedSlots(leftRoot, slots)
        unify(leftRoot, rightRoot)
      } else if (slots != rightRoot.slots) {
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
        touchedNodes(newData)
      } else if (classData(leftRoot.ref).nodes.size < classData(rightRoot.ref).nodes.size) {
        mergeInto(rightRoot, leftRoot)
      } else {
        mergeInto(leftRoot, rightRoot)
      }
    }

    def inferSelfSymmetries(ref: EClassRef, node: ENode[NodeT]): Unit = {
      val shape = node.asShapeCall
      groupCompatibleVariants(node).foreach(variant => {
        val variantShape = variant.asShapeCall
        if (shape.shape == variantShape.shape) {
          val permutation = shape.renaming.compose(variantShape.renaming.inverse)

          val data = classData(ref)
          data.permutations.tryAdd(permutation) match {
            case Some(newPerms) =>
              classData = classData + (ref -> data.copy(permutations = newPerms))
              touchedNodes(data)

            case None =>
          }
        }
      })
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
      val canonicalNode = canonicalize(node)
      if (canonicalNode.shape != node) {
        val data = classData(ref)

        // oldRenaming : old node slot -> old e-class slot.
        val oldRenaming = data.nodes(node)

        // newRenaming : canonical node slot -> old e-class slot,
        // is obtained by composing canonicalNode.renaming (canonical node slot -> old node slot) with oldRenaming.
        val newRenaming = canonicalNode.renaming.compose(oldRenaming)

        hashCons.get(canonicalNode.shape) match {
          case Some(other) =>
            // canonicalNodeRenaming : canonical node slot -> canonical e-class slot.
            val canonicalNodeRenaming = classData(other).nodes(canonicalNode.shape)

            // Union the original node with the canonicalized node in the class data. Remove the old node from the
            // hash-cons.
            classData = classData + (ref -> data.copy(nodes = data.nodes - node))
            hashCons = hashCons - node

            // Construct two calls that we append to the union worklist. The first is a call to the original old e-class
            // and takes an oldRenaming.inverse : old e-class slot -> old node slot.
            // The second is a call to the canonicalized e-class and takes an argument map
            // canonicalCallArgs : canonical e-class slot -> old node slot.
            val canonicalCallArgs = canonicalNodeRenaming.inverse.compose(canonicalNode.renaming)
            unionWorklist = (EClassCall(ref, oldRenaming.inverse), EClassCall(other, canonicalCallArgs)) :: unionWorklist

          case None =>
            // Shrink e-class slots if the canonical node has fewer slots
            if (!data.slots.subsetOf(newRenaming.keys)) {
              shrinkSlots(ref, data.slots.intersect(newRenaming.keys))
              repairNode(node)
              return
            }

            // Update the e-class data and hash-cons map.
            classData = classData + (ref -> data.copy(nodes = data.nodes - node + (canonicalNode.shape -> newRenaming)))
            hashCons = hashCons - node + (canonicalNode.shape -> ref)
            touchedUsers(data)

            // Infer symmetries from the canonicalized node.
            inferSelfSymmetries(ref, canonicalNode.rename(newRenaming).asNode)
        }
      }
    }

    def repairUsers(ref: EClassRef): Unit = {
      // Repairing the users of an e-class consists of canonicalizing all users of the e-class.
      val data = classData(ref)
      val canonicalParents = data.users.map(canonicalize).map(_.shape)
      if (canonicalParents != data.users) {
        classData = classData + (ref -> EClassData(data.slots, data.nodes, data.permutations, canonicalParents))
      }
    }

    while (unionWorklist.nonEmpty || nodesRepairWorklist.nonEmpty || usersRepairWorklist.nonEmpty) {
      // Process all unions in the worklist. The unify operation may add new e-classes to the repair worklists,
      // but will not add elements to its own union worklist.
      for ((left, right) <- unionWorklist) {
        unify(left, right)
      }
      unionWorklist = List.empty

      // Process the node repair worklist. The repairNodes operation may add new e-classes to any worklist.
      while (nodesRepairWorklist.nonEmpty) {
        val copy = nodesRepairWorklist
        nodesRepairWorklist = Set.empty
        for (node <- copy) {
          repairNode(node)
        }
      }

      // Process the parents repair worklist. The repairUsers operation will not add elements to any worklist.
      for (ref <- usersRepairWorklist.map(canonicalize)) {
        repairUsers(ref.ref)
      }
      usersRepairWorklist = Set.empty
    }

    val touched = unifiedPairs.flatMap(p => Seq(p._1, p._2)).map(_.ref).toSet
    touched.map(c => (canonicalize(c), c)).groupBy(_._1.ref).values.map(_.map {
      case (canonical, original) =>
        EClassCall(original, SlotMap.identity(oldClassData(original).slots).composeFresh(canonical.args.inverse))
    }).toSet
  }
}
