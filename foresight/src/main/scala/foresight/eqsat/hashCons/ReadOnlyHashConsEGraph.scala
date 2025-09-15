package foresight.eqsat.hashCons

import foresight.eqsat.{EClassCall, EClassRef, ENode, ReadOnlyEGraph, ShapeCall, Slot, SlotMap}
import foresight.util.Debug

/**
 * A read-only hash-consed e-graph.
 *
 * This trait provides methods to interact with a hash-consed e-graph without modifying it.
 * It allows for canonicalization of e-classes and e-nodes, as well as finding e-classes
 * corresponding to given e-nodes.
 *
 * @tparam NodeT The type of the nodes in the e-graph.
 */
private[hashCons] trait ReadOnlyHashConsEGraph[NodeT] extends ReadOnlyEGraph[NodeT] {
  /**
   * Retrieves the e-class reference for a given e-node, or returns a default value if the e-node is not found.
   * This method does not canonicalize the e-node before looking it up; it assumes the caller has already done so
   * and simply performs a hash cons lookup.
   * @param node The e-node to look up.
   * @param default A default e-class reference to return if the e-node is not found.
   * @return The e-class reference corresponding to the e-node, or the default value if not found.
   */
  def nodeToRefOrElse(node: ENode[NodeT], default: => EClassRef): EClassRef

  /**
   * Retrieves the data associated with a given e-class. Assumes that the e-class reference is canonical.
   * @param ref The e-class reference whose data is to be retrieved.
   * @return The data associated with the e-class reference.
   */
  def dataForClass(ref: EClassRef): EClassData[NodeT]

  def isCanonical(ref: EClassRef): Boolean

  /**
   * Retrieves the e-class reference for a given e-node. Assumes that the e-node is canonical.
   * @param value The canonical e-node to look up.
   * @return The e-class reference corresponding to the e-node.
   * @throws NoSuchElementException if the e-node is not found in the e-graph.
   */
  final def nodeToRef(value: ENode[NodeT]): EClassRef = {
    nodeToRefOrElse(value, throw new NoSuchElementException(s"Node $value is not in the e-graph."))
  }

  final def isCanonical(call: EClassCall): Boolean = {
    isCanonical(call.ref)
  }

  private final def canonicalizeWithoutSlots(node: ENode[NodeT]): ENode[NodeT] = {
    if (Debug.isEnabled && node.hasSlots) {
      throw new IllegalArgumentException("Node has slots.")
    }

    if (node.args.forall(isCanonical)) {
      node
    } else {
      val canonicalArgs = node.args.map(canonicalize)
      node.withArgs(canonicalArgs)
    }
  }

  final def canonicalize(node: ENode[NodeT]): ShapeCall[NodeT] = {
    import foresight.util.ordering.SeqOrdering

    if (node.args.forall(isCanonical) && !node.hasSlots) {
      return ShapeCall(node, SlotMap.empty)
    }

    val canonicalArgs = node.args.map(canonicalize)
    val nodeWithCanonicalizedArgs = node.withArgs(canonicalArgs)
    groupCompatibleVariants(nodeWithCanonicalizedArgs)
      .toSeq
      .map(_.asShapeCall)
      .minBy(_.shape.slots)(SeqOrdering.lexOrdering(Ordering.by(identity[Slot])))
  }

  /**
   * Generates all possible variants of an `ENode` by considering symmetries (permutations)
   * in its argument e-classes.
   *
   * For each argument (`EClassCall`) of the node:
   *   - Retrieves the associated e-class data and its permutation group.
   *   - If the permutation group is trivial (no symmetries), only the original call is used.
   *   - If there are symmetries, applies all permutations to the call, generating multiple variants.
   *
   * Computes the cartesian product of all argument variants, creating every possible combination.
   * Each combination is used to create a new `ENode` variant by copying the original node with the new arguments.
   * The result is a set of all compatible variants of the input node, accounting for argument symmetries.
   */
  protected final def groupCompatibleVariants(node: ENode[NodeT]): Set[ENode[NodeT]] = {
    // For each argument, generate all possible variants by applying its permutation group.
    val groups = node.args.map({
      case call@EClassCall(ref, renaming) =>
        val refData = dataForClass(ref)
        val permutations = refData.permutations
        if (permutations.isTrivial) {
          // No symmetries: only one variant.
          Seq(call)
        } else {
          // Symmetries present: apply all permutations to the e-class call.
          refData.permutations.allPerms.toSeq.map(perm => EClassCall(ref, perm.compose(renaming)))
        }
    })

    // Compute the cartesian product of all argument variants to get every possible combination.
    Helpers.cartesian(groups).map(node.withArgs).toSet
  }

  final def findOrNull(node: ENode[NodeT]): EClassCall = {
    if (node.hasSlots) {
      findUnsafe(canonicalize(node))
    } else {
      findUnsafeWithoutSlots(canonicalizeWithoutSlots(node))
    }
  }

  /**
   * Finds the e-class of a given e-node. The e-node must be canonical.
   * @param renamedShape The canonicalized e-node to find the e-class of.
   * @return The e-class of the e-node, if it is defined in this e-graph; otherwise, null.
   */
  protected final def findUnsafe(renamedShape: ShapeCall[NodeT]): EClassCall = {
    if (Debug.isEnabled) {
      assert(renamedShape.shape.isShape)
    }

    val ref = nodeToRefOrElse(renamedShape.shape, return null)

    if (!renamedShape.shape.hasSlots) {
      return ref.callWithoutSlots
    }

    val refData = dataForClass(ref)
    val classToNode = refData.nodes(renamedShape.shape)
    val out = classToNode.inverse.compose(renamedShape.renaming)
    EClassCall(ref, out)
  }

  private final def findUnsafeWithoutSlots(node: ENode[NodeT]): EClassCall = {
    if (Debug.isEnabled) {
      assert(!node.hasSlots)
    }

    nodeToRefOrElse(node, return null).callWithoutSlots
  }

  final override def users(ref: EClassRef): Set[ENode[NodeT]] = {
    val canonicalApp = canonicalize(ref)
    dataForClass(canonicalApp.ref).users.map(node => {
      val c = nodeToRef(node)
      val mapping = dataForClass(c).nodes(node)
      ShapeCall(node, mapping).asNode
    })
  }

  final override def nodes(call: EClassCall): Set[ENode[NodeT]] = {
    val canonicalApp = canonicalize(call)
    val data = dataForClass(canonicalApp.ref)
    if (data.hasSlots) {
      data.appliedNodes.map(_.renamePartial(canonicalApp.args).asNode)
    } else {
      data.nodes.keySet
    }
  }

  /**
   * Determines whether two e-classes are the same. Both classes are assumed to be in the e-graph.
   *
   * @param first  The first e-class to compare.
   * @param second The second e-class to compare.
   * @return True if the e-classes are the same; otherwise, false.
   */
  final override def areSame(first: EClassCall, second: EClassCall): Boolean = {
    // First canonicalize the e-classes.
    val canonicalFirst = canonicalize(first)
    val canonicalSecond = canonicalize(second)

    if (canonicalFirst.ref != canonicalSecond.ref) {
      // If the canonical e-class references are different, then the e-classes are different.
      false
    } else if (canonicalFirst.args == canonicalSecond.args) {
      // If the canonical e-class calls are the same, then the argument calls are the same.
      true
    } else if (canonicalFirst.args.valueSet == canonicalSecond.args.valueSet) {
      // If the canonical e-class calls refer to the same e-class but have differently ordered arguments, then the
      // e-class calls may still be the same if the e-class slots can be permuted.
      dataForClass(canonicalFirst.ref).permutations.allPerms.exists { perm =>
        perm.compose(canonicalFirst.args) == canonicalSecond.args
      }
    } else {
      false
    }
  }
}
