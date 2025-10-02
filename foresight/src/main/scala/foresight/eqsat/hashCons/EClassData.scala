package foresight.eqsat.hashCons

import foresight.eqsat.collections.{SlotMap, SlotSet}
import foresight.eqsat.{EClassCall, ENode, ShapeCall}

/**
 * The data of an e-class in a hash-consed e-graph.
 *
 * @param slots The slots of the e-class.
 * @param nodes The nodes of the e-class, along with the renaming of the slots. The keys of the map are e-node shapes
 *              and the values are slot maps in which the keys are the slots of the e-node shapes and the values are the
 *              arguments in the form of either slots of the e-class or redundant slots.
 * @param permutations The permutations of the e-class.
 * @param users The e-nodes that take the e-class as an argument.
 * @tparam NodeT The type of the nodes.
 */
private[eqsat] final case class EClassData[NodeT](slots: SlotSet,
                                                  nodes: Map[ENode[NodeT], SlotMap],
                                                  permutations: PermutationGroup[SlotMap],
                                                  users: Set[ENode[NodeT]])
  extends AbstractEClassData[NodeT] {

  /**
   * Gets the applied nodes.
   * @return The applied nodes.
   */
  private lazy val preAppliedNodes: Seq[ShapeCall[NodeT]] = {
    nodes.map { case (node, renaming) => ShapeCall(node, renaming) }.toSeq
  }

  private lazy val appliedNodesWithIdentity: Seq[ENode[NodeT]] = {
    val mapping = SlotMap.identity(slots)
    preAppliedNodes.map(_.renamePartial(mapping).asNode)
  }

  private val hasSlots: Boolean = {
    nodes.values.exists(!_.isEmpty)
  }

  override def appliedNodes(canonicalApp: EClassCall): Iterable[ENode[NodeT]] = {
    assert(canonicalApp.args.size == slots.size)

    if (hasSlots) {
      if (canonicalApp.args.isIdentity) {
        // Common case: the e-class call's arguments are the identity mapping.
        // We can return a precomputed set of applied nodes with identity renaming.
        appliedNodesWithIdentity
      } else {
        // E-class has slots and the e-class call's arguments are not the identity mapping.
        // We rename all applied nodes by the e-class call's arguments.
        preAppliedNodes.map(_.renamePartial(canonicalApp.args).asNode)
      }
    } else {
      // E-class has no slots: all nodes are the same regardless of the e-class call's arguments.
      nodes.keys
    }
  }

  override def appliedNodes(canonicalApp: EClassCall, nodeType: NodeT): Iterable[ENode[NodeT]] = {
    assert(canonicalApp.args.size == slots.size)

    if (hasSlots) {
      if (canonicalApp.args.isIdentity) {
        // Common case: the e-class call's arguments are the identity mapping.
        // We can return a precomputed set of applied nodes with identity renaming.
        appliedNodesWithIdentity.view.filter(_.nodeType == nodeType)
      } else {
        // E-class has slots and the e-class call's arguments are not the identity mapping.
        // We rename all applied nodes by the e-class call's arguments.
        preAppliedNodes.view.filter(_.shape.nodeType == nodeType).map(_.renamePartial(canonicalApp.args).asNode)
      }
    } else {
      // E-class has no slots: all nodes are the same regardless of the e-class call's arguments.
      nodes.keys.view.filter(_.nodeType == nodeType)
    }
  }
}
