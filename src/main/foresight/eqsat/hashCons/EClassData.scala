package foresight.eqsat.hashCons

import foresight.eqsat.{ENode, ShapeCall, Slot, SlotMap}

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
private[eqsat] final case class EClassData[NodeT](slots: Set[Slot],
                                                  nodes: Map[ENode[NodeT], SlotMap],
                                                  permutations: PermutationGroup[SlotMap],
                                                  users: Set[ENode[NodeT]]) {
  /**
   * Gets the applied nodes.
   * @return The applied nodes.
   */
  def appliedNodes: Set[ShapeCall[NodeT]] = {
    nodes.map { case (node, renaming) => ShapeCall(node, renaming) }.toSet
  }
}
