package foresight.eqsat.hashCons

import foresight.eqsat.{EClassCall, ENode}
import foresight.eqsat.collections.{SlotMap, SlotSet}

/**
 * An abstract representation of the data stored in an e-class.
 * This is used to define the interface for both mutable and immutable e-classes.
 * @tparam NodeT The type of nodes stored in the e-class.
 */
private[eqsat] trait AbstractEClassData[NodeT] {
  /**
   * Get the slots of the e-class.
   * @return The slots of the e-class.
   */
  def slots: SlotSet

  /**
   * Get the e-class' slot permutations.
   * @return The slot permutations of the e-class.
   */
  def permutations: PermutationGroup[SlotMap]

  /**
   * Get the nodes of the e-class, along with their renamings.
   * @return The nodes of the e-class and their renamings.
   */
  def hasSlots: Boolean

  /**
   * Get the nodes of the e-class, along with their renamings.
   * @return The nodes of the e-class and their renamings.
   */
  def nodes: collection.Map[ENode[NodeT], SlotMap]

  /**
   * Get the e-nodes that take the e-class as an argument.
   * @return The e-nodes that take the e-class as an argument.
   */
  def users: collection.Set[ENode[NodeT]]

  /**
   * Get the applied nodes for a given canonical e-class call to this e-class.
   * @param canonicalApp The canonical e-class call.
   * @return The applied nodes for the given e-class call.
   */
  def appliedNodes(canonicalApp: EClassCall): Iterable[ENode[NodeT]]

  /**
   * Get the applied nodes for a given canonical e-class call to this e-class, returning nodes of a specific type.
   * @param canonicalApp The canonical e-class call.
   * @param nodeType The type of nodes to return.
   * @return The applied nodes of type nodeType for the given e-class call.
   */
  def appliedNodes(canonicalApp: EClassCall, nodeType: NodeT): Iterable[ENode[NodeT]]
}
