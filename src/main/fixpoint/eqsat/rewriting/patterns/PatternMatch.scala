package fixpoint.eqsat.rewriting.patterns

import fixpoint.eqsat.rewriting.PortableMatch
import fixpoint.eqsat.{EClassCall, EGraph, MixedTree, Slot}

/**
 * A match of a pattern.
 * @param root The e-class in which the pattern was found.
 * @param varMapping The mapping from pattern variables to e-graph nodes.
 * @param slotMapping The mapping from slot variables to slots.
 * @tparam NodeT The type of the nodes in the e-graph.
 */
final case class PatternMatch[NodeT](root: EClassCall,
                                     varMapping: Map[Pattern.Var[NodeT], MixedTree[NodeT, EClassCall]],
                                     slotMapping: Map[Slot, Slot]) extends PortableMatch[NodeT, PatternMatch[NodeT]] {

  /**
   * Gets the tree that corresponds to a variable.
   * @param variable The variable.
   * @return The tree.
   */
  def apply(variable: Pattern.Var[NodeT]): MixedTree[NodeT, EClassCall] = varMapping(variable)

  /**
   * Gets the slot that corresponds to a slot variable.
   * @param slot The slot variable.
   * @return The slot.
   */
  def apply(slot: Slot): Slot = slotMapping(slot)

  /**
   * Merges this match with another match.
   * @param other The other match.
   * @return The merged match.
   */
  def merge(other: PatternMatch[NodeT]): PatternMatch[NodeT] = {
    if (varMapping.keySet.intersect(other.varMapping.keySet).exists(k => varMapping(k) != other.varMapping(k))) {
      throw new IllegalArgumentException("Variable mappings are not compatible")
    }

    if (slotMapping.keySet.intersect(other.slotMapping.keySet).exists(k => slotMapping(k) != other.slotMapping(k))) {
      throw new IllegalArgumentException("Slot mappings are not compatible")
    }

    val newVarMapping = varMapping ++ other.varMapping
    val newSlotMapping = slotMapping ++ other.slotMapping
    PatternMatch(root, newVarMapping, newSlotMapping)
  }

  override def port(egraph: EGraph[NodeT]): PatternMatch[NodeT] = {
    val newRoot = egraph.canonicalize(root)
    val newVarMapping = varMapping.mapValues(_.mapCalls(egraph.canonicalize)).view.force
    val newSlotMapping = slotMapping
    PatternMatch(newRoot, newVarMapping, newSlotMapping)
  }

  /**
   * Checks if an expression is independent of a given set of slots. That is, none of the slots in the set occur in the
   * expression.
   * @param expr The expression to check.
   * @param slots The slots to check.
   * @return True if the expression is independent of the slots, false otherwise.
   */
  def isIndependent(expr: Pattern.Var[NodeT], slots: Set[Slot]): Boolean = {
    val exprSlots = apply(expr).slotSet
    val commonSlots = exprSlots.intersect(slots)
    commonSlots.isEmpty
  }
}
