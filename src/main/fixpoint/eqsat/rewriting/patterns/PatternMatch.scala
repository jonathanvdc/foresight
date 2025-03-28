package fixpoint.eqsat.rewriting.patterns

import fixpoint.eqsat.rewriting.PortableMatch
import fixpoint.eqsat.{EClassCall, EGraph, Slot}

/**
 * A match of a pattern.
 * @param root The e-class in which the pattern was found.
 * @param varMapping The mapping from pattern variables to e-graph nodes.
 * @param slotMapping The mapping from slot variables to slots.
 * @tparam NodeT The type of the nodes in the e-graph.
 */
final case class PatternMatch[NodeT](root: EClassCall,
                                     varMapping: Map[Pattern.Var[NodeT], EClassCall],
                                     slotMapping: Map[SlotVar, Slot]) extends PortableMatch[EGraph[NodeT], PatternMatch[NodeT]] {

  /**
   * Gets the e-class application that corresponds to a variable.
   * @param variable The variable.
   * @return The e-class application.
   */
  def apply(variable: Pattern.Var[NodeT]): EClassCall = varMapping(variable)

  /**
   * Gets the slot that corresponds to a slot variable.
   * @param slot The slot variable.
   * @return The slot.
   */
  def apply(slot: SlotVar): Slot = slotMapping(slot)

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
    val newVarMapping = varMapping.mapValues(egraph.canonicalize).view.force
    val newSlotMapping = slotMapping
    PatternMatch(newRoot, newVarMapping, newSlotMapping)
  }
}
