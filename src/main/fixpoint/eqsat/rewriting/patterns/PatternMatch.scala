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

  override def port(egraph: EGraph[NodeT]): PatternMatch[NodeT] = {
    val newRoot = egraph.canonicalize(root)
    val newVarMapping = varMapping.mapValues(egraph.canonicalize).view.force
    val newSlotMapping = slotMapping
    PatternMatch(newRoot, newVarMapping, newSlotMapping)
  }
}
