package foresight.eqsat.rewriting.patterns

import foresight.eqsat.rewriting.PortableMatch
import foresight.eqsat.{EClassCall, EGraph, MixedTree, Slot}
import foresight.util.collections.ArrayMap
import foresight.util.collections.StrictMapOps.toStrictMapOps

/**
 * A match of a pattern.
 *
 * @param root The e-class in which the pattern was found.
 * @param varMapping The mapping from pattern variables to e-graph nodes.
 * @param slotMapping The mapping from slot variables to slots.
 * @tparam NodeT The type of the nodes in the e-graph.
 */
final case class PatternMatch[NodeT](root: EClassCall,
                                     varMapping: ArrayMap[Pattern.Var, MixedTree[NodeT, EClassCall]],
                                     slotMapping: ArrayMap[Slot, Slot]) extends PortableMatch[NodeT, PatternMatch[NodeT]] {

  /**
   * Gets the tree that corresponds to a variable.
   * @param variable The variable.
   * @return The tree.
   */
  def apply(variable: Pattern.Var): MixedTree[NodeT, EClassCall] = varMapping(variable)

  /**
   * Gets the slot that corresponds to a slot variable.
   * @param slot The slot variable.
   * @return The slot.
   */
  def apply(slot: Slot): Slot = slotMapping(slot)

  /**
   * Creates an updated match with a new variable binding.
   * @param variable The variable to bind.
   * @param value The value to bind the variable to.
   * @return The updated match with the new binding.
   */
  def bind(variable: Pattern.Var, value: MixedTree[NodeT, EClassCall]): PatternMatch[NodeT] = {
    PatternMatch(root, varMapping.updated(variable, value), slotMapping)
  }

  /**
   * Merges this match with another match if possible. The matches are compatible if they do not bind the same variable
   * to different values or the same slot variable to different slots. If the matches are compatible, the new match
   * contains all the bindings of both matches. If the matches are incompatible, None is returned.
   * @param other The other match.
   * @return The merged match, or None if the matches are incompatible.
   */
  def tryMerge(other: PatternMatch[NodeT]): Option[PatternMatch[NodeT]] = {
    if (varMapping.keySet.intersect(other.varMapping.keySet).exists(k => varMapping(k) != other.varMapping(k))) {
      None
    } else if (slotMapping.keySet.intersect(other.slotMapping.keySet).exists(k => slotMapping(k) != other.slotMapping(k))) {
      None
    } else {
      val newVarMapping = varMapping ++ other.varMapping
      val newSlotMapping = slotMapping ++ other.slotMapping
      Some(PatternMatch(root, newVarMapping, newSlotMapping))
    }
  }

  /**
   * Merges this match with another match. The matches must be compatible, that is, they must not bind the same variable
   * to different values or the same slot variable to different slots. If the matches are incompatible, an exception is
   * thrown.
   * @param other The other match.
   * @return The merged match.
   */
  def merge(other: PatternMatch[NodeT]): PatternMatch[NodeT] = {
    tryMerge(other).getOrElse(throw new IllegalArgumentException("Cannot merge incompatible matches"))
  }

  override def port(egraph: EGraph[NodeT]): PatternMatch[NodeT] = {
    val newRoot = egraph.canonicalize(root)
    val newVarMapping = varMapping.mapValuesStrict(_.mapAtoms(egraph.canonicalize))
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
  def isIndependent(expr: Pattern.Var, slots: Set[Slot]): Boolean = {
    varMapping.get(expr) match {
      case Some(tree) =>
        val exprSlots = tree.slotSet
        val commonSlots = exprSlots.intersect(slots.flatMap(slotMapping.get))
        commonSlots.isEmpty

      case None => true
    }
  }
}
