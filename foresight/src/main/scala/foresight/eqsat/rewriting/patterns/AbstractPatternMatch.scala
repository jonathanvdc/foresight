package foresight.eqsat.rewriting.patterns

import foresight.eqsat.{CallTree, Slot}

/**
 * An abstract pattern match that maps pattern variables to trees and slots.
 *
 * @tparam NodeT The type of the nodes in the e-graph.
 */
trait AbstractPatternMatch[NodeT] {
  /**
   * Gets the tree that corresponds to a variable.
   *
   * @param variable The variable.
   * @return The tree.
   */
  def apply(variable: Pattern.Var): CallTree[NodeT]

  /**
   * Gets the slot that corresponds to a slot variable.
   *
   * @param slot The slot variable.
   * @return The slot.
   */
  def apply(slot: Slot): Slot
}
