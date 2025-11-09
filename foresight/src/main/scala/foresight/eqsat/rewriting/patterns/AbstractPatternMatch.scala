package foresight.eqsat.rewriting.patterns

import foresight.eqsat.{CallTree, EClassCall, Slot}

/**
 * An abstract pattern match that maps pattern variables to trees and slots.
 *
 * @tparam NodeT The type of the nodes in the e-graph.
 */
trait AbstractPatternMatch[NodeT] {
  /**
   * The e-class in which the pattern was found.
   */
  def root: EClassCall

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

  /**
   * Gets the slot that corresponds to a slot variable, returning an option.
   *
   * @param slot The slot variable.
   * @return The slot if it exists; None otherwise.
   */
  def get(slot: Slot): Option[Slot]
}
