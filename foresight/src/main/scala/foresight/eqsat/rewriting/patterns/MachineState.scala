package foresight.eqsat.rewriting.patterns

import foresight.eqsat.{EClassCall, ENode, MixedTree, Slot}
import foresight.util.collections.ArrayMap

import scala.collection.compat._

/**
 * The state of a pattern machine.
 *
 * @param registers The registers of the machine.
 * @param boundVars The variables that are bound in the machine.
 * @param boundSlots The slots that are bound in the machine.
 * @param boundNodes The nodes that are bound in the machine.
 * @tparam NodeT The type of the nodes in the e-graph.
 */
final case class MachineState[NodeT](registers: immutable.ArraySeq[EClassCall],
                                     boundVars: ArrayMap[Pattern.Var, MixedTree[NodeT, EClassCall]],
                                     boundSlots: ArrayMap[Slot, Slot],
                                     boundNodes: immutable.ArraySeq[ENode[NodeT]]) {

  /**
   * Binds the given node to the machine state.
   * @param node The node to bind.
   * @param definitions The definitions of the node.
   * @param uses The uses of the node.
   * @return The new machine state.
   */
  def bindNode(node: ENode[NodeT], definitions: Seq[Slot], uses: Seq[Slot]): MachineState[NodeT] = {
    val newRegisters = registers ++ node.args
    val newBoundSlots = if (definitions.isEmpty && uses.isEmpty) {
      boundSlots
    } else {
      boundSlots ++ (definitions zip node.definitions) ++ (uses zip node.uses)
    }
    MachineState(newRegisters, boundVars, newBoundSlots, boundNodes :+ node)
  }

  /**
   * Binds the given variable to the machine state.
   * @param variable The variable to bind.
   * @param value The value to bind the variable to.
   * @return The new machine state.
   */
  def bindVar(variable: Pattern.Var, value: MixedTree[NodeT, EClassCall]): MachineState[NodeT] = {
    val newBoundVars = boundVars.updated(variable, value)
    MachineState(registers, newBoundVars, boundSlots, boundNodes)
  }
}
