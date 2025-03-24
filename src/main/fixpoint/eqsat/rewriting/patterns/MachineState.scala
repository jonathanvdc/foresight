package fixpoint.eqsat.rewriting.patterns

import fixpoint.eqsat.{EClassCall, ENode, Slot}

/**
 * The state of a pattern machine.
 * @param registers The registers of the machine.
 * @param boundVars The variables that are bound in the machine.
 * @param boundSlots The slots that are bound in the machine.
 * @param boundNodes The nodes that are bound in the machine.
 * @tparam NodeT The type of the nodes in the e-graph.
 */
final case class MachineState[NodeT](registers: Seq[EClassCall],
                                     boundVars: Map[Pattern.Var[NodeT], EClassCall],
                                     boundSlots: Map[SlotVar, Slot],
                                     boundNodes: Seq[ENode[NodeT]]) {

  /**
   * Binds the given node to the machine state.
   * @param node The node to bind.
   * @param definitions The definitions of the node.
   * @param uses The uses of the node.
   * @return The new machine state.
   */
  def bindNode(node: ENode[NodeT], definitions: Seq[SlotVar], uses: Seq[SlotVar]): MachineState[NodeT] = {
    val newRegisters = registers ++ node.args
    val newBoundSlots = boundSlots ++ (definitions zip node.definitions) ++ (uses zip node.uses)
    MachineState(newRegisters, boundVars, newBoundSlots, boundNodes :+ node)
  }

  /**
   * Binds the given variable to the machine state.
   * @param variable The variable to bind.
   * @param value The value to bind the variable to.
   * @return The new machine state.
   */
  def bindVar(variable: Pattern.Var[NodeT], value: EClassCall): MachineState[NodeT] = {
    val newBoundVars = boundVars + (variable -> value)
    MachineState(registers, newBoundVars, boundSlots, boundNodes)
  }
}

/**
 * A companion object for machine states.
 */
object MachineState {
  /**
   * Creates a new machine state from the given root e-class call.
   * @param root The root e-class call.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @return The new machine state.
   */
  def apply[NodeT](root: EClassCall): MachineState[NodeT] = {
    MachineState(Seq(root), Map.empty, Map.empty, Seq.empty)
  }
}
