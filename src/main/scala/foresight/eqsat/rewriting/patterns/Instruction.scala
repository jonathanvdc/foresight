package foresight.eqsat.rewriting.patterns

import foresight.eqsat.{EClassCall, EGraph, EGraphLike, ENode, MixedTree, Slot}

/**
 * An instruction for the pattern-matching virtual machine.
 *
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam EGraphT The type of the e-graph.
 */
sealed trait Instruction[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]] {
  /**
   * Executes the instruction on the given machine state.
   * @param graph The e-graph to execute the instruction on.
   * @param machine The machine state to execute the instruction on.
   * @return Either a nonempty set of new machine states or a machine error.
   */
  def execute(graph: EGraphT, machine: MachineState[NodeT]): Either[Set[MachineState[NodeT]], MachineError[NodeT]]
}

/**
 * A companion object for the `Instruction` trait.
 */
object Instruction {
  /**
   * An instruction that binds an e-node to a register.
   * @param register The index of the register to bind the e-node to.
   * @param nodeType The type of the e-node to bind.
   * @param definitions The slot variables that the e-node defines.
   * @param uses The slot variables that the e-node uses.
   * @param argCount The number of arguments the e-node has.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph.
   */
  final case class BindNode[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](register: Int,
                                                                                             nodeType: NodeT,
                                                                                             definitions: Seq[Slot],
                                                                                             uses: Seq[Slot],
                                                                                             argCount: Int)
    extends Instruction[NodeT, EGraphT] {

    private def matchesSlot(machine: MachineState[NodeT], pair: (Slot, Slot)): Boolean = {
      val (expected, actual) = pair
      machine.boundSlots.get(expected).forall(_ == actual)
    }

    private def allSlotsMatch(machine: MachineState[NodeT], expected: Seq[Slot], actual: Seq[Slot]): Boolean = {
      if (expected.length != actual.length) return false
      var i = 0
      while (i < expected.length) {
        if (!matchesSlot(machine, (expected(i), actual(i)))) return false
        i += 1
      }
      true
    }

    private def findInEClass(graph: EGraphT, call: EClassCall, machine: MachineState[NodeT]): Set[ENode[NodeT]] = {
      graph.nodes(call).filter { node =>
        node.nodeType == nodeType &&
          node.args.size == argCount &&
          allSlotsMatch(machine, definitions, node.definitions) &&
          allSlotsMatch(machine, uses, node.uses)
      }
    }

    override def execute(graph: EGraphT, machine: MachineState[NodeT]): Either[Set[MachineState[NodeT]], MachineError[NodeT]] = {
      val call = machine.registers(register)
      findInEClass(graph, call, machine) match {
        case nodes if nodes.isEmpty => Right(MachineError.NoMatchingNode(this, call))
        case nodes => Left(nodes.map(machine.bindNode(_, definitions, uses)))
      }
    }
  }

  /**
   * An instruction that binds a pattern variable to an e-class call in register.
   * @param register The index of the register to bind the variable to.
   * @param variable The variable to bind.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph.
   */
  final case class BindVar[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](register: Int,
                                                                                            variable: Pattern.Var)
    extends Instruction[NodeT, EGraphT] {

    override def execute(graph: EGraphT, machine: MachineState[NodeT]): Either[Set[MachineState[NodeT]], MachineError[NodeT]] = {
      Left(Set(machine.bindVar(variable, MixedTree.Atom[NodeT, EClassCall](machine.registers(register)))))
    }
  }

  /**
   * An instruction that compares two registers for equality.
   * @param register1 The index of the first register to compare.
   * @param register2 The index of the second register to compare.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph.
   */
  final case class Compare[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](register1: Int, register2: Int)
    extends Instruction[NodeT, EGraphT] {
    override def execute(graph: EGraphT, machine: MachineState[NodeT]): Either[Set[MachineState[NodeT]], MachineError[NodeT]] = {
      if (graph.areSame(machine.registers(register1), machine.registers(register2))) {
        Left(Set(machine))
      } else {
        Right(MachineError.InconsistentVars(this, machine.registers(register1), machine.registers(register2)))
      }
    }
  }
}
