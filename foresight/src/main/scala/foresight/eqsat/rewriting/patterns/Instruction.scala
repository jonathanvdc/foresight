package foresight.eqsat.rewriting.patterns

import foresight.eqsat._

/**
 * An instruction for the pattern-matching virtual machine.
 *
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam EGraphT The type of the e-graph.
 */
trait Instruction[NodeT, -EGraphT <: ReadOnlyEGraph[NodeT]] {
  /** A compact summary of what this instruction will create and bind. */
  def effects: Instruction.Effects

  /**
   * Executes the instruction on the given machine state.
   * @param graph The e-graph to execute the instruction on.
   * @param machine The machine state to execute the instruction on.
   * @return Either a nonempty set of new machine states or a machine error.
   */
  def execute(graph: EGraphT, machine: MachineState[NodeT]): Either[Seq[MachineState[NodeT]], MachineError[NodeT]]
}

/**
 * A companion object for the `Instruction` trait.
 */
object Instruction {
  /**
   * Summarizes the static effects of an instruction on the matching VM.
   * @param createdRegisters The number of virtual-machine registers created.
   * @param boundVars The sequence of pattern variables bound.
   * @param boundSlots The sequence of slots bound (ordered).
   * @param boundNodes The number of e-nodes bound.
   */
  final case class Effects(
    createdRegisters: Int,
    boundVars: Seq[Pattern.Var],
    boundSlots: Seq[Slot],
    boundNodes: Int
  )

  /** A companion object for [[Effects]]. */
  object Effects {
    /** An effect summary for an instruction that does nothing. */
    val none: Effects = Effects(0, Seq.empty, Seq.empty, 0)
  }

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
  final case class BindNode[NodeT, EGraphT <: ReadOnlyEGraph[NodeT]](register: Int,
                                                                     nodeType: NodeT,
                                                                     definitions: Seq[Slot],
                                                                     uses: Seq[Slot],
                                                                     argCount: Int)
    extends Instruction[NodeT, EGraphT] {

    override val effects: Instruction.Effects = Instruction.Effects(
      createdRegisters = argCount,
      boundVars = Seq.empty,
      boundSlots = definitions ++ uses,
      boundNodes = 1
    )

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

    private def findInEClass(graph: EGraphT, call: EClassCall, machine: MachineState[NodeT]): Seq[ENode[NodeT]] = {
      graph.nodes(call).toSeq.filter { node =>
        node.nodeType == nodeType &&
          node.args.size == argCount &&
          allSlotsMatch(machine, definitions, node.definitions) &&
          allSlotsMatch(machine, uses, node.uses)
      }
    }

    override def execute(graph: EGraphT, machine: MachineState[NodeT]): Either[Seq[MachineState[NodeT]], MachineError[NodeT]] = {
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
  final case class BindVar[NodeT, EGraphT <: ReadOnlyEGraph[NodeT]](register: Int,
                                                                    variable: Pattern.Var)
    extends Instruction[NodeT, EGraphT] {

    override val effects: Instruction.Effects = Instruction.Effects(
      createdRegisters = 0,
      boundVars = Seq(variable),
      boundSlots = Seq.empty,
      boundNodes = 0
    )

    override def execute(graph: EGraphT, machine: MachineState[NodeT]): Either[Seq[MachineState[NodeT]], MachineError[NodeT]] = {
      Left(Seq(machine.bindVar(variable, MixedTree.Atom[NodeT, EClassCall](machine.registers(register)))))
    }
  }

  /**
   * An instruction that compares two registers for equality.
   * @param register1 The index of the first register to compare.
   * @param register2 The index of the second register to compare.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph.
   */
  final case class Compare[NodeT, EGraphT <: ReadOnlyEGraph[NodeT]](register1: Int, register2: Int)
    extends Instruction[NodeT, EGraphT] {

    override def effects: Instruction.Effects = Instruction.Effects.none

    override def execute(graph: EGraphT, machine: MachineState[NodeT]): Either[Seq[MachineState[NodeT]], MachineError[NodeT]] = {
      if (graph.areSame(machine.registers(register1), machine.registers(register2))) {
        Left(Seq(machine))
      } else {
        Right(MachineError.InconsistentVars(this, machine.registers(register1), machine.registers(register2)))
      }
    }
  }
}
