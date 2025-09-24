package foresight.eqsat.rewriting.patterns

import foresight.eqsat._

import scala.collection.compat._

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
  def execute(graph: EGraphT, machine: MutableMachineState[NodeT]): Either[Seq[MutableMachineState[NodeT]], MachineError[NodeT]]
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
    boundVars: immutable.ArraySeq[Pattern.Var],
    boundSlots: immutable.ArraySeq[Slot],
    boundNodes: Int
  ) {
    /**
     * Combine this effect summary with another by summing counts and concatenating sequences.
     * @param other The other effect summary to combine with.
     * @return The combined effect summary.
     */
    def ++(other: Effects): Effects = Effects(
      createdRegisters = this.createdRegisters + other.createdRegisters,
      boundVars = this.boundVars ++ other.boundVars,
      boundSlots = this.boundSlots ++ other.boundSlots,
      boundNodes = this.boundNodes + other.boundNodes
    )
  }

  /** A companion object for [[Effects]]. */
  object Effects {
    /** An effect summary for an instruction that does nothing. */
    val none: Effects = Effects(0, immutable.ArraySeq.empty, immutable.ArraySeq.empty, 0)

    /**
     * Aggregate a collection of effect summaries into a single summary.
     * @param effects The collection of effect summaries to aggregate.
     * @return The aggregated effect summary.
     */
    def aggregate(effects: Iterable[Effects]): Effects =
      effects.foldLeft(none)(_ ++ _)

    /**
     * Aggregate the effects of a collection of instructions.
     * @param instructions The collection of instructions to aggregate the effects of.
     * @return The aggregated effect summary.
     */
    def from(instructions: Iterable[Instruction[_, _]]): Effects =
      aggregate(instructions.map(_.effects))
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
                                                                     definitions: immutable.ArraySeq[Slot],
                                                                     uses: immutable.ArraySeq[Slot],
                                                                     argCount: Int)
    extends Instruction[NodeT, EGraphT] {

    override val effects: Instruction.Effects = Instruction.Effects(
      createdRegisters = argCount,
      boundVars = immutable.ArraySeq.empty[Pattern.Var],
      boundSlots = definitions ++ uses,
      boundNodes = 1
    )

    private def matchesSlot(machine: MutableMachineState[NodeT], expected: Slot, actual: Slot): Boolean = {
      machine.boundSlotOrElse(expected, null) match {
        case null => true // expected slot is unbound, so it can match anything
        case bound if bound == actual => true // expected slot is bound to the actual slot
        case _ => false // expected slot is bound to a different slot
      }
    }

    private def allSlotsMatch(machine: MutableMachineState[NodeT], expected: Seq[Slot], actual: Seq[Slot]): Boolean = {
      if (expected.length != actual.length) return false
      var i = 0
      while (i < expected.length) {
        if (!matchesSlot(machine, expected(i), actual(i))) return false
        i += 1
      }
      true
    }

    private def findInEClass(graph: EGraphT, call: EClassCall, machine: MutableMachineState[NodeT]): Seq[ENode[NodeT]] = {
      graph.nodes(call, nodeType)
        .filter { node =>
            node.args.size == argCount &&
            allSlotsMatch(machine, definitions, node.definitions) &&
            allSlotsMatch(machine, uses, node.uses)
        }
        .toSeq
    }

    override def execute(graph: EGraphT, machine: MutableMachineState[NodeT]): Either[Seq[MutableMachineState[NodeT]], MachineError[NodeT]] = {
      val call = machine.registerAt(register)
      val nodes = findInEClass(graph, call, machine)
      nodes.size match {
        case 0 => Right(MachineError.NoMatchingNode(this, call))
        case 1 =>
          machine.bindNode(nodes.head)
          Left(Seq(machine))
        case _ =>
          val forks = nodes.tail.map { node =>
            val forked = machine.fork()
            forked.bindNode(node)
            forked
          }
          machine.bindNode(nodes.head)
          Left(machine +: forks)
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
      boundVars = immutable.ArraySeq(variable),
      boundSlots = immutable.ArraySeq.empty[Slot],
      boundNodes = 0
    )

    override def execute(graph: EGraphT, machine: MutableMachineState[NodeT]): Either[Seq[MutableMachineState[NodeT]], MachineError[NodeT]] = {
      val value = MixedTree.Atom[NodeT, EClassCall](machine.registerAt(register))
      machine.bindVar(value)
      Left(Seq(machine))
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

    override def execute(graph: EGraphT, machine: MutableMachineState[NodeT]): Either[Seq[MutableMachineState[NodeT]], MachineError[NodeT]] = {
      if (graph.areSame(machine.registerAt(register1), machine.registerAt(register2))) {
        Left(Seq(machine))
      } else {
        Right(MachineError.InconsistentVars(this, machine.registerAt(register1), machine.registerAt(register2)))
      }
    }
  }
}
