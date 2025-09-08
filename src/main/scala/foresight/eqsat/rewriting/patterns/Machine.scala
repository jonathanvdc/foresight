package foresight.eqsat.rewriting.patterns

import foresight.eqsat.{EGraph, EGraphLike}

/**
 * A pattern-matching virtual machine. The machine executes a sequence of instructions on a graph and maintains a
 * machine state that is updated after each instruction.
 */
object Machine {
  /**
   * Runs a pattern-matching virtual machine on a graph with a given initial machine state and list of instructions.
   * Returns all successful runs of the machine. Unsuccessful runs are not listed.
   * @param graph The graph to match against.
   * @param machine The initial machine state.
   * @param instructions The instructions to apply to the machine.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam GraphT The type of the e-graph.
   * @return A list of final machine states that result from successfully applying all instructions to the machine.
   */
  def run[NodeT, GraphT <: EGraphLike[NodeT, GraphT] with EGraph[NodeT]](graph: GraphT,
                                                                         machine: MachineState[NodeT],
                                                                         instructions: List[Instruction[NodeT, GraphT]]): Set[MachineState[NodeT]] = {

    tryRun(graph, machine, instructions).collect {
      case MachineResult.Success(finalMachine) => finalMachine
    }
  }

  /**
   * Runs a pattern-matching virtual machine on a graph with a given initial machine state and list of instructions.
   * Lists all successfully and unsuccessfully terminated machine runs.
   * @param graph The graph to match against.
   * @param machine The initial machine state.
   * @param instructions The instructions to apply to the machine.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam GraphT The type of the e-graph.
   * @return A list of successful and unsuccessful runs of the machine obtained by applying all instructions to the
   *         machine.
   */
  def tryRun[NodeT, GraphT <: EGraphLike[NodeT, GraphT] with EGraph[NodeT]](graph: GraphT,
                                                                            machine: MachineState[NodeT],
                                                                            instructions: List[Instruction[NodeT, GraphT]]): Set[MachineResult[NodeT, GraphT]] = {
    instructions match {
      case Nil => Set(MachineResult.Success(machine))
      case firstInstruction :: remainingInstructions =>
        firstInstruction.execute(graph, machine) match {
          case Left(newMachines) if newMachines.size == 1 =>
            tryRun(graph, newMachines.head, remainingInstructions)
          case Left(newMachines) =>
            newMachines.flatMap(newMachine => tryRun(graph, newMachine, remainingInstructions))
          case Right(error) => Set(MachineResult.Failure(machine, error, instructions))
        }
    }
  }
}
