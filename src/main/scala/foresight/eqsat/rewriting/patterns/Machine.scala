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
   *
   * @param graph        The graph to match against.
   * @param machine      The initial machine state.
   * @param instructions The instructions to apply to the machine.
   * @tparam NodeT  The type of the nodes in the e-graph.
   * @tparam GraphT The type of the e-graph.
   * @return A list of final machine states that result from successfully applying all instructions to the machine.
   */
  def run[NodeT, GraphT <: EGraphLike[NodeT, GraphT] with EGraph[NodeT]](graph: GraphT,
                                                                         machine: MachineState[NodeT],
                                                                         instructions: List[Instruction[NodeT, GraphT]]): Seq[MachineState[NodeT]] = {

    //    tryRun(graph, machine, instructions).collect {
    //      case MachineResult.Success(finalMachine) => finalMachine
    //    }

    val results = Seq.newBuilder[MachineState[NodeT]]
    tryRunCPSWithoutFailure(graph, machine, instructions,
      onSuccess = (finalMachine: MachineState[NodeT]) => results += finalMachine
    )
    results.result()
  }

  /**
   * Runs a pattern-matching virtual machine on a graph with a given initial machine state and list of instructions.
   * Lists all successfully and unsuccessfully terminated machine runs.
   *
   * @param graph        The graph to match against.
   * @param machine      The initial machine state.
   * @param instructions The instructions to apply to the machine.
   * @tparam NodeT  The type of the nodes in the e-graph.
   * @tparam GraphT The type of the e-graph.
   * @return A list of successful and unsuccessful runs of the machine obtained by applying all instructions to the
   *         machine.
   */
  def tryRun[NodeT, GraphT <: EGraphLike[NodeT, GraphT] with EGraph[NodeT]](graph: GraphT,
                                                                            machine: MachineState[NodeT],
                                                                            instructions: List[Instruction[NodeT, GraphT]]): Seq[MachineResult[NodeT, GraphT]] = {
    val results = Seq.newBuilder[MachineResult[NodeT, GraphT]]
    tryRunCPS(graph, machine, instructions,
      onSuccess = (finalMachine: MachineState[NodeT]) => results += MachineResult.Success(finalMachine),
      onFailure = (failedMachine: MachineState[NodeT], error: MachineError[NodeT], remainingInstructions: List[Instruction[NodeT, GraphT]]) =>
        results += MachineResult.Failure(failedMachine, error, remainingInstructions)
    )
    results.result()
  }

  private def tryRunCPS[NodeT, GraphT <: EGraphLike[NodeT, GraphT] with EGraph[NodeT]](graph: GraphT,
                                                                                       machine: MachineState[NodeT],
                                                                                       instructions: List[Instruction[NodeT, GraphT]],
                                                                                       onSuccess: MachineState[NodeT] => Unit,
                                                                                       onFailure: (MachineState[NodeT], MachineError[NodeT], List[Instruction[NodeT, GraphT]]) => Unit): Unit = instructions match {
    case Nil => onSuccess(machine)
    case firstInstruction :: remainingInstructions =>
      firstInstruction.execute(graph, machine) match {
        case Left(newMachines) if newMachines.size == 1 =>
          tryRunCPS(graph, newMachines.head, remainingInstructions, onSuccess, onFailure)
        case Left(newMachines) =>
          newMachines.foreach(newMachine => tryRunCPS(graph, newMachine, remainingInstructions, onSuccess, onFailure))
        case Right(error) =>
          onFailure(machine, error, instructions)
      }
  }

  private def tryRunCPSWithoutFailure[NodeT, GraphT <: EGraphLike[NodeT, GraphT] with EGraph[NodeT]](graph: GraphT,
                                                                                                     machine: MachineState[NodeT],
                                                                                                     instructions: List[Instruction[NodeT, GraphT]],
                                                                                                     onSuccess: MachineState[NodeT] => Unit): Unit = instructions match {
    case Nil => onSuccess(machine)
    case firstInstruction :: remainingInstructions =>
      firstInstruction.execute(graph, machine) match {
        case Left(newMachines) if newMachines.size == 1 =>
          tryRunCPSWithoutFailure(graph, newMachines.head, remainingInstructions, onSuccess)
        case Left(newMachines) =>
          newMachines.foreach(newMachine => tryRunCPSWithoutFailure(graph, newMachine, remainingInstructions, onSuccess))
        case Right(error) => {
          /* Do nothing on failure */
        }
      }
  }
}
