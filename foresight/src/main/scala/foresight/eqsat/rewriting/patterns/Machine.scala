package foresight.eqsat.rewriting.patterns

import foresight.eqsat.ReadOnlyEGraph

/**
 * A pattern-matching virtual machine. The machine executes a sequence of instructions on a graph and maintains a
 * machine state that is updated after each instruction.
 */
object Machine {
  /**
   * Runs a pattern-matching virtual machine on a graph with a given initial machine state and a single instruction.
   * The continuation is called for each successful run of the machine. Unsuccessful runs are not listed.
   *
   * @param graph        The graph to match against.
   * @param machine      The initial machine state. This method takes ownership of all machines that are not passed
   *                     to the continuation and will release them. The continuation takes ownership of the machines
   *                     passed to it and must release them.
   * @param instructions  The instructions to apply to the machine.
   * @param continuation A continuation that is called for each successful run of the machine. If the continuation
   *                     returns false, the search is stopped.
   * @tparam NodeT  The type of the nodes in the e-graph.
   * @tparam GraphT The type of the e-graph.
   */
  def run[NodeT, GraphT <: ReadOnlyEGraph[NodeT]](graph: GraphT,
                                                  machine: MutableMachineState[NodeT],
                                                  instructions: List[Instruction[NodeT, GraphT]],
                                                  continuation: MutableMachineState[NodeT] => Boolean): Unit = {
    tryRunCPSWithoutFailure(graph, machine, instructions, continuation)
  }

  /**
   * Runs a pattern-matching virtual machine on a graph with a given initial machine state and list of instructions.
   * Returns all successful runs of the machine. Unsuccessful runs are not listed.
   *
   * @param graph        The graph to match against.
   * @param machine      The initial machine state. This method takes ownership of the machine and will release it.
   * @param instructions The instructions to apply to the machine.
   * @tparam NodeT  The type of the nodes in the e-graph.
   * @tparam GraphT The type of the e-graph.
   * @return A list of final machine states that result from successfully applying all instructions to the machine.
   */
  def run[NodeT, GraphT <: ReadOnlyEGraph[NodeT]](graph: GraphT,
                                                  machine: MutableMachineState[NodeT],
                                                  instructions: List[Instruction[NodeT, GraphT]]): Seq[MachineState[NodeT]] = {
    val results = Seq.newBuilder[MachineState[NodeT]]
    tryRunCPSWithoutFailure(graph, machine, instructions,
      onSuccess = (finalMachine: MutableMachineState[NodeT]) => {
        results += finalMachine.freeze()
        true // Continue searching for more results
      }
    )
    results.result()
  }

  /**
   * Runs a pattern-matching virtual machine on a graph with a given initial machine state and list of instructions.
   * Lists all successfully and unsuccessfully terminated machine runs.
   *
   * @param graph        The graph to match against.
   * @param machine      The initial machine state. This method takes ownership of the machine and will release it.
   * @param instructions The instructions to apply to the machine.
   * @tparam NodeT  The type of the nodes in the e-graph.
   * @tparam GraphT The type of the e-graph.
   * @return A list of successful and unsuccessful runs of the machine obtained by applying all instructions to the
   *         machine.
   */
  def tryRun[NodeT, GraphT <: ReadOnlyEGraph[NodeT]](graph: GraphT,
                                                     machine: MutableMachineState[NodeT],
                                                     instructions: List[Instruction[NodeT, GraphT]]): Seq[MachineResult[NodeT, GraphT]] = {
    val results = Seq.newBuilder[MachineResult[NodeT, GraphT]]
    tryRunCPS(graph, machine, instructions,
      onSuccess = (finalMachine: MutableMachineState[NodeT]) => {
        results += MachineResult.Success(finalMachine.freeze())
        finalMachine.release()
      },
      onFailure = (failedMachine: MutableMachineState[NodeT], error: MachineError[NodeT], remainingInstructions: List[Instruction[NodeT, GraphT]]) => {
        results += MachineResult.Failure(failedMachine.freeze(), error, remainingInstructions)
        failedMachine.release()
      }
    )
    results.result()
  }

  private def tryRunCPS[NodeT, GraphT <: ReadOnlyEGraph[NodeT]](graph: GraphT,
                                                                machine: MutableMachineState[NodeT],
                                                                instructions: List[Instruction[NodeT, GraphT]],
                                                                onSuccess: MutableMachineState[NodeT] => Unit,
                                                                onFailure: (MutableMachineState[NodeT], MachineError[NodeT], List[Instruction[NodeT, GraphT]]) => Unit): Unit = instructions match {
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

  /**
   * A continuation-passing style implementation of the pattern-matching virtual machine that does not report failures.
   * Instead, it simply ignores failures and continues searching for other successful runs of the machine.
   *
   * This is more efficient than the version that reports failures, as it avoids the overhead of constructing
   * failure results.
   *
   * The continuation is called for each successful run of the machine. If the continuation returns false,
   * the search is stopped.
   *
   * @param graph The graph to match against.
   * @param machine The initial machine state.
   * @param instructions The instructions to apply to the machine.
   * @param onSuccess A continuation that is called for each successful run of the machine. If the continuation
   *                  returns false, the search is stopped.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam GraphT The type of the e-graph.
   * @return True if the search completed without the continuation requesting to stop, false otherwise.
   */
  private def tryRunCPSWithoutFailure[NodeT, GraphT <: ReadOnlyEGraph[NodeT]](graph: GraphT,
                                                                              machine: MutableMachineState[NodeT],
                                                                              instructions: List[Instruction[NodeT, GraphT]],
                                                                              onSuccess: MutableMachineState[NodeT] => Boolean): Boolean = instructions match {
    case Nil => onSuccess(machine)
    case firstInstruction :: remainingInstructions =>
      firstInstruction.execute(graph, machine) match {
        case Left(newMachines) if newMachines.size == 1 =>
          tryRunCPSWithoutFailure(graph, newMachines.head, remainingInstructions, onSuccess)

        case Left(newMachines) =>
          for (newMachine <- newMachines) {
            if (!tryRunCPSWithoutFailure(graph, newMachine, remainingInstructions, onSuccess)) {
              // Continuation requested to stop searching. Do not release the machine, as it is
              // owned by the continuation.
              return false
            }
          }
          true

        case Right(_) =>
          /* On failure, release the machine and return true to continue searching. */
          machine.release()
          true
      }
  }
}
