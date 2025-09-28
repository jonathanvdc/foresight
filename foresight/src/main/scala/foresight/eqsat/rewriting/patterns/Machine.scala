package foresight.eqsat.rewriting.patterns

import foresight.eqsat.readonly.EGraph
import scala.collection.compat._

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
  def run[NodeT, GraphT <: EGraph[NodeT]](graph: GraphT,
                                          machine: MutableMachineState[NodeT],
                                          instructions: immutable.ArraySeq[Instruction[NodeT, GraphT]],
                                          continuation: MutableMachineState[NodeT] => Boolean): Unit = {
    Instruction.Execution.pool.run(graph, machine, instructions, continuation)
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
  def run[NodeT, GraphT <: EGraph[NodeT]](graph: GraphT,
                                          machine: MutableMachineState[NodeT],
                                          instructions: immutable.ArraySeq[Instruction[NodeT, GraphT]]): Seq[MachineState[NodeT]] = {
    val results = Seq.newBuilder[MachineState[NodeT]]
    run(graph, machine, instructions, (finalMachine: MutableMachineState[NodeT]) => {
        results += finalMachine.freeze()
        finalMachine.release()
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
  def tryRun[NodeT, GraphT <: EGraph[NodeT]](graph: GraphT,
                                             machine: MutableMachineState[NodeT],
                                             instructions: immutable.ArraySeq[Instruction[NodeT, GraphT]]): Seq[MachineResult[NodeT, GraphT]] = {
    val results = Seq.newBuilder[MachineResult[NodeT, GraphT]]
    Instruction.Execution.pool.run(graph, machine, instructions,
      onSuccess = (finalMachine: MutableMachineState[NodeT]) => {
        results += MachineResult.Success(finalMachine.freeze())
        finalMachine.release()
        true // Continue searching for more results
      },
      onFailure = (ctx: Instruction.Execution[NodeT, GraphT], error: MachineError[NodeT]) => {
        results += MachineResult.Failure(ctx.machine.freeze(), error, instructions.slice(0, ctx.instructionPointer))
        ctx.terminate()
        true // Continue searching for more results
      }
    )
    results.result()
  }
}
