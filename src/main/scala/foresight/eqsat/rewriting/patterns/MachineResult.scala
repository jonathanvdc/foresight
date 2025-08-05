package foresight.eqsat.rewriting.patterns

import foresight.eqsat.{EGraph, EGraphLike}

/**
 * The result of executing a pattern machine.
 *
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam EGraphT The type of the e-graph.
 */
sealed trait MachineResult[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]]

/**
 * A companion object for the `MachineResult` trait.
 */
object MachineResult {
  /**
   * A successful result of executing a pattern machine.
   * @param state The final machine state.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph.
   */
  final case class Success[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](state: MachineState[NodeT])
    extends MachineResult[NodeT, EGraphT]

  /**
   * A failed result of executing a pattern machine.
   * @param state The machine state at the time of failure.
   * @param error The error that caused the failure.
   * @param remainingInstructions The instructions that were not executed, starting with the instruction that caused the
   *                              failure.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph.
   */
  final case class Failure[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](state: MachineState[NodeT],
                                                                                            error: MachineError[NodeT],
                                                                                            remainingInstructions: List[Instruction[NodeT, EGraphT]])
    extends MachineResult[NodeT, EGraphT]
}
