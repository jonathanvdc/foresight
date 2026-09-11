package foresight.eqsat.rewriting.patterns

import foresight.eqsat.EClassCall
import foresight.eqsat.readonly.EGraph

/**
 * An error that occurs during the execution of a pattern machine.
 *
 * @tparam NodeT The type of the nodes in the e-graph.
 */
trait MachineError[NodeT]

/**
 * A companion object for the `MachineError` trait.
 */
object MachineError {
  /**
   * An error that occurs when the fully bound subtree of a `Lookup` instruction is absent from the e-graph
   * or does not match the expected e-class call.
   * @param instruction The `Lookup` instruction that failed to find a matching subtree.
   * @param call The e-class call that the subtree was expected to match.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph.
   */
  final case class LookupFailed[NodeT, EGraphT <: EGraph[NodeT]](
    instruction: Instruction.Lookup[NodeT, EGraphT], call: EClassCall
  ) extends MachineError[NodeT]

  /**
   * An error that occurs when no e-node in an e-class matches the node type, definitions, uses, and argument count
   * of a `BindNode` instruction.
   * @param instruction The `BindNode` instruction that failed to find a matching e-node.
   * @param call The e-class call that was searched for a matching e-node.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph.
   */
  final case class NoMatchingNode[NodeT, EGraphT <: EGraph[NodeT]](instruction: Instruction.BindNode[NodeT, EGraphT],
                                                                   call: EClassCall)
    extends MachineError[NodeT]

  /**
   * An error that occurs when two e-class calls are expected to be equal but are not.
   * @param instruction The `Compare` instruction that failed to find equal e-class calls.
   * @param first The first e-class call.
   * @param second The second e-class call.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph.
   */
  final case class InconsistentVars[NodeT, EGraphT <: EGraph[NodeT]](instruction: Instruction.Compare[NodeT, EGraphT],
                                                                     first: EClassCall,
                                                                     second: EClassCall)
    extends MachineError[NodeT]
}
