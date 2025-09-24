package foresight.eqsat.examples.incremental

import foresight.eqsat.extraction.CostAnalysis
import foresight.eqsat.metadata.{AnalysisMetadata, EGraphWithMetadata}
import foresight.eqsat.rewriting.patterns.{Instruction, MachineError, MachineState, MutableMachineState}
import foresight.eqsat.{EClassCall, EGraph, EGraphLike, ENode}

final case class LatestVersionOrTopKInstruction[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT], C]
(
  register: Int,
  nodeIndex: Int,
  k: Int,
  versionMetadataName: String,
  costAnalysis: CostAnalysis[NodeT, C]
) extends Instruction[NodeT, EGraphWithMetadata[NodeT, EGraphT]] {
  require(k > 0, "k must be greater than 0")

  override def effects: Instruction.Effects = Instruction.Effects.none

  /**
   * Executes the instruction on the given machine state.
   *
   * @param ctx The execution context for running instructions.
   * @return true to continue search, false to abort.
   */
  override def execute(ctx: Instruction.Execution[NodeT, EGraphWithMetadata[NodeT, EGraphT]]): Boolean = {
    val egraph = ctx.graph
    val node = ctx.machine.boundNodeAt(nodeIndex)
    val eclass = ctx.machine.registerAt(register)

    if (IncrementalSaturation.isLatestVersion(eclass.ref, egraph, versionMetadataName)
      || IncrementalSaturation.isTopK(node, eclass, egraph, k, costAnalysis)) {

      ctx.continue()
    } else {
      ctx.error(NotLatestVersionOrTopKError(this, node, eclass))
    }
  }
}
