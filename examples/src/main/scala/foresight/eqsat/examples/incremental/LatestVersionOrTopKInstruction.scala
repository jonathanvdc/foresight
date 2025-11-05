package foresight.eqsat.examples.incremental

import foresight.eqsat.rewriting.patterns.Instruction
import foresight.eqsat.readonly.{EGraph, EGraphWithMetadata}

final case class LatestVersionOrTopKInstruction[NodeT, EGraphT <: EGraph[NodeT], C]
(
  register: Int,
  nodeIndex: Int,
  versionMetadataName: String,
  costAnalysis: TopKCostAnalysis[NodeT, C]
) extends Instruction[NodeT, EGraphWithMetadata[NodeT, EGraphT]] {
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
      || IncrementalSaturation.isTopK(node, eclass, egraph, costAnalysis)) {

      ctx.continue()
    } else {
      ctx.error(NotLatestVersionOrTopKError(this, node, eclass))
    }
  }
}
