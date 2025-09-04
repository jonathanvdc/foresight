package foresight.eqsat.examples.incremental

import foresight.eqsat.extraction.CostAnalysis
import foresight.eqsat.metadata.{AnalysisMetadata, EGraphWithMetadata}
import foresight.eqsat.rewriting.patterns.{Instruction, MachineError, MachineState}
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

  override def execute(egraph: EGraphWithMetadata[NodeT, EGraphT], state: MachineState[NodeT]): Either[Set[MachineState[NodeT]], MachineError[NodeT]] = {
    val node = state.boundNodes(nodeIndex)
    val eclass = state.registers(register)

    if (IncrementalSaturation.isLatestVersion(eclass.ref, egraph, versionMetadataName)
      || IncrementalSaturation.isTopK(node, eclass, egraph, k, costAnalysis)) {

      Left(Set(state))
    } else {
      Right(NotLatestVersionOrTopKError(this, node, eclass))
    }
  }
}
