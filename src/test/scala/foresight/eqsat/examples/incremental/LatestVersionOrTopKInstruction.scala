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

    if (isPermissible(node, eclass, egraph)) {
      Left(Set(state))
    } else {
      Right(NotLatestVersionOrTopKError(this, node, eclass))
    }
  }

  private def nodeCost(node: ENode[NodeT], costs: AnalysisMetadata[NodeT, C]): C = {
    costAnalysis.make(node, node.args.map(arg => costs.results(arg.ref)))
  }

  private def isPermissible(node: ENode[NodeT],
                            eclass: EClassCall,
                            egraph: EGraphWithMetadata[NodeT, EGraphT]): Boolean = {

    val versionMetadata = egraph.getMetadata[VersionMetadata[NodeT]](versionMetadataName)
    val costs = egraph.getMetadata[AnalysisMetadata[NodeT, C]](costAnalysis.name)

    val globalVersion = versionMetadata.version
    val eClassVersion = versionMetadata.data(eclass.ref)

    if (eClassVersion == globalVersion) {
      // If the e-class containing the node is at the latest version, all nodes are permissible
      true
    } else {
      val nodes = egraph.nodes(eclass)
      if (nodes.size <= k) {
        // If there are k or fewer nodes in the e-class, all nodes are permissible
        true
      } else {
        // Otherwise, only the top-k cheapest nodes are permissible
        val nodeCosts = nodes.toSeq.map(n => nodeCost(n, costs))
        val cutoff = nodeCosts.sorted(costAnalysis.costOrdering)(k - 1)
        costAnalysis.costOrdering.lt(nodeCost(node, costs), cutoff)
      }
    }
  }
}
