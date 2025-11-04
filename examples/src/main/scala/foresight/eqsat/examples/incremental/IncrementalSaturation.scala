package foresight.eqsat.examples.incremental

import foresight.eqsat.extraction.CostAnalysis
import foresight.eqsat.metadata.AnalysisMetadata
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.rewriting.{EClassSearcher, Rule, Searcher}
import foresight.eqsat.rewriting.patterns.{CompiledPattern, Instruction, MachineEClassSearcher, PatternMatch}
import foresight.eqsat.{EClassCall, EClassRef, ENode}
import foresight.eqsat.readonly.{EGraph, EGraphWithMetadata}
import foresight.util.collections.UnsafeSeqFromArray

import scala.collection.mutable.ArrayBuffer

object IncrementalSaturation {
  def isLatestVersion[NodeT, EGraphT <: EGraph[NodeT]](
    eclass: EClassRef,
    egraph: EGraphWithMetadata[NodeT, EGraphT],
    versionMetadataName: String
  ): Boolean = {
    val meta = egraph.getMetadata[VersionMetadata[NodeT]](versionMetadataName)
    meta.isLatestVersion(eclass)
  }

  def isTopK[NodeT, EGraphT <: EGraph[NodeT], C](
    node: ENode[NodeT],
    eclass: EClassCall,
    egraph: EGraphWithMetadata[NodeT, EGraphT],
    k: Int,
    costAnalysis: CostAnalysis[NodeT, C]
  ): Boolean = {
    val nodes = egraph.nodes(eclass)
    if (nodes.size <= k) {
      // If there are k or fewer nodes in the e-class, all nodes are permissible
      true
    } else {
      // Otherwise, only the top-k cheapest nodes are permissible
      val costs = egraph.getMetadata[AnalysisMetadata[NodeT, C]](costAnalysis.name)

      def nodeCost(node: ENode[NodeT]): C = {
        costAnalysis.make(node, node.args.map(arg => costs.results(arg.ref)))
      }

      val nodeCosts = nodes.toSeq.map(nodeCost)
      val cutoff = nodeCosts.sorted(costAnalysis.costOrdering)(k)
      costAnalysis.costOrdering.lt(nodeCost(node), cutoff)
    }
  }

  def makeIncremental[
    NodeT,
    EGraphT <: EGraph[NodeT],
    C
  ]
  (
    rule: Rule[NodeT, PatternMatch[NodeT], EGraphWithMetadata[NodeT, EGraphT]],
    k: Int,
    versionMetadataName: String,
    costAnalysis: CostAnalysis[NodeT, C]
  ): Rule[NodeT, PatternMatch[NodeT], EGraphWithMetadata[NodeT, EGraphT]] = {

    rule match {
      case Rule(name, MachineEClassSearcher(pattern: CompiledPattern[NodeT, EGraphT], buildContinuation), applier) =>
        // For machine searchers, convert to an incremental searcher, which only matches on the latest version
        // and for each node binding only matches the top-k cheapest nodes
        Rule(name, toIncrementalSearcher(pattern, k, versionMetadataName, costAnalysis, buildContinuation), applier)

      case Rule(name, searcher, applier) =>
        // For other searchers, filter to only apply on the latest version
        Rule(name, searcher.filter({
          case (m, egraph) => isLatestVersion(m.root.ref, egraph, versionMetadataName)
        }), applier)
    }
  }

  def makeIncremental[
    NodeT,
    EGraphT <: EGraph[NodeT],
    C
  ]
  (
    rules: Seq[Rule[NodeT, PatternMatch[NodeT], EGraphWithMetadata[NodeT, EGraphT]]],
    k: Int,
    versionMetadataName: String,
    costAnalysis: CostAnalysis[NodeT, C]
  ): Seq[Rule[NodeT, PatternMatch[NodeT], EGraphWithMetadata[NodeT, EGraphT]]] = {

    rules.map(makeIncremental(_, k, versionMetadataName, costAnalysis))
  }

  def toIncrementalSearcher[NodeT, EGraphT <: EGraph[NodeT], C]
  (
    pattern: CompiledPattern[NodeT, EGraphWithMetadata[NodeT, EGraphT]],
    k: Int,
    versionMetadataName: String,
    costAnalysis: CostAnalysis[NodeT, C],
    buildContinuation: MachineEClassSearcher[NodeT, EGraphWithMetadata[NodeT, EGraphT]]#ContinuationBuilder
  ): Searcher[NodeT, PatternMatch[NodeT], EGraphWithMetadata[NodeT, EGraphT]] = {
    val newInstructions = ArrayBuffer[Instruction[NodeT, EGraphWithMetadata[NodeT, EGraphT]]]()
    var bindings = 0
    for (instr <- pattern.instructions) {
      newInstructions.append(instr)
      instr match {
        case Instruction.BindNode(register, nodeType, definitions, uses, arity) =>
          newInstructions.append(LatestVersionOrTopKInstruction(register, bindings, k, versionMetadataName, costAnalysis))
          bindings += 1
        case _ =>
      }
    }

    val updatedPattern = CompiledPattern(pattern.pattern, newInstructions.toSeq)

    final case class IncrementalMachineSearcher(buildContinuation: MachineEClassSearcher[NodeT, EGraphWithMetadata[NodeT, EGraphT]]#ContinuationBuilder)
      extends EClassSearcher[NodeT, PatternMatch[NodeT], EGraphWithMetadata[NodeT, EGraphT]] {

      override def search(egraph: EGraphWithMetadata[NodeT, EGraphT], parallelize: ParallelMap): Unit = {
        val meta = egraph.getMetadata[VersionMetadata[NodeT]](versionMetadataName)
        val latest = UnsafeSeqFromArray(meta.latestVersionClasses.map(egraph.canonicalize).toArray)
        search(latest, egraph, parallelize)
      }

      protected override def search(call: EClassCall, egraph: EGraphWithMetadata[NodeT, EGraphT], continuation: Continuation): Unit = {
        if (!isLatestVersion(call.ref, egraph, versionMetadataName)) {
          // Skip searching this e-class if it's not the latest version
          return
        }

        updatedPattern.search(call, egraph, continuation)
      }

      /**
       * Returns a new instance of this searcher-like object with the specified continuation *builder*.
       *
       * @param continuation The new continuation builder to use.
       * @return A new instance of this searcher-like object with the updated continuation builder.
       */
      override def withContinuationBuilder(continuation: ContinuationBuilder): EClassSearcher[NodeT, PatternMatch[NodeT], EGraphWithMetadata[NodeT, EGraphT]] = {
        IncrementalMachineSearcher(continuation)
      }
    }

    IncrementalMachineSearcher(buildContinuation)
  }
}
