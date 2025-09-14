package foresight.eqsat.examples.incremental

import foresight.eqsat.extraction.CostAnalysis
import foresight.eqsat.metadata.{AnalysisMetadata, EGraphWithMetadata}
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.rewriting.{ReversibleSearcher, Rule, Searcher}
import foresight.eqsat.rewriting.patterns.{CompiledPattern, Instruction, MachineSearcherPhase, PatternMatch}
import foresight.eqsat.{EClassCall, EClassRef, EGraph, EGraphLike, ENode}

import scala.collection.mutable.ArrayBuffer

object IncrementalSaturation {
  def isLatestVersion[NodeT, EGraphT <: EGraph[NodeT] with EGraphLike[NodeT, EGraphT]](
    eclass: EClassRef,
    egraph: EGraphWithMetadata[NodeT, EGraphT],
    versionMetadataName: String
  ): Boolean = {
    val meta = egraph.getMetadata[VersionMetadata[NodeT]](versionMetadataName)
    val eclassVersion = meta.data(eclass)
    eclassVersion == meta.version
  }

  def isTopK[NodeT, EGraphT <: EGraph[NodeT] with EGraphLike[NodeT, EGraphT], C](
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
    EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT],
    C
  ]
  (
    rule: Rule[NodeT, PatternMatch[NodeT], EGraphWithMetadata[NodeT, EGraphT]],
    k: Int,
    versionMetadataName: String,
    costAnalysis: CostAnalysis[NodeT, C]
  ): Rule[NodeT, PatternMatch[NodeT], EGraphWithMetadata[NodeT, EGraphT]] = {

    rule match {
      case Rule(name, ReversibleSearcher.SinglePhaseReversibleSearcher(MachineSearcherPhase(pattern)), applier) =>
        // For machine searchers, convert to an incremental searcher, which only matches on the latest version
        // and for each node binding only matches the top-k cheapest nodes
        Rule(name, toIncrementalSearcher(pattern, k, versionMetadataName, costAnalysis), applier)

      case Rule(name, searcher, applier) =>
        // For other searchers, filter to only apply on the latest version
        Rule(name, searcher.filter({
          case (m, egraph) => isLatestVersion(m.root.ref, egraph, versionMetadataName)
        }), applier)
    }
  }

  def makeIncremental[
    NodeT,
    EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT],
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

  def toIncrementalSearcher[NodeT, EGraphT <: EGraph[NodeT] with EGraphLike[NodeT, EGraphT], C]
  (
    pattern: CompiledPattern[NodeT, EGraphWithMetadata[NodeT, EGraphT]],
    k: Int,
    versionMetadataName: String,
    costAnalysis: CostAnalysis[NodeT, C]
  ): Searcher[NodeT, Seq[PatternMatch[NodeT]], EGraphWithMetadata[NodeT, EGraphT]] = {
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

    val updatedPattern = CompiledPattern(pattern.pattern, newInstructions.toList)

    new Searcher[NodeT, Seq[PatternMatch[NodeT]], EGraphWithMetadata[NodeT, EGraphT]] {
      override def search(egraph: EGraphWithMetadata[NodeT, EGraphT], parallelize: ParallelMap): Seq[PatternMatch[NodeT]] = {
        val classes = egraph.classes
        val metadata = egraph.getMetadata[VersionMetadata[NodeT]](versionMetadataName)
        val currentVersion = metadata.version
        val classesAtCurrentVersion = classes.filter(c => metadata.data(c) == currentVersion)
        val searchClass = (c: EClassRef) => {
          updatedPattern.search(egraph.canonicalize(c), egraph)
        }
        parallelize(classesAtCurrentVersion, searchClass).flatten.toSeq
      }
    }
  }
}
