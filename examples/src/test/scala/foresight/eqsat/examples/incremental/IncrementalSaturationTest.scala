package foresight.eqsat.examples.incremental

import foresight.eqsat.examples.arith._
import foresight.eqsat.extraction.{CostAnalysis, CostFunction, ExtractionAnalysis}
import foresight.eqsat.saturation.{MaximalRuleApplication, Strategy}
import foresight.eqsat.MixedTree
import foresight.eqsat.immutable.{EGraph, EGraphWithMetadata}
import foresight.eqsat.rewriting.patterns.PatternMatch
import org.junit.Test

class IncrementalSaturationTest {
  @Test
  def incrementalSaturationWorks(): Unit = {
    // Define some arithmetic expressions to add to the e-graph
    val terms = Seq(
      Mul(Mul(Number(1), Number(2)), Number(3)),
      Add(Number(3), Mul(Number(1), Number(2)))
    )
    val expected: Seq[MixedTree[ArithIR, Nothing]] = Seq(
      Number(6),
      Number(5)
    )

    // Define the metadata name and cost analysis
    val metadataName = "version"
    val k = 2
    val costAnalysis = TopKCostAnalysis[ArithIR, Int]("cost", k, CostFunction.size)
    val extractionAnalysis = ExtractionAnalysis.smallest[ArithIR]

    val rules = IncrementalSaturation.makeIncremental(Rules.all, metadataName, costAnalysis)
    val strategy: Strategy[EGraphWithMetadata[ArithIR, EGraph[ArithIR]], Unit] =
      MaximalRuleApplication[ArithIR, EGraphWithMetadata[ArithIR, EGraph[ArithIR]], PatternMatch[ArithIR]](rules)
        .withIterationLimit(5)
        .repeatUntilStable

    // Initialize an e-graph with version metadata and a cost analysis
    var egraph = EGraphWithMetadata[ArithIR, EGraph[ArithIR]](EGraph.empty[ArithIR])
      .addMetadata(metadataName, VersionMetadata.empty)
      .addAnalysis(costAnalysis)
      .addAnalysis(extractionAnalysis)
      .addAnalysis(ConstantAnalysis)

    for ((term, expectedTerm) <- terms.zip(expected)) {
      // Add the new term to the e-graph
      val (root, newEgraph) = egraph.add(term)
      egraph = newEgraph

      // Update the version metadata to reflect the addition of the new term
      val versionMetadata = egraph.getMetadata[VersionMetadata[ArithIR]](metadataName)
      egraph = egraph.addMetadata(metadataName, versionMetadata.onNewTermAdded(term, egraph.egraph))

      // Saturate the e-graph
      egraph = strategy(egraph).getOrElse(egraph)

      val found = extractionAnalysis.extractor(root, egraph)
      assert(MixedTree.fromTree(found) == expectedTerm)
    }
  }
}
