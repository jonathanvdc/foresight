package foresight.eqsat.examples.arith.tests

import foresight.eqsat.examples.arith.Number.toMixedTree
import foresight.eqsat.{EGraph, Slot}
import foresight.eqsat.examples.arith.{Add, ArithIR, ConstantAnalysis, Number, Rules, Var}
import foresight.eqsat.examples.arith.Rules.ArithRule
import foresight.eqsat.extraction.ExtractionAnalysis
import foresight.eqsat.saturation.{MaximalRuleApplicationWithCaching, Strategy}
import org.junit.Test

class RuleTests {
  private def strategy(iterationLimit: Int, rules: Seq[ArithRule] = Rules.all): Strategy[EGraph[ArithIR], Unit] =
    MaximalRuleApplicationWithCaching(rules)
      .withIterationLimit(iterationLimit)
      .untilFixpoint
      .closeRecording
      .addAnalysis(ExtractionAnalysis.smallest[ArithIR])
      .addAnalysis(ConstantAnalysis)
      .closeMetadata
      .dropData

//  @Test
//  def simplifyAddZeroRight(): Unit = {
//    val egraph = EGraph.empty[ArithIR]
//
//    val x = Var(Slot.fresh())
//    val zero = toMixedTree(Number(0))
//    val sum = Add(x, zero)
//
//    val (_, egraph2) = egraph.add(sum)
//
//    val egraph3 = strategy(1)(egraph2).get
//
//    assert(egraph3.areSame(egraph3.find(x).get, egraph3.find(sum).get))
//  }
}
