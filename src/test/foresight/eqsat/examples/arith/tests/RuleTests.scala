package foresight.eqsat.examples.arith.tests

import foresight.eqsat.{EClassCall, EGraph, MixedTree, Slot}
import foresight.eqsat.examples.arith.{Add, ArithIR, ConstantAnalysis, Mul, Rules, Var}
import foresight.eqsat.examples.arith.Rules.ArithRule
import foresight.eqsat.extraction.ExtractionAnalysis
import foresight.eqsat.saturation.{MaximalRuleApplicationWithCaching, Strategy}
import org.junit.Test

class RuleTests {
  private def strategy(iterationLimit: Int, rules: Seq[ArithRule] = Rules.all): Strategy[EGraph[ArithIR], Unit] =
    MaximalRuleApplicationWithCaching(rules)
      .withIterationLimit(iterationLimit)
      .repeatUntilStable
      .closeRecording
      .addAnalysis(ExtractionAnalysis.smallest[ArithIR], ConstantAnalysis)
      .closeMetadata
      .dropData

  private def strategies: Seq[Strategy[EGraph[ArithIR], Unit]] =
    Seq(strategy(10))

  /**
   * Test that addition is associative.
   */
  @Test
  def additionIsAssociative(): Unit = {
    // x + y = y + x
    val x = Var(Slot.fresh())
    val y = Var(Slot.fresh())
    val xPlusY = Add(x, y)
    val yPlusX = Add(y, x)

    val egraph = EGraph.empty[ArithIR]
    val (c1, egraph2) = egraph.add(xPlusY)
    val (c2, egraph3) = egraph2.add(yPlusX)

    assert(!egraph3.areSame(c1, c2))

    for (strategy <- strategies) {
      val Some(egraph4) = strategy(egraph3)
      assert(egraph4.areSame(c1, c2))
    }
  }

  @Test
  def additionIsAssociative2(): Unit = {
    // (x + y) * (x + y) = (x + y) * (y + x)
    val x = Var(Slot.fresh())
    val y = Var(Slot.fresh())
    val xPlusY = Add(x, y)
    val yPlusX = Add(y, x)
    val xPlusYTimesXPlusY = Mul(xPlusY, xPlusY)
    val xPlusYTimesYPlusX = Mul(xPlusY, yPlusX)

    val egraph = EGraph.empty[ArithIR]
    val (c1, egraph2) = egraph.add(xPlusYTimesXPlusY)
    val (c2, egraph3) = egraph2.add(xPlusYTimesYPlusX)

    assert(!egraph3.areSame(c1, c2))

    for (strategy <- strategies) {
      val Some(egraph4) = strategy(egraph3)
      assert(egraph4.areSame(c1, c2))
    }
  }

  @Test
  def distributivity(): Unit = {
    // (x+y) * (y+z) = (z+y) * (y+x)
    val x = Var(Slot.fresh())
    val y = Var(Slot.fresh())
    val z = Var(Slot.fresh())
    val xPlusY = Add(x, y)
    val yPlusZ = Add(y, z)
    val zPlusY = Add(z, y)
    val yPlusX = Add(y, x)
    val xPlusYTimesYPlusZ = Mul(xPlusY, yPlusZ)
    val zPlusYTimesYPlusX = Mul(zPlusY, yPlusX)

    val egraph = EGraph.empty[ArithIR]
    val (c1, egraph2) = egraph.add(xPlusYTimesYPlusZ)
    val (c2, egraph3) = egraph2.add(zPlusYTimesYPlusX)

    assert(!egraph3.areSame(c1, c2))

    for (strategy <- strategies) {
      val Some(egraph4) = strategy(egraph3)
      assert(egraph4.areSame(c1, c2))
    }
  }

  @Test
  def squareOfSum(): Unit = {
    // (x+y)**2 = x**2 + x*y + x*y + y**2
    val x = Var(Slot.fresh())
    val y = Var(Slot.fresh())
    val lhs = Mul(Add(x, y), Add(x, y))
    val rhs = Add(Mul(x, x), Add(Mul(x, y), Add(Mul(y, x), Mul(y, y))))

    val egraph = EGraph.empty[ArithIR]
    val (c1, egraph2) = egraph.add(lhs)
    val (c2, egraph3) = egraph2.add(rhs)

    assert(!egraph3.areSame(c1, c2))

    for (strategy <- strategies) {
      val Some(egraph4) = strategy(egraph3)
      assert(egraph4.areSame(c1, c2))
    }
  }

  @Test
  def distributivity2(): Unit = {
    // z*(x+y) = z*(y+x)
    val x = Var(Slot.fresh())
    val y = Var(Slot.fresh())
    val z = Var(Slot.fresh())
    val lhs = Mul(z, Add(x, y))
    val rhs = Mul(z, Add(y, x))

    val egraph = EGraph.empty[ArithIR]
    val (c1, egraph2) = egraph.add(lhs)
    val (c2, egraph3) = egraph2.add(rhs)

    assert(!egraph3.areSame(c1, c2))

    for (strategy <- strategies) {
      val Some(egraph4) = strategy(egraph3)
      assert(egraph4.areSame(c1, c2))
    }
  }

  def addChain(slots: Seq[Slot]): MixedTree[ArithIR, EClassCall] = {
    slots.map(s => Var(s)).reduceLeft(Add(_, _))
  }

  @Test
  def rearrangeChains(): Unit = {
    // x0+...+xN = xN+...+x0
    val N = 5

    val slots = (0 to N).map(_ => Slot.fresh())

    val a = addChain(slots)
    val b = addChain(slots.reverse)

    val egraph = EGraph.empty[ArithIR]
    val (c1, egraph2) = egraph.add(a)
    val (c2, egraph3) = egraph2.add(b)

    assert(!egraph3.areSame(c1, c2))

    for (strategy <- strategies) {
      val Some(egraph4) = strategy(egraph3)
      assert(egraph4.areSame(c1, c2))
    }
  }
}
