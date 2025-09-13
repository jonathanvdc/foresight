package foresight.eqsat.examples.arithWithLang

import foresight.eqsat.examples.arithWithLang.{*, given}
import foresight.eqsat.extraction.ExtractionAnalysis
import foresight.eqsat.lang.*
import foresight.eqsat.saturation.{MaximalRuleApplicationWithCaching, Strategy}
import foresight.eqsat.{EClassCall, EGraph, Slot}
import org.junit.Test

import scala.language.implicitConversions

class RuleTests {
  val L: Language[ArithExpr] = summon[Language[ArithExpr]]
  val R: Rules = Rules()(using L)
  type ArithRule = R.ArithRule

  private def strategy(iterationLimit: Int, rules: Seq[ArithRule] = R.all): Strategy[ArithIR, EGraph[ArithIR], Unit] =
    MaximalRuleApplicationWithCaching(rules)
      .withIterationLimit(iterationLimit)
      .repeatUntilStable
      .closeRecording
      .addAnalyses(ExtractionAnalysis.smallest[ArithIR], ConstantAnalysis)
      .closeMetadata
      .dropData

  private def strategies: Seq[Strategy[ArithIR, EGraph[ArithIR], Unit]] =
    Seq(strategy(12))

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

  def addChain(slots: Seq[Slot]): ArithExpr = {
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

  @Test
  def strengthReduceAndExtract(): Unit = {
    val x = Var(Slot.fresh())

    // Desired identity: 1 * 3 * x + 3 * x = 6 * x.
    val expr = 1 * (3 * x) + 3 * x
    val expected = Seq(6 * x, x * 6)

    // Construct an e-graph from the expression and saturate it.
    val (root, egraph) = L.toEGraph(expr)
    val egraph2 = strategies.head(egraph).get

    val cost = new LanguageCostFunction[ArithExpr, Int] {
      override def apply(expr: ArithExpr): Int = expr match {
        case Number(_) => 1
        case Var(_) => 1
        case Add(Fact(l: Int), Fact(r: Int)) => 2 + l + r
        case Mul(Fact(l: Int), Fact(r: Int)) => 4 + l + r
        case _ => throw new IllegalArgumentException(s"Unexpected node in expression: $expr")
      }
    }

    val extracted = egraph2.extract(root, cost)
    assert(expected.contains(extracted), s"Expected one of: $expected, but got: $extracted")
  }
}
