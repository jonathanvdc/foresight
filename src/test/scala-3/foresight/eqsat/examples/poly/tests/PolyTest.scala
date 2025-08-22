package foresight.eqsat.examples.poly.tests

import foresight.eqsat.EGraph
import foresight.eqsat.examples.poly.*
import foresight.eqsat.lang.*
import foresight.eqsat.saturation.{MaximalRuleApplication, MaximalRuleApplicationWithCaching, Strategy}
import org.junit.Test

import scala.language.implicitConversions

class PolyTest {
  type ArithRule = R.ArithRule
  val L: Language[ArithExpr] = summon[Language[ArithExpr]]
  val R: Rules = Rules()(using L)

  def strategies: Seq[Strategy[ArithIR, EGraph[ArithIR], Unit]] = Seq(simpleStrategy, cachingStrategy)

  def simpleStrategy: Strategy[ArithIR, EGraph[ArithIR], Unit] = MaximalRuleApplication(R.all).repeatUntilStable

  def cachingStrategy: Strategy[ArithIR, EGraph[ArithIR], Unit] = MaximalRuleApplicationWithCaching(R.all).repeatUntilStable.closeRecording

  @Test
  def testPoly2(): Unit = {
    // polynomial of degree 2: ax^2 + bx + c
    val c0 = Zero // 0
    val c1 = Succ(c0) // 1
    val c2 = Succ(c1) // 2

    val x = Var("x")
    val a = Var("a")
    val b = Var("b")
    val c = Var("c")

    val poly2 = a * (x ** c2) + b * x + c
    val poly2Simplified = c + x * (b + a * x)

    val egraph = EGraph.empty[ArithIR]
    val (class1, egraph2) = egraph.add(poly2)
    val (class2, egraph3) = egraph2.add(poly2Simplified)

    assert(!egraph3.areSame(class1, class2))

    for (strategy <- strategies) {
      val Some(egraph4) = strategy(egraph3)
      assert(egraph4.areSame(class1, class2))
    }
  }

  @Test
  def testPoly5(): Unit = {
    // polynomial of degree 5: ax^5 + bx^4 + cx^3 + dx^2 + ex + f
    val c0 = Zero // 0
    val c1 = Succ(c0) // 1
    val c2 = Succ(c1) // 2
    val c3 = Succ(c2) // 3
    val c4 = Succ(c3) // 4
    val c5 = Succ(c4) // 5

    val x = Var("x")
    val a = Var("a")
    val b = Var("b")
    val c = Var("c")
    val d = Var("d")
    val e = Var("e")
    val f = Var("f")

    val poly5 = a * (x ** c5) + b * (x ** c4) + c * (x ** c3) + d * (x ** c2) + e * x + f
    val poly5Simplified = f + x * (e + x * (d + x * (c + x * (b + a * x))))

    val egraph = EGraph.empty[ArithIR]
    val (class1, egraph2) = egraph.add(poly5)
    val (class2, egraph3) = egraph2.add(poly5Simplified)

    assert(!egraph3.areSame(class1, class2))

    for (strategy <- strategies) {
      val Some(egraph4) = strategy(egraph3)
      assert(egraph4.areSame(class1, class2))
    }
  }
}