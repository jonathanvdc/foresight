package foresight.eqsat.examples.poly

import foresight.eqsat.lang._
import foresight.eqsat.saturation.{MaximalRuleApplication, MaximalRuleApplicationWithCaching, Strategy}
import foresight.eqsat.{EClassCall, EGraph}
import org.junit.Test

import scala.language.implicitConversions

class PolyTest {
  type ArithRule = R.ArithRule
  val L: Language[ArithExpr] = summon[Language[ArithExpr]]
  val R: Rules = Rules()(using L)

  val simpleStrategy: Strategy[ArithIR, EGraph[ArithIR], Unit] = MaximalRuleApplication(R.all).repeatUntilStable

  val cachingStrategy: Strategy[ArithIR, EGraph[ArithIR], Unit] = MaximalRuleApplicationWithCaching(R.all).repeatUntilStable.closeRecording

  val strategies: Seq[Strategy[ArithIR, EGraph[ArithIR], Unit]] = Seq(simpleStrategy, cachingStrategy)

  val costFunction: LanguageCostFunction[ArithExpr, Int] = new LanguageCostFunction[ArithExpr, Int]() {
    override def apply(expr: ArithExpr): Int = {
      expr match {
        case Add(lhs, rhs) => apply(lhs) + apply(rhs) + 10
        case Mul(lhs, rhs) => apply(lhs) + apply(rhs) + 100
        case Pow(lhs, rhs) => apply(lhs) + apply(rhs) + 1000
        case Fact(cost: Int) => cost
        case _ => 1
      }
    }
  }

  @Test
  def testPoly2Saturation(): Unit = {
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
  def testPoly2Extraction(): Unit = {
    // polynomial of degree 2: ax^2 + bx + c
    val c0 = Zero // 0
    val c1 = Succ(c0) // 1
    val c2 = Succ(c1) // 2

    val x = Var("x")
    val a = Var("a")
    val b = Var("b")
    val c = Var("c")

    val poly2 = a * (x ** c2) + b * x + c
    val poly2Expanded = (a * x * x) + (b * x) + c
    val poly2Simplified = c + x * (b + a * x)

    val (root, egraph) = L.toEGraph(poly2)
    val egraph2 = cachingStrategy(egraph).get

    val poly2Cost = costFunction(poly2)
    val poly2ExpandedCost = costFunction(poly2Expanded)
    val poly2SimplifiedCost = costFunction(poly2Simplified)

    assert(poly2Cost == 1226, s"Expected cost 128 but got $poly2Cost")
    assert(poly2ExpandedCost == 326, s"Expected cost 326 but got $poly2ExpandedCost")
    assert(poly2SimplifiedCost == 225, s"Expected cost 23 but got $poly2SimplifiedCost")

    val extracted = egraph2.extract(root, costFunction)
    assert(extracted == poly2Simplified, s"Expected $poly2Simplified but got $extracted")
  }

  @Test
  def testPoly5Saturation(): Unit = {
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

  @Test
  def testPoly5Extraction(): Unit = {
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

    val (root, egraph) = L.toEGraph(poly5)
    val egraph2 = simpleStrategy(egraph).get

    val extracted = egraph2.extract(root, costFunction)
    assert(extracted == poly5Simplified, s"Expected $poly5Simplified but got $extracted")
  }

  @Test
  def testPoly5Bench(): Unit = {
    type ArithRule = R.ArithRule
    val L: Language[ArithExpr] = summon[Language[ArithExpr]]
    val R: Rules = Rules()(using L)
    val simpleStrategy: Strategy[ArithIR, EGraph[ArithIR], Unit] = MaximalRuleApplication(R.all).repeatUntilStable
    val costFunction: LanguageCostFunction[ArithExpr, Int] = new LanguageCostFunction[ArithExpr, Int]() {
      override def apply(expr: ArithExpr): Int = {
        expr match {
          case Add(lhs, rhs) => apply(lhs) + apply(rhs) + 10
          case Mul(lhs, rhs) => apply(lhs) + apply(rhs) + 100
          case Pow(lhs, rhs) => apply(lhs) + apply(rhs) + 1000
          case Fact(cost: Int) => cost
          case _ => 1
        }
      }
    }

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

    val (root, egraph) = L.toEGraph(poly5)
    val egraph2 = simpleStrategy(egraph).get

    val extracted = egraph2.extract(root, costFunction)
  }

//  @Test
//  def benchPoly5(): Unit = {
//    val time = 60_000_000_000L
//    val start = System.nanoTime()
//    var times: List[Long] = List()
//    var iterations = 0
//    while (System.nanoTime() - start < time) {
//      val testStart = System.nanoTime()
//      testPoly5Bench()
//      val testEnd = System.nanoTime()
//      val duration = testEnd - testStart
////      println(s"Iteration $iterations took $duration ms")
//      times = duration :: times
//      iterations += 1
//    }
//    println(s"Completed $iterations iterations in 60 seconds")
//    val avgTime = if (times.nonEmpty) times.sum / times.length else 0
//    println(s"Average time per iteration: ${avgTime/1e6} ms")
//  }
}