package foresight.eqsat.examples.poly

import foresight.eqsat.EGraph
import foresight.eqsat.examples.poly
import foresight.eqsat.lang.*
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.saturation.{MaximalRuleApplication, Strategy}
import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit

// State and scope tell JMH how to manage instances
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class PolyBenchmarks {
  @Benchmark
  def poly5Sequential(): ArithExpr = {
    poly5Bench(ParallelMap.sequential)
  }

  val arithCostFunction: LanguageCostFunction[ArithExpr, Int] = new LanguageCostFunction[ArithExpr, Int]() {
    override def apply(expr: ArithExpr): Int = {
      expr match {
        case poly.Add(lhs, rhs) => apply(lhs) + apply(rhs) + 10
        case poly.Mul(lhs, rhs) => apply(lhs) + apply(rhs) + 100
        case poly.Pow(lhs, rhs) => apply(lhs) + apply(rhs) + 1000
        case poly.Fact(cost: Int) => cost
        case _ => 1
      }
    }
  }

  def poly5Bench(map: ParallelMap): ArithExpr = {
    val L: Language[ArithExpr] = summon[Language[ArithExpr]]
    val R: Rules = poly.Rules()(using L)
    type ArithRule = R.ArithRule

    val simpleStrategy: Strategy[ArithIR, EGraph[ArithIR], Unit] = MaximalRuleApplication(R.all)
      .repeatUntilStable

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
    val egraph2 = simpleStrategy(egraph, map).get

    egraph2.extract(root, arithCostFunction)
  }
}
