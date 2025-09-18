package foresight.eqsat.examples.poly

import foresight.eqsat.EGraph
import foresight.eqsat.examples.poly
import foresight.eqsat.lang._
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.saturation.{MaximalRuleApplication, Strategy}
import foresight.util.BenchmarksWithParallelMap
import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class PolyBenchmarks extends BenchmarksWithParallelMap {
  @Param(Array("5", "6"))
  var size: Int = _

  @Benchmark
  def polynomial(): ArithExpr = {
    polyBench(size, parallelMap)
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

  /**
    * Constructs a polynomial expression of degree `n` in variable `x` with symbolic coefficients.
    * Coefficients are named 'a', 'b', ..., corresponding to each term.
    * The polynomial is built as: a_n * x^n + a_{n-1} * x^{n-1} + ... + a_0
    *
    * @param n Degree of the polynomial
    * @return The constructed polynomial as an `ArithExpr`
    */
  def polynomialExpr(n: Int): ArithExpr = {
    val x = Var("x")
    val coeffs = (0 to n).map(i => Var(('a' + i).toChar.toString))
    coeffs.zipWithIndex.reverse.map { case (c, i) =>
      if (i == 0) c
      else c * (x ** const(i))
    }.reduce(_ + _)
  }

  def const(n: Int): ArithExpr = {
    if (n == 0) Zero
    else Succ(const(n - 1))
  }

  def polyBench(n: Int, map: ParallelMap): ArithExpr = {
    val L: Language[ArithExpr] = summon[Language[ArithExpr]]
    val R: Rules = poly.Rules()(using L)
    type ArithRule = R.ArithRule

    val simpleStrategy: Strategy[ArithIR, EGraph[ArithIR], Unit] = MaximalRuleApplication(R.all)
      .repeatUntilStable

    val (root, egraph) = L.toEGraph(polynomialExpr(n))
    val egraph2 = simpleStrategy(egraph, map).get

    egraph2.extract(root, arithCostFunction)
  }
}
