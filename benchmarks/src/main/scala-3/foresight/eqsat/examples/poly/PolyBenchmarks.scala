package foresight.eqsat.examples.poly

import foresight.eqsat.immutable
import foresight.eqsat.mutable
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

  @Param(Array("true", "false"))
  var mutableEGraph: Boolean = _

  @Benchmark
  def polynomial(): ArithExpr = {
    if (mutableEGraph) polyBenchMutable(size, parallelMap)
    else polyBenchImmutable(size, parallelMap)
  }

  private val arithCostFunction: LanguageCostFunction[ArithExpr, Int] = new LanguageCostFunction[ArithExpr, Int]() {
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
  private def polynomialExpr(n: Int): ArithExpr = {
    val x = Var("x")
    val coeffs = (0 to n).map(i => Var(('a' + i).toChar.toString))
    coeffs.zipWithIndex.reverse.map { case (c, i) =>
      if (i == 0) c
      else c * (x ** const(i))
    }.reduce(_ + _)
  }

  private def const(n: Int): ArithExpr = {
    if (n == 0) Zero
    else Succ(const(n - 1))
  }

  val L: Language[ArithExpr] = summon[Language[ArithExpr]]
  val R: Rules = poly.Rules()(using L)
  type ArithRule = R.ArithRule

  private def polyBenchMutable(n: Int, map: ParallelMap): ArithExpr = {
    val simpleStrategy: Strategy[ArithIR, mutable.EGraph[ArithIR], Unit] = MaximalRuleApplication.mutable(R.all)
      .repeatUntilStable

    val (root, egraph) = L.toMutableEGraph(polynomialExpr(n))
    val egraph2 = simpleStrategy(egraph, map).get

    L.extract(root, egraph2, arithCostFunction)
  }

  private def polyBenchImmutable(n: Int, map: ParallelMap): ArithExpr = {
    val simpleStrategy: Strategy[ArithIR, immutable.EGraph[ArithIR], Unit] = MaximalRuleApplication(R.all)
      .repeatUntilStable

    val (root, egraph) = L.toEGraph(polynomialExpr(n))
    val egraph2 = simpleStrategy(egraph, map).get

    L.extract(root, egraph2, arithCostFunction)
  }
}
