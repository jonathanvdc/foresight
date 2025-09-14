package foresight.eqsat.examples.mm

import foresight.eqsat.EGraph
import foresight.eqsat.examples.mm
import foresight.eqsat.lang.*
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.saturation.{MaximalRuleApplication, Strategy}
import foresight.util.BenchmarksWithParallelMap
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit, Param, Scope, State}

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class MatmulBenchmarks extends BenchmarksWithParallelMap {
  @Param(Array("20", "40", "80"))
  var size: Int = _

  @Benchmark
  def nmm(): LinalgExpr = {
    nmmBench(size, parallelMap)
  }

  final case class DimAndCost(nrows: Int, ncols: Int, cost: Int)

  given Ordering[DimAndCost] = Ordering.by(_.cost)

  val linalgCostFunction: LanguageCostFunction[LinalgExpr, DimAndCost] = new LanguageCostFunction[LinalgExpr, DimAndCost]() {
    override def apply(expr: LinalgExpr): DimAndCost = {
      expr match {
        case mm.Mat(rows, cols) => DimAndCost(rows, cols, 0)
        case mm.Fact(dimAndCost: DimAndCost) => dimAndCost
        case mm.Mul(lhs, rhs) =>
          val DimAndCost(lrows, lcols, lcost) = apply(lhs)
          val DimAndCost(rrows, rcols, rcost) = apply(rhs)
          if (lcols != rrows) {
            throw new RuntimeException(s"Dimension mismatch: $lcols != $rrows")
          } else {
            DimAndCost(lrows, rcols, lrows * lcols * rcols + lcost + rcost)
          }
        case _ => DimAndCost(-1, -1, 0)
      }
    }
  }

  def createNmm(n: Int): LinalgExpr = {
    if (n == 0) {
      mm.Mat(10, 10)
    } else {
      mm.Mul(createNmm(n - 1), Mat(10, 10))
    }
  }

  def nmmBench(n: Int, map: ParallelMap): LinalgExpr = {
    val L: Language[LinalgExpr] = summon[Language[LinalgExpr]]
    val R: mm.Rules = mm.Rules()(using L)
    type LinalgRule = R.LinalgRule


    val simpleStrategy: Strategy[LinalgIR, EGraph[LinalgIR], Unit] = MaximalRuleApplication(R.all)
      .repeatUntilStable

    val expr = createNmm(n)

    val (root, egraph) = L.toEGraph(expr)
    val egraph2 = simpleStrategy(egraph, map).get

    egraph2.extract(root, linalgCostFunction)
  }
}
