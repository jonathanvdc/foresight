package foresight.eqsat.examples.incremental

import foresight.eqsat.examples.poly.*
import foresight.eqsat.extraction.CostAnalysis
import foresight.eqsat.mutable
import foresight.eqsat.immutable
import foresight.eqsat.lang.{Language, LanguageCostFunction}
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.saturation.{MaximalRuleApplication, Strategy}
import foresight.util.BenchmarksWithParallelMap
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit, Param, Scope, State}

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class IncrementalBenchmarks extends BenchmarksWithParallelMap {
  @Param(Array("5", "6"))
  var size: Int = _

  @Param(Array("true", "false"))
  var mutableEGraph: Boolean = _

  @Benchmark
  def oneByOnePolynomial(): Seq[ArithExpr] = {
    for (term <- terms(size)) yield {
      if (mutableEGraph) optimizeMutable(term, regularMutableStrategy, parallelMap)
      else optimizeImmutable(term, regularImmutableStrategy, parallelMap)
    }
  }

  @Benchmark
  def incrementalPolynomial(): Seq[ArithExpr] = {
    if (mutableEGraph) {
      val egraph = mutable.EGraphWithMetadata(mutable.EGraph.empty[ArithIR])

      egraph.addMetadata(metadataName, VersionMetadata.empty)
      egraph.addAnalysis(costAnalysis)

      for (term <- terms(size)) yield {
        val tree = L.toTree(term)
        val root = egraph.add(tree)

        val versionMetadata = egraph.getMetadata[VersionMetadata[ArithIR]](metadataName)
        egraph.addMetadata(metadataName, versionMetadata.onNewTermAdded(tree, egraph.egraph))

        incrementalMutableStrategy(egraph, parallelMap)

        L.extract(root, egraph, arithCostFunction)
      }
    } else {
      var egraph = immutable.EGraphWithMetadata(immutable.EGraph.empty[ArithIR])
        .addMetadata(metadataName, VersionMetadata.empty)
        .addAnalysis(costAnalysis)

      for (term <- terms(size)) yield {
        val tree = L.toTree(term)
        val (root, egraph2) = egraph.add(tree)
        egraph = egraph2

        val versionMetadata = egraph.getMetadata[VersionMetadata[ArithIR]](metadataName)
        egraph = egraph.addMetadata(metadataName, versionMetadata.onNewTermAdded(tree, egraph.egraph))

        egraph = incrementalImmutableStrategy(egraph, parallelMap).getOrElse(egraph)

        L.extract(root, egraph, arithCostFunction)
      }
    }
  }

  private val arithCostFunction: LanguageCostFunction[ArithExpr, Int] = new LanguageCostFunction[ArithExpr, Int]() {
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

  private def terms(n: Int): Seq[ArithExpr] = {
    for (i <- 2 to n) yield {
      polynomialExpr(i)
    }
  }

  val L: Language[ArithExpr] = summon[Language[ArithExpr]]
  val R: Rules = Rules()(using L)
  type ArithRule = R.ArithRule

  private def regularRules = R.all

  // Define the metadata name and cost analysis
  private val metadataName = "version"
  private val costAnalysis = CostAnalysis[ArithIR, Int]("cost", arithCostFunction)

  private val k = 2
  private val incrementalRules = IncrementalSaturation.makeIncremental(
    R.all,
    k,
    metadataName,
    costAnalysis)

  type MutableEGraph = mutable.EGraphWithMetadata[ArithIR, mutable.EGraph[ArithIR]]
  type ImmutableEGraph = immutable.EGraphWithMetadata[ArithIR, immutable.EGraph[ArithIR]]

  private def regularMutableStrategy: Strategy[MutableEGraph, Unit] =
    MaximalRuleApplication.mutable(R.all).repeatUntilStable

  private def regularImmutableStrategy: Strategy[ImmutableEGraph, Unit] =
    MaximalRuleApplication(R.all).repeatUntilStable

  private def incrementalMutableStrategy: Strategy[MutableEGraph, Unit] =
    MaximalRuleApplication.mutable(incrementalRules).repeatUntilStable

  private def incrementalImmutableStrategy: Strategy[ImmutableEGraph, Unit] =
    MaximalRuleApplication(incrementalRules).repeatUntilStable

  private def optimizeMutable(term: ArithExpr, strategy: Strategy[MutableEGraph, Unit], map: ParallelMap): ArithExpr = {
    val (root, egraph) = L.toMutableEGraph(term)
    val egraph2 = strategy(mutable.EGraphWithMetadata(egraph), map).get

    L.extract(root, egraph2, arithCostFunction)
  }

  private def optimizeImmutable(term: ArithExpr, strategy: Strategy[ImmutableEGraph, Unit], map: ParallelMap): ArithExpr = {
    val (root, egraph) = L.toEGraph(term)
    val egraph2 = strategy(immutable.EGraphWithMetadata(egraph), map).get

    L.extract(root, egraph2, arithCostFunction)
  }
}
