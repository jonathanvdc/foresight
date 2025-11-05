package foresight.eqsat.examples.incremental

import foresight.eqsat.examples.poly.*
import foresight.eqsat.mutable
import foresight.eqsat.immutable
import foresight.eqsat.lang.{Language, LanguageCostFunction}
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.saturation.{MaximalRuleApplication, Strategy}
import foresight.util.BenchmarksWithParallelMap
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Level, Mode, OutputTimeUnit, Param, Scope, Setup, State}

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class IncrementalBenchmarks extends BenchmarksWithParallelMap {
  @Param(Array("6"))
  var depth: Int = _

  @Param(Array("1000"))
  var size: Int = _

  @Param(Array("true", "false"))
  var mutableEGraph: Boolean = _

  private var cachedTerms: Seq[ArithExpr] = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    cachedTerms = RandomArithExprGen.randomTerms(size, depth, seed = 42)
  }

  @Benchmark
  def oneByOnePolynomial(): Seq[ArithExpr] = {
    for (term <- cachedTerms) yield {
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
      egraph.addAnalysis(extractionAnalysis)

      for (term <- cachedTerms) yield {
        val tree = L.toTree(term)
        val root = egraph.add(tree)

        val versionMetadata = egraph.getMetadata[VersionMetadata[ArithIR]](metadataName)
        egraph.addMetadata(metadataName, versionMetadata.onNewTermAdded(tree, egraph.egraph))

        incrementalMutableStrategy(egraph, parallelMap)

        L.fromTree(extractionAnalysis.extractor(root, egraph))
      }
    } else {
      var egraph = immutable.EGraphWithMetadata(immutable.EGraph.empty[ArithIR])
        .addMetadata(metadataName, VersionMetadata.empty)
        .addAnalysis(costAnalysis)
        .addAnalysis(extractionAnalysis)

      for (term <- cachedTerms) yield {
        val tree = L.toTree(term)
        val (root, egraph2) = egraph.add(tree)
        egraph = egraph2

        val versionMetadata = egraph.getMetadata[VersionMetadata[ArithIR]](metadataName)
        egraph = egraph.addMetadata(metadataName, versionMetadata.onNewTermAdded(tree, egraph.egraph))

        egraph = incrementalImmutableStrategy(egraph, parallelMap).getOrElse(egraph)

        L.fromTree(extractor(root, egraph))
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

  val L: Language[ArithExpr] = summon[Language[ArithExpr]]
  val R: Rules = Rules()(using L)
  type ArithRule = R.ArithRule

  private def regularRules = R.all

  // Define the metadata name and cost analysis
  private val metadataName = "version"
  private val costAnalysis = TopKCostAnalysis[ArithIR, Int]("cost", 4, arithCostFunction)
  private val extractionAnalysis = L.extractionAnalysis("extract", arithCostFunction)
  private val extractor = extractionAnalysis.extractor

  private val incrementalRules = IncrementalSaturation.makeIncremental(
    R.all,
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
    val withMetadata = mutable.EGraphWithMetadata(egraph)
    withMetadata.addMetadata(metadataName, VersionMetadata.empty)
    withMetadata.addAnalysis(costAnalysis)
    withMetadata.addAnalysis(extractionAnalysis)

    val egraph2 = strategy(withMetadata, map).getOrElse(withMetadata)

    L.fromTree(extractor(root, egraph2))
  }

  private def optimizeImmutable(term: ArithExpr, strategy: Strategy[ImmutableEGraph, Unit], map: ParallelMap): ArithExpr = {
    val (root, egraph) = L.toEGraph(term)
    val withMetadata = immutable.EGraphWithMetadata(egraph)
      .addMetadata(metadataName, VersionMetadata.empty)
      .addAnalysis(costAnalysis)
      .addAnalysis(extractionAnalysis)

    val egraph2 = strategy(withMetadata, map).getOrElse(withMetadata)

    L.fromTree(extractor(root, egraph2))
  }
}
