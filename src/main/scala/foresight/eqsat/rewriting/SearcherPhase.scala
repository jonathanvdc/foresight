package foresight.eqsat.rewriting

import foresight.eqsat.{EClassCall, EClassRef, EGraph, EGraphLike}
import foresight.eqsat.parallel.ParallelMap

/**
 * A single stage in a multi-stage e-graph search pipeline.
 *
 * `SearcherPhase` expresses: (1) how to compute class-local results for each e-class, and
 * (2) how to aggregate those per-class results into a whole-graph output. Phases are composed
 * by [[Searcher.chain]] and are typically wrapped by [[Searcher.apply]] for the first stage.
 *
 * # Responsibilities
 *  - **Per-class search** (`search(call, egraph, input)`): given a canonical e-class and an
 *    `input` (from the previous phase), compute a result `IntermediateT` for that class alone.
 *  - **Aggregation** ([[aggregate]]): combine the `IntermediateT` results for *all* classes into a
 *    single `OutputT` for the phase.
 *
 * # Execution model
 * The convenience search(egraph, input, parallelize) provided here:
 *  1. enumerates all classes via `egraph.classes`,
 *  2. canonicalizes each class ref before calling the per-class `search`,
 *  3. runs per-class work in parallel via [[foresight.eqsat.parallel.ParallelMap]],
 *  4. passes the `Map[EClassRef, IntermediateT]` to [[aggregate]].
 *
 * Implementations may assume the [[EClassCall]] passed to per-class `search` refers to the
 * class's **canonical representative** at the time of the call.
 *
 * # Parallelism
 * The helper `search(egraph, input, parallelize)` uses `parallelize` to distribute per-class work.
 * Per-class `search` must be thread-safe and must not capture mutable shared state.
 *
 * # Determinism
 * Aggregation over an unordered `Map` may yield non-deterministic ordering in `OutputT` if your
 * aggregator cares about order. If you need stable order, sort keys (e.g., by `EClassRef`) inside
 * [[aggregate]].
 *
 * # Performance tips
 *  - Keep `IntermediateT` compact; avoid large transient allocations per class.
 *  - Short-circuit in per-class `search` when possible (e.g., empty/None).
 *  - Push filtering into the per-class step to shrink what `aggregate` must combine.
 *
 * @example Collecting pattern matches per class
 * {{{
 * final case class PatternPhase(pattern: CompiledPattern[MyNode])
 *   extends SearcherPhase[MyNode, Unit, Vector[PatternMatch[MyNode]], Vector[PatternMatch[MyNode]], MyEGraph] {
 *
 *   def search(call: EClassCall, egraph: MyEGraph, input: Unit): Vector[PatternMatch[MyNode]] =
 *     pattern.matchesInClass(call, egraph) // class-local work
 *
 *   def aggregate(matches: Map[EClassRef, Vector[PatternMatch[MyNode]]]): Vector[PatternMatch[MyNode]] =
 *     matches.valuesIterator.flatten.toVector // whole-graph output
 * }
 *
 * // First phase in a Searcher:
 * val searcher = Searcher(PatternPhase(myPattern))
 * val allMatches = searcher.search(egraph)
 * }}}
 * @example Boolean existence check (any match?)
 * {{{
 * final case class ExistsPhase(...)
 *   extends SearcherPhase[MyNode, Unit, Boolean, Boolean, MyEGraph] {
 *   def search(call: EClassCall, egraph: MyEGraph, input: Unit): Boolean =
 *     computeHasMatch(call, egraph)
 *
 *   def aggregate(matches: Map[EClassRef, Boolean]): Boolean =
 *     matches.valuesIterator.exists(identity)
 * }
 * }}}
 *
 * @tparam NodeT          Node payload type stored in the e-graph.
 * @tparam InputT         Input carried from the previous phase (often `Unit` for the first phase).
 * @tparam IntermediateT  Per-class result type produced by this phase.
 * @tparam OutputT        Phase-level result type (often `Seq[...]` for match sets).
 * @tparam EGraphT        Concrete e-graph type (must be both [[EGraphLike]] and [[EGraph]]).
 */
trait SearcherPhase[
  NodeT,
  -InputT,
  IntermediateT,
  +OutputT,
  EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]
] {

  /**
   * Compute the class-local result for a single e-class.
   *
   * Implementations can assume `call` refers to the class's canonical representative.
   *
   * @param call     Canonical e-class handle to search within.
   * @param egraph   Immutable e-graph snapshot.
   * @param input    Input from the previous phase (or `Unit` for the first phase).
   * @return         Class-local result to be consumed by [[aggregate]].
   */
  def search(call: EClassCall, egraph: EGraphT, input: InputT): IntermediateT

  /**
   * Combine per-class results into the phase's output.
   *
   * Note: `matches` is keyed by canonical `EClassRef`. If output requires a specific order,
   * sort keys here before combining.
   *
   * @param matches  Map from canonical class ref to that class's `IntermediateT`.
   * @return         The whole-graph output of this phase.
   */
  def aggregate(matches: Map[EClassRef, IntermediateT]): OutputT

  /**
   * Run this phase across the entire e-graph, in parallel where possible, then aggregate.
   *
   * The implementation:
   *  - iterates over `egraph.classes`,
   *  - canonicalizes each class ref,
   *  - computes per-class results with `search(call, egraph, input)`,
   *  - aggregates via [[aggregate]].
   *
   * @param egraph       Immutable e-graph snapshot to search.
   * @param input        Input from the previous phase (or `Unit` for the first phase).
   * @param parallelize  Parallelization strategy and task labeling.
   * @return             The aggregated output for this phase.
   */
  final def search(egraph: EGraphT, input: InputT, parallelize: ParallelMap = ParallelMap.default): OutputT = {
    val classes = egraph.classes
    val searchClass = (c: EClassRef) => c -> search(egraph.canonicalize(c), egraph, input)
    val matches = parallelize(classes, searchClass).toMap
    aggregate(matches)
  }
}
