package foresight.eqsat

/**
 * Building blocks for rewrite-driven equality saturation over immutable e-graphs.
 * This package defines the search/apply pipeline, delayed command execution, parallel search,
 * reversible rules, and match portability for caching.
 *
 * # Core ideas
 * - **Immutable e-graphs**: All edits are expressed as [[foresight.eqsat.commands.Command]] values and
 *   executed later to produce a new snapshot. No in-place mutation.
 * - **Search → Apply → Compose**: A [[Searcher]] finds matches; an [[Applier]] turns each match into a
 *   command; a [[Rule]] bundles per-match commands into a single, optimized operation.
 * - **Parallelism**: Methods accept [[foresight.eqsat.parallel.ParallelMap]] to label and distribute work.
 * - **Reversal**: Many components admit an inverse: [[ReversibleSearcher]], [[ReversibleApplier]],
 *   and [[ReversibleSearcherPhase]]. A rule built from reversible parts can be flipped via [[Rule.tryReverse]].
 * - **Portability**: Matches can remain meaningful across snapshots via [[PortableMatch]], enabling
 *   caching and application recording through
 *   [[foresight.eqsat.saturation.EGraphWithRecordedApplications]] and
 *   [[foresight.eqsat.saturation.SearchAndApply$.withCaching]].
 *
 * # Typical workflow
 * 1. **Define search** using phases:
 *    - A phase implements [[SearcherPhase]]: per-class work via `search(call, egraph, input)` and
 *      whole-graph aggregation via `aggregate(map)`.
 *    - Compose phases with [[Searcher.chain]] or run independent searches with [[Searcher.product]].
 * 2. **Define apply** by mapping a match to a [[foresight.eqsat.commands.Command]] using [[Applier]].
 * 3. **Create a rule** by pairing searcher and applier in a [[Rule]], then either run it immediately
 *    ([[Rule.apply]]) or stage it and batch with others ([[Rule.delayed]]).
 * 4. **Optionally cache** with [[foresight.eqsat.saturation.SearchAndApply$.withCaching]], which relies
 *    on [[PortableMatch]] for snapshot-to-snapshot consistency.
 *
 * # From low-level rules to high-level saturation
 * High-level **saturation strategies** (see [[foresight.eqsat.saturation]]) orchestrate iterations,
 * stopping conditions, caching, and rebasing:
 *   - [[foresight.eqsat.saturation.Strategy]]: Composable driver for saturation steps.
 *   - [[foresight.eqsat.saturation.SearchAndApply]]: Separates search from application.
 *   - [[foresight.eqsat.saturation.MaximalRuleApplication]] and `WithCaching` variants.
 *   - [[foresight.eqsat.saturation.BackoffRuleApplication]] for quota/cooldown balancing.
 *   - [[foresight.eqsat.saturation.StochasticRuleApplication]] and `WithCaching` variants.
 *
 * # Reversal (bidirectional rules)
 * - A [[ReversibleSearcher]] exposes `tryReverse: Option[Applier]`.
 * - A [[ReversibleApplier]] exposes `tryReverse: Option[Searcher]`.
 * - A pipeline of phases can reverse step-by-step using [[ReversibleSearcherPhase.tryReverse]].
 * - [[Rule.tryReverse]] returns a flipped rule when both parts support reversal.
 *
 * # Portability and caching
 *   - A match type implements [[PortableMatch]] to remain valid as the e-graph evolves.
 *   - Structural-only matches can implement `port` as a no-op; ID-bearing matches translate IDs
 *     to the new snapshot (e.g., canonicalization).
 *   - [[foresight.eqsat.saturation.EGraphWithRecordedApplications]] records per-rule applied matches and
 *     re-ports them after unions. [[foresight.eqsat.saturation.SearchAndApply$.withCaching]] uses that
 *     record to skip already-applied matches.
 *
 * # Design contracts (high level)
 * - **Purity**: searchers and phases do not mutate the e-graph; appliers build commands instead of editing in place.
 * - **Thread-safety**: search and apply can run in parallel over independent work units.
 * - **Command aggregation**: rules combine per-match commands into an optimized batch; commands aim to be idempotent.
 *
 * # Quick reference
 *   - Search: [[Searcher]], [[SearcherPhase]], [[ReversibleSearcher]], [[ReversibleSearcherPhase]]
 *   - Apply:  [[Applier]], [[ReversibleApplier]]
 *   - Rules:  [[Rule]]
 *   - Matches across snapshots: [[PortableMatch]]
 *   - Caching/recording: [[foresight.eqsat.saturation.EGraphWithRecordedApplications]],
 *     [[foresight.eqsat.saturation.SearchAndApply$.withCaching]]
 *   - Strategies: [[foresight.eqsat.saturation.Strategy]],
 *     [[foresight.eqsat.saturation.MaximalRuleApplication]],
 *     [[foresight.eqsat.saturation.BackoffRuleApplication]],
 *     [[foresight.eqsat.saturation.StochasticRuleApplication]]
 *
 * @example Typical workflow
 * {{{
 * import foresight.eqsat.rewriting._
 * import foresight.eqsat.parallel.ParallelMap
 * import foresight.eqsat.commands.CommandQueue
 *
 * // 1) Search: single-phase searcher (returns Seq[MyMatch])
 * val s: Searcher[MyNode, Seq[MyMatch], MyEGraph] = Searcher(myPhase)
 *
 * // 2) Apply: turn a match into a Command program
 * val a: Applier[MyNode, MyMatch, MyEGraph] = (m, g) => buildCommand(m, g) // user-defined
 *
 * // 3) Rule: run now or stage/batch
 * val r = Rule("my-rule", s, a)
 * val updated = r(egraph, ParallelMap.default)                     // immediate
 * val staged  = r.delayed(egraph)                                  // staged
 * val batched = CommandQueue(Seq(staged, r2.delayed(egraph))).optimized
 * val (next, _) = batched(egraph, Map.empty, ParallelMap.default)  // execute batch
 * }}}
 *
 * @example Run with a saturation strategy
 * {{{
 * import foresight.eqsat.saturation._
 * import scala.concurrent.duration._
 *
 * val rules: Seq[Rule[MyNode, MyMatch, MyEGraph]] = Seq(r1, r2, r3)
 *
 * val strategy =
 *   MaximalRuleApplicationWithCaching(rules)
 *     .withTimeout(30.seconds)
 *     .repeatUntilStable
 *
 * val result = strategy.run(myEGraph)
 * }}}
 *
 * @example Reversal (bidirectional rules)
 * {{{
 * val forward: Rule[MyNode, MyMatch, MyEGraph] = ...
 * val maybeBack = forward.tryReverse
 * val back = maybeBack.get  // if defined, 'back' applies the inverse transformation
 * }}}
 *
 * @example Portability and caching
 * {{{
 * import foresight.eqsat.saturation._
 *
 * type M = MyMatch with PortableMatch[MyNode, M]
 * val sa = SearchAndApply.withCaching[MyNode, MyEGraph, M]
 *
 * val found: Map[String, Seq[M]] =
 *   sa.search(Seq(r1, r2), EGraphWithRecordedApplications(egraph), ParallelMap.default)
 * val after = sa.apply(Seq(r1, r2), found, EGraphWithRecordedApplications(egraph), ParallelMap.default)
 * }}}
 */
package object rewriting
