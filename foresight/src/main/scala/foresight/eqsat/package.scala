package foresight

/**
 * The root of Foresight's equality saturation framework.
 * It provides immutable e-graph data structures, symbolic references, slot-based
 * binding semantics, and core interfaces for search, application, and extraction.
 *
 * An **e-graph** represents equivalence classes of expressions, or *e-classes*, each
 * containing multiple *e-nodes* that differ in structure but are semantically equal.
 * This representation supports efficient, parallel, and reversible rewrite-driven
 * optimization, with explicit tracking of symbolic bindings (*slots*) and rich metadata.
 *
 * E-graphs in Foresight are purely functional: every operation returns a new value
 * rather than mutating in place. This immutability underpins reproducibility,
 * concurrent analysis, and caching across snapshots.
 *
 * ## Core Concepts
 *
 * At the heart of the API are symbolic identifiers and slot-based binding. An [[eqsat.EClassRef]]
 * is a reference to an e-class within a particular e-graph snapshot. Over time, merges
 * may cause multiple references to point to the same canonical e-class; callers can
 * normalize them with [[eqsat.readonly.EGraph.canonicalize]]. An [[eqsat.EClassCall]] couples a root reference
 * with a total slot map, allowing the same structural e-class to be applied with different
 * bindings. A [[eqsat.Slot]] identifies a bound variable in an expression, and a [[eqsat.SlotMap]] gives a
 * complete mapping from slots to other slots or values.
 *
 * ## EGraph and EGraphLike
 *
 * [[eqsat.readonly.EGraph]] is the fundamental e-graph interface, available in a mutable
 * variant ([[eqsat.mutable.EGraph]]) and an immutable variant ([[eqsat.immutable.EGraph]]).
 *
 * Canonicalization and identity management are central to working with e-graphs. Methods like
 * `canonicalize(ref)` return the leader representative for an [[eqsat.EClassRef]], reflecting all
 * unions so far. The graph can answer membership queries (`contains`) and retrieve the e-class
 * containing a given [[eqsat.ENode]]. Batch operations like `tryAddMany` and `unionMany` allow for
 * inserting multiple nodes or merging multiple classes in one pass, improving efficiency and
 * reducing intermediate rebuilding.
 *
 * ## Subpackages
 *
 * The `foresight.eqsat.commands` package defines a system for batching and replaying e-graph
 * edits. Commands are immutable descriptions of insertions, merges, and other structural changes,
 * which can be staged and optimized before execution.
 *
 * The `foresight.eqsat.extraction` package turns e-classes into concrete expression trees using
 * cost models. It computes minimal-cost trees for each class and carries a slot renaming to ensure
 * context validity. These analyses are deterministic and fully parametric over cost type and node
 * ordering.
 *
 * The `foresight.eqsat.metadata` package attaches derived information to e-graphs and keeps it
 * incrementally updated. This includes both general-purpose metadata managers and semilattice-style
 * analyses. The `EGraphWithMetadata` wrapper ensures consistency after additions and merges.
 *
 * The `foresight.eqsat.parallel` package provides composable parallelism utilities.
 * [[foresight.eqsat.parallel.ParallelMap]] defines the core mapping strategy interface,
 * with sequential, parallel, and fixed-thread variants, plus wrappers for cooperative
 * cancellation and hierarchical timing.
 *
 * The `foresight.eqsat.rewriting` package implements the search–apply pipeline for equality
 * saturation. Searchers find matches, appliers produce commands, and rules combine them into
 * optimized batches. Features like reversal and match portability enable bidirectional reasoning
 * and cross-snapshot caching.
 *
 * The `foresight.eqsat.saturation` package orchestrates rewrite application at scale. A
 * [[foresight.eqsat.saturation.Strategy]] composes search–apply steps into full saturation runs,
 * with variants for maximal application, stochastic selection, backoff quotas, caching, and rebasing.
 *
 * ## Design Principles
 *
 * All core data types are persistent values: applying a command, running a rule, or performing
 * extraction yields a new e-graph or analysis result without modifying the original. Symbolic
 * planning is encouraged—commands and calls are constructed over symbolic identifiers and slot
 * mappings, making it possible to plan transformations before runtime IDs are known. Extraction,
 * rewriting, and saturation are parametric over cost types and node orderings, allowing the same
 * framework to target latency minimization, code size reduction, or other goals. Parallelism and
 * cooperative cancellation are explicit in the API: major batch operations accept a
 * [[foresight.eqsat.parallel.ParallelMap]], avoiding hidden threading behavior.
 *
 * @example
 * {{{
 * import foresight.eqsat._
 * import foresight.eqsat.commands._
 * import foresight.eqsat.immutable._
 * import foresight.eqsat.rewriting._
 * import foresight.eqsat.saturation._
 *
 * // Create an empty e-graph for a custom node type
 * val g0: EGraph[MyNode] = EGraph.empty
 *
 * // Build a command queue
 * val builder = new CommandQueueBuilder[MyNode]
 * val root    = builder.add(myENode)
 * val queue   = builder.queue.optimized
 *
 * // Apply commands to produce a new graph
 * val (g1, reif) = queue.applyImmutable(g0, Map.empty, ParallelMap.sequential)
 *
 * // Run a saturation strategy
 * val rules: Seq[Rule[MyNode, MyMatch, EGraph[MyNode]]] = Seq(r1, r2)
 * val strat = MaximalRuleApplicationWithCaching(rules).repeatUntilStable
 * val result = strat.run(g1)
 * }}}
 */
package object eqsat
