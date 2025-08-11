package foresight.eqsat

/**
 * Provides a functional framework for attaching metadata to an [[foresight.eqsat.EGraph]] and
 * keeping it in sync as the graph evolves.
 * It covers both general-purpose metadata and dataflow-style analyses.
 *
 * ## Overview
 *
 * Metadata represents any derived information that depends on the structure of the e-graph,
 * such as statistics, summaries, or precomputed auxiliary data. This package makes it possible
 * to keep such metadata up-to-date automatically as nodes are inserted or e-classes are merged.
 *
 * Two main usage patterns are supported. The first is direct use of the
 * [[foresight.eqsat.metadata.Metadata]] trait, which defines how to update a metadata value
 * incrementally when the e-graph changes. The second is the [[foresight.eqsat.metadata.Analysis]]
 * API, which allows you to describe a dataflow-like transfer function and a join operation over
 * e-classes, and then evaluate it to a fixpoint. The results of an analysis are stored in an
 * [[foresight.eqsat.metadata.AnalysisMetadata]] object, which itself implements [[Metadata]].
 *
 * The [[foresight.eqsat.metadata.EGraphWithMetadata]] wrapper is the central tool for managing
 * metadata. It wraps an e-graph and ensures that all registered metadata instances are updated
 * in response to changes in the graph. You can register either general-purpose metadata managers
 * or the results of analyses.
 *
 * ## Lifecycle and Consistency
 *
 * All metadata is managed functionally. Update operations never mutate the metadata in place;
 * instead, they return a new metadata instance that reflects the current state of the e-graph.
 * The old instance remains valid and usable. When an operation like
 * [[foresight.eqsat.metadata.EGraphWithMetadata.tryAddMany]] or
 * [[foresight.eqsat.metadata.EGraphWithMetadata.unionMany]] is performed, the underlying
 * e-graph is updated first. Only then are the registered metadata managers invoked, with the
 * post-update graph as their reference.
 *
 * Batches of changes are treated as unordered sets. Metadata implementations produce
 * the same result regardless of the order in which changes are listed.
 *
 * ## Concurrency
 *
 * Many update methods accept a [[foresight.eqsat.parallel.ParallelMap]] argument, which
 * allows work to be parallelized across multiple items in a batch. Metadata implementations
 * can use this to speed up expensive updates, but correctness must never depend on parallelism.
 * All code must also be safe to run sequentially.
 *
 * ## Designing an Analysis
 *
 * An [[foresight.eqsat.metadata.Analysis]] is defined by three main operations. The
 * `make` method acts as a transfer function from a node (and its argument results) to a
 * new analysis result. The `join` method combines two results for the same e-class into
 * one, and must satisfy the usual semilattice laws of associativity, commutativity, and
 * idempotence to ensure convergence and order independence. The `rename` method applies
 * alpha-renaming to slot identifiers inside a result, allowing the result to be specialized
 * to a particular e-class application.
 *
 * The `apply` method on an analysis runs it over the entire e-graph, producing an
 * [[foresight.eqsat.metadata.AnalysisMetadata]] object that can be queried for results and
 * updated incrementally as the graph changes.
 *
 * ## Quickstart
 *
 * {{{
 * import foresight.eqsat._
 * import foresight.eqsat.metadata._
 *
 * final case class UsesCount(n: Int)
 *
 * object CountUses extends Analysis[MyNode, UsesCount] {
 *   val name = "uses-count"
 *
 *   def rename(r: UsesCount, _: SlotMap): UsesCount = r
 *   def join(l: UsesCount, r: UsesCount): UsesCount = UsesCount(l.n max r.n)
 *   def make(node: MyNode, defs: Seq[Slot], uses: Seq[Slot], args: Seq[UsesCount]): UsesCount =
 *     UsesCount(uses.size + args.map(_.n).sum)
 * }
 *
 * val base: EGraph[MyNode] = EGraph.empty
 * val g0  = EGraphWithMetadata(base).addAnalysis(CountUses)
 *
 * val (addResults, g1) = g0.tryAddMany(Seq(/* new ENode[MyNode]s */), ParallelMap.sequential)
 * val (equivs,    g2)  = g1.unionMany(Seq(/* unions */), ParallelMap.sequential)
 *
 * val meta  = CountUses.get(g2)
 * val call  = /* some EClassCall */
 * val value = meta(call, g2)
 * }}}
 *
 * ## Implementing Custom Metadata
 *
 * If your derived information is not naturally expressed as an analysis, you can implement
 * [[foresight.eqsat.metadata.Metadata]] directly. In `onAddMany`, update only the portions
 * of the metadata that depend on the newly inserted nodes. In `onUnionMany`, merge state
 * across unified classes and ensure that any dependent state is revalidated. The `emptied`
 * method must produce a completely fresh metadata instance with no hidden aliasing.
 *
 * ## Error Handling
 *
 * Callers are responsible for providing the correct type parameter when retrieving metadata
 * with [[foresight.eqsat.metadata.EGraphWithMetadata.getMetadata]]. Analyses must be total
 * and pure, and throw exceptions only for unrecoverable programmer errors.
 */
package object metadata
