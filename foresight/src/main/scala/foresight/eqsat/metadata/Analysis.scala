package foresight.eqsat.metadata

import foresight.eqsat.{ENode, ReadOnlyEGraph, Slot}
import foresight.eqsat.immutable.{EGraph, EGraphWithMetadata}
import foresight.eqsat.collections.SlotMap

/**
 * Defines a dataflow-style analysis over an [[EGraph]].
 *
 * An analysis maps e-nodes to results of type `A`, combines results across
 * alternatives within an e-class via [[join]], and produces a metadata snapshot
 * that stays consistent with the e-graph.
 *
 * Implementations are expected to be pure: no in-place mutation of the analysis instance.
 *
 * ## Conceptual model
 *   - [[make]]: local transfer from a node and its argument results to a result `A`.
 *   - [[join]]: combine results that flow to the same e-class (e.g., from different nodes or paths).
 *   - [[rename]]: alpha-renaming utility to rewrite slot identifiers inside a result.
 *
 * The default [[apply]] method evaluates the analysis over the entire e-graph using a worklist:
 *   1. Seed with nodes that have **no arguments** (nullary) by calling [[make]].
 *   2. Propagate through dependent nodes until a fixed point is reached.
 *
 * ## Algebraic expectations for `join`
 * To behave like a least-upper-bound over a partial order, `join` is expected to satisfy:
 *   - **Associative**: `join(a, join(b, c)) == join(join(a, b), c)`
 *   - **Commutative**: `join(a, b) == join(b, a)`
 *   - **Idempotent**: `join(a, a) == a`
 *
 * While not enforced by the type system, violating these properties can prevent convergence
 * or yield order-dependent results.
 *
 * @tparam NodeT The IR node type carried by the e-graph's e-nodes.
 * @tparam A The analysis result type.
 */
trait Analysis[NodeT, A] {

  /** Stable registration name used when storing/retrieving this analysis as metadata. */
  def name: String

  /**
   * Alpha-rename slot identifiers within an analysis result.
   *
   * Useful when results reference definition/use positions abstractly via [[Slot]]s.
   * Keys in `renaming` are the *original* slots present in `result`; values are their replacements.
   * Implementations are total over the slots they actually carry and leave unmapped slots unchanged.
   *
   * @param result   The result to be rewritten.
   * @param renaming Mapping from old → new slots.
   * @return A result identical to `result` except with specified slots renamed.
   */
  def rename(result: A, renaming: SlotMap): A

  /**
   * Computes the local analysis result for a node, given its arguments and definitions.
   *
   * @param node The node payload.
   * @param defs Slots this node defines.
   * @param uses Slots this node reads.
   * @param args Analysis results for the node's arguments (in order).
   * @return The analysis result for this node.
   */
  def make(node: NodeT, defs: Seq[Slot], uses: Seq[Slot], args: Seq[A]): A

  /**
   * Computes the local analysis result for a fully-formed e-node.
   *
   * @param node The e-node.
   * @param args Analysis results for the e-node's arguments (in order).
   * @return The analysis result for this e-node.
   */
  final def make(node: ENode[NodeT], args: Seq[A]): A = {
    make(node.nodeType, node.definitions, node.uses, args)
  }

  /**
   * Join two results that flow to the same e-class.
   *
   * See the algebraic expectations in the trait-level docs. Implementations typically
   * encode a meet/join on a lattice (e.g., union/intersection, max/min, or a custom semilattice).
   *
   * @param left One incoming result.
   * @param right Another incoming result.
   * @return The combined result.
   */
  def join(left: A, right: A): A

  /**
   * Evaluate this analysis over an entire e-graph and return its metadata snapshot.
   *
   * Evaluation strategy:
   *   1. Seed: For each e-class, compute results for **nullary** nodes (`args.isEmpty`) and
   *      initialize per-class state.
   *   2. Propagate: Use a worklist to process remaining nodes whose argument results become available,
   *      repeatedly applying [[make]] and merging with [[join]] until no changes occur.
   *
   * Termination relies on `join` being monotone and idempotent w.r.t. the underlying partial order.
   *
   * @param egraph The e-graph to analyze.
   * @return [[AnalysisMetadata]] capturing the per-class results for this analysis.
   */
  final def apply(egraph: ReadOnlyEGraph[NodeT]): AnalysisMetadata[NodeT, A] = {
    val updater = new AnalysisUpdater(this, egraph, Map.empty)

    // Seed: nodes with no arguments.
    for (c <- egraph.classes) {
      for (node <- egraph.nodes(egraph.canonicalize(c))) {
        if (node.args.isEmpty) {
          updater.update(c, make(node, Seq.empty))
        }
      }
    }

    // Propagate to a fixed point; eventually touches all e-nodes.
    updater.processPending(initialized = false)

    AnalysisMetadata(this, updater.results)
  }

  /**
   * Retrieve this analysis' metadata from an [[EGraphWithMetadata]] by name.
   *
   * Equivalent to `egraph.getMetadata(name)` with the proper type.
   *
   * @param egraph The e-graph wrapper carrying registered metadata.
   * @return The analysis metadata previously registered under [[name]].
   * @throws NoSuchElementException if the analysis was not registered.
   * @throws ClassCastException if the stored metadata has a different type.
   */
  final def get(egraph: EGraphWithMetadata[NodeT, _]): AnalysisMetadata[NodeT, A] = {
    egraph.getMetadata(name)
  }
}
