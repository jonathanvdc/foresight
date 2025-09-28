package foresight.eqsat.commands

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.{EClassCall, EClassSymbol, MixedTree}
import foresight.eqsat.immutable
import foresight.eqsat.mutable
import foresight.eqsat.readonly.EGraph

/**
 * A [[Command]] encapsulates a single, replayable edit to an e-graph.
 *
 * Commands are pure values that describe what to do; they don’t perform any mutation until applied.
 * This design allows callers to build, simplify, batch, or reorder edits before committing them to
 * an e-graph.
 *
 * @tparam NodeT Node type for expressions represented by the e-graph.
 */
trait Command[NodeT] {

  /**
   * The e-class symbols this command expects to already exist when it runs.
   *
   * These are the external dependencies needed to interpret the command (e.g., “union the new class
   * with this existing one”). Implementations list all required symbols so that callers can:
   *   - validate a reification plan ahead of time
   *   - topologically sort commands
   */
  def uses: Seq[EClassSymbol]

  /**
   * The virtual e-class symbols this command promises to define when executed.
   *
   * Each virtual symbol represents an e-class that will be materialized in the target e-graph and
   * receive a concrete [[EClassCall]]. After a successful [[apply]], the returned reification map
   * contains an entry for every symbol listed here.
   */
  def definitions: Seq[EClassSymbol.Virtual]

  /**
   * Executes the command against an e-graph.
   *
   * @param egraph
   *   Destination e-graph that will be mutated in place.
   * @param reification
   *   Mapping from virtual symbols to concrete calls available before this command runs. This is used
   *   to ground virtual references present in [[uses]].
   * @param parallelize
   *   Parallelization strategy to label and distribute any internal work.
   * @return
   *   A pair `(updated, outMap)` where:
   *     - `updated` is `true` if any change occurred, or `false` for a no-op.
   *     - `outMap` binds every symbol in [[definitions]] to its realized [[EClassCall]].
   */
  def apply(egraph: mutable.EGraph[NodeT],
            reification: Map[EClassSymbol.Virtual, EClassCall],
            parallelize: ParallelMap): (Boolean, Map[EClassSymbol.Virtual, EClassCall])

  /**
   * Executes the command against an e-graph.
   *
   * @param egraph
   *   Destination e-graph. Implementations may either return it unchanged or produce a new immutable
   *   e-graph snapshot.
   * @param reification
   *   Mapping from virtual symbols to concrete calls available before this command runs. This is used
   *   to ground virtual references present in [[uses]].
   * @param parallelize
   *   Parallelization strategy to label and distribute any internal work.
   * @return
   A pair `(maybeNewGraph, outMap)` where:
   *     - `maybeNewGraph` is `Some(newGraph)` if any change occurred, or `None` for a no-op.
   *     - `outMap` binds every symbol in [[definitions]] to its realized [[EClassCall]].
   */
  final def applyImmutable[
    Repr <: immutable.EGraphLike[NodeT, Repr] with immutable.EGraph[NodeT]
  ](
     egraph: Repr,
     reification: Map[EClassSymbol.Virtual, EClassCall],
     parallelize: ParallelMap
   ): (Option[Repr], Map[EClassSymbol.Virtual, EClassCall]) = {
    val mutableEGraph = mutable.FreezableEGraph[NodeT, Repr](egraph)
    val (updated, outMap) = apply(mutableEGraph, reification, parallelize)
    if (updated) {
      (Some(mutableEGraph.freeze()), outMap)
    } else {
      (None, outMap)
    }
  }

  /**
   * Returns a semantically equivalent command that is cheaper to execute on the given e-graph.
   *
   * Typical simplifications include:
   *   - eliminating unions whose endpoints are already congruent
   *   - dropping inserts of nodes already present
   *   - narrowing work by pre-binding outputs in the returned partial reification
   *
   * The returned partial map contains bindings this command can prove without running, which callers
   * may compose across multiple commands to reduce future work.
   *
   * @param egraph Target e-graph used as context for optimization.
   * @param partialReification Known virtual-to-concrete bindings available upstream.
   * @return A pair `(simplifiedCommand, partialBindings)` for this command.
   */
  def simplify(
                egraph: EGraph[NodeT],
                partialReification: Map[EClassSymbol.Virtual, EClassCall]
              ): (Command[NodeT], Map[EClassSymbol.Virtual, EClassCall])

  /**
   * Returns a semantically equivalent command that is cheaper to execute on the given e-graph.
   * Assumes no prior reification information and does not return any partial bindings.
   *
   * Typical simplifications include:
   *   - eliminating unions whose endpoints are already congruent
   *   - dropping inserts of nodes already present
   *   - narrowing work by pre-binding outputs in the returned partial reification
   *
   * @param egraph Target e-graph used as context for optimization.
   * @return A simplified command.
   */
  final def simplify(egraph: EGraph[NodeT]): Command[NodeT] =
    simplify(egraph, Map.empty)._1
}

/**
 * Factory methods for constructing common [[Command]] instances.
 */
object Command {

  /**
   * Creates a [[Command]] that asserts an existing [[EClassSymbol]] is equivalent to a given
   * expression tree.
   *
   * Internally, this:
   *   1. Inserts the tree (creating a fresh virtual symbol for its root if needed)
   *   2. Unions that root with `symbol`
   *
   * This is the canonical “add-and-unify” operation when you want to grow the e-graph with a
   * concrete term and immediately equate it with an existing class.
   *
   * @param symbol E-class symbol to unify with the tree’s root.
   * @param tree   Expression to insert/reuse and equate.
   * @tparam NodeT Node type for the expression.
   * @return A compound command performing the insert and the union.
   *
   * @example
   * {{{
   * import foresight.eqsat.commands.Command
   *
   * val cmd: Command[MyNode] =
   *   Command.equivalence(existingSym, myTree)
   *
   * val (optGraph, out) =
   *   cmd.simplify(egraph).apply(egraph, Map.empty, parallel)
   * }}}
   */
  def equivalence[NodeT](
                          symbol: EClassSymbol,
                          tree: MixedTree[NodeT, EClassSymbol]
                        ): Command[NodeT] = {
    val builder = new CommandQueueBuilder[NodeT]
    val c = builder.add(tree)
    builder.union(symbol, c)
    builder.result()
  }

  /**
   * Creates a [[Command]] that asserts an existing [[EClassSymbol]] is equivalent to a given
   * expression tree, simplifying the command with respect to the given e-graph.
   *
   * Internally, this:
   *   1. Inserts the tree (creating a fresh virtual symbol for its root if needed)
   *   2. Unions that root with `symbol`
   *   3. Simplifies the resulting command with respect to `egraph`
   *
   * This is the canonical “add-and-unify” operation when you want to grow the e-graph with a
   * concrete term and immediately equate it with an existing class, while avoiding redundant work.
   *
   * @param symbol E-class symbol to unify with the tree’s root.
   * @param tree   Expression to insert/reuse and equate.
   * @param egraph E-graph used as context for simplification.
   * @tparam NodeT Node type for the expression.
   * @return A compound command performing the insert and the union, simplified with respect to `egraph`.
   *
   * @example
   * {{{
   * import foresight.eqsat.commands.Command
   *
   * val cmd: Command[MyNode] =
   *   Command.equivalenceSimplified(existingSym, myTree, egraph)
   *
   * val (optGraph, out) =
   *   cmd.apply(egraph, Map.empty, parallel)
   * }}}
   */
  def equivalenceSimplified[NodeT](
                                    symbol: EClassSymbol,
                                    tree: MixedTree[NodeT, EClassSymbol],
                                    egraph: EGraph[NodeT]
                                  ): Command[NodeT] = {
    val builder = new CommandQueueBuilder[NodeT]
    val c = builder.addSimplified(tree, egraph)
    builder.unionSimplified(symbol, c, egraph)
    builder.result()
  }
}
