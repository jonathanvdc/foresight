package foresight.eqsat.commands

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.{EClassCall, EClassSymbol}
import foresight.eqsat.mutable
import foresight.eqsat.readonly

/**
 * A [[Command]] that unions multiple pairs of e-classes in a single batch.
 *
 * Each entry in [[pairs]] may contain real or virtual [[EClassSymbol]]s. Virtual symbols are
 * resolved via the reification map passed to [[apply]]. All unions are accumulated using
 * [[EGraphWithPendingUnions]] and committed in one rebuild step if needed.
 *
 * This command defines no new virtual symbols; it only merges existing classes.
 *
 * @tparam NodeT Node type for expressions represented by the e-graph.
 * @param pairs Pairs of symbols whose underlying classes should be unified.
 */
final case class UnionManyCommand[NodeT](pairs: Seq[(EClassSymbol, EClassSymbol)]) extends Command[NodeT] {

  /** All symbols referenced by this batch of unions. */
  override def uses: Seq[EClassSymbol] =
    pairs.flatMap { case (l, r) => Seq(l, r) }

  /** No new e-classes are created by a union; this command defines nothing. */
  override def definitions: Seq[EClassSymbol.Virtual] = Seq.empty

  /**
   * Resolves all symbols using the given map and enqueues each union into
   * an [[EGraphWithPendingUnions]]. If any union changes the structure,
   * a rebuild is performed.
   *
   * @param egraph Target e-graph to update.
   * @param reification Mapping from virtual symbols to concrete calls,
   *                    used to resolve the left/right sides before unioning.
   * @param parallelize Strategy for distributing the rebuild, if needed.
   * @return
   *   - `true` if at least one union required changes (triggering a rebuild),
   *     otherwise `false`.
   *   - An empty reification map (unions do not define outputs).
   *
   * @example
   * {{{
   * val a: EClassSymbol = EClassSymbol.real(callA)
   * val b: EClassSymbol = EClassSymbol.real(callB)
   * val cmd = UnionManyCommand(Seq(a -> b))
   * val (updated, _) = cmd.apply(egraph, Map.empty, parallel)
   * }}}
   */
  override def apply(
                      egraph: mutable.EGraph[NodeT],
                      reification: Map[EClassSymbol.Virtual, EClassCall],
                      parallelize: ParallelMap
                    ): (Boolean, Map[EClassSymbol.Virtual, EClassCall]) = {
    val reifiedPairs = pairs
      .map { case (l, r) => (l.reify(reification), r.reify(reification)) }
      .filter { case (l, r) => !egraph.areSame(l, r) }

    if (reifiedPairs.isEmpty) {
      (false, Map.empty)
    }
    else {
      egraph.unionMany(reifiedPairs, parallelize)
      (true, Map.empty)
    }
  }

  /**
   * Simplifies the union set against the current e-graph and partial bindings.
   *
   * Each side is first refined using `partialReification`. Pairs that become
   * two real calls already known to be equal are dropped. If all pairs drop,
   * the result is [[CommandQueue.empty]]; otherwise a reduced [[UnionManyCommand]]
   * is returned.
   *
   * @param egraph Context used for equality checks.
   * @param partialReification Known virtual-to-real bindings.
   * @return A simplified command and an (empty) partial reification.
   *
   * @example
   * {{{
   * val v = EClassSymbol.virtual()
   * val simplified = UnionManyCommand(Seq(v -> EClassSymbol.real(callX)))
   *   .simplify(egraph, Map(v -> callX))
   * // Becomes CommandQueue.empty because both sides resolve to the same class.
   * }}}
   */
  override def simplify(
                         egraph: readonly.EGraph[NodeT],
                         partialReification: Map[EClassSymbol.Virtual, EClassCall]
                       ): (Command[NodeT], Map[EClassSymbol.Virtual, EClassCall]) = {
    val builder = Seq.newBuilder[(EClassSymbol, EClassSymbol)]
    for ((left, right) <- pairs) {
      val lRefined = left.refine(partialReification)
      val rRefined = right.refine(partialReification)
      (lRefined, rRefined) match {
        case (l: EClassCall, r: EClassCall) =>
          if (!egraph.areSame(l, r)) builder += ((lRefined, rRefined))
        case _ =>
          builder += ((lRefined, rRefined))
      }
    }
    val simplifiedPairs = builder.result()
    if (simplifiedPairs.isEmpty) (CommandQueue.empty, Map.empty)
    else (UnionManyCommand(simplifiedPairs), Map.empty)
  }
}
