package fixpoint.eqsat

/**
 * An e-graph with pending unions. This class is used to represent an e-graph that has unions that have not been applied yet.
 * @param egraph The e-graph.
 * @param pending The pending unions.
 * @tparam Repr The type of the e-graph.
 */
final case class EGraphWithPendingUnions[+Repr <: EGraphLike[_, Repr] with EGraph[_]](egraph: Repr,
                                                                                      pending: List[(EClassCall, EClassCall)]) {
  /**
   * Determines whether the e-graph requires a rebuild.
   * @return True if the e-graph requires a rebuild, otherwise false.
   */
  def requiresRebuild: Boolean = pending.nonEmpty

  /**
   * Unions two e-classes in this e-graph. The resulting e-class contains all e-nodes from both e-classes.
   * The effects of this operation are deferred until the e-graph is rebuilt.
   *
   * @param left The reference to the first e-class to union.
   * @param right The reference to the second e-class to union.
   * @return The e-class reference of the resulting e-class, and the new e-graph with the e-classes unioned.
   */
  def union(left: EClassCall, right: EClassCall): EGraphWithPendingUnions[Repr] = {
    if (egraph.areSame(left, right)) {
      this
    } else {
      EGraphWithPendingUnions(egraph, (left, right) :: pending)
    }
  }

  /**
   * Rebuilds the e-graph, applying all pending unions.
   * @return The new e-graph with the e-graph rebuilt.
   */
  def rebuilt: Repr = {
    if (pending.isEmpty) {
      egraph
    } else {
      egraph.unionMany(pending)._2
    }
  }
}

/**
 * A companion object for the e-graph with pending unions.
 */
object EGraphWithPendingUnions {
  /**
   * Creates a new e-graph with pending unions.
   * @param egraph The e-graph.
   * @tparam Repr The type of the e-graph.
   * @return An e-graph with pending unions.
   */
  def apply[Repr <: EGraphLike[_, Repr] with EGraph[_]](egraph: Repr): EGraphWithPendingUnions[Repr] =
    EGraphWithPendingUnions(egraph, Nil)
}
