package fixpoint.eqsat.rewriting.patterns

/**
 * A pattern that can be matched against e-classes in e-graphs.
 *
 * @tparam NodeT The type of the nodes in the pattern.
 */
sealed trait Pattern[NodeT]

/**
 * A companion object for patterns.
 */
object Pattern {
  /**
   * A variable in a pattern. This pattern matches any e-class.
   * @tparam NodeT The type of the nodes.
   */
  final class Var[NodeT] extends Pattern[NodeT]

  /**
   * A companion object for Var.
   */
  object Var {
    /**
     * Creates a fresh variable.
     * @tparam NodeT The type of the nodes.
     * @return The fresh variable.
     */
    def fresh[NodeT](): Var[NodeT] = new Var[NodeT]
  }
}
