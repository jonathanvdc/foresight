package foresight.eqsat.rewriting.patterns

/**
 * A companion object for patterns.
 */
object Pattern {
  /**
   * A variable in a pattern. This pattern matches any e-class.
   */
  final class Var

  /**
   * A companion object for Var.
   */
  object Var {
    /**
     * Creates a fresh variable.
     * @return The fresh variable.
     */
    def fresh(): Var = new Var
  }
}
