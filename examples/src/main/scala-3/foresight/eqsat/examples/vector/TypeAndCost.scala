package foresight.eqsat.examples.vector

/**
 * A pair of an expression's type and its associated cost.
 *
 * @param t    The type of the expression.
 * @param cost The cost associated with the expression.
 */
final case class TypeAndCost(t: Type, cost: Int)

/**
 * Companion object for `TypeAndCost` providing an implicit ordering based on cost.
 */
object TypeAndCost {
  /** Implicit ordering for `TypeAndCost` based on the `cost` field. */
  given Ordering[TypeAndCost] = Ordering.by(_.cost)
}
