package foresight.eqsat

/**
 * The result of adding a node to the e-graph.
 */
sealed trait AddNodeResult {
  /**
   * The e-class call corresponding to the e-node whose addition was attempted.
   */
  def call: EClassCall
}

/**
 * The companion object for [[AddNodeResult]].
 */
object AddNodeResult {
  /**
   * The e-node was already present in the e-graph.
   *
   * @param call The e-class call corresponding to the e-node that was already present.
   */
  final case class AlreadyThere(call: EClassCall) extends AddNodeResult

  /**
   * The e-node was added to the e-graph.
   *
   * @param call The e-class call corresponding to the e-node that was added.
   */
  final case class Added(call: EClassCall) extends AddNodeResult
}
