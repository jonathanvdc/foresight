package foresight.eqsat

/**
 * A trait representing a node in a tree or e-graph.
 *
 * @tparam NodeT The type of the node.
 * @tparam ArgT The type of the arguments of the node.
 */
trait Node[+NodeT, ArgT] {
  /**
   * The type of the node.
   */
  def nodeType: NodeT

  /**
   * Slots introduced by this node that are scoped locally and invisible to parents. These are
   * redundant by construction at the boundary of this node and exist to model binders such as
   * lambda-abstraction or let.
   *
   * @return Sequence of definition slots.
   */
  def definitions: Seq[Slot]

  /**
   * Slots referenced by this node that are visible to its parent and must be satisfied by the
   * surrounding e-class application.
   *
   * @return Sequence of use slots.
   */
  def uses: Seq[Slot]

  /**
   * The arguments of the node.
   *
   * @return Sequence of arguments.
   */
  def args: Seq[ArgT]
}
