package eqsat

/**
 * An immutable e-graph. An e-graph is a data structure that represents a set of expressions. Each expression is
 * represented by an e-node, which is a node in the e-graph. E-nodes are grouped into e-classes, which are sets of
 * equivalent e-nodes.
 * @tparam ExprT The type of the expression that the e-graph represents.
 */
trait ImmutableEGraph[ExprT] {
  /**
   * Gets the current canonical reference to an e-class, if the e-class is defined in this e-graph; otherwise, returns
   * None.
   * @param ref The reference to canonicalize.
   * @return The canonical reference to the e-class pointed to by ref, if the e-class is defined in this e-graph;
   *         otherwise, None.
   */
  def tryCanonicalize(ref: EClassRef): Option[EClassRef]

  /**
   * Gets the current canonical reference to an e-class.
   * @param ref The reference to canonicalize.
   * @return The canonical reference to the e-class pointed to by ref.
   */
  def canonicalize(ref: EClassRef): EClassRef = tryCanonicalize(ref).get

  /**
   * The set of nodes of a given e-class.
   * @param ref The e-class whose nodes to find.
   * @return All nodes in the e-class pointed to by ref.
   */
  def nodes(ref: EClassRef): Seq[ENode[ExprT]]

  /**
   * Finds the e-class of a given e-node.
   * @param node The e-node to find the e-class of.
   * @return The e-class of the e-node, if it is defined in this e-graph; otherwise, None.
   */
  def find(node: ENode[ExprT]): Option[EClassRef]

  /**
   * Adds an e-node to this e-graph If it is already present, then the e-node is not added, and the e-class reference of
   * the existing e-node is returned. Otherwise, the e-node is added to a unique e-class, whose reference is returned.
   * @param node The e-node to add to the e-graph.
   * @return The e-class reference of the e-node in the e-graph, and the new e-graph with the e-node added.
   */
  def add(node: ENode[ExprT]): (EClassRef, ImmutableEGraph[ExprT])

  /**
   * Unions two e-classes in this e-graph. The e-class reference of the resulting e-class is returned.
   * @param left The reference to the first e-class to union.
   * @param right The reference to the second e-class to union.
   * @return The e-class reference of the resulting e-class, and the new e-graph with the e-classes unioned.
   */
  def union(left: EClassRef, right: EClassRef): (EClassRef, ImmutableEGraph[ExprT])

  /**
   * Determines whether the e-graph requires a rebuild. A rebuild is required when the e-graph is in an inconsistent
   * state.
   * @return True if the e-graph requires a rebuild; otherwise, false.
   */
  def requiresRebuild: Boolean

  /**
   * Rebuilds the e-graph. This operation is used to ensure that the e-graph is in a consistent state.
   * @return The new e-graph with the e-graph rebuilt.
   */
  def rebuilt: ImmutableEGraph[ExprT]
}
