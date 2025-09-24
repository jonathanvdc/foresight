package foresight.eqsat.commands

import foresight.eqsat.{EClassCall, EClassSymbol, ENode, Slot}

/**
 * Symbolic handle for an [[ENode]] in an e-graph.
 *
 * Unlike a concrete [[ENode]], an [[ENodeSymbol]] may reference [[EClassSymbol.Virtual]]
 * arguments and therefore may not yet exist in the e-graph. It describes:
 *   - the node's operator or type (`nodeType`),
 *   - the value slots it defines (`definitions`),
 *   - the value slots it consumes (`uses`),
 *   - and its child e-classes (`args`).
 *
 * Symbols are typically produced by [[Command]] instances to describe planned insertions.
 *
 * @param nodeType Operator or label of the e-node (e.g., `+`, `*`, function name).
 * @param definitions Slots whose values are defined directly by this node.
 * @param uses Slots whose values are read directly by this node.
 * @param args Child e-class symbols representing the node's operands.
 * @tparam NodeT Domain-specific type used to represent operators or node labels.
 */
final case class ENodeSymbol[NodeT](
                                     nodeType: NodeT,
                                     definitions: Seq[Slot],
                                     uses: Seq[Slot],
                                     args: Seq[EClassSymbol]
                                   ) {

  /**
   * Resolves this symbolic node to a concrete [[ENode]] using the given reification.
   *
   * All [[EClassSymbol.Virtual]]s in [[args]] must be present in the `reification` map.
   * Any [[EClassCall]] in [[args]] is left unchanged.
   *
   * @param reification Mapping from virtual e-class symbols to concrete [[EClassCall]]s.
   * @return An [[ENode]] with all arguments fully resolved.
   *
   * @example
   * {{{
   * val v1 = EClassSymbol.virtual()
   * val call1 = EClassCall(...)
   *
   * val symbol = ENodeSymbol(
   *   nodeType = MyOp.Add,
   *   definitions = Seq.empty,
   *   uses = Seq.empty,
   *   args = Seq(v1)
   * )
   *
   * val node: ENode[MyOp] = symbol.reify(Map(v1 -> call1))
   * }}}
   */
  def reify(reification: Map[EClassSymbol.Virtual, EClassCall]): ENode[NodeT] = {
    val reifiedArgs = args.map(_.reify(reification))
    ENode(nodeType, definitions, uses, reifiedArgs)
  }
}
