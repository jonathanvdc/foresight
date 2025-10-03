package foresight.eqsat.commands

import foresight.eqsat.collections.SlotSeq
import foresight.eqsat.{EClassCall, EClassSymbol, ENode}

import scala.collection.compat.immutable.ArraySeq

/**
 * Symbolic description of either an [[ENode]] in an e-graph or a planned insertion of one.
 *
 * Unlike a concrete [[ENode]], an [[ENodeSymbol]] may reference [[EClassSymbol.Virtual]]
 * arguments and therefore may not yet exist in the e-graph. It describes:
 *   - the node's operator or type (`nodeType`),
 *   - the value slots it defines (`definitions`),
 *   - the value slots it consumes (`uses`),
 *   - and its child e-classes (`args`).
 */
trait ENodeSymbol[NodeT] {
  /**
   * Operator or label of the e-node (e.g., `+`, `*`, function name).
   * @return The node's type or operator.
   */
  def nodeType: NodeT

  /**
   * Slots whose values are defined directly by this node.
   * @return The node's definition slots.
   */
  def definitions: SlotSeq

  /**
   * Slots whose values are read directly by this node.
   * @return The node's usage slots.
   */
  def uses: SlotSeq

  /**
   * Child e-class symbols representing the node's operands.
   * These may be either concrete [[EClassCall]]s or [[EClassSymbol.Virtual]]s.
   * @return The node's argument e-classes.
   */
  def args: ArraySeq[EClassSymbol]

  /**
   * Resolves this symbolic node to a concrete [[ENode]] using the given reification.
   *
   * All [[EClassSymbol.Virtual]]s in [[args]] must be present in the `reification` map.
   * Any [[EClassCall]] in [[args]] is left unchanged.
   *
   * @param reification Mapping from virtual e-class symbols to concrete [[EClassCall]]s.
   * @return An [[ENode]] with all arguments fully resolved.
   */
  def reify(reification: collection.Map[EClassSymbol.Virtual, EClassCall]): ENode[NodeT]

  /**
   * Creates a copy of this symbol with the given arguments.
   * @param newArgs New argument e-class symbols.
   * @return A copy of this symbol with `args` replaced by `newArgs`.
   */
  def withArgs(newArgs: ArraySeq[EClassSymbol]): ENodeSymbol[NodeT] = {
    ENodeSymbol(nodeType, definitions, uses, newArgs)
  }
}

object ENodeSymbol {
  def apply[NodeT](
                    nodeType: NodeT,
                    definitions: SlotSeq,
                    uses: SlotSeq,
                    args: ArraySeq[EClassSymbol]
                  ): ENodeSymbol[NodeT] = {
    Virtual(nodeType, definitions, uses, args)
  }

  /**
   * Symbolic description of either an [[ENode]] in an e-graph or a planned insertion of one.
   *
   * Unlike a concrete [[ENode]], an [[ENodeSymbol]] may reference [[EClassSymbol.Virtual]]
   * arguments and therefore may not yet exist in the e-graph. It describes:
   *   - the node's operator or type (`nodeType`),
   *   - the value slots it defines (`definitions`),
   *   - the value slots it consumes (`uses`),
   *   - and its child e-classes (`args`).
   *
   * @param nodeType    Operator or label of the e-node (e.g., `+`, `*`, function name).
   * @param definitions Slots whose values are defined directly by this node.
   * @param uses        Slots whose values are read directly by this node.
   * @param args        Child e-class symbols representing the node's operands.
   * @tparam NodeT Domain-specific type used to represent operators or node labels.
   */
  final case class Virtual[NodeT](
                                   nodeType: NodeT,
                                   definitions: SlotSeq,
                                   uses: SlotSeq,
                                   args: ArraySeq[EClassSymbol]
                                 ) extends ENodeSymbol[NodeT] {

    /**
     * Resolves this symbolic node to a concrete [[ENode]] using the given reification.
     *
     * All [[EClassSymbol.Virtual]]s in [[args]] must be present in the `reification` map.
     * Any [[EClassCall]] in [[args]] is left unchanged.
     *
     * @param reification Mapping from virtual e-class symbols to concrete [[EClassCall]]s.
     * @return An [[ENode]] with all arguments fully resolved.
     * @example
     * {{{
     * val v1 = EClassSymbol.virtual()
     * val call1 = EClassCall(...)
     *
     * val symbol = ENodeSymbol(
     *   nodeType = MyOp.Add,
     *   definitions = SlotSeq.empty,
     *   uses = SlotSeq.empty,
     *   args = Seq(v1)
     * )
     *
     * val node: ENode[MyOp] = symbol.reify(Map(v1 -> call1))
     * }}}
     */
    def reify(reification: collection.Map[EClassSymbol.Virtual, EClassCall]): ENode[NodeT] = {
      val reifiedArgs = args.map(_.reify(reification))
      ENode(nodeType, definitions, uses, reifiedArgs)
    }
  }
}
