package fixpoint.eqsat.commands

import fixpoint.eqsat.Slot

/**
 * A symbolic e-node that may or may not yet be in the e-graph.
 * @param nodeType The type of the e-node.
 * @param definitions The slots that are defined directly by the e-node.
 * @param uses The slots that are used directly by the e-node.
 * @param args The arguments of the e-node, represented as e-class symbols.
 * @tparam NodeT The type of the e-node.
 */
final case class ENodeSymbol[NodeT](nodeType: NodeT, definitions: Seq[Slot], uses: Seq[Slot], args: Seq[EClassSymbol])
