package fixpoint.eqsat.extraction

import fixpoint.eqsat.{Slot, SlotMap}

/**
 * A tree data structure for extraction.
 * @param cost The cost of the tree.
 * @param depth The depth of the tree.
 * @param nodeType The type of the tree's root.
 * @param definitions The slots of the root that are defined by the root itself.
 * @param uses The slots of the root that are used by the root and are defined elsewhere.
 * @param args The children of the root node.
 * @tparam NodeT The type of the nodes in the tree.
 * @tparam C The type of the cost.
 */
final case class ExtractionTree[+NodeT, C](cost: C,
                                           depth: Int,
                                           nodeType: NodeT,
                                           definitions: Seq[Slot],
                                           uses: Seq[Slot],
                                           args: Seq[(ExtractionTree[NodeT, C], SlotMap)])
