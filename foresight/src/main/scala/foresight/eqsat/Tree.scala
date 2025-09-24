package foresight.eqsat

import foresight.eqsat.collections.SlotSeq
import foresight.util.collections.UnsafeSeqFromArray

import scala.collection.compat.*

/**
 * An immutable, slot-aware, ordered tree representation of a term.
 *
 * Each node contains:
 *   - `nodeType`: The domain-specific operator or constructor at this position in the tree.
 *   - `definitions`: The slots bound by this node (e.g., local variables it introduces).
 *   - `uses`: The slots referenced by this node that are defined elsewhere in the tree.
 *   - `args`: A sequence of child subtrees.
 *
 * `Tree` is the structural (non-hash-consed) analogue to [[MixedTree]] and [[ENode]].
 * Unlike an e-graph, which merges equivalent subterms, a `Tree` retains explicit
 * duplication and hierarchy, making it suitable for parsing, printing, and intermediate
 * transformations prior to e-graph insertion.
 *
 * Slot information makes the `Tree` representation aware of variable binding and usage
 * sites, enabling correct handling of alpha-equivalence and scoping-sensitive rewrites.
 *
 * @param nodeType    The operator or constructor at this node.
 * @param definitions Slots bound/introduced by this node itself.
 * @param uses        Slots consumed by this node but defined elsewhere.
 * @param args        Ordered children of this node.
 * @tparam NodeT      The type used to represent operators/constructors.
 *
 * @example
 * {{{
 * // Construct a simple tree for: λx. x
 * val xSlot = Slot.fresh()
 * val body  = Tree("var", definitions = Seq.empty, uses = Seq(xSlot), args = Seq.empty)
 * val lam   = Tree("lambda", definitions = Seq(xSlot), uses = Seq.empty, args = Seq(body))
 *
 * assert(lam.definitions == Seq(xSlot))
 * assert(lam.args.head.uses.contains(xSlot))
 *
 * // Map over node types to produce a pretty-printed label:
 * val labeled = lam.map(op => s"<$op>")
 * }}}
 */
final case class Tree[+NodeT](
                               nodeType: NodeT,
                               definitions: SlotSeq,
                               uses: SlotSeq,
                               args: immutable.ArraySeq[Tree[NodeT]]
                             ) {

  /**
   * Transform the `nodeType` at every position in the tree.
   *
   * This is a pure top-down map that preserves slot structure and argument order.
   *
   * @param f         Function to transform the `nodeType` at each node.
   * @tparam NewNodeT The resulting node type.
   * @return          A structurally identical tree with `nodeType` replaced according to `f`.
   */
  def map[NewNodeT](f: NodeT => NewNodeT): Tree[NewNodeT] =
    Tree(f(nodeType), definitions, uses, args.map(_.map(f)))
}

/**
 * Companion utilities for [[Tree]].
 */
object Tree {

  /**
   * Construct a `Tree` node.
   *
   * @param nodeType    The operator or constructor at this node.
   * @param definitions Slots bound/introduced by this node itself.
   * @param uses        Slots consumed by this node but defined elsewhere.
   * @param args        Ordered children of this node.
   * @tparam NodeT      The type used to represent operators/constructors.
   * @return            A new `Tree` with the given structure.
   */
  def apply[NodeT](nodeType: NodeT, definitions: Seq[Slot], uses: Seq[Slot], args: Seq[Tree[NodeT]]): Tree[NodeT] = {
    new Tree(
      nodeType,
      SlotSeq.from(definitions),
      SlotSeq.from(uses),
      UnsafeSeqFromArray(args))
  }

  /**
   * Construct a `Tree` node with no bound or used slots.
   *
   * This is useful for building trees in contexts without any slot bindings.
   *
   * @param nodeType The operator or constructor at this node.
   * @param args     The ordered children of the node.
   * @tparam NodeT   The type used to represent operators/constructors.
   * @return         A new `Tree` with empty `definitions` and `uses`.
   */
  def unslotted[NodeT](nodeType: NodeT, args: Seq[Tree[NodeT]]): Tree[NodeT] =
    Tree(nodeType, immutable.ArraySeq.empty[Slot], immutable.ArraySeq.empty[Slot], args)
}
