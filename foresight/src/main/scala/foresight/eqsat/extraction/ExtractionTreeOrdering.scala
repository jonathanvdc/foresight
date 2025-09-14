package foresight.eqsat.extraction

import foresight.eqsat.Slot

/**
 * Total ordering for [[ExtractionTree]] values used during extraction tie-breaking.
 *
 * Comparison is lexicographic over the following tuple:
 * `(cost, size, depth, nodeType, definitions, uses, args)`.
 *
 *  1) `cost` — primary key, ordered by the implicit `Ordering[C]`.
 *  2) `size` — total node count; prefers smaller trees when costs tie.
 *  3) `depth` — tree height; prefers shallower trees after size.
 *  4) `nodeType` — ordered by the implicit `Ordering[NodeT]` for deterministic choice
 *     among structurally identical candidates.
 *  5) `definitions` — sequence of locally defined slots; compared lexicographically.
 *  6) `uses` — sequence of externally used slots; compared lexicographically.
 *  7) `args` — sequence of child trees; compared lexicographically (uses this ordering recursively).
 *
 * Performance: comparing two trees may traverse corresponding subtrees; worst-case is proportional
 * to the size of the compared structures when earlier keys (cost/size/depth) do not decide quickly.
 *
 * @param costOrdering Implicit total ordering for the cost domain `C`.
 * @param nodeOrdering Implicit deterministic ordering for `NodeT`, used as a tie-breaker.
 * @tparam NodeT The node/operator type.
 * @tparam C     The cost type.
 *
 * @example
 * {{{
 * implicit val costOrd: Ordering[Int] = Ordering.Int
 * implicit val nodeOrd: Ordering[Op]  = ...
 * val ord = ExtractionTreeOrdering[Op, Int]()
 *
 * val best: ExtractionTreeCall[Op, Int] =
 *   List(callA, callB, callC).min(ord.callOrdering)
 * }}}
 */
final case class ExtractionTreeOrdering[NodeT, C]()(implicit costOrdering: Ordering[C],
                                                    nodeOrdering: Ordering[NodeT])
  extends Ordering[ExtractionTree[NodeT, C]] {

  /**
   * Compares two extraction trees by the lexicographic tuple described in the class docs.
   */
  override def compare(x: ExtractionTree[NodeT, C], y: ExtractionTree[NodeT, C]): Int = {
    import foresight.util.ordering.SeqOrdering.lexOrdering

    Ordering.Tuple7[C, Int, Int, NodeT, Seq[Slot], Seq[Slot], Seq[ExtractionTreeCall[NodeT, C]]]
      .compare(
        (x.cost, x.size, x.depth, x.nodeType, x.definitions, x.uses, x.args),
        (y.cost, y.size, y.depth, y.nodeType, y.definitions, y.uses, y.args)
      )
  }

  // Enable recursive comparison of child trees (`args`) within this ordering.
  private implicit val treeOrdering: Ordering[ExtractionTree[NodeT, C]] = this

  /**
   * Ordering for [[ExtractionTreeCall]]s used by analyses and extractors.
   *
   * Primary key: the underlying tree, compared via `treeOrdering` above.
   * Tie-break: the call's slot renaming (`x.renaming.compare(y.renaming)`), ensuring a total order
   * even when trees and structure are identical but alpha-renamings differ.
   */
  implicit val callOrdering: Ordering[ExtractionTreeCall[NodeT, C]] = ExtractionTreeCallOrdering

  private object ExtractionTreeCallOrdering extends Ordering[ExtractionTreeCall[NodeT, C]] {
    override def compare(x: ExtractionTreeCall[NodeT, C], y: ExtractionTreeCall[NodeT, C]): Int = {
      treeOrdering.compare(x.tree, y.tree) match {
        case 0 => x.renaming.compare(y.renaming)
        case c => c
      }
    }
  }
}
