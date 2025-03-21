package fixpoint.eqsat.extraction

import scala.math.Ordered.orderingToOrdered
import scala.math.Ordering.Implicits.seqDerivedOrdering

/**
 * An ordering for extraction trees.
 * @param costOrdering The ordering for the cost.
 * @param nodeOrdering The ordering for the nodes.
 * @tparam NodeT The type of the nodes.
 * @tparam C The type of the cost.
 */
final case class ExtractionTreeOrdering[NodeT, C](implicit costOrdering: Ordering[C],
                                                  nodeOrdering: Ordering[NodeT]) extends Ordering[ExtractionTree[NodeT, C]] {
  override def compare(x: ExtractionTree[NodeT, C], y: ExtractionTree[NodeT, C]): Int = {
    (x.cost, x.size, x.depth, x.nodeType, x.definitions, x.uses, x.args) compare
      (y.cost, y.size, y.depth, y.nodeType, y.definitions, y.uses, y.args)
  }

  private implicit val treeOrdering: Ordering[ExtractionTree[NodeT, C]] = this

  /**
   * An ordering for extraction tree calls.
   */
  implicit val callOrdering: Ordering[ExtractionTreeCall[NodeT, C]] = ExtractionTreeCallOrdering

  private final object ExtractionTreeCallOrdering extends Ordering[ExtractionTreeCall[NodeT, C]] {

    override def compare(x: ExtractionTreeCall[NodeT, C], y: ExtractionTreeCall[NodeT, C]): Int = {
      (x.renaming, x.tree) compare (y.renaming, y.tree)
    }
  }
}
