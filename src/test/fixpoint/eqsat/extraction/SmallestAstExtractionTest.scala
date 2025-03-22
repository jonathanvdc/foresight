package fixpoint.eqsat.extraction

import fixpoint.eqsat.{EGraph, Slot, Tree}
import org.junit.Test

class SmallestAstExtractionTest {
  /**
   * Test extracting a single node.
   */
  @Test
  def extractFromSingleNode(): Unit = {
    val node = Tree.unslotted(0, Seq.empty)

    val analysis = ExtractionAnalysis.smallest[Int]

    val egraph = EGraph.empty[Int].withMetadata.addAnalysis(analysis)
    val (eclass, egraph2) = egraph.add(node)

    assert(node == analysis.extractor(eclass, egraph2))
  }

  /**
   * Test extracting a tree.
   */
  @Test
  def extractFromTree(): Unit = {
    val tree = Tree.unslotted(0, Seq(Tree.unslotted(1, Seq.empty), Tree.unslotted(2, Seq.empty)))

    val analysis = ExtractionAnalysis.smallest[Int]

    val egraph = EGraph.empty[Int].withMetadata.addAnalysis(analysis)
    val (eclass, egraph2) = egraph.add(tree)

    assert(tree == analysis.extractor(eclass, egraph2))
  }

  /**
   * Test that extraction prefers smaller trees when there are multiple equivalent trees.
   */
  @Test
  def extractSimplifiedExpr(): Unit = {
    sealed trait ArithNode extends Ordered[ArithNode] {
      override def compare(that: ArithNode): Int = this.hashCode().compareTo(that.hashCode())
    }
    case object Var extends ArithNode
    case object Minus extends ArithNode
    case class Const(value: Int) extends ArithNode

    val x = Slot.fresh()

    val tree = Tree(
      Minus,
      Seq.empty,
      Seq.empty,
      Seq(
        Tree(Var, Seq.empty, Seq(x), Seq.empty),
        Tree(Var, Seq.empty, Seq(x), Seq.empty)))

    val analysis = ExtractionAnalysis.smallest[ArithNode]

    val egraph = EGraph.empty[ArithNode].withMetadata.addAnalysis(analysis)
    val (c1, egraph2) = egraph.add(tree)

    assert(tree == analysis.extractor(c1, egraph2))

    val zero = Tree(Const(0), Seq.empty, Seq.empty, Seq.empty)
    val (c2, egraph3) = egraph2.add(zero)

    assert(tree == analysis.extractor(c1, egraph3))
    assert(zero == analysis.extractor(c2, egraph3))

    val egraph4 = egraph3.union(c1, c2).rebuilt

    assert(zero == analysis.extractor(c1, egraph4))
  }
}
