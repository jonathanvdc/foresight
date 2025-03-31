package foresight.eqsat.extraction

import foresight.eqsat.{EGraph, ENode, Slot, Tree}
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

    // Test that recomputing the analysis works too.
    assert(analysis(egraph2)(eclass, egraph2).applied.toTree == node)
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

    // Test that recomputing the analysis works too.
    assert(analysis(egraph2)(eclass, egraph2).applied.toTree == tree)
  }

  /**
   * Test extracting a tree with slots that appear in the root node.
   */
  @Test
  def extractWithSlots(): Unit = {
    sealed trait ArithNode extends Ordered[ArithNode] {
      override def compare(that: ArithNode): Int = this.hashCode().compareTo(that.hashCode())
    }
    case object Var extends ArithNode

    val x = Slot.fresh()
    val tree = Tree(Var, Seq.empty, Seq(x), Seq.empty)

    val analysis = ExtractionAnalysis.smallest[ArithNode]

    val egraph = EGraph.empty[ArithNode].withMetadata.addAnalysis(analysis)
    val (eclass, egraph2) = egraph.add(tree)

    assert(tree == analysis.extractor(eclass, egraph2))

    // Test that recomputing the analysis works too.
    assert(analysis(egraph2)(eclass, egraph2).applied.toTree == tree)
  }

  /**
   * Test extracting a tree with slots that do not appear in the root node.
   */
  @Test
  def extractWithSlotsIndirect(): Unit = {
    sealed trait ArithNode extends Ordered[ArithNode] {
      override def compare(that: ArithNode): Int = this.hashCode().compareTo(that.hashCode())
    }
    case object Var extends ArithNode
    case object Plus extends ArithNode

    val x = Slot.fresh()
    val y = Slot.fresh()
    val tree = Tree(
      Plus,
      Seq.empty,
      Seq.empty,
      Seq(Tree(Var, Seq.empty, Seq(x), Seq.empty), Tree(Var, Seq.empty, Seq(y), Seq.empty)))

    val analysis = ExtractionAnalysis.smallest[ArithNode]

    val egraph = EGraph.empty[ArithNode].withMetadata.addAnalysis(analysis)
    val (eclass, egraph2) = egraph.add(tree)

    assert(tree == analysis.extractor(eclass, egraph2))

    // Test that recomputing the analysis works too.
    assert(analysis(egraph2)(eclass, egraph2).applied.toTree == tree)
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

    // Test that recomputing the analysis works too.
    assert(analysis(egraph4)(c1, egraph4).applied.toTree == zero)
  }

  /**
   * Test that extraction terminates when there is a self-cycle.
   */
  @Test
  def extractFromSelfCycle(): Unit = {
    val analysis = ExtractionAnalysis.smallest[Int]

    val egraph = EGraph.empty[Int].withMetadata.addAnalysis(analysis)
    val node = Tree.unslotted(0, Seq.empty)
    val (c1, egraph2) = egraph.add(node)

    assert(analysis.extractor(c1, egraph2) == node)

    val selfCycle = ENode.unslotted(1, Seq(c1))
    val (c2, egraph3) = egraph2.add(selfCycle)

    assert(egraph3.classes.size == 2)
    assert(analysis.extractor(c2, egraph3) == Tree.unslotted(1, Seq(node)))

    val egraph4 = egraph3.union(c1, c2).rebuilt

    assert(egraph4.classes.size == 1)
    assert(analysis.extractor(c2, egraph4) == node)

    // Test that recomputing the analysis works too.
    assert(analysis(egraph4)(c2, egraph4).applied.toTree == node)
  }
}
