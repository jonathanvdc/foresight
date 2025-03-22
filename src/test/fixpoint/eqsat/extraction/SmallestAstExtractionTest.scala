package fixpoint.eqsat.extraction

import fixpoint.eqsat.{EGraph, Tree}
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
}
