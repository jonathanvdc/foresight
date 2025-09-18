package foresight.eqsat.extraction

import foresight.eqsat.{Slot, Tree}
import foresight.eqsat.collections.SlotMap

/**
 * A node-local cost model used during extraction.
 *
 * A `CostFunction` assigns a cost to a single node given:
 *   - its operator/type (`nodeType`),
 *   - which slots it defines locally (`definitions`),
 *   - which slots it uses that are defined elsewhere (`uses`),
 *   - and the already-costed children (`args`).
 *
 * The cost is computed bottom-up: each `args` entry is an [[ExtractionTreeCall]] that already
 * contains the child's cost, so implementations can simply aggregate child costs and add any
 * node-local contribution.
 *
 * This interface is deliberately minimal:
 *   - It is agnostic to the meaning of `C` (numbers, tuples, custom records, etc.).
 *   - It does not prescribe how to combine costs; that policy lives in your implementation.
 *   - It assumes purity: for the same inputs, return the same cost.
 *
 * @tparam NodeT The node/operator type used in the tree.
 * @tparam C     The cost type produced by the function.
 *
 * @example {{{
 * // Simple additive cost: weight(node) + sum(child costs)
 * final case class NumCost(value: Int)
 *
 * val weights: Map[Op, Int] = ...
 *
 * object AdditiveCost extends CostFunction[Op, NumCost] {
 *   def apply(nodeType: Op,
 *             definitions: Seq[Slot],
 *             uses: Seq[Slot],
 *             args: Seq[ExtractionTreeCall[Op, NumCost]]): NumCost = {
 *     val childSum = args.iterator.map(_.tree.cost.value).sum
 *     NumCost(weights.getOrElse(nodeType, 1) + childSum)
 *   }
 * }
 * }}}
 */
trait CostFunction[NodeT, C] {

  /**
   * Computes the cost contribution of a single node, given its already-costed children.
   *
   * @param nodeType    The operator/type of the current node.
   * @param definitions Slots defined by this node itself (e.g., outputs or bindings introduced here).
   * @param uses        Slots used by this node but defined elsewhere (its external dependencies).
   * @param args        Already-costed children, in evaluation order.
   * @return The total cost for the subtree rooted at this node (including children).
   *
   * @note Implementations are expected to be pure and total. Avoid relying on global state.
   */
  def apply(nodeType: NodeT,
            definitions: Seq[Slot],
            uses: Seq[Slot],
            args: Seq[C]): C

  /**
   * Computes the cost of a whole [[Tree]] by recursively converting it to an
   * [[ExtractionTree]] and reading its accumulated cost.
   *
   * @param tree The root tree to evaluate.
   * @return The cost of the entire tree.
   *
   * @example {{{
   * val cost: C = myCostFn(programTree)
   * }}}
   */
  final def apply(tree: Tree[NodeT]): C = {
    toExtractionTree(tree).cost
  }

  // Internal helpers: convert a plain Tree into cost-annotated extraction forms.

  private final def toExtractionTreeCall(tree: Tree[NodeT]): ExtractionTreeCall[NodeT, C] = {
    val extractionTree = toExtractionTree(tree)
    ExtractionTreeCall(
      extractionTree,
      SlotMap.identity(extractionTree.slotSet)
    )
  }

  private final def toExtractionTree(tree: Tree[NodeT]): ExtractionTree[NodeT, C] = {
    val args = tree.args.map(toExtractionTreeCall)
    ExtractionTree(
      apply(tree.nodeType, tree.definitions, tree.uses, args.map(_.cost)),
      tree.nodeType,
      tree.definitions,
      tree.uses,
      args
    )
  }
}

/**
 * Common preset cost functions for typical extraction strategies.
 */
object CostFunction {
  /**
   * A cost function that counts the total number of nodes in a tree,
   * defined as `1 + sum(child sizes)`.
   *
   * @tparam NodeT The node/operator type (unused).
   * @return A cost function that counts nodes.
   *
   * @example {{{
   * val countNodes: CostFunction[Op, Int] = CostFunction.size[Op]
   * val numNodes: Int = countNodes(someTree) // total node count
   * }}}
   */
  def size[NodeT]: CostFunction[NodeT, Int] = new CostFunction[NodeT, Int] {
    override def apply(nodeType: NodeT,
                       definitions: Seq[Slot],
                       uses: Seq[Slot],
                       args: Seq[Int]): Int = {
      args.sum + 1
    }
  }

  /**
   * A cost function that computes the depth (height) of a tree, defined as
   * `1 + max(child depths)`, with leaf nodes having depth `1`.
   *
   * @tparam NodeT The node/operator type (unused).
   * @return A cost function that computes tree depth.
   *
   * @example {{{
   * val computeDepth: CostFunction[Op, Int] = CostFunction.depth[Op]
   * val treeDepth: Int = computeDepth(someTree) // maximum depth
   * }}}
   */
  def depth[NodeT]: CostFunction[NodeT, Int] = new CostFunction[NodeT, Int] {
    override def apply(nodeType: NodeT,
                       definitions: Seq[Slot],
                       uses: Seq[Slot],
                       args: Seq[Int]): Int = {
      (args :+ 0).max + 1
    }
  }
}
