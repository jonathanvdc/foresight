package fixpoint.eqsat

import fixpoint.eqsat.rewriting.{Applier, Searcher}
import fixpoint.eqsat.rewriting.patterns.{CompiledPattern, MachineSearcherPhase, Pattern, PatternApplier, PatternCompiler, PatternMatch}

/**
 * A mixed tree that can contain both nodes and e-class calls.
 *
 * @tparam NodeT The type of the nodes in the tree.
 * @tparam CallT The type of the e-class calls in the tree.
 */
trait MixedTree[+NodeT, +CallT] {
  /**
   * Maps the nodes in the tree.
   * @param f The function to map the nodes.
   * @tparam NewNodeT The type of the new nodes.
   * @return The tree with the nodes mapped.
   */
  def mapNodes[NewNodeT](f: NodeT => NewNodeT): MixedTree[NewNodeT, CallT] = this match {
    case MixedTree.Node(nodeType, definitions, uses, children) =>
      MixedTree.Node[NewNodeT, CallT](f(nodeType), definitions, uses, children.map(_.mapNodes(f)))

    case MixedTree.Call(call) =>
      MixedTree.Call(call)
  }

  /**
   * Maps the e-class calls in the tree.
   * @param f The function to map the e-class calls.
   * @tparam NewCallT The type of the new e-class calls.
   * @return The tree with the e-class calls mapped.
   */
  def mapCalls[NewCallT](f: CallT => NewCallT): MixedTree[NodeT, NewCallT] = this match {
    case MixedTree.Node(nodeType, definitions, uses, children) =>
      MixedTree.Node[NodeT, NewCallT](nodeType, definitions, uses, children.map(_.mapCalls[NewCallT](f)))

    case MixedTree.Call(call) =>
      MixedTree.Call(f(call))
  }
}

/**
 * A companion object for mixed trees.
 */
object MixedTree {
  import scala.language.implicitConversions

  /**
   * Creates a slot-free node in a mixed tree.
   * @param nodeType The type of the node.
   * @param children The children of the node.
   * @tparam NodeT The type of the node.
   * @tparam CallT The type of the e-class calls.
   * @return The created node.
   */
  def unslotted[NodeT, CallT](nodeType: NodeT, children: Seq[MixedTree[NodeT, CallT]]): MixedTree[NodeT, CallT] = {
    MixedTree.Node(nodeType, Seq.empty, Seq.empty, children)
  }

  /**
   * A node in a mixed tree.
   * @param nodeType The type of the node.
   * @param definitions The slots that are defined directly by the node.
   * @param uses The slots that are used directly by the node.
   * @param children The children of the node.
   * @tparam NodeT The type of the node.
   * @tparam CallT The type of the e-class calls.
   */
  final case class Node[NodeT, CallT](nodeType: NodeT,
                                      definitions: Seq[Slot],
                                      uses: Seq[Slot],
                                      children: Seq[MixedTree[NodeT, CallT]]) extends MixedTree[NodeT, CallT]

  /**
   * A call in a mixed tree.
   * @param call The e-class call.
   * @tparam NodeT The type of the nodes.
   * @tparam CallT The type of the e-class calls.
   */
  final case class Call[NodeT, CallT](call: CallT) extends MixedTree[NodeT, CallT]

  /**
   * Converts an e-node to a mixed tree.
   * @param node The e-node to convert.
   * @tparam NodeT The type of the node.
   * @return The converted mixed tree.
   */
  implicit def fromENode[NodeT](node: ENode[NodeT]): MixedTree[NodeT, EClassCall] = {
    MixedTree.Node[NodeT, EClassCall](node.nodeType, node.definitions, node.uses, node.args.map(Call[NodeT, EClassCall]))
  }

  /**
   * Converts a tree to a mixed tree.
   * @param tree The tree to convert.
   * @tparam NodeT The type of the nodes in the tree.
   * @tparam A The type of the arguments in the tree.
   * @return The converted mixed tree.
   */
  implicit def fromTree[NodeT, A](tree: Tree[NodeT]): MixedTree[NodeT, A] = {
    val args = tree.args.map(fromTree[NodeT, A])
    MixedTree.Node[NodeT, A](tree.nodeType, tree.definitions, tree.uses, args)
  }

  /**
   * Extension methods for mixed trees of patterns.
   * @param tree The mixed tree of patterns.
   * @tparam NodeT The type of the nodes in the tree.
   */
  implicit class MixedTreeOfPatternOps[NodeT](val tree: MixedTree[NodeT, Pattern[NodeT]]) extends AnyVal {
    /**
     * Compiles the pattern into a compiled pattern.
     * @return The compiled pattern.
     */
    def compiled[EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]]: CompiledPattern[NodeT, EGraphT] = CompiledPattern(tree)

    /**
     * Converts the pattern to a searcher.
     * @return The searcher.
     */
    def toSearcher[EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]]: Searcher[NodeT, Seq[PatternMatch[NodeT]], EGraphT] = {
      Searcher(MachineSearcherPhase(compiled))
    }

    /**
     * Converts the pattern to a pattern applier.
     * @return The pattern applier.
     */
    def toApplier[EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]]: Applier[NodeT, PatternMatch[NodeT], EGraphT] = PatternApplier(tree)
  }
}
