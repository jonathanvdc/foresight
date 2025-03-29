package fixpoint.eqsat.integration.liar

import fixpoint.eqsat.{EClassCall, ENode, MixedTree, SlotMap}
import fixpoint.eqsat.metadata.Analysis

object TypeInferenceAnalysis extends Analysis[ArrayIR, MixedTree[Type, EClassCall]] {
  override def name: String = "TypeInference"

  override def rename(result: MixedTree[Type, EClassCall], renaming: SlotMap): MixedTree[Type, EClassCall] = result

  /**
   * Makes an analysis result for a node.
   *
   * @param node The node to make the analysis result for.
   * @param args The analysis results for the arguments to the node.
   * @return The analysis result for the node.
   */
  override def make(node: ENode[ArrayIR], args: Seq[MixedTree[Type, EClassCall]]): MixedTree[Type, EClassCall] = {
    val typeArgs = args.take(node.nodeType.typeArgCount)
    val valueArgs = args.drop(node.nodeType.typeArgCount)
    node.nodeType match {
      case t: Type => MixedTree.Node(t, node.definitions, node.uses, typeArgs ++ valueArgs)
      case v: Value => v.inferType(typeArgs, valueArgs)
    }
  }

  override def join(left: MixedTree[Type, EClassCall], right: MixedTree[Type, EClassCall]): MixedTree[Type, EClassCall] = {
    assert(left == right)
    left
  }
}
