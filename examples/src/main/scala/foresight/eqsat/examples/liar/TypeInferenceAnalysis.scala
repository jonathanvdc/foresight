package foresight.eqsat.examples.liar

import foresight.eqsat.collections.SlotMap
import foresight.eqsat.metadata.Analysis
import foresight.eqsat.{EClassCall, MixedTree, Slot}

object TypeInferenceAnalysis extends Analysis[ArrayIR, MixedTree[Type, EClassCall]] {
  override def name: String = "TypeInference"

  override def rename(result: MixedTree[Type, EClassCall], renaming: SlotMap): MixedTree[Type, EClassCall] = result

  override def make(node: ArrayIR, defs: Seq[Slot], uses: Seq[Slot], args: Seq[MixedTree[Type, EClassCall]]): MixedTree[Type, EClassCall] = {
    val typeArgs = args.take(node.typeArgCount)
    val valueArgs = args.drop(node.typeArgCount)
    node match {
      case t: Type => MixedTree.Node(t, defs, uses, typeArgs ++ valueArgs)
      case v: Value => v.inferType(typeArgs, valueArgs)
    }
  }

  override def join(left: MixedTree[Type, EClassCall], right: MixedTree[Type, EClassCall]): MixedTree[Type, EClassCall] = {
    assert(left == right)
    left
  }
}
