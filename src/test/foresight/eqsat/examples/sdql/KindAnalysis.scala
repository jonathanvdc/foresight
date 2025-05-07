package foresight.eqsat.examples.sdql

import foresight.eqsat.{ENode, SlotMap}
import foresight.eqsat.metadata.Analysis

/**
 * An analysis that determines the kind of a SDQL expression.
 */
object KindAnalysis extends Analysis[SdqlIR, SdqlKind] {
  override val name: String = "SdqlKindAnalysis"

  override def rename(result: SdqlKind, renaming: SlotMap): SdqlKind = result

  override def make(node: ENode[SdqlIR], args: Seq[SdqlKind]): SdqlKind = {
    node.nodeType match {
      case SubArray | Range => SdqlKind.vector
      case Equality => SdqlKind.bool
      case Num(_) => SdqlKind.scalar
      case Sing => SdqlKind.dict
      case _ => SdqlKind.nothing
    }
  }

  override def join(left: SdqlKind, right: SdqlKind): SdqlKind =
    SdqlKind.join(left, right)
}
