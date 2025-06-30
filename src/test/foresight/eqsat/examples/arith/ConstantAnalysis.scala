package foresight.eqsat.examples.arith

import foresight.eqsat.{ENode, Slot, SlotMap}
import foresight.eqsat.metadata.Analysis

/**
 * An analysis that computes constant values for arithmetic expressions.
 */
object ConstantAnalysis extends Analysis[ArithIR, Option[BigInt]] {
  override def name: String = "ConstantAnalysis"

  override def rename(result: Option[BigInt], renaming: SlotMap): Option[BigInt] = result

  override def make(node: ArithIR, defs: Seq[Slot], uses: Seq[Slot], args: Seq[Option[BigInt]]): Option[BigInt] = {
    (node, args) match {
      case (Number(value), Seq()) => Some(value)
      case (Add, Seq(Some(left), Some(right))) => Some(left + right)
      case (Mul, Seq(Some(left), Some(right))) => Some(left * right)
      case _ => None
    }
  }

  override def join(left: Option[BigInt], right: Option[BigInt]): Option[BigInt] = {
    (left, right) match {
      case (Some(l), Some(r)) => assert(l == r, s"Cannot join different constants: $l and $r"); Some(l)
      case (Some(l), None) => Some(l)
      case (None, Some(r)) => Some(r)
      case (None, None) => None
    }
  }
}
