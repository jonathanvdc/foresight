package foresight.eqsat.examples.arithWithLang

import foresight.eqsat.SlotMap
import foresight.eqsat.lang.LanguageAnalysis

object ConstantAnalysis extends LanguageAnalysis[ArithExpr, Option[BigInt]] {
  def name: String = "ConstantAnalysis"

  def rename(result: Option[BigInt], renaming: SlotMap): Option[BigInt] = result

  def make(expr: ArithExpr): Option[BigInt] = {
    expr match {
      case Add(Fact(Some(left: BigInt)), Fact(Some(right: BigInt))) => Some(left + right)
      case Mul(Fact(Some(left: BigInt)), Fact(Some(right: BigInt))) => Some(left * right)
      case Number(value) => Some(value)
      case _ => None
    }
  }

  def join(left: Option[BigInt], right: Option[BigInt]): Option[BigInt] = {
    (left, right) match {
      case (Some(l), Some(r)) if l == r => Some(l)
      case (Some(l), Some(r)) => throw new IllegalArgumentException("Cannot join different constants")
      case (Some(l), None) => Some(l)
      case (None, Some(r)) => Some(r)
      case (None, None) => None
    }
  }
}