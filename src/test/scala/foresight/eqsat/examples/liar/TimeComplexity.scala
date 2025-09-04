package foresight.eqsat.examples.liar

import foresight.eqsat.{EClassCall, ENode, MixedTree, Slot}
import foresight.eqsat.extraction.{CostFunction, ExtractionAnalysis, ExtractionTreeCall}

/**
 * The LIAR time complexity cost function, used for the final extraction step.
 */
object TimeComplexity extends CostFunction[ArrayIR, (BigInt, MixedTree[Type, EClassCall])] {
  def analysis: ExtractionAnalysis[ArrayIR, (BigInt, MixedTree[Type, EClassCall])] = {
    ExtractionAnalysis("TimeComplexityExtraction", TimeComplexity)(new Ordering[(BigInt, MixedTree[Type, EClassCall])] {
      override def compare(x: (BigInt, MixedTree[Type, EClassCall]), y: (BigInt, MixedTree[Type, EClassCall])): Int = {
        x._1.compare(y._1)
      }
    }, implicitly[Ordering[ArrayIR]])
  }

  override def apply(nodeType: ArrayIR,
                     definitions: Seq[Slot],
                     uses: Seq[Slot],
                     args: Seq[(BigInt, MixedTree[Type, EClassCall])]): (BigInt, MixedTree[Type, EClassCall]) = {

    val t = TypeInferenceAnalysis.make(ENode(nodeType, definitions, uses, Seq.empty), args.map(_._2))

    val argsCosts = args.map(_._1)
    val argTypes = args.map(_._2)
    val c = nodeType match {
      case v: Value =>
        v.cost(argTypes.take(v.typeArgCount), argTypes.drop(v.typeArgCount), argsCosts.drop(v.typeArgCount))

      case _: Type => BigInt(0)
    }
    (c, t)
  }

  def rows(t: MixedTree[Type, _]): BigInt = t match {
    case ArrayType(_, rows) => ConstIntType.toNumber(rows)
    case _ => ???
  }

  def cols(t: MixedTree[Type, _]): BigInt = t match {
    case ArrayType(ArrayType(_, cols), _) => ConstIntType.toNumber(cols)
    case _ => ???
  }

  def rescale(value: BigInt, numerator: Int, denominator: Int): BigInt = {
    val (a, b) = value * numerator /% denominator
    if (b == 0) {
      a
    } else {
      a + 1
    }
  }
}
