package foresight.eqsat.examples.liar

import foresight.eqsat.{EClassCall, ENode, MixedTree, Slot}
import foresight.eqsat.extraction.{CostFunction, ExtractionTreeCall}

object TimeComplexity extends CostFunction[ArrayIR, (BigInt, MixedTree[Type, EClassCall])] {
  private def toNumber(t: MixedTree[Type, EClassCall]): BigInt = t match {
    case MixedTree.Node(ConstIntType(n), _, _, _) => n
    case _ => ???
  }

  private def rows(t: MixedTree[Type, EClassCall]): BigInt = t match {
    case ArrayType(_, rows) => toNumber(rows)
    case _ => ???
  }

  private def cols(t: MixedTree[Type, EClassCall]): BigInt = t match {
    case ArrayType(ArrayType(_, cols), _) => toNumber(cols)
    case _ => ???
  }

  override def apply(nodeType: ArrayIR,
                     definitions: Seq[Slot],
                     uses: Seq[Slot],
                     args: Seq[ExtractionTreeCall[ArrayIR, (BigInt, MixedTree[Type, EClassCall])]]): (BigInt, MixedTree[Type, EClassCall]) = {

    val t = TypeInferenceAnalysis.make(ENode(nodeType, definitions, uses, Seq.empty), args.map(_.cost._2))

    val argsCosts = args.map(_.cost._1)
    val argTypes = args.map(_.cost._2)
    val c = nodeType match {
      case BlasIdioms.Gemm(_, _) =>
        val Seq(_, aType, bType, _, _) = argTypes
        argsCosts.sum +
          rescale(rows(aType) * cols(aType) * rows(bType), 6, 10) + 1

      case BlasIdioms.Gemv(_) =>
        val Seq(_, aType, _, _) = argTypes
        argsCosts.sum +
          rescale(rows(aType) * cols(aType), 7, 10) + 1

      case BlasIdioms.Dot =>
        val Seq(leftType, _) = argTypes
        argsCosts.sum +
          rescale(rows(leftType), 8, 10) + 1

      case BlasIdioms.Transpose =>
        val Seq(aType, _) = argTypes
        argsCosts.sum +
          rescale(rows(aType) * cols(aType), 9, 10) + 1

      case BlasIdioms.Axpy =>
        val Seq(_, _, yType) = argTypes
        argsCosts.sum +
          rescale(rows(yType) * cols(yType), 8, 10) + 1

      case BlasIdioms.Memset =>
        argsCosts.sum +
          rescale(rows(t), 8, 10) + 1

      case Build =>
        val Seq(size, _) = argTypes
        val Seq(_, fCost) = argsCosts
        toNumber(size) * (fCost + 1) + 1

      case _ => ???
    }

    (c, t)
  }

  private def rescale(value: BigInt, numerator: Int, denominator: Int): BigInt = {
    val (a, b) = value * numerator /% denominator
    if (b == 0) {
      a
    } else {
      a + 1
    }
  }
}
