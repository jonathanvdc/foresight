package foresight.eqsat.lang

import scala.language.implicitConversions
import foresight.eqsat.{EClassCall, EGraph, MixedTree, Slot, SlotMap}
import org.junit.Test

class AnalysisTest {
  sealed trait ArithExpr derives Language
  final case class Var(slot: Use[Slot]) extends ArithExpr
  final case class Lam(param: Def[Slot], body: ArithExpr) extends ArithExpr
  final case class App(fun: ArithExpr, arg: ArithExpr) extends ArithExpr
  final case class Add(lhs: ArithExpr, rhs: ArithExpr) extends ArithExpr
  final case class Mul(lhs: ArithExpr, rhs: ArithExpr) extends ArithExpr
  final case class Number(value: BigInt) extends ArithExpr

  final case class Fact[A](value: A) extends ArithExpr
  object ArithExpr {
    given AnalysisBox[ArithExpr] with
      type Box[A] = Fact[A]
      def box[A](a: A): Fact[A] = Fact(a)
  }

  // Implicit conversion: Int => ArithExpr (Number)
  given Conversion[Int, ArithExpr] with
    def apply(n: Int): ArithExpr = Number(BigInt(n))

  // Operator overloads on ArithExpr
  extension (lhs: ArithExpr)
    infix def +(rhs: ArithExpr): ArithExpr = Add(lhs, rhs)
    infix def *(rhs: ArithExpr): ArithExpr = Mul(lhs, rhs)

  val Lang: Language[ArithExpr] = summon[Language[ArithExpr]]
  type ArithIR = LanguageOp[ArithExpr]

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

  @Test
  def atomDecoderWorks(): Unit = {
    val expr = Fact(Some(BigInt(42)): Option[BigInt])
    val atom = AnalysisFact(Some(BigInt(42)): Option[BigInt])
    val decoder = summon[AtomDecoder[ArithExpr, AnalysisFact[Option[BigInt]]]]
    println(AtomDecoder.decode(decoder, atom))
    assert(AtomDecoder.decode(decoder, atom).contains(expr))
  }

  @Test
  def computeTree(): Unit = {
    // x + y = y + x
    val x = Number(1)
    val y = Number(2)

    val egraph = EGraph.empty[ArithIR]
    val (root, egraph2) = egraph.add(Lang.toTree(x + y * y))

    val analysisResult = ConstantAnalysis(egraph2)
    val expected = Some(BigInt(5)) // 1 + 2 * 2
    assert(analysisResult(root, egraph2) == expected, s"Expected $expected but got ${analysisResult(root, egraph2)}")
  }
}
