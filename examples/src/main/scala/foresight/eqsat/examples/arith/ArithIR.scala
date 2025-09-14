package foresight.eqsat.examples.arith

import foresight.eqsat.{ENode, MixedTree, Slot}

import scala.language.implicitConversions
import scala.math.Ordered.orderingToOrdered

// Define a language for arithmetic expressions involving variable references, addition, multiplication, integer
// literals, lambdas and function applications.

/**
 * A trait representing arithmetic expressions in the e-graph.
 */
sealed trait ArithIR extends Ordered[ArithIR] {
  /**
   * Compares two arithmetic expressions.
   *
   * @param that The other arithmetic expression to compare with.
   * @return An integer indicating the comparison result.
   */
  override def compare(that: ArithIR): Int = {
    (this, that) match {
      case (Number(v1), Number(v2)) => v1.compareTo(v2)
      case (left, right) => left.toString.compareTo(right.toString)
    }
  }
}

/**
 * A variable reference in an arithmetic expression.
 */
object Var extends ArithIR {
  /**
   * Creates a variable reference in an arithmetic expression.
   *
   * @param slot The slot representing the variable.
   * @return An ENode representing the variable reference.
   */
  def apply[A](slot: Slot): MixedTree[ArithIR, A] = MixedTree.Node[ArithIR, A](this, Seq.empty, Seq(slot), Seq.empty)
}

/**
 * A lambda expression in an arithmetic expression.
 */
object Lam extends ArithIR {
  /**
   * Creates a lambda expression in an arithmetic expression.
   *
   * @param param The parameter of the lambda.
   * @param body  The body of the lambda.
   * @tparam A The type of the MixedTree.
   * @return A MixedTree representing the lambda expression.
   */
  def apply[A](param: Slot, body: MixedTree[ArithIR, A]): MixedTree[ArithIR, A] =
    MixedTree.Node[ArithIR, A](this, Seq(param), Seq.empty, Seq(body))
}

/**
 * An application of a lambda expression in an arithmetic expression.
 */
object App extends ArithIR {
  /**
   * Creates an application of a lambda expression in an arithmetic expression.
   *
   * @param lambda The lambda expression to apply.
   * @param arg    The argument to apply to the lambda.
   * @tparam A The type of the MixedTree.
   * @return A MixedTree representing the application of the lambda.
   */
  def apply[A](lambda: MixedTree[ArithIR, A], arg: MixedTree[ArithIR, A]): MixedTree[ArithIR, A] =
    MixedTree.Node[ArithIR, A](this, Seq.empty, Seq.empty, Seq(lambda, arg))
}

/**
 * An addition operation in an arithmetic expression.
 */
object Add extends ArithIR {
  /**
   * Creates an addition operation in an arithmetic expression.
   *
   * @param lhs The left-hand side of the addition.
   * @param rhs The right-hand side of the addition.
   * @tparam A The type of the MixedTree.
   * @return A MixedTree representing the addition operation.
   */
  def apply[A](lhs: MixedTree[ArithIR, A], rhs: MixedTree[ArithIR, A]): MixedTree[ArithIR, A] =
    MixedTree.Node[ArithIR, A](this, Seq.empty, Seq.empty, Seq(lhs, rhs))
}

/**
 * A multiplication operation in an arithmetic expression.
 */
object Mul extends ArithIR {
  /**
   * Creates a multiplication operation in an arithmetic expression.
   *
   * @param lhs The left-hand side of the multiplication.
   * @param rhs The right-hand side of the multiplication.
   * @tparam A The type of the MixedTree.
   * @return A MixedTree representing the multiplication operation.
   */
  def apply[A](lhs: MixedTree[ArithIR, A], rhs: MixedTree[ArithIR, A]): MixedTree[ArithIR, A] =
    MixedTree.Node[ArithIR, A](this, Seq.empty, Seq.empty, Seq(lhs, rhs))
}

/**
 * A number literal in an arithmetic expression.
 *
 * @param value The integer value of the number.
 */
final case class Number(value: BigInt) extends ArithIR

/**
 * Companion object for the Number class, providing implicit conversions to MixedTree and ENode.
 */
object Number {
  /**
   * Implicit conversion from Number to MixedTree.
   *
   * @param number The Number instance to convert.
   * @tparam A The type of the MixedTree.
   * @return A MixedTree representation of the Number.
   */
  implicit def toMixedTree[A](number: Number): MixedTree[ArithIR, A] = MixedTree.Node(number, Seq.empty, Seq.empty, Seq.empty)

  /**
   * Implicit conversion from Number to ENode.
   *
   * @param number The Number instance to convert.
   * @return An ENode representation of the Number.
   */
  implicit def toENode(number: Number): ENode[ArithIR] = ENode(number, Seq.empty, Seq.empty, Seq.empty)
}
