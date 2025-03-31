package foresight.eqsat.examples.liar

import foresight.eqsat.MixedTree

/**
 * A collection of BLAS idioms.
 */
object BlasIdioms {
  /**
   * A BLAS `dot` function call.
   */
  object Dot extends ExternFunctionCall {
    override def name: String = "dot"
    override def typeArgCount: Int = 0
    override def valueArgCount: Int = 2

    override def inferType[A](typeArgs: Seq[MixedTree[Type, A]], valueArgTypes: Seq[MixedTree[Type, A]]): MixedTree[Type, A] = {
      val Seq(a, b) = valueArgTypes
      require(a == b)

      a match {
        case ArrayType(elementType, _) => elementType
        case _ => throw new IllegalArgumentException(s"dot requires array arguments, got $a")
      }
    }

    def apply[A](a: MixedTree[ArrayIR, A], b: MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] =
      MixedTree.unslotted(this, Seq(a, b))
  }

  /**
   * A BLAS `axpy` function call.
   */
  object Axpy extends ExternFunctionCall {
    override def name: String = "axpy"
    override def typeArgCount: Int = 0
    override def valueArgCount: Int = 3

    override def inferType[A](typeArgs: Seq[MixedTree[Type, A]], valueArgTypes: Seq[MixedTree[Type, A]]): MixedTree[Type, A] = {
      val Seq(a, x, y) = valueArgTypes
      require(x == y)

      x match {
        case ArrayType(elementType, _) => require(a == elementType)
        case _ => throw new IllegalArgumentException(s"axpy requires one scalar and two array arguments, got $a, $x, $y")
      }

      x
    }

    def apply[A](a: MixedTree[ArrayIR, A], x: MixedTree[ArrayIR, A], y: MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] =
      MixedTree.unslotted(this, Seq(a, x, y))
  }

  /**
   * A BLAS `gemv` function call.
   */
  final case class Gemv(aTransposed: Boolean) extends ExternFunctionCall {
    override def name: String = if (aTransposed) "gemvT" else "gemvF"
    override def typeArgCount: Int = 0
    override def valueArgCount: Int = 5

    override def inferType[A](typeArgs: Seq[MixedTree[Type, A]], valueArgTypes: Seq[MixedTree[Type, A]]): MixedTree[Type, A] = {
      val Seq(alpha, a, x, beta, y) = valueArgTypes
      require(alpha == beta)

      val n = x match {
        case ArrayType(elementType, n) =>
          require(alpha == elementType)
          n

        case _ => throw new IllegalArgumentException(s"gemv requires (scalar, matrix, vector, scalar, vector) arguments, got $alpha, $a, $x, $beta, $y")
      }

      val k = a match {
        case ArrayType(ArrayType(elementType, n2), k) =>
          require(n == n2)
          require(alpha == elementType)
          k

        case _ => throw new IllegalArgumentException(s"gemv requires (scalar, matrix, vector, scalar, vector) arguments, got $alpha, $a, $x, $beta, $y")
      }

      y match {
        case ArrayType(elementType, k2) =>
          require(k == k2)
          require(alpha == elementType)
        case _ => throw new IllegalArgumentException(s"gemv requires (scalar, matrix, vector, scalar, vector) arguments, got $alpha, $a, $x, $beta, $y")
      }

      y
    }

    def apply[A](alpha: MixedTree[ArrayIR, A], a: MixedTree[ArrayIR, A], x: MixedTree[ArrayIR, A], beta: MixedTree[ArrayIR, A], y: MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] =
      MixedTree.unslotted(this, Seq(alpha, a, x, beta, y))
  }
}
