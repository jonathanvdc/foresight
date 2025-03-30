package fixpoint.eqsat.integration.liar

import fixpoint.eqsat.MixedTree

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
}
