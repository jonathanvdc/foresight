package fixpoint.eqsat.integration.liar

import fixpoint.eqsat.MixedTree

/**
 * A collection of BLAS idioms.
 */
object BlasIdioms {
  /**
   * A BLAS `ddot` function call.
   */
  object DDot extends ExternFunctionCall {
    override def name: String = "ddot"
    override def typeArgCount: Int = 0
    override def valueArgCount: Int = 2

    override def inferType[A](typeArgs: Seq[MixedTree[Type, A]], valueArgTypes: Seq[MixedTree[Type, A]]): MixedTree[Type, A] = {
      val Seq(a, b) = valueArgTypes
      require(a == b)

      a match {
        case ArrayType(elementType, _) => elementType
        case _ => throw new IllegalArgumentException(s"ddot requires array arguments, got $a")
      }
    }

    def apply[A](a: MixedTree[ArrayIR, A], b: MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] =
      MixedTree.unslotted(this, Seq(a, b))
  }
}
