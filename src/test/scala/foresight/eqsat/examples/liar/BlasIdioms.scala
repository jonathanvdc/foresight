package foresight.eqsat.examples.liar

import foresight.eqsat.MixedTree

/**
 * A collection of BLAS idioms.
 */
object BlasIdioms {
  def transpositionToString(transposed: Boolean): String = if (transposed) "T" else "F"

  /**
   * A `memset` function call that initializes an array.
   */
  object Memset extends ExternFunctionCall {
    override def name: String = "memset"
    override def typeArgCount: Int = 1
    override def valueArgCount: Int = 1

    override def inferType[A](typeArgs: Seq[MixedTree[Type, A]],
                              valueArgTypes: Seq[MixedTree[Type, A]]): MixedTree[Type, A] = {
      val Seq(size) = typeArgs
      val Seq(valueType) = valueArgTypes

      ArrayType(valueType, size)
    }

    def apply[A](size: MixedTree[Type, A], value: MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] =
      MixedTree.unslotted(this, Seq(size, value))

    override def cost[A](typeArgs: Seq[MixedTree[Type, A]],
                         valueArgTypes: Seq[MixedTree[Type, A]],
                         valueArgCosts: Seq[BigInt]): BigInt = {
      val Seq(size) = typeArgs
      val Seq(valueCost) = valueArgCosts
      val n = ConstIntType.toNumber(size)
      valueCost + TimeComplexity.rescale(n, 8, 10) + 1
    }
  }

  /**
   * A BLAS `dot` function call.
   */
  object Dot extends ExternFunctionCall {
    override def name: String = "dot"
    override def typeArgCount: Int = 0
    override def valueArgCount: Int = 2

    override def inferType[A](typeArgs: Seq[MixedTree[Type, A]],
                              valueArgTypes: Seq[MixedTree[Type, A]]): MixedTree[Type, A] = {
      val Seq(a, b) = valueArgTypes
      require(a == b)

      a match {
        case ArrayType(elementType, _) => elementType
        case _ => throw new IllegalArgumentException(s"dot requires array arguments, got $a")
      }
    }

    def apply[A](a: MixedTree[ArrayIR, A], b: MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] =
      MixedTree.unslotted(this, Seq(a, b))

    override def cost[A](typeArgs: Seq[MixedTree[Type, A]],
                         valueArgTypes: Seq[MixedTree[Type, A]],
                         valueArgCosts: Seq[BigInt]): BigInt = {
      val Seq(a, b) = valueArgTypes
      require(a == b)
      val Seq(aCost, bCost) = valueArgCosts
      val n = TimeComplexity.rows(a)
      val m = TimeComplexity.rows(b)
      require(n == m)
      aCost + bCost + TimeComplexity.rescale(n, 8, 10) + 1
    }
  }

  /**
   * A BLAS `axpy` function call.
   */
  object Axpy extends ExternFunctionCall {
    override def name: String = "axpy"
    override def typeArgCount: Int = 0
    override def valueArgCount: Int = 3

    override def inferType[A](typeArgs: Seq[MixedTree[Type, A]],
                              valueArgTypes: Seq[MixedTree[Type, A]]): MixedTree[Type, A] = {
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

    override def cost[A](typeArgs: Seq[MixedTree[Type, A]],
                         valueArgTypes: Seq[MixedTree[Type, A]],
                         valueArgCosts: Seq[BigInt]): BigInt = {
      val Seq(_, x, y) = valueArgTypes
      require(x == y)
      val Seq(aCost, xCost, yCost) = valueArgCosts
      val n = TimeComplexity.rows(x)
      aCost + xCost + yCost + TimeComplexity.rescale(n, 8, 10) + 1
    }
  }

  /**
   * A BLAS `gemv` function call.
   */
  final case class Gemv(aTransposed: Boolean) extends ExternFunctionCall {
    override def name: String = s"gemv${transpositionToString(aTransposed)}"
    override def typeArgCount: Int = 0
    override def valueArgCount: Int = 5

    override def inferType[A](typeArgs: Seq[MixedTree[Type, A]],
                              valueArgTypes: Seq[MixedTree[Type, A]]): MixedTree[Type, A] = {
      val Seq(alpha, a, x, beta, y) = valueArgTypes
      require(alpha == beta)

      val n = x match {
        case ArrayType(elementType, n) =>
          require(alpha == elementType)
          n

        case _ => throw new IllegalArgumentException(s"gemv requires (scalar, matrix, vector, scalar, vector) arguments, got $alpha, $a, $x, $beta, $y")
      }

      val k = a match {
        case ArrayType(ArrayType(elementType, l), k) =>
          if (aTransposed) require(n == k) else require(n == l)
          require(alpha == elementType)
          if (aTransposed) l else k

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

    def apply[A](alpha: MixedTree[ArrayIR, A],
                 a: MixedTree[ArrayIR, A],
                 x: MixedTree[ArrayIR, A],
                 beta: MixedTree[ArrayIR, A],
                 y: MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] =
      MixedTree.unslotted(this, Seq(alpha, a, x, beta, y))

    override def cost[A](typeArgs: Seq[MixedTree[Type, A]],
                         valueArgTypes: Seq[MixedTree[Type, A]],
                         valueArgCosts: Seq[BigInt]): BigInt = {
      val Seq(_, a, _, _, _) = valueArgTypes
      val n = TimeComplexity.rows(a)
      val m = TimeComplexity.cols(a)
      valueArgCosts.sum + TimeComplexity.rescale(n * m, 7, 10) + 1
    }
  }

  /**
   * A BLAS `gemm` function call.
   */
  final case class Gemm(aTransposed: Boolean, bTransposed: Boolean) extends ExternFunctionCall {
    override def name: String = s"gemm${transpositionToString(aTransposed)}${transpositionToString(bTransposed)}"

    override def typeArgCount: Int = 0
    override def valueArgCount: Int = 5

    /**
     * Returns the three unique dimensions of the input matrices.
     * @param a The first input matrix.
     * @param b The second input matrix.
     * @tparam A The type of the calls in the mixed trees.
     * @return The three unique dimensions of the input matrices.ß
     */
    private def dimsFromArgTypes[A](a: MixedTree[ArrayIR, A],
                                    b: MixedTree[ArrayIR, A]): (MixedTree[Type, A], MixedTree[Type, A], MixedTree[Type, A]) = {
      (a, b) match {
        case (ArrayType(ArrayType(_, m: MixedTree[ArrayIR, A]), n: MixedTree[ArrayIR, A]),
          ArrayType(ArrayType(_, k: MixedTree[ArrayIR, A]), l: MixedTree[ArrayIR, A])) =>

          (aTransposed, bTransposed) match {
            case (false, false) => (n, m, k)
            case (false, true) => (n, m, l)
            case (true, false) => (m, n, k)
            case (true, true) => (m, n, l)
          }

        case _ =>
          throw new IllegalArgumentException(
            s"gemm requires (alpha: scalar, a: matrix, b: matrix, beta: scalar, c: matrix) arguments, got a: $a, b: $b")
      }
    }

    override def inferType[A](typeArgs: Seq[MixedTree[Type, A]],
                              valueArgTypes: Seq[MixedTree[Type, A]]): MixedTree[Type, A] = {

      val Seq(alpha, a, b, beta, c) = valueArgTypes
      require(alpha == beta)

      (a, b) match {
        case (ArrayType(ArrayType(et1, m), n), ArrayType(ArrayType(et2, k), l)) =>
          require(alpha == et1)
          require(alpha == et2)

        case _ =>
          throw new IllegalArgumentException(s"gemm requires (scalar, matrix, matrix, scalar, matrix) arguments, got $alpha, $a, $b, $beta, $c")
      }

      val dims = dimsFromArgTypes(a, b)

      val resultType = ArrayType(ArrayType(alpha, dims._3), dims._1)
      require(c == resultType)

      resultType
    }

    def apply[A](alpha: MixedTree[ArrayIR, A],
                 a: MixedTree[ArrayIR, A],
                 b: MixedTree[ArrayIR, A],
                 beta: MixedTree[ArrayIR, A],
                 c: MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] =
      MixedTree.unslotted(this, Seq(alpha, a, b, beta, c))

    override def cost[A](typeArgs: Seq[MixedTree[Type, A]],
               valueArgTypes: Seq[MixedTree[Type, A]],
               valueArgCosts: Seq[BigInt]): BigInt = {
      val Seq(_, a, b, _, _) = valueArgTypes
      val (n, m, k) = dimsFromArgTypes(a, b)
      val (numN, numM, numK) = (ConstIntType.toNumber(n), ConstIntType.toNumber(m), ConstIntType.toNumber(k))
      valueArgCosts.sum + TimeComplexity.rescale(numN * numM * numK, 6, 10) + 1
    }
  }

  /**
   * A BLAS matrix transpose.
   */
  object Transpose extends ExternFunctionCall {
    override def name: String = "transpose"
    override def typeArgCount: Int = 0
    override def valueArgCount: Int = 1

    override def inferType[A](typeArgs: Seq[MixedTree[Type, A]],
                              valueArgTypes: Seq[MixedTree[Type, A]]): MixedTree[Type, A] = {
      val Seq(a) = valueArgTypes
      a match {
        case ArrayType(ArrayType(elementType, n), m) =>
          ArrayType(ArrayType(elementType, m), n)
        case _ => throw new IllegalArgumentException(s"transpose requires a matrix argument, got $a")
      }
    }

    def apply[A](a: MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] =
      MixedTree.unslotted(this, Seq(a))

    override def cost[A](typeArgs: Seq[MixedTree[Type, A]],
               valueArgTypes: Seq[MixedTree[Type, A]],
               valueArgCosts: Seq[BigInt]): BigInt = {
      val Seq(a) = valueArgTypes
      val Seq(aCost) = valueArgCosts
      val n = TimeComplexity.rows(a)
      val m = TimeComplexity.cols(a)
      aCost + TimeComplexity.rescale(n * m, 9, 10) + 1
    }
  }
}
