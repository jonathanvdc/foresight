package foresight.eqsat.examples.liar

import foresight.eqsat.{MixedTree, Slot}

/**
 * Defines linear algebra operations on arrays.
 */
object Lib {
  private def inferType[A](expr: MixedTree[ArrayIR, A]): MixedTree[Type, A] = {
    expr match {
      case MixedTree.Node(v: Value, _, _, args) =>
        val tArgs = args.take(v.typeArgCount).map(Type.asType)
        val vArgs = args.drop(v.typeArgCount)
        val vArgTypes = vArgs.map(inferType[A])
        v.inferType(tArgs, vArgTypes)

      case _ => ???
    }
  }

  private def elementType[A](t: MixedTree[Type, A]): MixedTree[Type, A] = {
    t match {
      case ArrayType(elementType, _) => elementType
      case _ => throw new IllegalArgumentException(s"Expected array type, got $t")
    }
  }

  private def length[A](t: MixedTree[Type, A]): MixedTree[Type, A] = {
    t match {
      case ArrayType(_, length) => length
      case _ => throw new IllegalArgumentException(s"Expected array type, got $t")
    }
  }

  private def rows[A](t: MixedTree[Type, A]): MixedTree[Type, A] = {
    t match {
      case ArrayType(ArrayType(_, _), rows) => rows
      case _ => throw new IllegalArgumentException(s"Expected 2D array type, got $t")
    }
  }

  private def cols[A](t: MixedTree[Type, A]): MixedTree[Type, A] = {
    t match {
      case ArrayType(ArrayType(_, cols), _) => cols
      case _ => throw new IllegalArgumentException(s"Expected 2D array type, got $t")
    }
  }

  /**
   * Computes the dot product of two vectors.
   *
   * @param left A first vector.
   * @param right A second vector.
   * @return The dot product of `left` and `right`.
   */
  def dotProduct[A](left: MixedTree[ArrayIR, A], right: MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] = {
    val leftType = inferType(left)
    val rightType = inferType(right)
    val elemType = elementType(leftType)
    val intType = Int32Type.toTree

    require(elemType == MixedTree.fromTree(DoubleType.toTree))
    require(length(leftType) == length(rightType))

    val i = Slot.fresh()
    val acc = Slot.fresh()
    Ifold(
      length(leftType),
      ConstDouble(0.0).toTree,
      Lambda(
        i,
        intType,
        Lambda(
          acc,
          elemType,
          Add(
            Mul(IndexAt(left, Var(i, intType)), IndexAt(right, Var(i, intType))),
            Var(acc, elemType)))))
  }

  /**
   * Computes a matrix-vector product.
   * @param matrix A matrix.
   * @param vector A vector.
   * @return The product of `matrix` and `vector`, as a vector.
   */
  def matrixVectorProduct[A](matrix: MixedTree[ArrayIR, A], vector: MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] = {
    val vectorType = inferType(vector)
    val i = Slot.fresh()
    val intType = Int32Type.toTree
    Build(length(vectorType), Lambda(i, intType, dotProduct(IndexAt(matrix, Var(i, intType)), vector)))
  }

  /**
   * Computes a matrix-matrix product.
   * @param left A first matrix.
   * @param right A second, pre-transposed matrix.
   * @return The product of `left` and `right`.
   */
  def matrixMatrixProductPretransposed[A](left: MixedTree[ArrayIR, A], right: MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] = {
    val i = Slot.fresh()
    val intType = Int32Type.toTree
    Build(
      rows(inferType(left)),
      Lambda(i, intType, matrixVectorProduct(right, IndexAt(left, Var(i, intType)))))
  }

  /**
   * Transposes a matrix.
   * @param matrix A matrix to transpose.
   * @return A transposed version of the matrix.
   */
  def matrixTranspose[A](matrix: MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] = {
    val i = Slot.fresh()
    val j = Slot.fresh()
    val intType = Int32Type.toTree
    val matrixType = inferType(matrix)
    Build(
      cols(matrixType),
      Lambda(
        i,
        intType,
        Build(
          rows(matrixType),
          Lambda(j, intType, IndexAt(IndexAt(matrix, Var(j, intType)), Var(i, intType))))))
  }

  /**
   * Computes a matrix-matrix product.
   * @param left A first matrix.
   * @param right A second matrix.
   * @return The product of `left` and `right`.
   */
  def matrixMatrixProduct[A](left: MixedTree[ArrayIR, A], right: MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] = {
    matrixMatrixProductPretransposed(left, matrixTranspose(right))
  }

  /**
   * Performs an elementwise binary operation on a vector.
   * @param vector A vector.
   * @param op A function that transforms a vector element.
   * @return A vector of transformed elements.
   */
  def vectorElementwiseUnary[A](vector: MixedTree[ArrayIR, A], op: MixedTree[ArrayIR, A] => MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] = {
    val i = Slot.fresh()
    val intType = Int32Type.toTree
    val vectorType = inferType(vector)
    Build(length(vectorType), Lambda(i, intType, op(IndexAt(vector, Var(i, intType)))))
  }

  /**
   * Performs an elementwise binary operation on a matrix.
   * @param matrix A matrix.
   * @param op A function that transforms a matrix element.
   * @return A matrix of transformed elements.
   */
  def matrixElementwiseUnary[A](matrix: MixedTree[ArrayIR, A], op: MixedTree[ArrayIR, A] => MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] = {
    vectorElementwiseUnary(matrix, (v: MixedTree[ArrayIR, A]) => vectorElementwiseUnary(v, op))
  }

  /**
   * Performs an elementwise binary operation on a pair of vectors.
   * @param left A first vector.
   * @param right A second vector.
   * @param op A function that takes two vector elements and performs some operation on them.
   * @return A vector of which each element is the application of `op` to the corresponding elements of `left` and
   *         `right`.
   */
  def vectorElementwiseBinary[A](left: MixedTree[ArrayIR, A], right: MixedTree[ArrayIR, A], op: (MixedTree[ArrayIR, A], MixedTree[ArrayIR, A]) => MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] = {
    val i = Slot.fresh()
    val intType = Int32Type.toTree
    val leftType = inferType(left)
      Build(
        length(leftType),
        Lambda(i, intType, op(IndexAt(left, Var(i, intType)), IndexAt(right, Var(i, intType)))))
  }

  /**
   * Performs an elementwise binary operation on a pair of matrices.
   * @param left A first matrix.
   * @param right A second matrix.
   * @param op A function that takes two matrix elements and performs some operation on them.
   * @return A matrix of which each element is the application of `op` to the corresponding elements of `left` and
   *         `right`.
   */
  def matrixElementwiseBinary[A](left: MixedTree[ArrayIR, A], right: MixedTree[ArrayIR, A], op: (MixedTree[ArrayIR, A], MixedTree[ArrayIR, A]) => MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] = {
    vectorElementwiseBinary[A](left, right, vectorElementwiseBinary[A](_, _, op))
  }

  /**
   * Computes the product of a matrix and a scalar.
   * @param matrix A matrix.
   * @param scalar A scalar.
   * @return A matrix of which each element is the product of `scalar` and the corresponding element of `matrix`.
   */
  def matrixScalarProduct[A](matrix: MixedTree[ArrayIR, A], scalar: MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] = {
    matrixElementwiseUnary(matrix, (v: MixedTree[ArrayIR, A]) => Mul(v, scalar))
  }

  /**
   * Computes the product of a vector and a scalar.
   * @param vector A vector.
   * @param scalar A scalar.
   * @return A vector of which each element is the product of `scalar` and the corresponding element of `vector`.
   */
  def vectorScalarProduct[A](vector: MixedTree[ArrayIR, A], scalar: MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] = {
    vectorElementwiseUnary(vector, (v: MixedTree[ArrayIR, A]) => Mul(v, scalar))
  }

  /**
   * Computes the sum of two matrices.
   * @param left A first matrix.
   * @param right A second matrix.
   * @return The sum of `left` and `right`.
   */
  def matrixAdd[A](left: MixedTree[ArrayIR, A], right: MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] = {
    matrixElementwiseBinary(left, right, Add[A])
  }
}
