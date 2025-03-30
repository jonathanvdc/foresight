package fixpoint.eqsat.integration.liar

import fixpoint.eqsat.{MixedTree, Slot, Tree}

/**
 * A description of the minimalist array IR from Latent Idiom Recognition for a Minimalist Functional Array Language
 * Using Equality Saturation by Jonathan Van der Cruysse and Christophe Dubach.
 */
sealed trait ArrayIR extends Ordered[ArrayIR] {
  /**
   * A description of the number of type arguments taken by the IR node.
   * @return The number of type arguments.
   */
  def typeArgCount: Int

  /**
   * A description of the number of value arguments taken by the IR node.
   * @return The number of value arguments.
   */
  def valueArgCount: Int

  override def compare(that: ArrayIR): Int = {
    this.hashCode().compareTo(that.hashCode())
  }
}

/**
 * A type in the minimalist array IR.
 */
sealed trait Type extends ArrayIR

/**
 * The companion object for [[Type]].
 */
object Type {
  def asType[A](tree: MixedTree[ArrayIR, A]): MixedTree[Type, A] = {
    tree.mapNodes(_.asInstanceOf[Type])
  }
}

/**
 * The double type in the minimalist array IR.
 */
object DoubleType extends Type {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 0

  def toTree: Tree[Type] = {
    Tree.unslotted(DoubleType, Seq.empty)
  }
}

/**
 * The 32-bit integer type in the minimalist array IR.
 */
object Int32Type extends Type {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 0

  def toTree: Tree[Type] = {
    Tree.unslotted(Int32Type, Seq.empty)
  }
}

/**
 * An integer constant type in the minimalist array IR.
 */
final case class ConstIntType(value: Int) extends Type {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 0

  def toTree: Tree[Type] = {
    Tree.unslotted(ConstIntType(value), Seq.empty)
  }
}

/**
 * An array type in the minimalist array IR.
 */
object ArrayType extends Type {
  override def typeArgCount: Int = 2
  override def valueArgCount: Int = 0

  def apply[A](elementType: MixedTree[Type, A], sizeType: MixedTree[Type, A]): MixedTree[Type, A] = {
    MixedTree.unslotted(ArrayType, Seq(elementType, sizeType))
  }

  def unapply[A](tree: MixedTree[Type, A]): Option[(MixedTree[Type, A], MixedTree[Type, A])] = {
    tree match {
      case MixedTree.Node(ArrayType, Seq(), Seq(), Seq(elementType, sizeType)) => Some((elementType, sizeType))
      case _ => None
    }
  }
}

/**
 * A tuple type in the minimalist array IR.
 */
object TupleType extends Type {
  override def typeArgCount: Int = 2
  override def valueArgCount: Int = 0

  def apply[A](fstType: MixedTree[Type, A], sndType: MixedTree[Type, A]): MixedTree[Type, A] = {
    MixedTree.unslotted(TupleType, Seq(fstType, sndType))
  }

  def unapply[A](tree: MixedTree[Type, A]): Option[(MixedTree[Type, A], MixedTree[Type, A])] = {
    tree match {
      case MixedTree.Node(TupleType, Seq(), Seq(), Seq(fstType, sndType)) => Some((fstType, sndType))
      case _ => None
    }
  }
}

/**
 * A function type in the minimalist array IR.
 */
object FunctionType extends Type {
  override def typeArgCount: Int = 2
  override def valueArgCount: Int = 0

  def apply[A](argType: MixedTree[Type, A], returnType: MixedTree[Type, A]): MixedTree[Type, A] = {
    MixedTree.unslotted(FunctionType, Seq(argType, returnType))
  }

  def unapply[A](tree: MixedTree[Type, A]): Option[(MixedTree[Type, A], MixedTree[Type, A])] = {
    tree match {
      case MixedTree.Node(FunctionType, Seq(), Seq(), Seq(argType, returnType)) => Some((argType, returnType))
      case _ => None
    }
  }
}

/**
 * A value in the minimalist array IR.
 */
sealed trait Value extends ArrayIR {
  /**
   * Infers the type of the value given the type arguments and value argument types.
   * @param typeArgs The type arguments.
   * @param valueArgTypes The value argument types.
   * @return The inferred type of the value.
   */
  def inferType[A](typeArgs: Seq[MixedTree[Type, A]], valueArgTypes: Seq[MixedTree[Type, A]]): MixedTree[Type, A]
}

/**
 * A variable use in the minimalist array IR.
 */
object Var extends Value {
  override def typeArgCount: Int = 1
  override def valueArgCount: Int = 0

  override def inferType[A](typeArgs: Seq[MixedTree[Type, A]], valueArgTypes: Seq[MixedTree[Type, A]]): MixedTree[Type, A] = typeArgs.head

  def apply[A](slot: Slot, typeArg: MixedTree[Type, A]): MixedTree[ArrayIR, A] = {
    MixedTree.Node(Var, Seq.empty, Seq(slot), Seq(typeArg))
  }

  def unapply[A](tree: MixedTree[ArrayIR, A]): Option[(Slot, MixedTree[Type, A])] = {
    tree match {
      case MixedTree.Node(Var, Seq(), Seq(slot), Seq(typeArg)) => Some((slot, Type.asType(typeArg)))
      case _ => None
    }
  }
}

/**
 * A lambda abstraction in the minimalist array IR.
 */
object Lambda extends Value {
  override def typeArgCount: Int = 1
  override def valueArgCount: Int = 1

  override def inferType[A](typeArgs: Seq[MixedTree[Type, A]], valueArgTypes: Seq[MixedTree[Type, A]]): MixedTree[Type, A] = {
    FunctionType(typeArgs.head, valueArgTypes.head)
  }

  def apply[A](slot: Slot, argType: MixedTree[Type, A], body: MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] = {
    MixedTree.Node(Lambda, Seq(slot), Seq.empty, Seq(argType, body))
  }

  def unapply[A](tree: MixedTree[ArrayIR, A]): Option[(Slot, MixedTree[Type, A], MixedTree[ArrayIR, A])] = {
    tree match {
      case MixedTree.Node(Lambda, Seq(slot), Seq(), Seq(argType, body)) =>
        Some((slot, Type.asType(argType), body))

      case _ => None
    }
  }
}

/**
 * An application in the minimalist array IR.
 */
object Apply extends Value {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 2

  override def inferType[A](typeArgs: Seq[MixedTree[Type, A]], valueArgTypes: Seq[MixedTree[Type, A]]): MixedTree[Type, A] = {
    val functionType = valueArgTypes.head
    functionType match {
      case FunctionType(_, returnType) => returnType
      case _ => throw new IllegalArgumentException("The first argument of an application must be a function.")
    }
  }

  def apply[A](function: MixedTree[ArrayIR, A], argument: MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] = {
    MixedTree.unslotted(Apply, Seq(function, argument))
  }

  def unapply[A](tree: MixedTree[ArrayIR, A]): Option[(MixedTree[ArrayIR, A], MixedTree[ArrayIR, A])] = {
    tree match {
      case MixedTree.Node(Apply, Seq(), Seq(), Seq(function, argument)) =>
        Some((function, argument))
      case _ => None
    }
  }
}

/**
 * An array construction operation in the minimalist array IR.
 */
object Build extends Value {
  override def typeArgCount: Int = 1
  override def valueArgCount: Int = 1

  override def inferType[A](typeArgs: Seq[MixedTree[Type, A]], valueArgTypes: Seq[MixedTree[Type, A]]): MixedTree[Type, A] = {
    val functionType = valueArgTypes.head
    val elementType = functionType match {
      case FunctionType(_, returnType) => returnType
      case _ => throw new IllegalArgumentException("The argument of a build must be a function.")
    }
    ArrayType(elementType, typeArgs.head)
  }

  def apply[A](size: MixedTree[Type, A], function: MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] = {
    MixedTree.unslotted(Build, Seq(size, function))
  }

  def unapply[A](tree: MixedTree[ArrayIR, A]): Option[(MixedTree[Type, A], MixedTree[ArrayIR, A])] = {
    tree match {
      case MixedTree.Node(Build, Seq(), Seq(), Seq(size, function)) =>
        Some((Type.asType(size), function))

      case _ => None
    }
  }
}

/**
 * An array indexing operation in the minimalist array IR.
 */
object IndexAt extends Value {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 2

  override def inferType[A](typeArgs: Seq[MixedTree[Type, A]], valueArgTypes: Seq[MixedTree[Type, A]]): MixedTree[Type, A] = {
    val arrayType = valueArgTypes.head
    val elementType = arrayType match {
      case ArrayType(elementType, _) => elementType
      case _ => throw new IllegalArgumentException("The first argument of an index must be an array.")
    }
    elementType
  }

  def apply[A](array: MixedTree[ArrayIR, A], index: MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] = {
    MixedTree.unslotted(IndexAt, Seq(array, index))
  }

  def unapply[A](tree: MixedTree[ArrayIR, A]): Option[(MixedTree[ArrayIR, A], MixedTree[ArrayIR, A])] = {
    tree match {
      case MixedTree.Node(IndexAt, Seq(), Seq(), Seq(array, index)) => Some((array, index))
      case _ => None
    }
  }
}

/**
 * An ifold operation in the minimalist array IR.
 */
object Ifold extends Value {
  override def typeArgCount: Int = 1
  override def valueArgCount: Int = 2

  override def inferType[A](typeArgs: Seq[MixedTree[Type, A]], valueArgTypes: Seq[MixedTree[Type, A]]): MixedTree[Type, A] = {
    val foldFunction = valueArgTypes(1)
    val elementType = foldFunction match {
      case FunctionType(_, FunctionType(_, resultType)) => resultType
      case _ => throw new IllegalArgumentException("The second argument of an ifold must be a folding function.")
    }
    elementType
  }

  def apply[A](size: MixedTree[ArrayIR, A], init: MixedTree[ArrayIR, A], foldFunction: MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] = {
    MixedTree.unslotted(
      Ifold,
      Seq(
        size,
        init,
        foldFunction))
  }

  def unapply[A](tree: MixedTree[ArrayIR, A]): Option[(MixedTree[ArrayIR, A], MixedTree[ArrayIR, A], MixedTree[ArrayIR, A])] = {
    tree match {
      case MixedTree.Node(Ifold, Seq(), Seq(), Seq(array, init, foldFunction)) =>
        Some((array, init, foldFunction))
      case _ => None
    }
  }
}

/**
 * A tuple construction operation in the minimalist array IR.
 */
object Tuple extends Value {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 2

  override def inferType[A](typeArgs: Seq[MixedTree[Type, A]], valueArgTypes: Seq[MixedTree[Type, A]]): MixedTree[Type, A] = {
    TupleType(valueArgTypes.head, valueArgTypes(1))
  }

  def apply[A](fst: MixedTree[ArrayIR, A], snd: MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] = {
    MixedTree.unslotted(Tuple, Seq(fst, snd))
  }

  def unapply[A](tree: MixedTree[ArrayIR, A]): Option[(MixedTree[ArrayIR, A], MixedTree[ArrayIR, A])] = {
    tree match {
      case MixedTree.Node(Tuple, Seq(), Seq(), Seq(fst, snd)) => Some((fst, snd))
      case _ => None
    }
  }
}

/**
 * A tuple projection operation in the minimalist array IR.
 */
object Fst extends Value {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 1

  override def inferType[A](typeArgs: Seq[MixedTree[Type, A]], valueArgTypes: Seq[MixedTree[Type, A]]): MixedTree[Type, A] = {
    val tupleType = valueArgTypes.head
    tupleType match {
      case TupleType(fstType, _) => fstType
      case _ => throw new IllegalArgumentException("The argument of a fst must be a tuple.")
    }
  }

  def apply[A](tuple: MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] = {
    MixedTree.unslotted(Fst, Seq(tuple))
  }

  def unapply[A](tree: MixedTree[ArrayIR, A]): Option[MixedTree[ArrayIR, A]] = {
    tree match {
      case MixedTree.Node(Fst, Seq(), Seq(), Seq(tuple)) => Some(tuple)
      case _ => None
    }
  }
}

/**
 * A tuple projection operation in the minimalist array IR.
 */
object Snd extends Value {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 1

  override def inferType[A](typeArgs: Seq[MixedTree[Type, A]], valueArgTypes: Seq[MixedTree[Type, A]]): MixedTree[Type, A] = {
    val tupleType = valueArgTypes.head
    tupleType match {
      case TupleType(_, sndType) => sndType
      case _ => throw new IllegalArgumentException("The argument of a snd must be a tuple.")
    }
  }

  def apply[A](tuple: MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] = {
    MixedTree.unslotted(Snd, Seq(tuple))
  }

  def unapply[A](tree: MixedTree[ArrayIR, A]): Option[MixedTree[ArrayIR, A]] = {
    tree match {
      case MixedTree.Node(Snd, Seq(), Seq(), Seq(tuple)) => Some(tuple)
      case _ => None
    }
  }
}

/**
 * A constant in the minimalist array IR.
 */
final case class ConstDouble(value: Double) extends Value {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 0

  override def inferType[A](typeArgs: Seq[MixedTree[Type, A]], valueArgTypes: Seq[MixedTree[Type, A]]): MixedTree[Type, A] = {
    DoubleType.toTree
  }

  def toTree: Tree[ArrayIR] = {
    Tree.unslotted(ConstDouble(value), Seq.empty)
  }
}

/**
 * A 32-bit integer constant in the minimalist array IR.
 */
final case class ConstInt32(value: Int) extends Value {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 0

  override def inferType[A](typeArgs: Seq[MixedTree[Type, A]], valueArgTypes: Seq[MixedTree[Type, A]]): MixedTree[Type, A] = {
    Int32Type.toTree
  }

  def toTree: Tree[ArrayIR] = {
    Tree.unslotted(ConstInt32(value), Seq.empty)
  }
}

/**
 * An addition operation in the minimalist array IR.
 */
object Add extends Value {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 2

  override def inferType[A](typeArgs: Seq[MixedTree[Type, A]], valueArgTypes: Seq[MixedTree[Type, A]]): MixedTree[Type, A] = {
    valueArgTypes.head
  }

  def apply[A](lhs: MixedTree[ArrayIR, A], rhs: MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] = {
    MixedTree.unslotted(Add, Seq(lhs, rhs))
  }

  def unapply[A](tree: MixedTree[ArrayIR, A]): Option[(MixedTree[ArrayIR, A], MixedTree[ArrayIR, A])] = {
    tree match {
      case MixedTree.Node(Add, Seq(), Seq(), Seq(lhs, rhs)) => Some((lhs, rhs))
      case _ => None
    }
  }
}

/**
 * A multiplication operation in the minimalist array IR.
 */
object Mul extends Value {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 2

  override def inferType[A](typeArgs: Seq[MixedTree[Type, A]], valueArgTypes: Seq[MixedTree[Type, A]]): MixedTree[Type, A] = {
    valueArgTypes.head
  }

  def apply[A](lhs: MixedTree[ArrayIR, A], rhs: MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] = {
    MixedTree.unslotted(Mul, Seq(lhs, rhs))
  }

  def unapply[A](tree: MixedTree[ArrayIR, A]): Option[(MixedTree[ArrayIR, A], MixedTree[ArrayIR, A])] = {
    tree match {
      case MixedTree.Node(Mul, Seq(), Seq(), Seq(lhs, rhs)) => Some((lhs, rhs))
      case _ => None
    }
  }
}

/**
 * A function call in the minimalist array IR.
 */
trait ExternFunctionCall extends Value {
  /**
   * The name of the function.
   * @return The name of the function.
   */
  def name: String
}
