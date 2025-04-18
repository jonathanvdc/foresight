package foresight.eqsat.examples.liar

import foresight.eqsat.{MixedTree, Slot, Tree}

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
    tree.mapNodes({
      case n: Type => n
      case _ => throw new IllegalArgumentException("Type tree contains value.")
    })
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
 * The Boolean type in the minimalist array IR.
 */
object BoolType extends Type {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 0

  def toTree: Tree[Type] = {
    Tree.unslotted(BoolType, Seq.empty)
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

object ConstIntType {
  def toNumber(tree: MixedTree[Type, _]): BigInt = {
    tree match {
      case MixedTree.Node(ConstIntType(n), _, _, _) => n
      case _ => throw new IllegalArgumentException("The argument must be a constant integer type.")
    }
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

  def cost[A](typeArgs: Seq[MixedTree[Type, A]],
              valueArgTypes: Seq[MixedTree[Type, A]],
              valueArgCosts: Seq[BigInt]): BigInt
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

  override def cost[A](typeArgs: Seq[MixedTree[Type, A]],
                       valueArgTypes: Seq[MixedTree[Type, A]],
                       valueArgCosts: Seq[BigInt]): BigInt = {
    2
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

  override def cost[A](typeArgs: Seq[MixedTree[Type, A]],
                       valueArgTypes: Seq[MixedTree[Type, A]],
                       valueArgCosts: Seq[BigInt]): BigInt = {
    1 + valueArgCosts.head
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
      case FunctionType(argType, returnType) =>
        require(argType == valueArgTypes(1), "The argument type of the function must match the type of the argument.")
        returnType
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

  override def cost[A](typeArgs: Seq[MixedTree[Type, A]],
                       valueArgTypes: Seq[MixedTree[Type, A]],
                       valueArgCosts: Seq[BigInt]): BigInt = {
    1 + valueArgCosts.sum
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

  override def cost[A](typeArgs: Seq[MixedTree[Type, A]],
                       valueArgTypes: Seq[MixedTree[Type, A]],
                       valueArgCosts: Seq[BigInt]): BigInt = {
    val Seq(size) = typeArgs
    val Seq(fCost) = valueArgCosts
    ConstIntType.toNumber(size) * (fCost + 1) + 1
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

  override def cost[A](typeArgs: Seq[MixedTree[Type, A]],
                       valueArgTypes: Seq[MixedTree[Type, A]],
                       valueArgCosts: Seq[BigInt]): BigInt = {
    1 + valueArgCosts.sum
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

  def apply[A](size: MixedTree[Type, A], init: MixedTree[ArrayIR, A], foldFunction: MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] = {
    MixedTree.unslotted(
      Ifold,
      Seq(
        size,
        init,
        foldFunction))
  }

  def unapply[A](tree: MixedTree[ArrayIR, A]): Option[(MixedTree[ArrayIR, A], MixedTree[ArrayIR, A], MixedTree[ArrayIR, A])] = {
    tree match {
      case MixedTree.Node(Ifold, Seq(), Seq(), Seq(size, init, foldFunction)) =>
        Some((size, init, foldFunction))
      case _ => None
    }
  }

  override def cost[A](typeArgs: Seq[MixedTree[Type, A]],
                       valueArgTypes: Seq[MixedTree[Type, A]],
                       valueArgCosts: Seq[BigInt]): BigInt = {
    val Seq(size) = typeArgs
    val Seq(initCost, foldCost) = valueArgCosts
    ConstIntType.toNumber(size) * foldCost + initCost + 1
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

  override def cost[A](typeArgs: Seq[MixedTree[Type, A]],
                       valueArgTypes: Seq[MixedTree[Type, A]],
                       valueArgCosts: Seq[BigInt]): BigInt = {
    1 + valueArgCosts.sum
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

  override def cost[A](typeArgs: Seq[MixedTree[Type, A]],
                       valueArgTypes: Seq[MixedTree[Type, A]],
                       valueArgCosts: Seq[BigInt]): BigInt = {
    1 + valueArgCosts.head
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

  override def cost[A](typeArgs: Seq[MixedTree[Type, A]],
                       valueArgTypes: Seq[MixedTree[Type, A]],
                       valueArgCosts: Seq[BigInt]): BigInt = {
    1 + valueArgCosts.head
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

  override def cost[A](typeArgs: Seq[MixedTree[Type, A]],
             valueArgTypes: Seq[MixedTree[Type, A]],
             valueArgCosts: Seq[BigInt]): BigInt = {
    1
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

  override def cost[A](typeArgs: Seq[MixedTree[Type, A]],
             valueArgTypes: Seq[MixedTree[Type, A]],
             valueArgCosts: Seq[BigInt]): BigInt = {
    1
  }
}

/**
 * A binary arithmetic operation in the minimalist array IR.
 */
trait BinaryArith extends Value {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 2

  def apply[A](lhs: MixedTree[ArrayIR, A], rhs: MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] = {
    MixedTree.unslotted(this, Seq(lhs, rhs))
  }

  override def inferType[A](typeArgs: Seq[MixedTree[Type, A]], valueArgTypes: Seq[MixedTree[Type, A]]): MixedTree[Type, A] = {
    require(valueArgTypes.size == 2)
    require(valueArgTypes.head == valueArgTypes(1), "The two arguments of a binary operation must have the same type.")
    valueArgTypes.head
  }

  override def cost[A](typeArgs: Seq[MixedTree[Type, A]],
                       valueArgTypes: Seq[MixedTree[Type, A]],
                       valueArgCosts: Seq[BigInt]): BigInt = {
    1 + valueArgCosts.sum
  }
}

/**
 * An addition operation in the minimalist array IR.
 */
object Add extends BinaryArith {
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
object Mul extends BinaryArith {
  def unapply[A](tree: MixedTree[ArrayIR, A]): Option[(MixedTree[ArrayIR, A], MixedTree[ArrayIR, A])] = {
    tree match {
      case MixedTree.Node(Mul, Seq(), Seq(), Seq(lhs, rhs)) => Some((lhs, rhs))
      case _ => None
    }
  }
}

/**
 * A subtraction operation in the minimalist array IR.
 */
object Sub extends BinaryArith {
  def unapply[A](tree: MixedTree[ArrayIR, A]): Option[(MixedTree[ArrayIR, A], MixedTree[ArrayIR, A])] = {
    tree match {
      case MixedTree.Node(Sub, Seq(), Seq(), Seq(lhs, rhs)) => Some((lhs, rhs))
      case _ => None
    }
  }
}

/**
 * A division operation in the minimalist array IR.
 */
object Div extends BinaryArith {
  def unapply[A](tree: MixedTree[ArrayIR, A]): Option[(MixedTree[ArrayIR, A], MixedTree[ArrayIR, A])] = {
    tree match {
      case MixedTree.Node(Div, Seq(), Seq(), Seq(lhs, rhs)) => Some((lhs, rhs))
      case _ => None
    }
  }
}

/**
 * A modulo operation in the minimalist array IR.
 */
object Mod extends BinaryArith {
  def unapply[A](tree: MixedTree[ArrayIR, A]): Option[(MixedTree[ArrayIR, A], MixedTree[ArrayIR, A])] = {
    tree match {
      case MixedTree.Node(Mod, Seq(), Seq(), Seq(lhs, rhs)) => Some((lhs, rhs))
      case _ => None
    }
  }
}

trait Comparison extends Value {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 2

  override def inferType[A](typeArgs: Seq[MixedTree[Type, A]], valueArgTypes: Seq[MixedTree[Type, A]]): MixedTree[Type, A] = {
    require(valueArgTypes.size == 2)
    require(valueArgTypes.head == valueArgTypes(1), "The two arguments of a comparison must have the same type.")
    BoolType.toTree
  }

  override def cost[A](typeArgs: Seq[MixedTree[Type, A]],
                       valueArgTypes: Seq[MixedTree[Type, A]],
                       valueArgCosts: Seq[BigInt]): BigInt = {
    1 + valueArgCosts.sum
  }
}

/**
 * A less-than operation in the minimalist array IR.
 */
object LessThan extends Comparison {
  def apply[A](lhs: MixedTree[ArrayIR, A], rhs: MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] = {
    MixedTree.unslotted(LessThan, Seq(lhs, rhs))
  }

  def unapply[A](tree: MixedTree[ArrayIR, A]): Option[(MixedTree[ArrayIR, A], MixedTree[ArrayIR, A])] = {
    tree match {
      case MixedTree.Node(LessThan, Seq(), Seq(), Seq(lhs, rhs)) => Some((lhs, rhs))
      case _ => None
    }
  }
}

/**
 * A conditional operation in the minimalist array IR.
 */
object IfThenElse extends Value {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 3

  override def inferType[A](typeArgs: Seq[MixedTree[Type, A]], valueArgTypes: Seq[MixedTree[Type, A]]): MixedTree[Type, A] = {
    require(valueArgTypes.size == 3)
    require(valueArgTypes(1) == valueArgTypes(2), "The two branches of an if must have the same type.")
    valueArgTypes(1)
  }

  def apply[A](condition: MixedTree[ArrayIR, A], thenBranch: MixedTree[ArrayIR, A], elseBranch: MixedTree[ArrayIR, A]): MixedTree[ArrayIR, A] = {
    MixedTree.unslotted(IfThenElse, Seq(condition, thenBranch, elseBranch))
  }

  def unapply[A](tree: MixedTree[ArrayIR, A]): Option[(MixedTree[ArrayIR, A], MixedTree[ArrayIR, A], MixedTree[ArrayIR, A])] = {
    tree match {
      case MixedTree.Node(IfThenElse, Seq(), Seq(), Seq(condition, thenBranch, elseBranch)) =>
        Some((condition, thenBranch, elseBranch))
      case _ => None
    }
  }

  override def cost[A](typeArgs: Seq[MixedTree[Type, A]],
                       valueArgTypes: Seq[MixedTree[Type, A]],
                       valueArgCosts: Seq[BigInt]): BigInt = {
    1 + valueArgCosts.sum
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
