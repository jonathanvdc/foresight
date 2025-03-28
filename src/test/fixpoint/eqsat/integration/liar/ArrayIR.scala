package fixpoint.eqsat.integration.liar

import fixpoint.eqsat.rewriting.patterns.{Pattern, SlotVar}
import fixpoint.eqsat.{Slot, Tree}

/**
 * A description of the minimalist array IR from Latent Idiom Recognition for a Minimalist Functional Array Language
 * Using Equality Saturation by Jonathan Van Der Cruysse and Christophe Dubach.
 */
sealed trait ArrayIR {
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
}

/**
 * A type in the minimalist array IR.
 */
sealed trait Type extends ArrayIR

/**
 * The companion object for [[Type]].
 */
object Type {
  def asType(tree: Tree[ArrayIR]): Tree[Type] = {
    tree.map(_.asInstanceOf[Type])
  }
}

/**
 * The double type in the minimalist array IR.
 */
object DoubleType extends Type {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 0
}

/**
 * The 32-bit integer type in the minimalist array IR.
 */
object Int32Type extends Type {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 0
}

/**
 * An integer constant type in the minimalist array IR.
 */
final case class ConstIntType(value: Int) extends Type {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 0
}

/**
 * An array type in the minimalist array IR.
 */
object ArrayType extends Type {
  override def typeArgCount: Int = 2
  override def valueArgCount: Int = 0

  def apply(elementType: Pattern[ArrayIR], sizeType: Pattern[ArrayIR]): Pattern[ArrayIR] = {
    Pattern.Node(ArrayType, Seq.empty, Seq.empty, Seq(elementType, sizeType))
  }

  def apply(elementType: Tree[Type], sizeType: Tree[Type]): Tree[Type] = {
    Tree.unslotted(ArrayType, Seq(elementType, sizeType))
  }

  def unapply(tree: Tree[Type]): Option[(Tree[Type], Tree[Type])] = {
    tree match {
      case Tree(ArrayType, Seq(), Seq(), Seq(elementType, sizeType)) => Some((elementType, sizeType))
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

  def apply(fstType: Tree[Type], sndType: Tree[Type]): Tree[Type] = {
    Tree.unslotted(TupleType, Seq(fstType, sndType))
  }

  def unapply(tree: Tree[Type]): Option[(Tree[Type], Tree[Type])] = {
    tree match {
      case Tree(TupleType, Seq(), Seq(), Seq(fstType, sndType)) => Some((fstType, sndType))
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

  def apply(argType: Pattern[ArrayIR], returnType: Pattern[ArrayIR]): Pattern[ArrayIR] = {
    Pattern.Node(FunctionType, Seq.empty, Seq.empty, Seq(argType, returnType))
  }

  def apply(argType: Tree[Type], returnType: Tree[Type]): Tree[Type] = {
    Tree.unslotted(FunctionType, Seq(argType, returnType))
  }

  def unapply(tree: Tree[Type]): Option[(Tree[Type], Tree[Type])] = {
    tree match {
      case Tree(FunctionType, Seq(), Seq(), Seq(argType, returnType)) => Some((argType, returnType))
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
  def inferType(typeArgs: Seq[Tree[Type]], valueArgTypes: Seq[Tree[Type]]): Tree[Type]
}

/**
 * The companion object for [[Value]].
 */
object Value {
  /**
   * Obtains the type of a value in the minimalist array IR.
   * @param tree The value.
   * @return The type of the value.
   */
  def typeOf(tree: Tree[ArrayIR]): Tree[Type] = {
    tree match {
      case Tree(_: Value, _, _, args) => Type.asType(args.head)
      case Tree(_: Type, _, _, _)  => throw new IllegalArgumentException("Cannot get the type of a type.")
    }
  }
}

/**
 * A variable use in the minimalist array IR.
 */
object Var extends Value {
  override def typeArgCount: Int = 1
  override def valueArgCount: Int = 0

  override def inferType(typeArgs: Seq[Tree[Type]], valueArgTypes: Seq[Tree[Type]]): Tree[Type] = typeArgs.head

  def apply(slot: Slot, typeArg: Tree[Type]): Tree[ArrayIR] = {
    Tree(Var, Seq.empty, Seq(slot), Seq(typeArg))
  }

  def unapply(tree: Tree[ArrayIR]): Option[(Slot, Tree[Type])] = {
    tree match {
      case Tree(Var, Seq(), Seq(slot), Seq(typeArg)) => Some((slot, typeArg.map(_.asInstanceOf[Type])))
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

  override def inferType(typeArgs: Seq[Tree[Type]], valueArgTypes: Seq[Tree[Type]]): Tree[Type] = {
    FunctionType(typeArgs.head, valueArgTypes.head)
  }

  def apply(slot: SlotVar, argType: Pattern[ArrayIR], body: Pattern[ArrayIR]): Pattern[ArrayIR] = {
    Pattern.Node(Lambda, Seq(slot), Seq.empty, Seq(Pattern.Var.fresh[ArrayIR](), argType, body))
  }

  def apply(slot: Slot, argType: Tree[Type], body: Tree[ArrayIR]): Tree[ArrayIR] = {
    Tree(Lambda, Seq(slot), Seq.empty, Seq(inferType(Seq(argType), Seq(Value.typeOf(body))), argType, body))
  }

  def unapply(tree: Tree[ArrayIR]): Option[(Slot, Tree[Type], Tree[ArrayIR], Tree[Type])] = {
    tree match {
      case Tree(Lambda, Seq(slot), Seq(), Seq(t, argType, body)) =>
        Some((slot, argType.map(_.asInstanceOf[Type]), body, Type.asType(t)))
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

  override def inferType(typeArgs: Seq[Tree[Type]], valueArgTypes: Seq[Tree[Type]]): Tree[Type] = {
    val functionType = valueArgTypes.head
    functionType match {
      case FunctionType(_, returnType) => returnType
      case _ => throw new IllegalArgumentException("The first argument of an application must be a function.")
    }
  }

  def apply(function: Pattern[ArrayIR], argument: Pattern[ArrayIR]): Pattern[ArrayIR] = {
    Pattern.Node(Apply, Seq.empty, Seq.empty, Seq(Pattern.Var.fresh[ArrayIR](), function, argument))
  }

  def apply(function: Tree[ArrayIR], argument: Tree[ArrayIR]): Tree[ArrayIR] = {
    Tree.unslotted(Apply, Seq(inferType(Seq.empty, Seq(Value.typeOf(function), Value.typeOf(argument))), function, argument))
  }

  def unapply(tree: Tree[ArrayIR]): Option[(Tree[ArrayIR], Tree[ArrayIR], Tree[Type])] = {
    tree match {
      case Tree(Apply, Seq(), Seq(), Seq(t, function, argument)) =>
        Some((function, argument, Type.asType(t)))
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

  override def inferType(typeArgs: Seq[Tree[Type]], valueArgTypes: Seq[Tree[Type]]): Tree[Type] = {
    val functionType = valueArgTypes.head
    val elementType = functionType match {
      case FunctionType(_, returnType) => returnType
      case _ => throw new IllegalArgumentException("The argument of a build must be a function.")
    }
    ArrayType(elementType, typeArgs.head)
  }

  def apply(size: Pattern[ArrayIR], function: Pattern[ArrayIR]): Pattern[ArrayIR] = {
    Pattern.Node(Build, Seq.empty, Seq.empty, Seq(Pattern.Var.fresh[ArrayIR](), size, function))
  }

  def apply(size: Tree[Type], function: Tree[ArrayIR]): Tree[ArrayIR] = {
    Tree.unslotted(Build, Seq(inferType(Seq(size), Seq(Value.typeOf(function))), size, function))
  }

  def unapply(tree: Tree[ArrayIR]): Option[(Tree[Type], Tree[ArrayIR], Tree[Type])] = {
    tree match {
      case Tree(Build, Seq(), Seq(), Seq(t, size, function)) =>
        Some((Type.asType(size), function, Type.asType(t)))

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

  override def inferType(typeArgs: Seq[Tree[Type]], valueArgTypes: Seq[Tree[Type]]): Tree[Type] = {
    val arrayType = valueArgTypes.head
    val elementType = arrayType match {
      case ArrayType(elementType, _) => elementType
      case _ => throw new IllegalArgumentException("The first argument of an index must be an array.")
    }
    elementType
  }

  def apply(array: Pattern[ArrayIR], index: Pattern[ArrayIR]): Pattern[ArrayIR] = {
    Pattern.Node(IndexAt, Seq.empty, Seq.empty, Seq(Pattern.Var.fresh[ArrayIR](), array, index))
  }

  def apply(array: Tree[ArrayIR], index: Tree[ArrayIR]): Tree[ArrayIR] = {
    Tree.unslotted(IndexAt, Seq(inferType(Seq.empty, Seq(Value.typeOf(array), Value.typeOf(index))), array, index))
  }

  def unapply(tree: Tree[ArrayIR]): Option[(Tree[ArrayIR], Tree[ArrayIR], Tree[Type])] = {
    tree match {
      case Tree(IndexAt, Seq(), Seq(), Seq(t, array, index)) =>
        Some((array, index, Type.asType(t)))
      case _ => None
    }
  }
}

/**
 * An ifold operation in the minimalist array IR.
 */
object IFold extends Value {
  override def typeArgCount: Int = 1
  override def valueArgCount: Int = 2

  override def inferType(typeArgs: Seq[Tree[Type]], valueArgTypes: Seq[Tree[Type]]): Tree[Type] = {
    val foldFunction = valueArgTypes(1)
    val elementType = foldFunction match {
      case FunctionType(_, FunctionType(_, resultType)) => resultType
      case _ => throw new IllegalArgumentException("The second argument of an ifold must be a folding function.")
    }
    elementType
  }

  def apply(array: Pattern[ArrayIR], init: Pattern[ArrayIR], foldFunction: Pattern[ArrayIR]): Pattern[ArrayIR] = {
    Pattern.Node(IFold, Seq.empty, Seq.empty, Seq(Pattern.Var.fresh[ArrayIR](), array, init, foldFunction))
  }

  def apply(array: Tree[ArrayIR], init: Tree[ArrayIR], foldFunction: Tree[ArrayIR]): Tree[ArrayIR] = {
    Tree.unslotted(
      IFold,
      Seq(
        inferType(Seq.empty, Seq(Value.typeOf(init), Value.typeOf(array), Value.typeOf(foldFunction))),
        array,
        init,
        foldFunction))
  }

  def unapply(tree: Tree[ArrayIR]): Option[(Tree[ArrayIR], Tree[ArrayIR], Tree[ArrayIR], Tree[Type])] = {
    tree match {
      case Tree(IFold, Seq(), Seq(), Seq(t, array, init, foldFunction)) =>
        Some((array, init, foldFunction, Type.asType(t)))
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

  override def inferType(typeArgs: Seq[Tree[Type]], valueArgTypes: Seq[Tree[Type]]): Tree[Type] = {
    TupleType(valueArgTypes.head, valueArgTypes(1))
  }

  def apply(fst: Pattern[ArrayIR], snd: Pattern[ArrayIR]): Pattern[ArrayIR] = {
    Pattern.Node(Tuple, Seq.empty, Seq.empty, Seq(Pattern.Var.fresh[ArrayIR](), fst, snd))
  }

  def apply(fst: Tree[ArrayIR], snd: Tree[ArrayIR]): Tree[ArrayIR] = {
    Tree.unslotted(Tuple, Seq(inferType(Seq.empty, Seq(Value.typeOf(fst), Value.typeOf(snd))), fst, snd))
  }

  def unapply(tree: Tree[ArrayIR]): Option[(Tree[ArrayIR], Tree[ArrayIR], Tree[Type])] = {
    tree match {
      case Tree(Tuple, Seq(), Seq(), Seq(t, fst, snd)) =>
        Some((fst, snd, Type.asType(t)))
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

  override def inferType(typeArgs: Seq[Tree[Type]], valueArgTypes: Seq[Tree[Type]]): Tree[Type] = {
    val tupleType = valueArgTypes.head
    tupleType match {
      case TupleType(fstType, _) => fstType
      case _ => throw new IllegalArgumentException("The argument of a fst must be a tuple.")
    }
  }

  def apply(tuple: Pattern[ArrayIR]): Pattern[ArrayIR] = {
    Pattern.Node(Fst, Seq.empty, Seq.empty, Seq(Pattern.Var.fresh[ArrayIR](), tuple))
  }

  def apply(tuple: Tree[ArrayIR]): Tree[ArrayIR] = {
    Tree.unslotted(Fst, Seq(inferType(Seq.empty, Seq(Value.typeOf(tuple))), tuple))
  }

  def unapply(tree: Tree[ArrayIR]): Option[(Tree[ArrayIR], Tree[Type])] = {
    tree match {
      case Tree(Fst, Seq(), Seq(), Seq(t, tuple)) =>
        Some((tuple, Type.asType(t)))
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

  override def inferType(typeArgs: Seq[Tree[Type]], valueArgTypes: Seq[Tree[Type]]): Tree[Type] = {
    val tupleType = valueArgTypes.head
    tupleType match {
      case TupleType(_, sndType) => sndType
      case _ => throw new IllegalArgumentException("The argument of a snd must be a tuple.")
    }
  }

  def apply(tuple: Pattern[ArrayIR]): Pattern[ArrayIR] = {
    Pattern.Node(Snd, Seq.empty, Seq.empty, Seq(Pattern.Var.fresh[ArrayIR](), tuple))
  }

  def apply(tuple: Tree[ArrayIR]): Tree[ArrayIR] = {
    Tree.unslotted(Snd, Seq(inferType(Seq.empty, Seq(Value.typeOf(tuple))), tuple))
  }

  def unapply(tree: Tree[ArrayIR]): Option[(Tree[ArrayIR], Tree[Type])] = {
    tree match {
      case Tree(Snd, Seq(), Seq(), Seq(t, tuple)) =>
        Some((tuple, Type.asType(t)))
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

  override def inferType(typeArgs: Seq[Tree[Type]], valueArgTypes: Seq[Tree[Type]]): Tree[Type] = {
    Tree.unslotted(DoubleType, Seq.empty)
  }

  def toPattern: Pattern[ArrayIR] = {
    Pattern.Node(ConstDouble(value), Seq.empty, Seq.empty, Seq.empty)
  }

  def toTree: Tree[ArrayIR] = {
    Tree.unslotted(ConstDouble(value), Seq(inferType(Seq.empty, Seq.empty)))
  }
}

/**
 * A 32-bit integer constant in the minimalist array IR.
 */
final case class ConstInt32(value: Int) extends Value {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 0

  override def inferType(typeArgs: Seq[Tree[Type]], valueArgTypes: Seq[Tree[Type]]): Tree[Type] = {
    Tree.unslotted(Int32Type, Seq.empty)
  }

  def toPattern: Pattern[ArrayIR] = {
    Pattern.Node(ConstInt32(value), Seq.empty, Seq.empty, Seq.empty)
  }

  def toTree: Tree[ArrayIR] = {
    Tree.unslotted(ConstInt32(value), Seq(inferType(Seq.empty, Seq.empty)))
  }
}

/**
 * An addition operation in the minimalist array IR.
 */
object Add extends Value {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 2

  override def inferType(typeArgs: Seq[Tree[Type]], valueArgTypes: Seq[Tree[Type]]): Tree[Type] = {
    valueArgTypes.head
  }

  def apply(lhs: Pattern[ArrayIR], rhs: Pattern[ArrayIR]): Pattern[ArrayIR] = {
    Pattern.Node(Add, Seq.empty, Seq.empty, Seq(Pattern.Var.fresh[ArrayIR](), lhs, rhs))
  }

  def apply(lhs: Tree[ArrayIR], rhs: Tree[ArrayIR]): Tree[ArrayIR] = {
    Tree.unslotted(Add, Seq(inferType(Seq.empty, Seq(Value.typeOf(lhs), Value.typeOf(rhs))), lhs, rhs))
  }

  def unapply(tree: Tree[ArrayIR]): Option[(Tree[ArrayIR], Tree[ArrayIR], Tree[Type])] = {
    tree match {
      case Tree(Add, Seq(), Seq(), Seq(t, lhs, rhs)) => Some((lhs, rhs, Type.asType(t)))
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

  override def inferType(typeArgs: Seq[Tree[Type]], valueArgTypes: Seq[Tree[Type]): Tree[Type] = {
    valueArgTypes.head
  }

  def apply(lhs: Pattern[ArrayIR], rhs: Pattern[ArrayIR]): Pattern[ArrayIR] = {
    Pattern.Node(Mul, Seq.empty, Seq.empty, Seq(Pattern.Var.fresh[ArrayIR](), lhs, rhs))
  }

  def apply(lhs: Tree[ArrayIR], rhs: Tree[ArrayIR]): Tree[ArrayIR] = {
    Tree.unslotted(Mul, Seq(inferType(Seq.empty, Seq(Value.typeOf(lhs), Value.typeOf(rhs))), lhs, rhs))
  }

  def unapply(tree: Tree[ArrayIR]): Option[(Tree[ArrayIR], Tree[ArrayIR], Tree[Type])] = {
    tree match {
      case Tree(Mul, Seq(), Seq(), Seq(t, lhs, rhs)) => Some((lhs, rhs, Type.asType(t)))
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
