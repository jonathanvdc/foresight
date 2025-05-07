package foresight.eqsat.examples.sdql

import foresight.eqsat.{MixedTree, Slot, Tree}

/**
 * An SDQL IR node.
 */
sealed trait SdqlIR extends Ordered[SdqlIR] {
  def typeArgCount: Int
  def valueArgCount: Int

  override def compare(that: SdqlIR): Int = {
    this.hashCode().compareTo(that.hashCode())
  }
}

/**
 * A lambda abstraction in the Sdql IR.
 */
object Lam extends SdqlIR {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 1

  def apply[A](slot: Slot, body: MixedTree[SdqlIR, A]): MixedTree[SdqlIR, A] = {
    MixedTree.Node(Lam, Seq(slot), Seq.empty, Seq(body))
  }

  def unapply[A](tree: MixedTree[SdqlIR, A]): Option[(Slot, MixedTree[SdqlIR, A])] = {
    tree match {
      case MixedTree.Node(Lam, Seq(slot), Seq(), Seq(body)) => Some((slot, body))
      case _ => None
    }
  }
}

/**
 * A variable in the Sdql IR.
 */
object Var extends SdqlIR {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 0

  def apply[A](slot: Slot): MixedTree[SdqlIR, A] = {
    MixedTree.Node(Var, Seq.empty, Seq(slot), Seq.empty)
  }

  def unapply[A](tree: MixedTree[SdqlIR, A]): Option[Slot] = {
    tree match {
      case MixedTree.Node(Var, Seq(), Seq(slot), Seq()) => Some(slot)
      case _ => None
    }
  }
}

/**
 * A binary operation in the Sdql IR.
 */
object Binop extends SdqlIR {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 3

  def apply[A](op: MixedTree[SdqlIR, A], lhs: MixedTree[SdqlIR, A], rhs: MixedTree[SdqlIR, A]): MixedTree[SdqlIR, A] = {
    MixedTree.unslotted(Binop, Seq(op, lhs, rhs))
  }

  def unapply[A](tree: MixedTree[SdqlIR, A]): Option[(MixedTree[SdqlIR, A], MixedTree[SdqlIR, A], MixedTree[SdqlIR, A])] = {
    tree match {
      case MixedTree.Node(Binop, Seq(), Seq(), Seq(op, lhs, rhs)) => Some((op, lhs, rhs))
      case _ => None
    }
  }
}

/**
 * A singleton operation in the Sdql IR.
 */
object Sing extends SdqlIR {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 2

  def apply[A](lhs: MixedTree[SdqlIR, A], rhs: MixedTree[SdqlIR, A]): MixedTree[SdqlIR, A] = {
    MixedTree.unslotted(Sing, Seq(lhs, rhs))
  }

  def unapply[A](tree: MixedTree[SdqlIR, A]): Option[(MixedTree[SdqlIR, A], MixedTree[SdqlIR, A])] = {
    tree match {
      case MixedTree.Node(Sing, Seq(), Seq(), Seq(lhs, rhs)) => Some((lhs, rhs))
      case _ => None
    }
  }
}

/**
 * An addition operation in the Sdql IR.
 */
object Add extends SdqlIR {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 2

  def apply[A](lhs: MixedTree[SdqlIR, A], rhs: MixedTree[SdqlIR, A]): MixedTree[SdqlIR, A] = {
    MixedTree.unslotted(Add, Seq(lhs, rhs))
  }

  def unapply[A](tree: MixedTree[SdqlIR, A]): Option[(MixedTree[SdqlIR, A], MixedTree[SdqlIR, A])] = {
    tree match {
      case MixedTree.Node(Add, Seq(), Seq(), Seq(lhs, rhs)) => Some((lhs, rhs))
      case _ => None
    }
  }
}

/**
 * A multiplication operation in the Sdql IR.
 */
object Mult extends SdqlIR {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 2

  def apply[A](lhs: MixedTree[SdqlIR, A], rhs: MixedTree[SdqlIR, A]): MixedTree[SdqlIR, A] = {
    MixedTree.unslotted(Mult, Seq(lhs, rhs))
  }

  def unapply[A](tree: MixedTree[SdqlIR, A]): Option[(MixedTree[SdqlIR, A], MixedTree[SdqlIR, A])] = {
    tree match {
      case MixedTree.Node(Mult, Seq(), Seq(), Seq(lhs, rhs)) => Some((lhs, rhs))
      case _ => None
    }
  }
}

/**
 * A subtraction operation in the Sdql IR.
 */
object Sub extends SdqlIR {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 2

  def apply[A](lhs: MixedTree[SdqlIR, A], rhs: MixedTree[SdqlIR, A]): MixedTree[SdqlIR, A] = {
    MixedTree.unslotted(Sub, Seq(lhs, rhs))
  }

  def unapply[A](tree: MixedTree[SdqlIR, A]): Option[(MixedTree[SdqlIR, A], MixedTree[SdqlIR, A])] = {
    tree match {
      case MixedTree.Node(Sub, Seq(), Seq(), Seq(lhs, rhs)) => Some((lhs, rhs))
      case _ => None
    }
  }
}

/**
 * An equality operation in the Sdql IR.
 */
object Equality extends SdqlIR {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 2

  def apply[A](lhs: MixedTree[SdqlIR, A], rhs: MixedTree[SdqlIR, A]): MixedTree[SdqlIR, A] = {
    MixedTree.unslotted(Equality, Seq(lhs, rhs))
  }

  def unapply[A](tree: MixedTree[SdqlIR, A]): Option[(MixedTree[SdqlIR, A], MixedTree[SdqlIR, A])] = {
    tree match {
      case MixedTree.Node(Equality, Seq(), Seq(), Seq(lhs, rhs)) => Some((lhs, rhs))
      case _ => None
    }
  }
}

/**
 * A get operation in the Sdql IR.
 */
object Get extends SdqlIR {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 2

  def apply[A](array: MixedTree[SdqlIR, A], index: MixedTree[SdqlIR, A]): MixedTree[SdqlIR, A] = {
    MixedTree.unslotted(Get, Seq(array, index))
  }

  def unapply[A](tree: MixedTree[SdqlIR, A]): Option[(MixedTree[SdqlIR, A], MixedTree[SdqlIR, A])] = {
    tree match {
      case MixedTree.Node(Get, Seq(), Seq(), Seq(array, index)) => Some((array, index))
      case _ => None
    }
  }
}

/**
 * A range operation in the Sdql IR.
 */
object Range extends SdqlIR {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 2

  def apply[A](start: MixedTree[SdqlIR, A], end: MixedTree[SdqlIR, A]): MixedTree[SdqlIR, A] = {
    MixedTree.unslotted(Range, Seq(start, end))
  }

  def unapply[A](tree: MixedTree[SdqlIR, A]): Option[(MixedTree[SdqlIR, A], MixedTree[SdqlIR, A])] = {
    tree match {
      case MixedTree.Node(Range, Seq(), Seq(), Seq(start, end)) => Some((start, end))
      case _ => None
    }
  }
}

/**
 * A function application in the Sdql IR.
 */
object App extends SdqlIR {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 2

  def apply[A](func: MixedTree[SdqlIR, A], arg: MixedTree[SdqlIR, A]): MixedTree[SdqlIR, A] = {
    MixedTree.unslotted(App, Seq(func, arg))
  }

  def unapply[A](tree: MixedTree[SdqlIR, A]): Option[(MixedTree[SdqlIR, A], MixedTree[SdqlIR, A])] = {
    tree match {
      case MixedTree.Node(App, Seq(), Seq(), Seq(func, arg)) => Some((func, arg))
      case _ => None
    }
  }
}

/**
 * A conditional operation in the Sdql IR.
 */
object IfThen extends SdqlIR {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 2

  def apply[A](cond: MixedTree[SdqlIR, A], body: MixedTree[SdqlIR, A]): MixedTree[SdqlIR, A] = {
    MixedTree.unslotted(IfThen, Seq(cond, body))
  }

  def unapply[A](tree: MixedTree[SdqlIR, A]): Option[(MixedTree[SdqlIR, A], MixedTree[SdqlIR, A])] = {
    tree match {
      case MixedTree.Node(IfThen, Seq(), Seq(), Seq(cond, body)) => Some((cond, body))
      case _ => None
    }
  }
}

/**
 * A subarray operation in the Sdql IR.
 */
object SubArray extends SdqlIR {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 3

  def apply[A](array: MixedTree[SdqlIR, A], start: MixedTree[SdqlIR, A], end: MixedTree[SdqlIR, A]): MixedTree[SdqlIR, A] = {
    MixedTree.unslotted(SubArray, Seq(array, start, end))
  }

  def unapply[A](tree: MixedTree[SdqlIR, A]): Option[(MixedTree[SdqlIR, A], MixedTree[SdqlIR, A], MixedTree[SdqlIR, A])] = {
    tree match {
      case MixedTree.Node(SubArray, Seq(), Seq(), Seq(array, start, end)) => Some((array, start, end))
      case _ => None
    }
  }
}

/**
 * A unique operation in the Sdql IR.
 */
object Unique extends SdqlIR {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 1

  def apply[A](array: MixedTree[SdqlIR, A]): MixedTree[SdqlIR, A] = {
    MixedTree.unslotted(Unique, Seq(array))
  }

  def unapply[A](tree: MixedTree[SdqlIR, A]): Option[MixedTree[SdqlIR, A]] = {
    tree match {
      case MixedTree.Node(Unique, Seq(), Seq(), Seq(array)) => Some(array)
      case _ => None
    }
  }
}

/**
 * A sum operation in the Sdql IR.
 */
object Sum extends SdqlIR {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 2

  def apply[A](x: Slot, y: Slot, range: MixedTree[SdqlIR, A], body: MixedTree[SdqlIR, A]): MixedTree[SdqlIR, A] = {
    MixedTree.Node(Sum, Seq(x, y), Seq(), Seq(range, body))
  }

  def unapply[A](tree: MixedTree[SdqlIR, A]): Option[(Slot, Slot, MixedTree[SdqlIR, A], MixedTree[SdqlIR, A])] = {
    tree match {
      case MixedTree.Node(Sum, Seq(x, y), Seq(), Seq(range, body)) => Some((x, y, range, body))
      case _ => None
    }
  }
}

/**
 * A merge operation in the Sdql IR.
 */
object Merge extends SdqlIR {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 3

  def apply[A](k1: Slot,
               k2: Slot,
               v1: Slot,
               range1: MixedTree[SdqlIR, A],
               range2: MixedTree[SdqlIR, A],
               body: MixedTree[SdqlIR, A]): MixedTree[SdqlIR, A] = {
    MixedTree.Node(Merge, Seq(k1, k2, v1), Seq(), Seq(range1, range2, body))
  }

  def unapply[A](tree: MixedTree[SdqlIR, A]): Option[(Slot, Slot, Slot, MixedTree[SdqlIR, A], MixedTree[SdqlIR, A], MixedTree[SdqlIR, A])] = {
    tree match {
      case MixedTree.Node(Merge, Seq(k1, k2, v1), Seq(), Seq(range1, range2, body)) => Some((k1, k2, v1, range1, range2, body))
      case _ => None
    }
  }
}

/**
 * A let binding in the Sdql IR.
 */
object Let extends SdqlIR {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 2

  def apply[A](slot: Slot, value: MixedTree[SdqlIR, A], body: MixedTree[SdqlIR, A]): MixedTree[SdqlIR, A] = {
    MixedTree.Node(Let, Seq(slot), Seq.empty, Seq(value, body))
  }

  def unapply[A](tree: MixedTree[SdqlIR, A]): Option[(Slot, MixedTree[SdqlIR, A], MixedTree[SdqlIR, A])] = {
    tree match {
      case MixedTree.Node(Let, Seq(slot), Seq(), Seq(value, body)) => Some((slot, value, body))
      case _ => None
    }
  }
}

/**
 * An integer literal in the Sdql IR.
 * @param value The integer value.
 */
final case class Num(value: Int) extends SdqlIR {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 0

  override def toString: String = value.toString

  def apply[A](): MixedTree[SdqlIR, A] = {
    MixedTree.Node(this, Seq.empty, Seq.empty, Seq.empty)
  }

  def unapply[A](tree: MixedTree[SdqlIR, A]): Option[Int] = {
    tree match {
      case MixedTree.Node(Num(v), Seq(), Seq(), Seq()) => Some(v)
      case _ => None
    }
  }
}

/**
 * A symbol in the Sdql IR.
 * @param name The name of the symbol.
 */
final case class Symbol(name: String) extends SdqlIR {
  override def typeArgCount: Int = 0
  override def valueArgCount: Int = 0

  override def toString: String = name

  def apply[A](): MixedTree[SdqlIR, A] = {
    MixedTree.Node(this, Seq.empty, Seq.empty, Seq.empty)
  }

  def unapply[A](tree: MixedTree[SdqlIR, A]): Option[String] = {
    tree match {
      case MixedTree.Node(Symbol(n), Seq(), Seq(), Seq()) => Some(n)
      case _ => None
    }
  }
}
