package fixpoint.eqsat.rewriting

import fixpoint.eqsat.rewriting.patterns.{Pattern, SlotVar}
import fixpoint.eqsat.{EGraph, ENode, MixedTree, Slot}
import org.junit.Test

import scala.language.implicitConversions

class RewriteArithTest {
  sealed trait Arith
  object Var extends Arith {
    def apply(slot: Slot): ENode[Arith] = ENode(this, Seq.empty, Seq(slot), Seq.empty)
    def apply(slotVar: SlotVar): Pattern.Node[Arith] = Pattern.Node(this, Seq.empty, Seq(slotVar), Seq.empty)
  }
  object Add extends Arith {
    def apply(lhs: Pattern[Arith], rhs: Pattern[Arith]): Pattern.Node[Arith] = {
      Pattern.Node(this, Seq.empty, Seq.empty, Seq(lhs, rhs))
    }
    def apply[A](lhs: MixedTree[Arith, A], rhs: MixedTree[Arith, A]): MixedTree[Arith, A] =
      MixedTree.Node[Arith, A](this, Seq.empty, Seq.empty, Seq(lhs, rhs))
  }
  object Minus extends Arith {
    def apply(lhs: Pattern[Arith], rhs: Pattern[Arith]): Pattern.Node[Arith] = {
      Pattern.Node(this, Seq.empty, Seq.empty, Seq(lhs, rhs))
    }
    def apply[A](lhs: MixedTree[Arith, A], rhs: MixedTree[Arith, A]): MixedTree[Arith, A] =
      MixedTree.Node[Arith, A](this, Seq.empty, Seq.empty, Seq(lhs, rhs))
  }
  final case class Number(value: Int) extends Arith
  object Number {
    implicit def toPattern(number: Number): Pattern[Arith] = Pattern.Node(number, Seq.empty, Seq.empty, Seq.empty)
    implicit def toMixedTree[A](number: Number): MixedTree[Arith, A] = MixedTree.Node(number, Seq.empty, Seq.empty, Seq.empty)
    implicit def toENode(number: Number): ENode[Arith] = ENode(number, Seq.empty, Seq.empty, Seq.empty)
  }

  object Rules {
    val xPlusZeroEqualsX: Rule[Arith, _, EGraph[Arith]] = {
      val x = SlotVar.fresh()
      Rule(
        "x + 0 = x",
        Add(Var(x), Number(0)).toSearcher,
        Var(x).toApplier)
    }

    val xMinusXEqualsZero: Rule[Arith, _, EGraph[Arith]] = {
      val x = SlotVar.fresh()
      Rule(
        "x - x = 0",
        Minus(Var(x), Var(x)).toSearcher,
        Number(0).toApplier)
    }

    val xMinusXEqualsZeroWithPatternVars: Rule[Arith, _, EGraph[Arith]] = {
      val x = Pattern.Var.fresh[Arith]()
      Rule(
        "x - x = 0",
        Minus(x, x).toSearcher,
        Number(0).toApplier)
    }
  }

  @Test
  def xPlusZeroEqualsX(): Unit = {
    val x = Var(Slot.fresh())
    val zero = Number(0)
    val xPlusZero = Add(x, zero)

    val egraph = EGraph.empty[Arith]
    val (c, egraph2) = egraph.add(xPlusZero)

    assert(egraph2.classes.size == 3)
    assert(c.args.size == 1)
    assert(!egraph2.areSame(c, egraph2.find(x).get))

    val egraph3 = Rules.xPlusZeroEqualsX(egraph2)

    assert(egraph3.classes.size == 2)
    assert(egraph3.areSame(c, egraph3.find(x).get))
  }

  @Test
  def xMinusXEqualsZero(): Unit = {
    val x = Var(Slot.fresh())
    val xMinusX = Minus(x, x)
    val zero = Number(0)

    val egraph = EGraph.empty[Arith]
    val (c, egraph2) = egraph.add(xMinusX)

    assert(egraph2.classes.size == 2)
    assert(c.args.size == 1)

    for (rule <- Seq(Rules.xMinusXEqualsZero, Rules.xMinusXEqualsZeroWithPatternVars)) {
      val egraph3 = rule(egraph2)

      assert(egraph3.classes.size == 2)
      assert(egraph3.areSame(c, egraph3.find(zero).get))
    }
  }

  @Test
  def xMinusYDoesNotEqualsZero(): Unit = {
    val x = Var(Slot.fresh())
    val y = Var(Slot.fresh())
    val xMinusY = Minus(x, y)
    val zero = Number(0)

    val egraph = EGraph.empty[Arith]
    val (c, egraph2) = egraph.add(xMinusY)
    val (z, egraph3) = egraph2.add(zero)

    assert(egraph3.classes.size == 3)
    assert(c.args.size == 2)

    for (rule <- Seq(Rules.xMinusXEqualsZero, Rules.xMinusXEqualsZeroWithPatternVars)) {
      val egraph4 = rule(egraph3)

      assert(egraph4.classes.size == 3)
      assert(!egraph4.areSame(c, z))
    }
  }
}
