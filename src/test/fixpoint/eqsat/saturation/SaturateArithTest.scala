package fixpoint.eqsat.saturation

import fixpoint.eqsat.rewriting.Rule
import fixpoint.eqsat.{EClassCall, EGraph, ENode, MixedTree, Slot}
import fixpoint.eqsat.rewriting.patterns.{Pattern, PatternMatch, SlotVar}
import org.junit.Test

import scala.language.implicitConversions

class SaturateArithTest {
  // Define a language for arithmetic expressions involving variable references, addition, multiplication, and integer
  // literals.
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
  object Mul extends Arith {
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

  // Define a set of rules for transforming arithmetic expressions.
  object Rules {
    def all: Seq[Rule[Arith, PatternMatch[Arith], EGraph[Arith]]] = Seq(
      addCommutativity,
      addAssociativity1,
      addAssociativity2,
      mulCommutativity,
      mulAssociativity1,
      mulAssociativity2,
      distributivity1,
      distributivity2)

    def simpleStrategy: Strategy[EGraph[Arith], Unit] = MaximalRuleApplication(all).untilFixpoint
    def cachingStrategy: Strategy[EGraph[Arith], Unit] = MaximalRuleApplicationWithCaching(all).untilFixpoint.recordApplications

    def strategies: Seq[Strategy[EGraph[Arith], Unit]] = Seq(simpleStrategy, cachingStrategy)

    val addCommutativity: Rule[Arith, PatternMatch[Arith], EGraph[Arith]] = {
      val x = Pattern.Var.fresh[Arith]()
      val y = Pattern.Var.fresh[Arith]()
      Rule(
        "x + y = y + x",
        Add(x, y).toSearcher,
        Add(y, x).toApplier)
    }

    val addAssociativity1: Rule[Arith, PatternMatch[Arith], EGraph[Arith]] = {
      val x = Pattern.Var.fresh[Arith]()
      val y = Pattern.Var.fresh[Arith]()
      val z = Pattern.Var.fresh[Arith]()
      Rule(
        "(x + y) + z = x + (y + z)",
        Add(Add(x, y), z).toSearcher,
        Add(x, Add(y, z)).toApplier)
    }

    val addAssociativity2: Rule[Arith, PatternMatch[Arith], EGraph[Arith]] = {
      val x = Pattern.Var.fresh[Arith]()
      val y = Pattern.Var.fresh[Arith]()
      val z = Pattern.Var.fresh[Arith]()
      Rule(
        "x + (y + z) = (x + y) + z",
        Add(x, Add(y, z)).toSearcher,
        Add(Add(x, y), z).toApplier)
    }

    val mulCommutativity: Rule[Arith, PatternMatch[Arith], EGraph[Arith]] = {
      val x = Pattern.Var.fresh[Arith]()
      val y = Pattern.Var.fresh[Arith]()
      Rule(
        "x * y = y * x",
        Mul(x, y).toSearcher,
        Mul(y, x).toApplier)
    }

    val mulAssociativity1: Rule[Arith, PatternMatch[Arith], EGraph[Arith]] = {
      val x = Pattern.Var.fresh[Arith]()
      val y = Pattern.Var.fresh[Arith]()
      val z = Pattern.Var.fresh[Arith]()
      Rule(
        "(x * y) * z = x * (y * z)",
        Mul(Mul(x, y), z).toSearcher,
        Mul(x, Mul(y, z)).toApplier)
    }

    val mulAssociativity2: Rule[Arith, PatternMatch[Arith], EGraph[Arith]] = {
      val x = Pattern.Var.fresh[Arith]()
      val y = Pattern.Var.fresh[Arith]()
      val z = Pattern.Var.fresh[Arith]()
      Rule(
        "x * (y * z) = (x * y) * z",
        Mul(x, Mul(y, z)).toSearcher,
        Mul(Mul(x, y), z).toApplier)
    }

    val distributivity1: Rule[Arith, PatternMatch[Arith], EGraph[Arith]] = {
      val x = Pattern.Var.fresh[Arith]()
      val y = Pattern.Var.fresh[Arith]()
      val z = Pattern.Var.fresh[Arith]()
      Rule(
        "x * (y + z) = x * y + x * z",
        Mul(x, Add(y, z)).toSearcher,
        Add(Mul(x, y), Mul(x, z)).toApplier)
    }

    val distributivity2: Rule[Arith, PatternMatch[Arith], EGraph[Arith]] = {
      val a = Pattern.Var.fresh[Arith]()
      val b = Pattern.Var.fresh[Arith]()
      val c = Pattern.Var.fresh[Arith]()
      Rule(
        "a * b + a * c = a * (b + c)",
        Add(Mul(a, b), Mul(a, c)).toSearcher,
        Mul(a, Add(b, c)).toApplier)
    }
  }

  /**
   * Test that addition is commutative.
   */
  @Test
  def additionIsAssociative(): Unit = {
    // x + y = y + x
    val x = Var(Slot.fresh())
    val y = Var(Slot.fresh())
    val xPlusY = Add(x, y)
    val yPlusX = Add(y, x)

    val egraph = EGraph.empty[Arith]
    val (c1, egraph2) = egraph.add(xPlusY)
    val (c2, egraph3) = egraph2.add(yPlusX)

    assert(!egraph3.areSame(c1, c2))

    for (strategy <- Rules.strategies) {
      val Some(egraph4) = strategy(egraph3)
      assert(egraph4.areSame(c1, c2))
    }
  }

  @Test
  def additionIsAssociative2(): Unit = {
    // (x + y) * (x + y) = (x + y) * (y + x)
    val x = Var(Slot.fresh())
    val y = Var(Slot.fresh())
    val xPlusY = Add(x, y)
    val yPlusX = Add(y, x)
    val xPlusYTimesXPlusY = Mul(xPlusY, xPlusY)
    val xPlusYTimesYPlusX = Mul(xPlusY, yPlusX)

    val egraph = EGraph.empty[Arith]
    val (c1, egraph2) = egraph.add(xPlusYTimesXPlusY)
    val (c2, egraph3) = egraph2.add(xPlusYTimesYPlusX)

    assert(!egraph3.areSame(c1, c2))

    for (strategy <- Rules.strategies) {
      val Some(egraph4) = strategy(egraph3)
      assert(egraph4.areSame(c1, c2))
    }
  }

  @Test
  def distributivity(): Unit = {
    // (x+y) * (y+z) = (z+y) * (y+x)
    val x = Var(Slot.fresh())
    val y = Var(Slot.fresh())
    val z = Var(Slot.fresh())
    val xPlusY = Add(x, y)
    val yPlusZ = Add(y, z)
    val zPlusY = Add(z, y)
    val yPlusX = Add(y, x)
    val xPlusYTimesYPlusZ = Mul(xPlusY, yPlusZ)
    val zPlusYTimesYPlusX = Mul(zPlusY, yPlusX)

    val egraph = EGraph.empty[Arith]
    val (c1, egraph2) = egraph.add(xPlusYTimesYPlusZ)
    val (c2, egraph3) = egraph2.add(zPlusYTimesYPlusX)

    assert(!egraph3.areSame(c1, c2))

    for (strategy <- Rules.strategies) {
      val Some(egraph4) = strategy(egraph3)
      assert(egraph4.areSame(c1, c2))
    }
  }

  @Test
  def squareOfSum(): Unit = {
    // (x+y)**2 = x**2 + x*y + x*y + y**2
    val x = Var(Slot.fresh())
    val y = Var(Slot.fresh())
    val lhs = Mul(Add(x, y), Add(x, y))
    val rhs = Add(Mul(x, x), Add(Mul(x, y), Add(Mul(y, x), Mul(y, y))))

    val egraph = EGraph.empty[Arith]
    val (c1, egraph2) = egraph.add(lhs)
    val (c2, egraph3) = egraph2.add(rhs)

    assert(!egraph3.areSame(c1, c2))

    for (strategy <- Rules.strategies) {
      val Some(egraph4) = strategy(egraph3)
      assert(egraph4.areSame(c1, c2))
    }
  }

  @Test
  def distributivity2(): Unit = {
    // z*(x+y) = z*(y+x)
    val x = Var(Slot.fresh())
    val y = Var(Slot.fresh())
    val z = Var(Slot.fresh())
    val lhs = Mul(z, Add(x, y))
    val rhs = Mul(z, Add(y, x))

    val egraph = EGraph.empty[Arith]
    val (c1, egraph2) = egraph.add(lhs)
    val (c2, egraph3) = egraph2.add(rhs)

    assert(!egraph3.areSame(c1, c2))

    for (strategy <- Rules.strategies) {
      val Some(egraph4) = strategy(egraph3)
      assert(egraph4.areSame(c1, c2))
    }
  }

  def addChain(slots: Seq[Slot]): MixedTree[Arith, EClassCall] = {
    slots.map(s => MixedTree.fromENode(Var(s))).reduceLeft(Add(_, _))
  }

  @Test
  def rearrangeChains(): Unit = {
    // x0+...+xN = xN+...+x0
    val N = 5

    val slots = (0 to N).map(_ => Slot.fresh())

    val a = addChain(slots)
    val b = addChain(slots.reverse)

    val egraph = EGraph.empty[Arith]
    val (c1, egraph2) = egraph.add(a)
    val (c2, egraph3) = egraph2.add(b)

    assert(!egraph3.areSame(c1, c2))

    for (strategy <- Rules.strategies) {
      val Some(egraph4) = strategy(egraph3)
      assert(egraph4.areSame(c1, c2))
    }
  }
}
