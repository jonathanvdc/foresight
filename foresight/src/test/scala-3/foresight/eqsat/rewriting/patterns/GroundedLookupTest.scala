package foresight.eqsat.rewriting.patterns

import foresight.eqsat.{EClassCall, EClassRef, ENode, ShapeCall, Slot}
import foresight.eqsat.{mutable, readonly}
import foresight.eqsat.lang.*
import scala.language.implicitConversions
import org.junit.Test
import org.junit.Assert._

class GroundedLookupTest {
  sealed trait Expr derives Language
  final case class Literal(value: String) extends Expr
  final case class F(lhs: Expr, rhs: Expr) extends Expr
  final case class Four(a: Expr, b: Expr, c: Expr, d: Expr) extends Expr
  final case class GNode(arg: Expr) extends Expr
  final case class H(arg: Expr) extends Expr
  final case class Missing(arg: Expr) extends Expr
  final case class Variable(slot: Use[Slot]) extends Expr
  final case class Ref(call: EClassCall) extends Expr derives Box
  final case class PatternVar(variable: Pattern.Var) extends Expr derives Box

  private val lang = summon[Language[Expr]]
  type Op = LanguageOp[Expr]
  type G = readonly.EGraph[Op]
  private def compiled(p: Expr, optimized: Boolean): CompiledPattern[Op, G] = {
    val tree = lang.toTree[Pattern.Var](p)
    CompiledPattern(tree, PatternCompiler.compile[Op, G](tree, groundedLookups = optimized))
  }

  extension (g: mutable.EGraph[Op])
    private def addExpr(expr: Expr): EClassCall = g.add(lang.toTree[EClassCall](expr))

  private class CountingGraph(g: G) extends readonly.EGraph[Op] {
    var candidates = 0
    var lookups = 0
    def canonicalizeOrNull(r: EClassRef): EClassCall = g.canonicalizeOrNull(r)
    def canonicalize(n: ENode[Op]): ShapeCall[Op] = g.canonicalize(n)
    def classes: Iterable[EClassRef] = g.classes
    def nodes(c: EClassCall): Iterable[ENode[Op]] = g.nodes(c)
    override def nodes(c: EClassCall, t: Op): Iterable[ENode[Op]] = new Iterable[ENode[Op]] {
      def iterator: Iterator[ENode[Op]] = g.nodes(c, t).iterator.map { n => candidates += 1; n }
    }
    def users(r: EClassRef): Iterable[ENode[Op]] = g.users(r)
    def findOrNull(n: ENode[Op]): EClassCall = { lookups += 1; g.findOrNull(n) }
    def areSame(a: EClassCall, b: EClassCall): Boolean = g.areSame(a, b)
  }

  @Test def avoidsEnumeratingGroundedAlternatives(): Unit = {
    val g = mutable.EGraph.empty[Op]
    val leaves = (0 until 100).map(i => g.addExpr(Literal("a" + i)))
    val hs = leaves.map(a => g.addExpr(H(Ref(a))))
    g.unionMany(hs.tail.map(h => (hs.head, h)))
    val gx = g.addExpr(GNode(Ref(leaves.head)))
    val root = g.addExpr(F(Ref(gx), Ref(hs.head)))
    val x = PatternVar(Pattern.Var.fresh())
    val pattern = F(GNode(x), H(x))
    val oldGraph = new CountingGraph(g)
    val newGraph = new CountingGraph(g)
    val expected = compiled(pattern, false).search(root, oldGraph).toSet
    val actual = compiled(pattern, true).search(root, newGraph).toSet
    assertEquals(expected, actual)
    assertEquals(1, actual.size)
    assertEquals(102, oldGraph.candidates)
    assertEquals(2, newGraph.candidates)
    assertEquals(1, newGraph.lookups)
    var streamed = 0
    compiled(pattern, true).search(root, g, (_, _) => { streamed += 1; false })
    assertEquals(1, streamed)
  }

  @Test def nestedConstantsMissingNodesAndWrongClasses(): Unit = {
    val g = mutable.EGraph.empty[Op]
    val a = g.addExpr(Literal("a"))
    val b = g.addExpr(Literal("b"))
    val ha = g.addExpr(H(Ref(a)))
    val hb = g.addExpr(H(Ref(b)))
    val root = g.addExpr(F(Ref(a), Ref(hb)))
    val x = PatternVar(Pattern.Var.fresh())
    val p = F(x, H(x))
    assertFalse(compiled(p, true).matches(root, g)) // h(a) exists, but in the wrong class
    assertFalse(compiled(F(x, Missing(x)), true).matches(root, g))
    g.unionMany(Seq((ha, hb)))
    assertEquals(compiled(p, false).search(root, g).toSet, compiled(p, true).search(root, g).toSet)
    assertTrue(compiled(p, true).matches(root, g))
    val constant = H(Literal("a"))
    assertTrue(compiled(constant, true).matches(ha, g))
    assertFalse(compiled(H(Literal("absent")), true).matches(ha, g))
    val (ir, ig) = lang.toEGraph(Literal("a"))
    assertTrue(compiled(Literal("a"), true).matches(ir, ig))
  }

  @Test def preservesRegistersAfterLookupAndReportsFailures(): Unit = {
    val g = mutable.EGraph.empty[Op]
    val a = g.addExpr(Literal("a"))
    val b = g.addExpr(Literal("b"))
    val ha = g.addExpr(H(Ref(a)))
    val gb = g.addExpr(GNode(Ref(b)))
    val hb = g.addExpr(H(Ref(b)))
    val root = g.addExpr(Four(Ref(a), Ref(ha), Ref(gb), Ref(hb)))
    val x = PatternVar(Pattern.Var.fresh())
    val y = PatternVar(Pattern.Var.fresh())
    val p = Four(x, H(x), GNode(y), H(y))
    val optimized = compiled(p, true)
    assertEquals(compiled(p, false).search(root, g).toSet, optimized.search(root, g).toSet)
    assertEquals(1, optimized.search(root, g).size)
    val failure = compiled(Literal("absent"), true)
    val machine = MutableMachineState[Op](a, Instruction.Effects.from(failure.instructions))
    val results = Machine.tryRun(g, machine, failure.instructions)
    assertEquals(1, results.size)
    results.head match {
      case MachineResult.Failure(_, _: MachineError.LookupFailed[?, ?], _) =>
      case other => fail("Expected a lookup failure, got " + other)
    }
  }

  @Test def slottedBindingsRetainStructuralMatching(): Unit = {
    val g = mutable.EGraph.empty[Op]
    val a = g.addExpr(Variable(Slot.fresh()))
    val h = g.addExpr(H(Ref(a)))
    val root = g.addExpr(F(Ref(a), Ref(h)))
    val x = PatternVar(Pattern.Var.fresh())
    val p = F(x, H(x))
    val counted = new CountingGraph(g)
    assertEquals(compiled(p, false).search(root, g).toSet, compiled(p, true).search(root, counted).toSet)
    assertTrue(compiled(p, true).matches(root, g))
    assertEquals(0, counted.lookups)
    val slotPattern = Variable(Slot.fresh())
    assertFalse(compiled(slotPattern, true).instructions.exists(_.isInstanceOf[Instruction.Lookup[?, ?]]))
  }
}
