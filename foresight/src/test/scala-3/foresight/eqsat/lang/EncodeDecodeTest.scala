package foresight.eqsat.lang

import foresight.eqsat.{EClassCall, MixedTree, Slot}
import foresight.eqsat.immutable.EGraph
import org.junit.Test
import scala.language.implicitConversions

class EncodeDecodeTest {
  sealed trait ArithExpr derives Language
  final case class Var(slot: Use[Slot]) extends ArithExpr
  final case class Lam(param: Def[Slot], body: ArithExpr) extends ArithExpr
  final case class App(fun: ArithExpr, arg: ArithExpr) extends ArithExpr
  final case class Add(lhs: ArithExpr, rhs: ArithExpr) extends ArithExpr
  final case class Mul(lhs: ArithExpr, rhs: ArithExpr) extends ArithExpr
  final case class Number(value: BigInt) extends ArithExpr

  final case class Ref(eClass: EClassCall) extends ArithExpr derives Box

  // Implicit conversion: Int => ArithExpr (Number)
  given Conversion[Int, ArithExpr] with
    def apply(n: Int): ArithExpr = Number(BigInt(n))

  // Operator overloads on ArithExpr
  extension (lhs: ArithExpr)
    infix def +(rhs: ArithExpr): ArithExpr = Add(lhs, rhs)
    infix def *(rhs: ArithExpr): ArithExpr = Mul(lhs, rhs)

  val (classCall, _) = EGraph.from[Int](MixedTree.unslotted(0, Seq()))

  @Test
  def asAtomWorks(): Unit = {
    val atom = Ref(classCall)
    assert(summon[AsAtom[Ref, EClassCall]].toAtom(atom) == classCall)
    assert(summon[AsAtom[Ref, EClassCall]].fromAtom(classCall) == atom)
  }

  @Test
  def atomDecoderWorks(): Unit = {
    val decoder = summon[AtomDecoder[ArithExpr, EClassCall]]
    assert(AtomDecoder.decode(decoder, classCall).contains(Ref(classCall)))
  }

  @Test
  def languageWorks(): Unit = {
    val lang = summon[Language[ArithExpr]]
    assert(lang.toTree[EClassCall](Ref(classCall)) == MixedTree.Atom(classCall))
    assert(lang.fromTree[EClassCall](MixedTree.Atom(classCall)) == Ref(classCall))
  }

  @Test
  def languageRoundTripsTree(): Unit = {
    val L = summon[Language[ArithExpr]]
    val x = Slot.numeric(0)
    val expr = Lam(Def(x), Var(Use(x)) + 3 + Ref(classCall))
    assert(expr == L.fromTree[EClassCall](L.toTree[EClassCall](expr)))
  }
}
