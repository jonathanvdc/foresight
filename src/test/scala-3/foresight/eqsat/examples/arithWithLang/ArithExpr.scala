package foresight.eqsat.examples.arithWithLang

import foresight.eqsat.Slot
import foresight.eqsat.lang.{AnalysisBox, Atom, Def, Language, LanguageOp, Use}
import foresight.eqsat.rewriting.patterns.Pattern

sealed trait ArithExpr derives Language
final case class Var(slot: Use[Slot]) extends ArithExpr
final case class Lam(param: Def[Slot], body: ArithExpr) extends ArithExpr
final case class App(fun: ArithExpr, arg: ArithExpr) extends ArithExpr
final case class Add(lhs: ArithExpr, rhs: ArithExpr) extends ArithExpr
final case class Mul(lhs: ArithExpr, rhs: ArithExpr) extends ArithExpr
final case class Number(value: BigInt) extends ArithExpr

final case class PatternVar(variable: Pattern.Var) extends ArithExpr derives Atom
object PatternVar {
  def fresh(): PatternVar = PatternVar(Pattern.Var.fresh())
}

final case class Fact[A](value: A) extends ArithExpr
object ArithExpr {
  given AnalysisBox[ArithExpr] with
    type Box[A] = Fact[A]
    def box[A](a: A): Fact[A] = Fact(a)
}

// Implicit conversion: Int => ArithExpr (Number)
given Conversion[Int, ArithExpr] with
  def apply(n: Int): ArithExpr = Number(BigInt(n))

// Operator overloads on ArithExpr
extension (lhs: ArithExpr)
  infix def +(rhs: ArithExpr): ArithExpr = Add(lhs, rhs)
  infix def *(rhs: ArithExpr): ArithExpr = Mul(lhs, rhs)

type ArithIR = LanguageOp[ArithExpr]
