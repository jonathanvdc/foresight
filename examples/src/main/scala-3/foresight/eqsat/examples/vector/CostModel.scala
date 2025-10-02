package foresight.eqsat.examples.vector

import foresight.eqsat.lang.LanguageCostFunction

object CostModel extends LanguageCostFunction[VectorArithExpr, TypeAndCost] {
  override def apply(expr: VectorArithExpr): TypeAndCost = {
    expr match {
      case Add(lhs, rhs) => binary(lhs, rhs, 10)
      case Mul(lhs, rhs) => binary(lhs, rhs, 50)
      case Div(lhs, rhs) => binary(lhs, rhs, 200)

      case Sqrt(arg) => unary(arg, 1000)
      case FastInvSqrt(arg) => unary(arg, 1100)

      case Vector3(x, y, z) =>
        val TypeAndCost(tx, cx) = apply(x)
        val TypeAndCost(ty, cy) = apply(y)
        val TypeAndCost(tz, cz) = apply(z)
        assert(tx == ty && ty == tz)
        TypeAndCost(Type.Vector3Type(tx), cx + cy + cz + 0)

      case ElementAt(v, index) =>
        val TypeAndCost(tv, cv) = apply(v)
        val t = tv match {
          case Type.Vector3Type(elemType) => elemType
          case _ => throw new RuntimeException(s"ElementAt applied to non-vector type: $tv")
        }
        TypeAndCost(t, cv + 1)

      case FloatLiteral(value) => TypeAndCost(Type.FloatType, 1)
      case Var(name, t) => TypeAndCost(t, 1)
      case Fact(typeAndCost: TypeAndCost) => typeAndCost
    }
  }

  private def unary(arg: VectorArithExpr, baseCost: Int): TypeAndCost = {
    val TypeAndCost(t, c) = apply(arg)
    TypeAndCost(t, c + baseCost * t.scalarCount)
  }

  private def binary(lhs: VectorArithExpr, rhs: VectorArithExpr, baseCost: Int): TypeAndCost = {
    val TypeAndCost(tx, cx) = apply(lhs)
    val TypeAndCost(ty, cy) = apply(rhs)
    val t = tx.broadcast(ty).get
    TypeAndCost(t, cx + cy + baseCost * t.scalarCount)
  }
}
