package foresight.eqsat.examples.vector

import foresight.eqsat.lang.{Language, LanguageOp}
import foresight.eqsat.readonly.EGraph
import foresight.eqsat.rewriting.Rule
import foresight.eqsat.rewriting.patterns.PatternMatch

import scala.language.implicitConversions

/**
 * This object contains a collection of rules for rewriting arithmetic expressions.
 */
final case class Rules()(using L: Language[VectorArithExpr]) {
  type Op = LanguageOp[VectorArithExpr]
  type ArithEGraph = EGraph[ArithIR]
  type ArithRule = Rule[ArithIR, PatternMatch[ArithIR], ArithEGraph]

  import L.rule

  /**
   * Returns the idiom recognition rules.
   */
  def idioms: Seq[ArithRule] = Seq(
    recognizeFastInvSqrt,
    recognizeDotProduct
  )

  /**
   * Returns the arithmetic axioms as rewriting rules.
   */
  def arithAxioms: Seq[ArithRule] = Seq(
    // Ring axioms
//    addCommutativity,
    mulCommutativity,
//    addAssociativity1,
//    addAssociativity2,
    mulAssociativity1,
    mulAssociativity2,
//    distributivity1,
    distributivity2,
    multiplicativeIdentity,

    // Additional useful rules
    divMulDistributivity,
    divMulDistributivity2,
    mulDivDistributivity3,
//    mulDivDistributivity4,
    squareOfSqrt
  )

  /**
   * Returns the vector arithmetic axioms as rewriting rules.
   */
  def vectorAxioms: Seq[ArithRule] = Seq(
    mulVectorDistributivity,
    mulVectorDistributivity2,

    vectorExtractX,
    vectorExtractY,
    vectorExtractZ,
    vectorOfExtract
  )

  /**
   * Returns all the rules: idioms, arithmetic axioms, and vector axioms.
   */
  def all: Seq[ArithRule] = idioms ++ arithAxioms ++ vectorAxioms

  val recognizeFastInvSqrt: ArithRule =
    rule("recognize-fast-inv-sqrt") { x =>
      (FloatLiteral(1.0) / Sqrt(x)) -> FastInvSqrt(x)
    }

  val recognizeDotProduct: ArithRule =
    rule("recognize-dot-product") { (x1, y1, z1, x2, y2, z2) =>
      (x1 * x2 + y1 * y2 + z1 * z2) -> DotProduct(Vector3(x1, y1, z1), Vector3(x2, y2, z2))
    }

  val addCommutativity: ArithRule =
    rule("add-commutativity") { (x, y) =>
      (x + y) -> (y + x)
    }
  val mulCommutativity: ArithRule =
    rule("mul-commutativity") { (x, y) =>
      (x * y) -> (y * x)
    }
  val addAssociativity1: ArithRule =
    rule("add-associativity1") { (x, y, z) =>
      ((x + y) + z) -> (x + (y + z))
    }
  val addAssociativity2: ArithRule =
    rule("add-associativity2") { (x, y, z) =>
      (x + (y + z)) -> ((x + y) + z)
    }
  val mulAssociativity1: ArithRule =
    rule("mul-associativity1") { (x, y, z) =>
      ((x * y) * z) -> (x * (y * z))
    }
  val mulAssociativity2: ArithRule =
    rule("mul-associativity2") { (x, y, z) =>
      (x * (y * z)) -> ((x * y) * z)
    }
  val distributivity1: ArithRule =
    rule("distributivity1") { (x, a, b) =>
      (x * (a + b)) -> ((x * a) + (x * b))
    }
  val distributivity2: ArithRule =
    rule("distributivity2") { (x, a, b) =>
      ((x * a) + (x * b)) -> (x * (a + b))
    }
  val multiplicativeIdentity: ArithRule = {
    rule("multiplicative-identity") { x =>
      (x * FloatLiteral(1.0)) -> x
    }
  }

  val divMulDistributivity: ArithRule = {
    rule("div-mul-distributivity") { (x, y, z) =>
      ((x * y) / z) -> (x * (y / z))
    }
  }
  val divMulDistributivity2: ArithRule = divMulDistributivity.tryReverse.get

  val mulDivDistributivity3: ArithRule = {
    rule("mul-div-distributivity") { (x, y, z, w) =>
      ((x / y) * (z / w)) -> ((x * z) / (y * w))
    }
  }
  val mulDivDistributivity4: ArithRule = mulDivDistributivity3.tryReverse.get

  val squareOfSqrt: ArithRule = {
    rule("square-of-sqrt") { x =>
      (Sqrt(x) * Sqrt(x)) -> x
    }
  }

  val mulVectorDistributivity: ArithRule = {
    rule("mul-vector-distributivity") { (x, y, z, w) =>
      Vector3(x * w, y * w, z * w) -> (Vector3(x, y, z) * w)
    }
  }
  val mulVectorDistributivity2: ArithRule = mulVectorDistributivity.tryReverse.get

  val vectorExtractX: ArithRule = {
    rule("vector-extract-x") { (x, y, z) =>
      ElementAt(Vector3(x, y, z), 0) -> x
    }
  }
  val vectorExtractY: ArithRule = {
    rule("vector-extract-y") { (x, y, z) =>
      ElementAt(Vector3(x, y, z), 1) -> y
    }
  }
  val vectorExtractZ: ArithRule = {
    rule("vector-extract-z") { (x, y, z) =>
      ElementAt(Vector3(x, y, z), 2) -> z
    }
  }
  val vectorOfExtract: ArithRule = {
    rule("vector-extract-and-reconstruct") { (v) =>
      Vector3(
        ElementAt(v, 0),
        ElementAt(v, 1),
        ElementAt(v, 2)
      ) -> v
    }
  }
}
