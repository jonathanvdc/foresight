package foresight.eqsat.examples.incremental

import foresight.eqsat.examples.poly._

import scala.util.Random

object RandomArithExprGen {
  private val vars = Seq("x", "y", "z", "a", "b", "c")

  def randomExpr(maxDepth: Int, rng: Random = new Random()): ArithExpr = {
    if (maxDepth <= 0) {
      // base case: pick a leaf
      rng.nextInt(3) match {
        case 0 => Zero
        case 1 => Succ(Zero) // smallest non-zero Peano number
        case 2 => Var(vars(rng.nextInt(vars.length)))
      }
    } else {
      rng.nextInt(6) match {
        case 0 => Zero
        case 1 => Succ(randomExpr(maxDepth - 1, rng))
        case 2 => Var(vars(rng.nextInt(vars.length)))
        case 3 => Add(randomExpr(maxDepth - 1, rng), randomExpr(maxDepth - 1, rng))
        case 4 => Mul(randomExpr(maxDepth - 1, rng), randomExpr(maxDepth - 1, rng))
        case 5 => Pow(randomExpr(maxDepth - 1, rng), randomExpr(maxDepth - 1, rng))
      }
    }
  }

  /** Generates N random terms with varying depth (1..maxDepth). */
  def randomTerms(n: Int, maxDepth: Int, seed: Long = 0): Seq[ArithExpr] = {
    val rng = new Random(seed)
    (1 to n).map(_ => randomExpr(rng.nextInt(maxDepth) + 1, rng))
  }
}
