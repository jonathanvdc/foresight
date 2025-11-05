package foresight.eqsat.examples.incremental

import foresight.eqsat.examples.poly._

import scala.util.Random

object RandomArithExprGen {
  private val defaultReuseProb = 0.35
  private val vars = Seq("x") //, "y", "z", "a", "b", "c")

  /** Height (max depth) of a tree where leaves have height 0. Unknown nodes default to 0. */
  private def height(e: ArithExpr): Int = e match {
    case Zero         => 0
    case Var(_)       => 0
    case Succ(p)      => 1 + height(p)
    case Add(l, r)    => 1 + math.max(height(l), height(r))
    case Mul(l, r)    => 1 + math.max(height(l), height(r))
    case Pow(b, e2)   => 1 + math.max(height(b), height(e2))
    case _            => 0 // tolerate other node kinds from the broader IR
  }

  /** Collect all subexpressions of a term (including the term itself). Unknown nodes are included as-is. */
  private def subexprs(e: ArithExpr): Vector[ArithExpr] = e match {
    case z @ Zero         => Vector(z)
    case v @ Var(_)       => Vector(v)
    case s @ Succ(p)      => subexprs(p) :+ s
    case a @ Add(l, r)    => (subexprs(l) ++ subexprs(r)) :+ a
    case m @ Mul(l, r)    => (subexprs(l) ++ subexprs(r)) :+ m
    case p @ Pow(b, e2)   => (subexprs(b) ++ subexprs(e2)) :+ p
    case other            => Vector(other)
  }

  /** Build k nested successors ending in Zero. Requires k >= 1. */
  private def peano(k: Int): ArithExpr = {
    var r: ArithExpr = Zero
    var i = 0
    while (i < k) { r = Succ(r); i += 1 }
    r
  }

  /** Leaf generator: Zero or a variable. */
  private def leaf(rng: Random): ArithExpr =
    if (rng.nextBoolean()) Var(vars(rng.nextInt(vars.length))) else Zero

  /** True iff this node is a leaf by the user's policy (Zero/Var only). */
  private def isLeafPolicy(e: ArithExpr): Boolean = e match {
    case Zero   => true
    case Var(_) => true
    case _      => false
  }

  // ===== Pool as buckets by height for O(1) eligibility filtering =====

  /** A height-bucketed pool: index = height, value = bucket of subtrees of that height. */
  private final class HeightPool {
    // buckets(h) holds subtrees with height h
    private val buckets = scala.collection.mutable.ArrayBuffer.empty[scala.collection.mutable.ArrayBuffer[ArithExpr]]
    private var totalSize: Int = 0

    private def ensure(h: Int): Unit = {
      var i = buckets.length
      while (i <= h) { buckets += scala.collection.mutable.ArrayBuffer.empty[ArithExpr]; i += 1 }
    }

    def add(e: ArithExpr, h: Int): Unit = {
      ensure(h)
      buckets(h) += e
      totalSize += 1
    }

    def addAll(es: Iterable[(ArithExpr, Int)]): Unit = {
      es.foreach { case (e, h) => add(e, h) }
    }

    /** Pick a random subtree with height in [minH, maxH], weighted by bucket sizes. Returns None if empty. */
    def pick(rng: Random, minH: Int, maxH: Int): Option[ArithExpr] = {
      val hi = math.min(maxH, buckets.length - 1)
      if (hi < minH) return None
      // Compute total eligible size
      var sum = 0
      var h   = minH
      while (h <= hi) { sum += buckets(h).length; h += 1 }
      if (sum == 0) return None
      // Weighted choice
      var k = rng.nextInt(sum)
      h = minH
      while (h <= hi) {
        val b = buckets(h)
        if (k < b.length) return Some(b(k))
        k -= b.length
        h += 1
      }
      None
    }

    def maxHeight: Int = buckets.length - 1
    def nonEmpty: Boolean = totalSize > 0
  }

  /**
   * Generate a random expression with a strict remaining depth budget d.
   * Invariants requested:
   *  - Zero and Var only appear when d == 0 (at leaves).
   *  - Reused subtrees from the pool must fit in the remaining depth.
   *  - Bias towards Peano numerals (nested Succ) over large operator trees.
   */
  private def randomExprBiased(
      maxDepth: Int,
      rng: Random,
      pool: HeightPool,
      reuseProb: Double
  ): ArithExpr = {
    require(maxDepth >= 0)

    def go(d: Int): ArithExpr = {
      // Try to reuse from the pool, but only subtrees whose height <= d.
      // Also enforce the leaf policy: if d > 0, don't reuse Zero/Var (height 0 bucket).
      if (pool.nonEmpty && rng.nextDouble() < reuseProb) {
        val minH = if (d == 0) 0 else 1
        pool.pick(rng, minH, d) match {
          case Some(cand) => return cand
          case None       => // fall through to fresh generation
        }
      }

      if (d <= 0) {
        // Leaf layer: only Zero or Var allowed
        leaf(rng)
      } else {
        // Internal layer: structural constructors + occasional direct Peano collapse
        rng.nextInt(7) match {
          case 0 => Succ(go(d - 1))
          case 1 => Add(go(d - 1), go(d - 1))
          case 2 => Mul(go(d - 1), go(d - 1))
          case 3 => Pow(go(d - 1), go(d - 1))
          case _ => peano(rng.nextInt(d) + 1) // collapse into a numeral of height <= d
        }
      }
    }

    go(maxDepth)
  }

  /**
   * Generate N random terms with varying depth (1..maxDepth),
   * reusing previously seen subtrees when possible while respecting depth budgets.
   */
  def randomTerms(
      n: Int,
      maxDepth: Int,
      seed: Long = 0,
      reuseProb: Double = defaultReuseProb
  ): Seq[ArithExpr] = {
    val rng  = new Random(seed)
    val pool = new HeightPool

    (1 to n).map { _ =>
      val term = randomExprBiased(rng.nextInt(maxDepth) + 1, rng, pool, reuseProb)
      // Add all subtrees paired with their heights; enables depth-safe, O(#heights) reuse.
      pool.addAll(subexprs(term).map(e => e -> height(e)))
      term
    }
  }
}
