# Foresight

![Build](https://github.com/jonathanvdc/foresight/actions/workflows/ci.yml/badge.svg)
![Scala](https://img.shields.io/badge/scala-2.11%20|%202.13%20|%203-red?logo=scala)
![License](https://img.shields.io/github/license/jonathanvdc/foresight)
[![Docs](https://img.shields.io/badge/docs-latest-blue)](https://jonathanvdc.github.io/foresight/latest/api)
![GitHub package](https://img.shields.io/badge/GitHub%20Packages-published-blue?logo=github)

Foresight is a Scala library for **equality saturation**, a technique that enables compilers and program transformers to explore many semantically equivalent rewrites of a program simultaneously.
By separating the application of rewrite rules from the decision‑making process, equality saturation allows for more principled and comprehensive optimization.

Whereas existing equality saturation frameworks such as [egg](https://egraphs-good.github.io/egg/) rely on sequential rule execution, Foresight introduces a new architectural model built around **parallelism**, **flexibility**, and **extensibility**.
It offers both immutable (purely functional) and mutable e‑graph implementations, making it well‑suited to a wide range of compiler infrastructure scenarios.

Foresight's architecture includes slotted e‑graph representations with native support for bound variables, a parallel rewriting pipeline, a modular strategy interface, and a suite of built‑in saturation strategies.
These innovations enable safe speculative rewriting, efficient parallel execution, and flexible rewrite control.

---

## Table of Contents

- [Getting Started](#getting-started)
- [Key Concepts](#key-concepts)
- [Features](#features)
- [More Resources](#more-resources)
- [Citing Foresight](#citing-foresight)

---

## Getting Started

### Installation

Foresight is published to [GitHub Packages](https://github.com/jonathanvdc/foresight/packages).
Add the resolver and the dependency to your `build.sbt`:

```scala
resolvers += "GitHub Packages" at "https://maven.pkg.github.com/jonathanvdc/foresight"

libraryDependencies += "com.github.jonathanvdc" %% "foresight" % "0.1.0"
```

> **Note:** GitHub Packages requires authentication even for public packages.
> Add your GitHub username and a personal access token (with `read:packages` scope) to
> `~/.sbt/1.0/credentials` using the following format:
>
> ```
> realm=GitHub Package Registry
> host=maven.pkg.github.com
> user=<your-github-username>
> password=<your-personal-access-token>
> ```
>
> Alternatively, set the `GITHUB_ACTOR` / `GITHUB_TOKEN` environment variables before
> resolving. See the [sbt credentials documentation](https://www.scala-sbt.org/1.x/docs/Publishing.html#Credentials)
> for more details.

### Basic Usage (Scala 3)

The quickest way to get started is to define your language as a `sealed trait` that
`derives Language`. This gives you automatic encoding/decoding, a pattern‑matching DSL,
and concise rule helpers.

**Step 1 – Define your language**

```scala
import foresight.eqsat.lang.{Language, Def, Use}

sealed trait Expr derives Language

final case class Num(value: BigInt)        extends Expr
final case class Add(lhs: Expr, rhs: Expr) extends Expr
final case class Mul(lhs: Expr, rhs: Expr) extends Expr

// Convenient surface syntax
given Conversion[Int, Expr] = n => Num(BigInt(n))
extension (x: Expr)
  def +(y: Expr): Expr = Add(x, y)
  def *(y: Expr): Expr = Mul(x, y)
```

**Step 2 – Define rewrite rules**

```scala
import foresight.eqsat.lang.Language

val L = summon[Language[Expr]]

val addCommutativity = L.rule("add-commutativity") { (x, y) => (x + y) -> (y + x) }
val mulCommutativity = L.rule("mul-commutativity") { (x, y) => (x * y) -> (y * x) }
val distributivity   = L.rule("distributivity")   { (x, y, z) =>
  (x * (y + z)) -> ((x * y) + (x * z))
}
```

**Step 3 – Build an e‑graph and saturate**

```scala
import foresight.eqsat.immutable.EGraph
import foresight.eqsat.saturation.MaximalRuleApplicationWithCaching
import foresight.eqsat.extraction.ExtractionAnalysis

val rules = Seq(addCommutativity, mulCommutativity, distributivity)

// Encode the expression into a fresh e-graph.
val expr = (2: Expr) * ((3: Expr) + (4: Expr))
val (root, egraph) = L.toEGraph(expr)

// Build a saturation strategy: apply all rules until stable (or 100 iterations).
val strategy =
  MaximalRuleApplicationWithCaching(rules)
    .withIterationLimit(100)
    .repeatUntilStable
    .addAnalyses(ExtractionAnalysis.smallest)
    .closeMetadata
    .dropData

val saturated = strategy(egraph).getOrElse(egraph)
```

**Step 4 – Extract the best result**

```scala
import foresight.eqsat.lang.LanguageCostFunction

// A simple cost function: literals cost 1; Add costs 2 + children; Mul costs 4 + children.
val cost = new LanguageCostFunction[Expr, Int] {
  import foresight.eqsat.lang.AnalysisFact as Fact
  override def apply(expr: Expr): Int = expr match {
    case Num(_)                          => 1
    case Add(Fact(l: Int), Fact(r: Int)) => 2 + l + r
    case Mul(Fact(l: Int), Fact(r: Int)) => 4 + l + r
    case _                               => Int.MaxValue
  }
}

val best: Expr = saturated.extract(root, cost)
```

### Basic Usage (Scala 2 / 3 without derivation)

If you are on Scala 2, or prefer explicit control, you can define node types as a plain
`sealed trait` and build rules from lower‑level primitives:

```scala
import foresight.eqsat.{MixedTree, Slot}
import foresight.eqsat.rewriting.Rule
import foresight.eqsat.rewriting.patterns.Pattern

// In the lower-level API, operators are represented as singleton objects; the e-graph
// stores the operator tag and child e-classes separately, rather than as fields.
sealed trait Expr
object Add extends Expr
object Mul extends Expr
final case class Num(value: BigInt) extends Expr

// Pattern variables stand for arbitrary sub-expressions.
val x = MixedTree.Atom(Pattern.Var.fresh())
val y = MixedTree.Atom(Pattern.Var.fresh())

val addCommutativity = Rule(
  "add-commutativity",
  Add(x, y).toSearcher,
  Add(y, x).toApplier
)
```

The saturation loop looks the same as in the Scala 3 example: create an `EGraph`, add
expressions with `.add(...)`, pick a strategy, call `strategy(egraph)`, and inspect the
result.

For a fully worked example using these lower‑level primitives, see
[`examples/src/main/scala/foresight/eqsat/examples/arith`](examples/src/main/scala/foresight/eqsat/examples/arith).

---

## Key Concepts

### E‑Graphs and E‑Classes

An **e‑graph** is a compact data structure that represents many equivalent programs at
once. Expressions are grouped into **e‑classes**: every expression inside the same
e‑class is considered semantically equal. Applying a rewrite rule *merges* two
e‑classes, recording a new equality without discarding the original expression.

Foresight provides two interchangeable e‑graph flavors:

| Flavor | Import | When to use |
|---|---|---|
| Immutable | `foresight.eqsat.immutable.EGraph` | Thread‑safe, speculative or parallel execution, simpler reasoning |
| Mutable | `foresight.eqsat.mutable.EGraph` | Lower latency for sequential saturation workloads |

Both implement the same interface and work with all built‑in strategies and analyses.

### Rewrite Rules

A `Rule` pairs a **searcher** (a pattern that matches sub‑expressions) with an
**applier** (which records new equalities when a match is found). Rules are always
additive—they never delete information.

The high‑level `Language.rule` DSL (Scala 3) lets you write rules as ordinary Scala
expressions:

```scala
val addCommutativity = L.rule("add-commutativity") { (x, y) => (x + y) -> (y + x) }
```

For full control you can build rules from lower‑level primitives: `Pattern`,
`MixedTree`, custom `Searcher`, and custom `Applier`.

### Saturation Strategies

**Equality saturation** means applying rules repeatedly until no new equalities are
discovered, or until a budget is exhausted. Foresight decouples the rule‑application
*mechanism* from the *control policy* through the `Strategy` trait.

Built‑in strategies:

| Strategy | Description |
|---|---|
| `MaximalRuleApplication` | Applies every match of every rule once per iteration. Exhaustive and deterministic. |
| `MaximalRuleApplicationWithCaching` | Same as above, but skips matches already applied in a previous iteration. |
| `BackoffRuleApplication` | Assigns each rule a match quota and a cooldown period, preventing any single rule from dominating the search. |
| `StochasticRuleApplication` | Applies a randomly sampled batch of matches each iteration, guided by user‑defined `MatchPriorities`. |

Strategies compose via built‑in combinators:

```scala
MaximalRuleApplicationWithCaching(rules)
  .withIterationLimit(100)   // cap at 100 iterations
  .withTimeout(30.seconds)   // or 30 seconds wall time
  .repeatUntilStable         // loop until no new equalities are found
```

You can also chain strategies, add logging between iterations, or insert a rebasing step
that extracts the best term and restarts from a smaller e‑graph:

```scala
MaximalRuleApplication(fastRules)
  .repeatUntilStable
  .thenRebase(extractor)     // extract best term and restart
  .withIterationLimit(5)
  .repeatUntilStable
```

### Metadata and Analyses

**Metadata** lets you attach domain‑specific information—types, constant values, cost
estimates—to e‑classes. It is updated incrementally as the e‑graph evolves and remains
consistent across merges.

An **analysis** implements `Analysis[NodeT, A]` with three methods:

| Method | Role |
|---|---|
| `make(node, defs, uses, args)` | Computes a fact `A` for a single e‑node given the facts of its children. |
| `join(left, right)` | Merges two facts when e‑classes are unioned (must form a join‑semilattice). |
| `rename(result, renaming)` | Adjusts slot references when an e‑class is renamed. |

The built‑in `ExtractionAnalysis.smallest` tracks the smallest representative term in
each e‑class and is used by cost‑guided extraction.

A custom constant‑folding analysis looks like this:

```scala
import foresight.eqsat.metadata.Analysis
import foresight.eqsat.collections.SlotMap
import foresight.eqsat.Slot

object ConstantAnalysis extends Analysis[Expr, Option[BigInt]] {
  override def name = "ConstantAnalysis"
  override def rename(result: Option[BigInt], renaming: SlotMap) = result
  override def make(node: Expr, defs: Seq[Slot], uses: Seq[Slot],
                    args: Seq[Option[BigInt]]): Option[BigInt] =
    (node, args) match {
      case (Num(v), Seq())               => Some(v)
      case (Add,    Seq(Some(l), Some(r))) => Some(l + r)
      case (Mul,    Seq(Some(l), Some(r))) => Some(l * r)
      case _                             => None
    }
  override def join(left: Option[BigInt], right: Option[BigInt]) =
    (left, right) match {
      case (Some(l), Some(r)) => assert(l == r); Some(l)
      case (Some(l), None)    => Some(l)
      case (None,    Some(r)) => Some(r)
      case _                  => None
    }
}
```

Analyses are attached to a strategy with `.addAnalyses(...)`:

```scala
strategy
  .addAnalyses(ExtractionAnalysis.smallest, ConstantAnalysis)
  .closeMetadata
```

---

## Features

### Immutable, Thread‑Safe E‑Graphs
Foresight offers an immutable e‑graph built using purely functional data structures. Every operation returns a new, structurally consistent e‑graph without a rebuild phase. This makes reasoning about transformations simpler and enables speculative or parallel execution without side effects.

### High‑Performance Mutable E‑Graphs
For performance‑critical workloads, Foresight also provides a mutable e‑graph backed by a dedicated hash‑consing implementation. It supports in‑place addition and merging of e‑classes, delivering substantially lower latency than the immutable variant for sequential saturation workloads. Both variants share the same interface and are interchangeable across all strategies and analyses.

### Slotted E‑Graphs with Native Binding Support
E‑classes are parameterized by [slots](https://dl.acm.org/doi/10.1145/3729326), a mechanism that encodes variable binding directly in the e‑graph. This allows Foresight to correctly and efficiently unify expressions that differ only in variable names, making it ideal for functional intermediate representations with bound variables (e.g., lambdas).

### Parallel Rule Matching and Application
Rewriting in Foresight is divided into four fine‑grained stages: pattern matching, command generation, simplification, and graph mutation. The first three stages are embarrassingly parallel, and the final mutation stage benefits from deferred command batching and optimized application.

### Metadata and Analyses
Users can attach domain‑specific metadata (such as types, constants, or cost estimates) to e‑classes. Metadata is updated in parallel and remains consistent with the evolving graph. This enables type‑safe rewriting, cost‑guided extraction, and analysis‑informed rule application.

### Composable Strategy Interface
Rewrite control is decoupled from the engine using a modular strategy interface. Users can define custom saturation strategies with iteration limits, timeouts, stopping conditions, or metadata integration. Built‑in strategy combinators make it easy to experiment with complex workflows declaratively.

### Flexible Built‑In Saturation Strategies
Foresight ships with several ready‑to‑use saturation strategies:

- **`MaximalRuleApplication`** – performs a single exhaustive pass over all rules per iteration.
- **`BackoffRuleApplication`** – assigns each rule a dynamic match quota and a cooldown period, preventing any single rule from dominating the search.
- **`StochasticRuleApplication`** – uses weighted random sampling guided by user‑defined `MatchPriorities` to apply a batch of matches each iteration, enabling efficient exploration of large rule sets.

Caching variants of the last two strategies avoid reapplying matches across iterations.

### Cost‑Guided Extraction
Foresight includes a built‑in analysis that tracks the lowest‑cost term in each equivalence class using a user‑defined cost model. Extraction is incremental and can occur during or after saturation, supporting efficient selection of optimized expressions.

### Configurable Parallel Execution
All parallel behavior in Foresight is implemented via a pluggable `ParallelMap` interface. Users can select or implement custom concurrency backends for fine‑grained control over resource usage, instrumentation, or cancellation behavior.

---

## More Resources

- **[API Docs](https://jonathanvdc.github.io/foresight/latest/api)** – Full Scaladoc for all public APIs, including the `Strategy` trait, built‑in strategies, `Analysis`, and the `Language` DSL.
- **[Examples](examples/src/main/scala)** – Self‑contained worked examples in the repository:
  - [Arithmetic rewrites (Scala 2/3)](examples/src/main/scala/foresight/eqsat/examples/arith) – rewrite rules and a constant‑folding analysis using lower‑level primitives.
  - [Arithmetic rewrites with Language DSL (Scala 3)](examples/src/main/scala-3/foresight/eqsat/examples/arithWithLang) – the same example rewritten using `derives Language` for a more concise and idiomatic style.
  - [Matrix multiplication rewriting](examples/src/main/scala-3/foresight/eqsat/examples/mm) – demonstrates rewriting for linear algebra expressions.
  - [Vector arithmetic with typed cost extraction](examples/src/main/scala-3/foresight/eqsat/examples/vector) – shows type‑guided cost extraction and custom cost models.
- **[Equality Saturation](https://doi.org/10.1145/1480881.1480915)** (Tate et al., POPL 2009) – The original paper introducing equality saturation as a compilation strategy.
- **[egg: Fast and Extensible Equality Saturation](https://dl.acm.org/doi/10.1145/3434304)** (Willsey et al., POPL 2021) – A widely used equality saturation framework and a key point of reference for Foresight.
- **[Slotted E‑Graphs](https://dl.acm.org/doi/10.1145/3729326)** – The paper describing the slotted e‑graph representation that underpins Foresight's native support for bound variables.

---

## Citing Foresight

If you use Foresight in academic work, please cite the following paper:

```bibtex
@inproceedings{vandercruysse2026parallel,
  author    = {Van der Cruysse, Jonathan and Zayed, Abd-El-Aziz and
               Peng, Mai Jacob and Dubach, Christophe},
  title     = {Parallel and Customizable Equality Saturation},
  booktitle = {Proceedings of the 35th ACM SIGPLAN International Conference
               on Compiler Construction},
  series    = {CC '26},
  year      = {2026},
  pages     = {94--105},
  publisher = {Association for Computing Machinery},
  address   = {New York, NY, USA},
  doi       = {10.1145/3771775.3786266},
  url       = {https://doi.org/10.1145/3771775.3786266},
  keywords  = {Compiler Infrastructure, E-Graphs, Equality Saturation,
               Program Optimization, Rewrite Systems},
  location  = {Sydney, NSW, Australia},
}
```
