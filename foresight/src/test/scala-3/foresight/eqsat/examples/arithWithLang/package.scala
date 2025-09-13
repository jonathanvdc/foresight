package foresight.eqsat.examples

/**
 * This example demonstrates how to model a small arithmetic and lambda-calculus language in
 * Foresight, derive a language instance, define rewrite rules ergonomically, attach a custom
 * analysis, and validate behavior via saturation strategies and tests.
 *
 * The example is split into a few focused files:
 *
 *   - [[foresight.eqsat.examples.arithWithLang.ArithExpr ArithExpr]] — the surface AST and all
 *     language-facing niceties (operators, atoms, analysis boxing, etc.).
 *   - [[foresight.eqsat.examples.arithWithLang.ConstantAnalysis ConstantAnalysis]] — an analysis that
 *     computes constant values when possible.
 *   - [[foresight.eqsat.examples.arithWithLang.ApplierOps ApplierOps]] — a small utility that adds a
 *     substitution helper for appliers.
 *   - [[foresight.eqsat.examples.arithWithLang.Rules Rules]] — rewrite rules using the language-driven
 *     DSL.
 *   - [[foresight.eqsat.examples.arithWithLang.tests.RuleTests RuleTests]] — end-to-end tests wiring
 *     strategies, analyses, and the rule set.
 *
 * ## 1. Surface AST & Language Derivation
 * The core AST lives in `ArithExpr.scala`. We define a minimal lambda fragment
 * (`Var`, `Lam`, `App`) and arithmetic (`Add`, `Mul`, `Number`). The sealed trait
 * derives a Foresight [[foresight.eqsat.lang.Language Language]] instance, which teaches the
 * e-graph how to encode and decode the surface AST to the core representation.
 *
 * To make the e-graph interplay smoother, two classes of helpers are introduced:
 *
 *   - `Ref(eClass: EClassCall)` and `PatternVar(variable: Pattern.Var)` derive
 *     [[foresight.eqsat.lang.Box Atom]]. This marks them as atoms that can be embedded in
 *     mixed trees. `PatternVar.fresh()` creates uniquely
 *     identifiable pattern variables.
 *   - A `given` conversion `Int => ArithExpr` maps literals to `Number`, and extension methods define
 *     infix `+`/`*` on `ArithExpr` for concise rule bodies.
 *
 * Finally, `type ArithIR = LanguageOp[ArithExpr]` aliases the concrete e-graph node type for
 * this language.
 *
 * ## 2. Attaching Analysis with AnalysisBox
 * The companion of `ArithExpr` supplies an [[foresight.eqsat.lang.AnalysisBox AnalysisBox]] where
 * `Box[A] = Fact[A]`. This enables analyses to wrap intermediate results (facts) as AST nodes when
 * needed. The example analysis, [[ConstantAnalysis]], computes `Option[BigInt]` values by pattern
 * matching on the decoded surface AST that the derived language instance hands to `make`.
 *
 *   - `make` returns a constant for `Number`, propagates through `Add`/`Mul` when both sides are
 *     constant, and yields `None` otherwise.
 *   - `join` merges lattice values, permitting equality or `None` propagation while throwing on
 *     contradictions.
 *   - `rename` is a no-op for constants.
 *
 * Because analyses observe the surface AST via the language view, their implementations stay
 * idiomatic and free from e-graph internals.
 *
 * ## 3. Rule Definitions (Language-Aware DSL)
 * The [[Rules]] container exposes the rule set. Rules are typically declared with the `rule` helper
 * that comes from the derived language instance (available through an implicit `using L: Language`).
 * This `rule` helper spawns a tuple of pattern variables (arity inferred from the lambda), letting
 * you write bodies in direct Scala syntax over `ArithExpr`:
 *
 * {{{
 * val addCommutativity = rule("add-commutativity") { (x, y) => (x + y) -> (y + x) }
 * val mulAssociativity = rule("mul-associativity1") { (x, y, z) => ((x * y) * z) -> (x * (y * z)) }
 * }}}
 *
 * Two additional patterns illustrate integration points beyond the basic DSL:
 *
 *   - **β-reduction**: built from lower-level primitives: a `Rule` with an explicit `toSearcher` for
 *     the left-hand pattern and a custom applier that substitutes a bound slot in the captured body.
 *   - **η-expansion**: uses `rule` but demonstrates mixing pattern vars with a fresh binder `Slot` for
 *     the lambda parameter.
 *
 * **Constant propagation** shows how a rule can consult analysis results inside its searcher: it
 * looks up the `ConstantAnalysis` value for the matched expression and, when present, re-binds the
 * variable to a freshly built `Number` node before the normal `toApplier` fires.
 *
 * ## 4. Custom Applier Utility
 * In some cases, we want to transform a captured subtree before handing control back to the original
 * applier. `ApplierOps` adds `substitute`, which:
 *
 *   1. Uses `ExtractionAnalysis.smallest` to extract a representative tree for a matched variable.
 *   2. Decodes it to `ArithExpr` through the language instance.
 *   3. Traverses the expression, replacing occurrences of a particular `Slot` with another matched
 *      variable’s tree.
 *   4. Produces a new `PatternMatch` by extending the variable mapping with the transformed tree and
 *      delegates to the wrapped applier.
 *
 * This is used by β-reduction to implement capture-avoiding substitution at the AST level.
 *
 * ## 5. Strategies & Tests
 * The tests create an empty e-graph for `ArithIR`, add a pair of semantically equivalent expressions,
 * and check that they are initially distinct. Then they run a saturation strategy composed as:
 *
 * {{{
 * MaximalRuleApplicationWithCaching(rules)
 *   .withIterationLimit(n).repeatUntilStable
 *   .closeRecording.addAnalyses(ExtractionAnalysis.smallest, ConstantAnalysis)
 *   .closeMetadata.dropData
 * }}}
 *
 * After saturation, the tests verify that the original e-classes have merged.
 *
 * ## Takeaways
 * Deriving a `Language` keeps rule/analysis code idiomatic by working over your surface AST.
 *
 * The `rule` DSL gives concise, type-safe patterns that look like ordinary Scala.
 *
 * Analyses interoperate seamlessly: searchers/appliers can query analysis facts mid-rewrite.
 *
 * When needed, you can drop to lower-level primitives (custom searchers/appliers) without leaving
 * the language-centric view.
 */
package object arithWithLang
