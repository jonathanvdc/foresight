package foresight.eqsat

/**
 * Defines the type classes and data types that connect a surface AST
 * (your domain language) to Foresight’s core e-graph representation. It lets you
 * describe how your AST maps to e-graph nodes, mark which wrappers are leaf atoms,
 * encode and decode between surface terms and mixed trees, build rules from surface
 * syntax, and embed analysis results back into the surface language.
 *
 * The API is intentionally minimal and strongly typed. In practice most users only
 * derive [[foresight.eqsat.lang.Language]] and add `derives Box` to a few case
 * classes.
 *
 * ## Core abstractions
 *
 * [[foresight.eqsat.lang.Language]] defines the bridge for a surface language `E`.
 * It provides:
 *
 *   - `toTree` and `fromTree`: conversion between surface expressions and core
 *     trees.
 *   - `toSearcher`, `toApplier`, and `rule`: ways to build rewrite patterns
 *     directly from surface syntax.
 *   - `opOrdering`: a deterministic ordering over nodes for reproducibility.
 *
 * [[foresight.eqsat.lang.LanguageOp]] is the canonical node representation the
 * e-graph stores for a given language. You never construct it directly; obtain it
 * through a `Language[E]`. An `Ordering[LanguageOp[E]]` is summonable whenever a
 * `Language[E]` is in scope.
 *
 * [[foresight.eqsat.lang.Box]] and [[foresight.eqsat.lang.AsAtom]] mark a wrapper
 * type `A` as a leaf atom, whose stored payload type is `Payload`. You can write
 * `derives Box` on single-field case classes to derive this automatically, and the
 * corresponding `AsAtom[A, Payload]` bridge will be provided.
 *
 * [[foresight.eqsat.lang.AtomEncoder]] and [[foresight.eqsat.lang.AtomDecoder]]
 * handle encoding and decoding between surface values and payloads. These are
 * usually derived automatically from `Atom` and `AsAtom` instances.
 *
 * [[foresight.eqsat.lang.AnalysisBox]] is evidence that your language can box an
 * analysis result `A` into an `E` node (for example, `Fact[A]`). This enables
 * reconstruction of surface terms from analysis results.
 *
 * [[foresight.eqsat.lang.Use]] and [[foresight.eqsat.lang.Def]] are lightweight
 * wrappers to distinguish between read sites and binder sites, commonly over
 * [[Slot]]. Implicit conversions from `Slot` keep call sites clean.
 *
 * ## Getting started
 *
 * {{{
 * import foresight.eqsat.lang._
 * import foresight.eqsat.rewriting.{Rule, ReversibleSearcher}
 * import foresight.eqsat.rewriting.patterns.Pattern
 *
 * sealed trait Expr derives Language
 * final case class Lit(i: Int)           extends Expr derives Box
 * final case class Add(x: Expr, y: Expr) extends Expr
 * final case class Ref(id: EClassCall)   extends Expr derives Box
 *
 * val Lang = summon[Language[Expr]]
 *
 * val pat: Lang.MTree[Pattern.Var] =
 *   Lang.toTree(Add(Ref(ec1), Ref(ec2)))
 *
 * val r: Rule[Lang.Op, PatternMatch[Lang.Op], ?] =
 *   Lang.rule("comm-add", Add(x, y), Add(y, x))
 * }}}
 *
 * ## Atoms
 *
 * Single-field wrappers, such as handles or IDs, can be made first-class [[MixedTree]]
 * atoms by deriving `Atom`.
 *
 * {{{
 * final case class Ref(id: EClassCall) extends Expr derives Box
 * }}}
 *
 * If a case is not a single-field product, provide a manual `Atom` and `AsAtom`
 * instance.
 *
 * ## Encoding and decoding
 *
 * Encoders and decoders are derived when possible. For example:
 *
 *   - `AtomEncoder[Expr, Int]` succeeds on `Lit(i)` and yields `i`.
 *   - `AtomDecoder[Expr, EClassCall]` succeeds on `Ref(id)` and yields `Ref(id)`.
 *
 * You rarely need to implement these by hand. Add `derives Box` to your
 * single-field wrappers and let the compiler summon the right instances.
 *
 * ## Analysis integration
 *
 * If your language can wrap analysis results, such as `Fact[A] <: Expr`, provide
 * an `AnalysisBox`:
 *
 * {{{
 * given AnalysisBox[Expr] with
 *   type Box[X] = Fact[X]
 *   def box[X](x: X) = Fact(x)
 * }}}
 *
 * This enables decoders like `AtomDecoder[Expr, AnalysisFact[A]]` and allows
 * utilities such as `Language.fromAnalysisNode` to reconstruct surface terms from
 * analysis views.
 *
 * ## Slots, binders, and uses
 *
 * Use `Def[Slot]` to represent binders and `Use[Slot]` for read sites. Implicit
 * conversions from `Slot` keep code concise:
 *
 * {{{
 * final case class Lambda(param: Def[Slot], body: Expr) extends Expr
 * final case class Var(param: Use[Slot]) extends Expr
 *
 * val s = Slot.fresh()
 * Lambda(s, Var(s)) // creates Lambda(Def(s), Use(s))
 * }}}
 *
 * ## Checklist
 *
 *   - Add `derives Box` to single-field wrappers.
 *   - Implement or derive a single `Language[E]` for your AST.
 *   - Build rules and patterns with surface terms through `Language`.
 *   - Provide an `AnalysisBox[E]` if analyses should appear as surface nodes.
 *
 */
package object lang