package foresight.eqsat

/**
 * Tools for turning e-graph e-classes into concrete expression trees using a pluggable,
 * bottom-up cost model. This package is agnostic to the meaning of cost and to the
 * concrete e-graph representation; it works with any immutable `EGraph` that provides
 * the required metadata.
 *
 * ## Design
 *
 * Extraction is a two-step process. First, an analysis computes, for every e-class,
 * a minimal (under a user-defined ordering) cost-annotated tree together with a total
 * slot renaming that makes the tree valid in context. Second, an extractor projects
 * that analysis result into a plain expression tree.
 *
 * E-graphs in Foresight are immutable. All APIs return new values; nothing mutates in
 * place. Slot renamings are explicit, total mappings applied during materialization.
 *
 * ## Key abstractions
 *
 * `CostFunction[NodeT, C]` defines the node-local cost model. Given a node’s operator,
 * its locally defined/used slots, and already-costed children, it returns a cost of type `C`.
 * The cost may be numeric, tuple-based, or any type with an implicit `Ordering[C]`.
 *
 * `ExtractionAnalysis[NodeT, C]` runs over an e-graph to compute, per e-class, an
 * `ExtractionTreeCall[NodeT, C]` that minimizes cost and then applies deterministic
 * tie-breaking. The analysis requires `Ordering[C]` and `Ordering[NodeT]`. Helpers like
 * `ExtractionAnalysis.smallest` and `ExtractionAnalysis.shallowest` are provided.
 *
 * `ExtractionTree[NodeT, C]` is an immutable, cost-annotated tree:
 * it stores the root operator, definitions/uses, children, total cost, size, and depth.
 *
 * `ExtractionTreeCall[NodeT, C]` pairs an `ExtractionTree` with a total `SlotMap`
 * (alpha-renaming) over the tree’s slots. It supports composition of renamings and
 * applying them to produce a renamed tree.
 *
 * `ExtractionTreeOrdering[NodeT, C]` defines the total order used for selection and
 * tie-breaking. Comparison is lexicographic over:
 *  cost → size → depth → nodeType → definitions → uses → args.
 *
 * ## Typical workflow
 *
 *  1. Define a `CostFunction` that reflects your objective (e.g., node count, height, latency).
 *  2. Build an `ExtractionAnalysis(name, cost)` with implicit `Ordering[C]` and `Ordering[NodeT]` in scope.
 *  3. Obtain an `Extractor` via `analysis.extractor`, then call it with an `EClassCall` (or a `MixedTree`,
 *     which gets added to a derived graph) to get a plain `Tree`.
 *
 * ## Determinism and tie-breaking
 *
 * Two trees with equal cost are ordered by structure and `nodeType` using the provided
 * `Ordering[NodeT]`, then by slot sequences and children, yielding deterministic results.
 *
 * ## Slots and renaming
 *
 * Slots bound at a node are listed in `definitions`; externally provided ones are in `uses`.
 * An `ExtractionTreeCall` carries a total renaming over the tree’s `slotSet`. Composition
 * methods ensure the mapping remains total when threading results through callers.
 *
 * @example
 * {{{
 * implicit val costOrd: Ordering[Int] = Ordering.Int
 * implicit val nodeOrd: Ordering[Op]  = Op.ordering
 *
 * val analysis   = extraction.ExtractionAnalysis.smallest[Op]
 * val extractor  = analysis.extractor[MyEGraph]
 * val bestTree   = extractor(eclassCall, egraphWithMetadata) // Tree[Op]
 * }}}
 */
package object extraction
