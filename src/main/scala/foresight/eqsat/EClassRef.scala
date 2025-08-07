package foresight.eqsat

/**
 * Represents a reference to an e-class within an e-graph.
 *
 * An e-class is a set of equivalent expressions grouped together in an e-graph, a data structure used for representing
 * and manipulating equivalence relations between expressions.
 *
 * When a new e-class is created, a unique `EClassRef` is generated for it. Over time, as equivalence relations are
 * discovered and e-classes are merged, multiple `EClassRef` instances may refer to the same underlying e-class.
 *
 * To resolve ambiguity and obtain a single, canonical reference for an e-class, use
 * [[EGraphLike.canonicalize(EClassRef)]].
 */
final class EClassRef
