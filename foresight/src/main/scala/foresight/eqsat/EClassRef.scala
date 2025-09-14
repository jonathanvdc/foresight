package foresight.eqsat

/**
 * A stable handle identifying an e-class in an e-graph.
 *
 * An e-class is a set of equivalent expressions (e-nodes) maintained by the e-graph
 * as part of its congruence-closure structure. Each e-class may be parameterized
 * by slots (see Schneider et al., "E-Graphs with Classes and Slots: Reasoning with
 * Parameterized Equivalence Classes", Proc. ACM Program. Lang., OOPSLA 2024, extended version).
 *
 * When a new e-class is created, the e-graph generates a fresh `EClassRef` as its
 * identity. This reference is stable across transformations, but the equivalence
 * relation maintained by the e-graph may cause multiple `EClassRef` instances to
 * represent the same underlying e-class if merges occur.
 *
 * Because merges are common in equality saturation, `EClassRef` values are not
 * guaranteed to be canonical. To obtain a canonical representative of the current
 * equivalence class, use:
 *
 * {{{
 * egraph.canonicalize(ref)
 * }}}
 *
 * This returns an `EClassRef` that is the unique canonical handle for the merged class
 * in the given e-graph state.
 *
 * `EClassRef` is used both directly for identifying classes and indirectly in
 * parameterized applications via [[EClassCall]], where it is paired with a mapping
 * from the class's parameter slots to argument slots in the caller's context.
 *
 * Equality and hashing of `EClassRef` instances are based solely on their internal
 * identity, not on the current structure of the e-graph. This allows them to be
 * used as keys in maps or sets, but note that their equivalence meaning may change
 * as the e-graph evolves.
 */
final class EClassRef {
  /**
   * A pre-instantiated EClassCall representing this e-class with no argument slots.
   * This is a common case for non-parameterized e-classes, allowing efficient reuse
   * without needing to repeatedly construct new `EClassCall` instances.
   */
  private[eqsat] val callWithoutSlots: EClassCall = EClassCall(this, SlotMap.empty)
}
