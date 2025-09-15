package foresight.eqsat

/**
 * Represents the application of an [[EClassRef]] to a set of argument slots.
 *
 * An e-class application combines:
 *   - A reference to an e-class (`ref`), which may have parameter slots.
 *   - A [[SlotMap]] mapping each parameter slot to a corresponding argument slot.
 *
 * Slots are a generalization of term variables that allow the e-graph to represent
 * parameterized e-classes and maintain consistent argument binding across multiple
 * occurrences. This enables advanced rewriting techniques such as those described in
 * Schneider et al., "E-Graphs with Classes and Slots: Reasoning with Parameterized
 * Equivalence Classes" (Proc. ACM Program. Lang., OOPSLA 2024).
 *
 * The argument slot map (`args`) specifies, for each parameter slot in the e-class,
 * which argument slot should be used when this e-class is applied. This supports
 * sharing, renaming, and partial instantiation of e-classes within larger expressions.
 *
 * @param ref  The reference to the e-class being applied.
 * @param args The mapping from the e-class's parameter slots to argument slots.
 *
 * @example
 * {{{
 * // EClassRef `subXY` has parameter slots (x, y) representing "x - y"
 * val call1 = EClassCall(subXY, SlotMap(x -> a, y -> b)) // represents "a - b"
 * val call2 = EClassCall(subXY, SlotMap(x -> b, y -> a)) // represents "b - a"
 * }}}
 */
final case class EClassCall(ref: EClassRef, args: SlotMap) {
  /**
   * The set of slots used as arguments in this application, in sequence order.
   * These are the slots referenced by the argument values, not the parameter slots.
   */
  def slots: Seq[Slot] = args.values

  /**
   * The set of distinct slots used as arguments in this application.
   */
  def slotSet: Set[Slot] = args.valueSet

  /**
   * Renames all argument slots in this application according to a given mapping.
   * All argument slots must be present in the renaming map.
   *
   * @param renaming The mapping from old argument slots to new argument slots.
   * @return A new application with arguments renamed.
   */
  def rename(renaming: SlotMap): EClassCall = {
    if (args.isEmpty) return this

    assert(args.valueSet.subsetOf(renaming.keySet), "Argument slots must be in the renaming.")
    EClassCall(ref, args.composePartial(renaming))
  }

  /**
   * Renames argument slots in this application according to a given mapping.
   * Argument slots not in the mapping are dropped.
   *
   * @param renaming The mapping from old argument slots to new argument slots.
   * @return A new application with arguments renamed (possibly dropping some).
   */
  def renamePartial(renaming: SlotMap): EClassCall = {
    if (args.isEmpty) return this

    EClassCall(ref, args.composePartial(renaming))
  }

  /**
   * Renames argument slots in this application according to a given mapping.
   * Argument slots not in the mapping are retained as-is.
   *
   * @param renaming The mapping from old argument slots to new argument slots.
   * @return A new application with arguments renamed (retaining unmapped ones).
   */
  def renameRetain(renaming: SlotMap): EClassCall = {
    if (args.isEmpty || renaming.isEmpty) return this

    EClassCall(ref, args.composeRetain(renaming))
  }

  /**
   * Checks whether this application is well-formed in the given e-graph.
   * An application is well-formed if all parameter slots of the target e-class
   * are bound in its argument slot map.
   *
   * @param egraph The e-graph in which to check.
   * @return True if well-formed, false otherwise.
   */
  def isWellFormed(egraph: EGraph[_]): Boolean = {
    val slots = egraph.canonicalize(ref).args.valueSet
    slots.subsetOf(args.keySet)
  }
}

