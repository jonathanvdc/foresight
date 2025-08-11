package foresight.eqsat

/**
 * A slot is a symbolic placeholder used to model variables and bindings in a slotted e-graph.
 *
 * Two concrete kinds of slots exist:
 *   - [[Slot.NumberedSlot]]: value-based slots identified by an integer (e.g., `$0`, `$1`, ...). Any two numbered
 *     slots with the same number are equal. These are primarily used for canonicalization and shape-normal forms.
 *   - [[Slot.UniqueSlot]]: identity-based slots that are always distinct from every other slot unless they are the
 *     exact same object. These are appropriate for user- or pass-generated "fresh" variables.
 *
 * Equality semantics:
 *   - Numbered slots compare equal if their numbers match.
 *   - Unique slots compare equal only by object identity (reference equality).
 *
 * Ordering semantics:
 *   - All numbered slots sort before all unique slots.
 *   - Numbered slots are ordered by their integer `n`.
 *   - Unique slots use an implementation-defined, identity-derived ordering suitable for in-memory canonicalization
 *     but not for persistence. Do not rely on the absolute ordering of unique slots across processes or runs.
 *
 * Typical usage:
 *   - External callers should prefer `Slot.fresh()` (a new [[Slot.UniqueSlot]]) for generating fresh variables.
 *   - Canonicalization and shape computation use `Slot.numeric(n)` (a [[Slot.NumberedSlot]]) to assign `$0, $1, ...`.
 *
 * @note Slots are immutable labels; renaming is expressed separately via `SlotMap`.
 * @see [[Slot.numeric]] for canonical, numbered slots used in shapes
 * @see [[Slot.fresh]] for identity-unique slots suitable for user-facing variables
 *
 * @example
 * {{{
 * // Fresh, identity-unique slots (never equal unless the same object):
 * val x1: Slot = Slot.fresh()
 * val x2: Slot = Slot.fresh()
 * assert(x1 != x2)
 *
 * // Numbered slots used in canonical forms:
 * val s0: Slot = Slot.numeric(0)  // `$0`
 * val s1: Slot = Slot.numeric(1)  // `$1`
 * assert(s0 != s1)
 * assert(Slot.numeric(0) == s0)   // same number => equal
 *
 * // Ordering: numbered slots come before unique slots
 * val sorted = List(x1, s1, x2, s0).sorted
 * assert(sorted.take(2) == List(s0, s1))
 * }}}
 */
sealed trait Slot extends Ordered[Slot] {
  /**
   * True if this is a numbered slot (`Slot.NumberedSlot`).
   * Numbered slots are equal iff their numbers match. Unique slots are always distinct.
   */
  final def isNumbered: Boolean = this match {
    case Slot.NumberedSlot(_) => true
    case _: Slot.UniqueSlot => false
  }

  /**
   * True if this is an identity-unique slot (`Slot.UniqueSlot`).
   * Unique slots are distinct from all other slots, including other unique slots.
   */
  final def isUnique: Boolean = !isNumbered
}

/**
 * Constructors and concrete slot types.
 */
object Slot {

  /**
   * A value-based, numbered slot. All instances with the same `n` are equal.
   *
   * Typical use: canonicalization and shape-normal forms assign `$0, $1, ...` using numbered slots.
   *
   * @param n The integer identifier for the slot.
   */
  final case class NumberedSlot(n: Int) extends Slot {
    override def compare(that: Slot): Int = that match {
      case NumberedSlot(m) => n compare m
      case _: UniqueSlot => -1
    }
  }

  /**
   * An identity-unique slot. Two unique slots are equal only if they are the same object.
   *
   * Typical use: generating fresh variables for user-visible terms or rewrites.
   *
   * Ordering among unique slots is based on an identity-derived hash and is not stable across runs.
   * Do not persist or externally rely on this relative ordering.
   */
  final class UniqueSlot extends Slot {
    override def compare(that: Slot): Int = that match {
      case _: NumberedSlot => 1
      case _: UniqueSlot => hashCode() compare that.hashCode()
    }
  }

  /**
   * Creates a new identity-unique slot suitable for fresh variable generation.
   *
   * @return A new `UniqueSlot`.
   */
  def fresh(): Slot = new UniqueSlot

  /**
   * Creates a numbered slot for canonical forms (e.g., `$n`).
   *
   * @param n The integer identifier.
   * @return A `NumberedSlot(n)`.
   */
  def numeric(n: Int): Slot = NumberedSlot(n)
}
