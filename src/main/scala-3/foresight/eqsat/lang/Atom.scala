package foresight.eqsat.lang

import scala.deriving.*

/**
 * Type class declaring that a surface type `A` should appear as a leaf atom in a `MixedTree`.
 *
 * `Atom[A]` is an opt-in witness: by defining or deriving an instance, you state that values
 * of `A` are stored as leaf payloads rather than core language nodes. The associated type
 * [[Payload]] names the concrete type actually stored at the leaves, and [[asAtom]] provides a
 * bidirectional bridge between `A` and `Payload`.
 *
 * When to use: Mark single-field wrappers (e.g., handles, slots, small value wrappers) as `derives Atom`.
 *
 * @tparam A surface type to be treated as an atom (leaf).
 * @example Typical usage (single-field wrapper as an atom)
 * {{{
 * sealed trait Expr derives Language
 * final case class Ref(id: EClassCall) extends Expr derives Atom
 *
 * val L = summon[Language[Expr]]
 * val ref = Ref(id)
 * val tree = L.toTree[EClassCall](ref) // yields MixedTree.Atom(id)
 * }}}
 */
trait Atom[A]:
  /** Concrete payload type stored at leaves for atoms originating from `A`. */
  type Payload

  /**
   * Bridge converting between the surface wrapper `A` and its stored [[Payload]].
   * See `AsAtom` for details.
   */
  def asAtom: AsAtom[A, Payload]

object Atom:

  /**
   * Type alias that pins the associated [[Payload]] of `A` to `B`.
   *
   * Useful for APIs that need to mention the payload type explicitly:
   * {{{
   * def needsEClass(using Atom.Aux[Ref, EClassCall]) = ...
   * }}}
   */
  type Aux[A, B] = Atom[A] { type Payload = B }

  /**
   * Derive an `Atom[A]` instance for single-field case classes.
   *
   * The derived instance:
   *  - sets [[Payload]] to the single field's type,
   *  - implements [[asAtom]] by projecting/injecting that field.
   *
   * Fails to compile if `A` is not a product with exactly one element.
   *
   * Example:
   * {{{
   * final case class Ref(id: EClassCall) derives Atom
   * summon[Atom.Aux[Ref, EClassCall]] // OK
   * }}}
   */
  inline def derived[A](using
                        m: Mirror.ProductOf[A],
                        ev: Tuple.Size[m.MirroredElemTypes] =:= 1
                       ): Atom[A] =
    new Atom[A]:
      type Payload = Head[m.MirroredElemTypes]
      val asAtom: AsAtom[A, Payload] = new AsAtom[A, Payload]:
        def toAtom(a: A): Payload =
          a.asInstanceOf[Product].productElement(0).asInstanceOf[Payload]
        def fromAtom(b: Payload): A =
          m.fromProduct(Tuple1(b))

  /**
   * Helper `given` that reveals the concrete payload type `B` when:
   *  - an explicit/derived `Atom[A]` is already in scope (opt-in gate), and
   *  - `A` is a single-field product whose field type is `B`.
   *
   * Lets you summon `Atom.Aux[A, B]` in contexts that need the refined type.
   */
  inline given aux[A, B](using
                         d: Atom[A],                                   // opt-in gate (prevents silent auto instances)
                         m: Mirror.ProductOf[A],
                         eq: m.MirroredElemTypes =:= (B *: EmptyTuple) // enforce single field of type B
                        ): Atom.Aux[A, B] =
    d.asInstanceOf[Atom.Aux[A, B]]

  // Extract the first (only) field type of product
  private type Head[T <: Tuple] = T match
    case h *: _ => h
