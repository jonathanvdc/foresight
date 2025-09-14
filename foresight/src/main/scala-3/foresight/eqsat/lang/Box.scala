package foresight.eqsat.lang

import scala.deriving._

/**
 * Type class declaring that a surface type `A` should appear as a leaf atom in a `MixedTree`.
 *
 * `Box[A]` is an opt-in witness: by defining or deriving an instance, you state that values
 * of `A` are stored as leaf payloads rather than core language nodes. The associated type
 * [[Payload]] names the concrete type actually stored at the leaves, and [[asAtom]] provides a
 * bidirectional bridge between `A` and `Payload`.
 *
 * When to use: Mark single-field wrappers (e.g., handles, slots, small value wrappers) as `derives Box`.
 *
 * @tparam A surface type to be treated as an atom (leaf).
 * @example Typical usage (single-field wrapper as an atom)
 * {{{
 * sealed trait Expr derives Language
 * final case class Ref(id: EClassCall) extends Expr derives Box
 *
 * val L = summon[Language[Expr]]
 * val ref = Ref(id)
 * val tree = L.toTree[EClassCall](ref) // yields MixedTree.Atom(id)
 * }}}
 */
trait Box[A]:
  /** Concrete payload type stored at leaves for atoms originating from `A`. */
  type Payload

  /**
   * Bridge converting between the surface wrapper `A` and its stored [[Payload]].
   * See `AsAtom` for details.
   */
  def asAtom: AsAtom[A, Payload]

/**
 * Companion object for [[Box]].
 */
object Box:

  /**
   * Type alias that pins the associated `Payload` of `A` to `B`.
   *
   * Useful for APIs that need to mention the payload type explicitly:
   * {{{
   * def needsEClass(using Box.Aux[Ref, EClassCall]) = ...
   * }}}
   */
  type Aux[A, B] = Box[A] { type Payload = B }

  /**
   * Derive a `Box[A]` instance for single-field case classes.
   *
   * The derived instance:
   *  - sets `Payload` to the single field's type,
   *  - implements `asAtom` by projecting/injecting that field.
   *
   * Fails to compile if `A` is not a product with exactly one element.
   *
   * Example:
   * {{{
   * final case class Ref(id: EClassCall) derives Box
   * summon[Box.Aux[Ref, EClassCall]] // OK
   * }}}
   */
  inline def derived[A](using
                        m: Mirror.ProductOf[A],
                        ev: Tuple.Size[m.MirroredElemTypes] =:= 1
                       ): Box[A] =
    new Box[A]:
      type Payload = Head[m.MirroredElemTypes]
      val asAtom: AsAtom[A, Payload] = new AsAtom[A, Payload]:
        def toAtom(a: A): Payload =
          a.asInstanceOf[Product].productElement(0).asInstanceOf[Payload]
        def fromAtom(b: Payload): A =
          m.fromProduct(Tuple1(b))

  /**
   * Helper `given` that reveals the concrete payload type `B` when:
   *  - an explicit/derived `Box[A]` is already in scope (opt-in gate), and
   *  - `A` is a single-field product whose field type is `B`.
   *
   * Lets you summon `Box.Aux[A, B]` in contexts that need the refined type.
   */
  inline given aux[A, B](using
                         d: Box[A], // opt-in gate (prevents silent auto instances)
                         m: Mirror.ProductOf[A],
                         eq: m.MirroredElemTypes =:= (B *: EmptyTuple) // enforce single field of type B
                        ): Box.Aux[A, B] =
    d.asInstanceOf[Box.Aux[A, B]]

  // Extract the first (only) field type of product
  private type Head[T <: Tuple] = T match
    case h *: _ => h
