package foresight.eqsat.lang

/**
 * Bidirectional bridge between a surface type `T` and its atom payload `B`.
 *
 * An `AsAtom[T, B]` acts like a tiny isomorphism:
 *  - [[toAtom]]   : `T => B`   (strip the wrapper to the leaf payload)
 *  - [[fromAtom]] : `B => T`   (rebuild the surface value from the payload)
 *
 * You will most often not write this by hand. If `T` has an `Atom[T]` instance,
 * an `AsAtom[T, B]` (with `B = Atom[T]#Payload`) can be derived automatically.
 *
 * ### Laws
 * Implementations are expected to satisfy:
 *  - `fromAtom(toAtom(t)) == t`   (left inverse)
 *  - `toAtom(fromAtom(b)) == b`   (right inverse)
 *
 * Violating these laws can lead to incorrect decoding or non-idempotent rewrites.
 *
 * ### Typical usage
 *   - Inside encoders/decoders where you need to project/inject atom payloads.
 *   - In utilities that operate generically over “atom-like” wrappers.
 *
 * @example
 * {{{
 * final case class Ref(id: EClassCall) derives Atom
 *
 * val bridge: AsAtom[Ref, EClassCall] = summon[AsAtom[Ref, EClassCall]]
 * val id: EClassCall = bridge.toAtom(Ref(id))
 * val ref: Ref       = bridge.fromAtom(id)
 * }}}
 */
trait AsAtom[T, B]:
  /** Project a surface value `T` to its atom payload `B`. */
  def toAtom(t: T): B
  /** Inject an atom payload `B` back into the surface value `T`. */
  def fromAtom(b: B): T

object AsAtom:

  /**
   * Summon an implicit `AsAtom[T, B]`.
   *
   * Shorthand for `summon[AsAtom[T, B]]`.
   */
  inline def apply[T, B](using ev: AsAtom[T, B]): AsAtom[T, B] = ev

  /**
   * Build a two-way codec from plain functions.
   *
   * Prefer this helper when you have a bespoke type that isn’t a single-field case class,
   * or when you want an explicit instance instead of derivation.
   *
   * Make sure the functions form an isomorphism (see Laws in the trait).
   *
   * Example:
   * {{{
   * enum Lit: case I(i: Int)
   * given AsAtom[Lit, Int] =
   *   AsAtom.codec[Lit, Int](_.asInstanceOf[Lit.I].i, Lit.I.apply)
   * }}}
   */
  def codec[T, B](to: T => B, from: B => T): AsAtom[T, B] =
    new AsAtom[T, B]:
      override def toAtom(t: T): B = to(t)
      override def fromAtom(b: B): T = from(b)

  /**
   * Automatically derive `AsAtom[A, B]` from an in-scope `Atom.Aux[A, B]`.
   *
   * This is the usual path for single-field wrappers declared with `derives Atom`.
   *
   * Example:
   * {{{
   * final case class Ref(id: EClassCall) derives Atom
   * val aa: AsAtom[Ref, EClassCall] = summon[AsAtom[Ref, EClassCall]] // derived here
   * }}}
   */
  inline given derivedAsAtom[A, B](using d: Atom.Aux[A, B]): AsAtom[A, B] =
    d.asAtom
