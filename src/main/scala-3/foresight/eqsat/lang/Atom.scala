package foresight.eqsat.lang

import scala.deriving.*

trait Atom[A]:
  type Atom
  def asAtom: AsAtom[A, Atom]

object Atom:
  // Aux to refine the associated type so it equals a concrete B
  type Aux[A, B] = Atom[A] { type Atom = B }

  // Derive only for single-field case classes
  inline def derived[A](using m: Mirror.ProductOf[A],
                        ev: Tuple.Size[m.MirroredElemTypes] =:= 1): Atom[A] =
    new Atom[A]:
      type Atom = Head[m.MirroredElemTypes]
      val asAtom: AsAtom[A, Atom] = new AsAtom[A, Atom]:
        def toAtom(a: A): Atom =
          a.asInstanceOf[Product].productElement(0).asInstanceOf[Atom]
        def fromAtom(b: Atom): A =
          m.fromProduct(Tuple1(b))

  inline given aux[A, B](using
                         d: Atom[A], // opt-in gate
                         m: Mirror.ProductOf[A],
                         eq: m.MirroredElemTypes =:= (B *: EmptyTuple) // field type must be B
                        ): Atom.Aux[A, B] =
    d.asInstanceOf[Atom.Aux[A, B]]

  // Match-type to extract the head element type of the tuple of fields
  private type Head[T <: Tuple] = T match
    case h *: _ => h
