package foresight.eqsat.lang

import scala.deriving.Mirror

trait AsAtom[T, B]:
  def toAtom(t: T): B
  def fromAtom(b: B): T

object AsAtom:
  inline def apply[T, B](using ev: AsAtom[T, B]): AsAtom[T, B] = ev

  /** Helper to build a two-way codec. */
  def codec[T, B](to: T => B, from: B => T): AsAtom[T, B] =
    new AsAtom[T, B]:
      override def toAtom(t: T): B = to(t)
      override def fromAtom(b: B): T = from(b)

  inline given derivedAsAtom[A, B](using d: Atom.Aux[A, B]): AsAtom[A, B] =
    d.asAtom

//  /** Auto-generate AsAtom for any 1-field case class A whose sole field is of type B. */
//  inline given singleField[A, B](using m: Mirror.ProductOf[A], ev: m.MirroredElemTypes =:= (B *: EmptyTuple)): AsAtom[A, B] =
//    new AsAtom[A, B]:
//      def toAtom(a: A): B =
//        a.asInstanceOf[Product].productElement(0).asInstanceOf[B]
//
//      def fromAtom(b: B): A =
//        m.fromProduct(Tuple1(b))
