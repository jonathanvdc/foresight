package foresight.eqsat.lang

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