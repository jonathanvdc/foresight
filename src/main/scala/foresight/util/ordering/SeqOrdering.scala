package foresight.util.ordering

object SeqOrdering {
  implicit def lexOrdering[A](implicit ord: Ordering[A]): Ordering[Seq[A]] = {
    new Ordering[Seq[A]] {
      override def compare(x: Seq[A], y: Seq[A]): Int = {
        Ordering.Iterable(ord).compare(x, y)
      }
    }
  }
}
