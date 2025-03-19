package fixpoint.eqsat.hashCons

private[eqsat] object Helpers {
  def cartesian[A](input: Iterable[Iterable[A]]): Seq[Seq[A]] = {
    input.foldLeft(Seq(Seq.empty[A])) { (acc, seq) =>
      for {
        accSeq <- acc
        elem <- seq
      } yield accSeq :+ elem
    }
  }
}
