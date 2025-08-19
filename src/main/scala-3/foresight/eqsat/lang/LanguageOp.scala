package foresight.eqsat.lang

final case class LanguageOp[E] private[lang](private[lang] ord: Int,
                                             private[lang] schema: Seq[Byte],
                                             private[lang] payload: Seq[Any])

object LanguageOp {
  /** If a Language[E] is in scope, its Op ordering is summonable. */
  given opOrderingFor[E](using L: Language[E]): Ordering[LanguageOp[E]] =
    L.opOrdering
}
