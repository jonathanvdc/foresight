package foresight.eqsat.lang

// Mark Slot fields: use vs binder
final case class Use[A](value: A) extends AnyVal
final case class Defn[A](value: A) extends AnyVal
