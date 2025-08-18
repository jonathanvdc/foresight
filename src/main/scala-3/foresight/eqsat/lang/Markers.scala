package foresight.eqsat.lang

import foresight.eqsat.Slot
import scala.language.implicitConversions
import scala.Conversion

// Mark Slot fields: use vs binder
final case class Use[A](value: A) extends AnyVal
object Use:
  given Conversion[Slot, Use[Slot]] with
    def apply(s: Slot): Use[Slot] = Use(s)

final case class Def[A](value: A) extends AnyVal
object Def:
  given Conversion[Slot, Def[Slot]] with
    def apply(s: Slot): Def[Slot] = Def(s)
