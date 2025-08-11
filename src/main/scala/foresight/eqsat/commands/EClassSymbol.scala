package foresight.eqsat.commands

import foresight.eqsat.EClassCall

/**
 * Symbolic handle for an e-class in an e-graph.
 *
 * An [[EClassSymbol]] can be:
 *   - a concrete reference to an existing e-class ([[EClassSymbol.Real]]), or
 *   - a placeholder for an e-class not yet added ([[EClassSymbol.Virtual]]).
 *
 * Symbols are used by [[Command]] instances to refer to e-classes in a
 * portable, reifiable way.
 */
sealed trait EClassSymbol {

  /**
   * Returns `true` if this symbol refers to an existing e-class
   * (i.e. is an [[EClassSymbol.Real]]).
   */
  final def isReal: Boolean = this match {
    case EClassSymbol.Real(_) => true
    case _: EClassSymbol.Virtual => false
  }

  /**
   * Returns `true` if this symbol is a placeholder (i.e. an [[EClassSymbol.Virtual]]).
   */
  final def isVirtual: Boolean = !isReal

  /**
   * Resolves this symbol to its [[EClassCall]].
   *
   * If this symbol is real, its call is returned directly.
   * If it is virtual, the corresponding entry in `reification` must exist.
   *
   * @param reification Mapping from virtual symbols to concrete calls.
   * @throws NoSuchElementException if this symbol is virtual and not found in `reification`.
   * @return The concrete call for this symbol.
   *
   * @example
   * {{{
   * val v = EClassSymbol.virtual()
   * val call = EClassCall(...)
   * val realCall = v.reify(Map(v -> call)) // returns call
   * }}}
   */
  final def reify(reification: Map[EClassSymbol.Virtual, EClassCall]): EClassCall = this match {
    case EClassSymbol.Real(call) => call
    case virtual: EClassSymbol.Virtual => reification(virtual)
  }

  /**
   * Optionally resolves this symbol to its [[EClassCall]].
   *
   * If this symbol is real, its call is wrapped in `Some`.
   * If it is virtual, returns the matching entry in `reification` if present,
   * or `None` if missing.
   *
   * @param reification Mapping from virtual symbols to concrete calls.
   * @return The resolved call, or `None` if unresolved.
   */
  final def tryReify(reification: Map[EClassSymbol.Virtual, EClassCall]): Option[EClassCall] = this match {
    case EClassSymbol.Real(call) => Some(call)
    case virtual: EClassSymbol.Virtual => reification.get(virtual)
  }

  /**
   * Replaces this symbol with an [[EClassSymbol.Real]] if resolvable.
   *
   * - If already real, returns an equivalent [[EClassSymbol.Real]].
   * - If virtual and present in `reification`, returns a new [[EClassSymbol.Real]].
   * - Otherwise, returns `this` unchanged.
   *
   * @param reification Mapping from virtual symbols to concrete calls.
   * @return A real symbol if resolvable, else the original symbol.
   */
  final def refine(reification: Map[EClassSymbol.Virtual, EClassCall]): EClassSymbol = this match {
    case EClassSymbol.Real(call) => EClassSymbol.real(call)
    case virtual: EClassSymbol.Virtual => reification.get(virtual) match {
      case Some(call) => EClassSymbol.real(call)
      case None => virtual
    }
  }
}

/**
 * Constructors and concrete types for [[EClassSymbol]].
 */
object EClassSymbol {

  /**
   * Concrete reference to an e-class already in the e-graph.
   *
   * @param call The existing e-class call.
   */
  final case class Real(call: EClassCall) extends EClassSymbol

  /**
   * Placeholder reference for an e-class not yet added to the e-graph.
   *
   * Virtual symbols allow [[Command]] instances to describe edits that will
   * produce new e-classes, without knowing their final IDs or calls in advance.
   */
  final class Virtual extends EClassSymbol

  /**
   * Creates a fresh [[Virtual]] symbol.
   */
  def virtual(): Virtual = new Virtual

  /**
   * Wraps an [[EClassCall]] in a [[Real]] symbol.
   */
  def real(call: EClassCall): Real = Real(call)
}
