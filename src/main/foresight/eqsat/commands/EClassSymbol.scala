package foresight.eqsat.commands

import foresight.eqsat.EClassCall

/**
 * A symbolic e-class application.
 */
sealed trait EClassSymbol {
  /**
   * Checks if the e-class symbol is a concrete e-class application.
   * @return A Boolean value indicating whether the e-class symbol is a real e-class application.
   */
  final def isReal: Boolean = this match {
    case EClassSymbol.Real(_) => true
    case _: EClassSymbol.Virtual => false
  }

  /**
   * Checks if the e-class symbol is a virtual e-class application.
   * @return A Boolean value indicating whether the e-class symbol is a virtual e-class application.
   */
  final def isVirtual: Boolean = !isReal

  /**
   * Reifies the e-class symbol using the given reification.
   * @param reification A map from virtual e-class symbols to e-class calls.
   * @return The e-class call that the e-class symbol represents.
   */
  final def reify(reification: Map[EClassSymbol.Virtual, EClassCall]): EClassCall = this match {
    case EClassSymbol.Real(call) => call
    case virtual: EClassSymbol.Virtual => reification(virtual)
  }

  /**
   * Tries to reify the e-class symbol using the given reification.
   * @param reification A map from virtual e-class symbols to e-class calls.
   * @return The e-class call that the e-class symbol represents, if it is a real e-class symbol or a virtual symbol
   *         that is in the reification map. Otherwise, `None`.
   */
  final def tryReify(reification: Map[EClassSymbol.Virtual, EClassCall]): Option[EClassCall] = this match {
    case EClassSymbol.Real(call) => Some(call)
    case virtual: EClassSymbol.Virtual => reification.get(virtual)
  }

  /**
   * Refines the e-class symbol using the given reification.
   * @param reification A map from virtual e-class symbols to e-class calls.
   * @return The real e-class symbol that the e-class symbol represents, if one exists in the reification map. Otherwise,
   *         the original e-class symbol.
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
 * A companion object for e-class symbols.
 */
object EClassSymbol {
  /**
   * A real e-class application that is already in the graph.
   * @param call The e-class call.
   */
  final case class Real(call: EClassCall) extends EClassSymbol

  /**
   * A virtual e-class application, referring to a not-yet-added e-class.
   */
  final class Virtual extends EClassSymbol

  /**
   * Creates a new virtual e-class symbol.
   * @return A new virtual e-class symbol.
   */
  def virtual(): Virtual = new Virtual

  /**
   * Creates a new real e-class symbol.
   * @param call The e-class call.
   * @return A new real e-class symbol.
   */
  def real(call: EClassCall): Real = Real(call)
}
