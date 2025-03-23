package fixpoint.eqsat.commands

import fixpoint.eqsat.EClassCall

/**
 * A symbolic e-class application.
 */
sealed trait EClassSymbol {
  /**
   * Reifies the e-class symbol using the given reification.
   * @param reification A map from virtual e-class symbols to e-class calls.
   * @return The e-class call that the e-class symbol represents.
   */
  def reify(reification: Map[VirtualEClassSymbol, EClassCall]): EClassCall = this match {
    case RealEClassSymbol(call) => call
    case virtual: VirtualEClassSymbol => reification(virtual)
  }
}

/**
 * A companion object for e-class symbols.
 */
object EClassSymbol {
  /**
   * Creates a new virtual e-class symbol.
   * @return A new virtual e-class symbol.
   */
  def virtual(): VirtualEClassSymbol = new VirtualEClassSymbol

  /**
   * Creates a new real e-class symbol.
   * @param call The e-class call.
   * @return A new real e-class symbol.
   */
  def real(call: EClassCall): RealEClassSymbol = RealEClassSymbol(call)
}

/**
 * A real e-class application that is already in the graph.
 * @param call The e-class call.
 */
final case class RealEClassSymbol(call: EClassCall) extends EClassSymbol

/**
 * A virtual e-class application, referring to a not-yet-added e-class.
 */
final class VirtualEClassSymbol extends EClassSymbol
