package fixpoint.eqsat.commands

import fixpoint.eqsat.EClassCall

/**
 * A symbolic e-class application.
 */
sealed trait EClassSymbol

/**
 * A real e-class application that is already in the graph.
 * @param call The e-class call.
 */
final case class RealEClassSymbol(call: EClassCall) extends EClassSymbol

/**
 * A virtual e-class application, referring to a not-yet-added e-class.
 */
final class VirtualEClassSymbol extends EClassSymbol
