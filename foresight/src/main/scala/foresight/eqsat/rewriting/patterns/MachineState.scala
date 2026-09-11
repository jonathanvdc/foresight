package foresight.eqsat.rewriting.patterns

import foresight.eqsat.{CallTree, EClassCall, ENode, Slot}
import foresight.util.collections.ArrayMap

import scala.collection.compat.immutable.ArraySeq

/**
 * The state of a pattern machine.
 *
 * @param registers The registers of the machine.
 * @param boundVars The variables that are bound in the machine.
 * @param boundSlots The slots that are bound in the machine.
 * @param boundNodes The nodes that are bound in the machine.
 * @tparam NodeT The type of the nodes in the e-graph.
 */
final case class MachineState[NodeT](registers: ArraySeq[EClassCall],
                                     boundVars: ArrayMap[Pattern.Var, CallTree[NodeT]],
                                     boundSlots: ArrayMap[Slot, Slot],
                                     boundNodes: ArraySeq[ENode[NodeT]])
