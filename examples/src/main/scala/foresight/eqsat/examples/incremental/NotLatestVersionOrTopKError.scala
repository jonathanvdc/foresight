package foresight.eqsat.examples.incremental

import foresight.eqsat.{EClassCall, ENode}
import foresight.eqsat.readonly.EGraph
import foresight.eqsat.rewriting.patterns.MachineError

final case class NotLatestVersionOrTopKError[NodeT, EGraphT <: EGraph[NodeT], C](instruction: LatestVersionOrTopKInstruction[NodeT, EGraphT, C],
                                                                                 node: ENode[NodeT],
                                                                                 eclass: EClassCall)
  extends MachineError[NodeT]
