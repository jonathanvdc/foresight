package foresight.eqsat.examples.incremental

import foresight.eqsat.{EClassCall, ENode}
import foresight.eqsat.immutable.{EGraph, EGraphLike}
import foresight.eqsat.rewriting.patterns.MachineError

final case class NotLatestVersionOrTopKError[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT], C](instruction: LatestVersionOrTopKInstruction[NodeT, EGraphT, C],
                                                                                                                 node: ENode[NodeT],
                                                                                                                 eclass: EClassCall)
  extends MachineError[NodeT]
