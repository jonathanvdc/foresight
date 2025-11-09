package foresight.eqsat.commands

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.{AddNodeResult, EClassCall, EClassSymbol, ENode, ENodeSymbol, mutable, immutable}

import java.util
import scala.collection.compat.immutable.ArraySeq

/**
 * A schedule of commands to be executed on an e-graph.
 *
 * Additions are grouped into batches, where all additions in lower-numbered
 * batches are executed before additions in higher-numbered batches. There is
 * no ordering guarantee between additions within the same batch.
 *
 * Unions are executed after all additions have been processed.
 *
 * @tparam NodeT Node type for expressions represented by the e-graph.
 */
final case class CommandSchedule[NodeT](batchZero: (ArraySeq[EClassSymbol.Virtual], ArraySeq[ENode[NodeT]]),
                                        otherBatches: Seq[(ArraySeq[EClassSymbol.Virtual], ArraySeq[ENodeSymbol[NodeT]])],
                                        unions: ArraySeq[(EClassSymbol, EClassSymbol)]
                                       ) {
  /**
   * The additions scheduled in this command schedule, grouped by batch.
   */
  def additions: Seq[(ArraySeq[EClassSymbol.Virtual], ArraySeq[ENodeSymbol[NodeT]])] = {
    batchZero +: otherBatches
  }

  private type ReificationMap = util.IdentityHashMap[EClassSymbol.Virtual, EClassCall]

  private def processAdditionResults(symbols: ArraySeq[EClassSymbol.Virtual],
                                     addResults: ArraySeq[AddNodeResult],
                                     reification: ReificationMap): Boolean = {
    var anyChanges: Boolean = false

    var i = 0
    while (i < symbols.length) {
      val symbol = symbols(i)
      val result = addResults(i)
      reification.put(symbol, result.call)

      result match {
        case AddNodeResult.Added(_) => anyChanges = true
        case AddNodeResult.AlreadyThere(_) => // no change
      }
      i += 1
    }

    anyChanges
  }

  private def applyBatchZero(egraph: mutable.EGraph[NodeT],
                             parallelize: ParallelMap,
                             reification: ReificationMap): Boolean = {
    processAdditionResults(batchZero._1, egraph.tryAddMany(batchZero._2, parallelize), reification)
  }

  private def applyReifiedBatch(egraph: mutable.EGraph[NodeT],
                         batch: (ArraySeq[EClassSymbol.Virtual], ArraySeq[ENodeSymbol[NodeT]]),
                         parallelize: ParallelMap,
                         reification: ReificationMap): Boolean = {
    val reifiedNodes = batch._2.map(_.reify(reification.get))
    processAdditionResults(batch._1, egraph.tryAddMany(reifiedNodes, parallelize), reification)
  }

  private def applyUnions(egraph: mutable.EGraph[NodeT],
                          unions: ArraySeq[(EClassSymbol, EClassSymbol)],
                          parallelize: ParallelMap,
                          reification: ReificationMap): Boolean = {
    val reifiedUnions = unions
      .map { case (l, r) => (l.reify(reification.get), r.reify(reification.get)) }
      .filter { case (l, r) => !egraph.areSame(l, r) }

    if (reifiedUnions.isEmpty) {
      false
    } else {
      egraph.unionMany(reifiedUnions, parallelize)
      true
    }
  }

  /**
   * Executes the scheduled commands against an e-graph.
   *
   * @param egraph
   *   Destination e-graph that will be mutated in place.
   * @param parallelize
   *   Parallelization strategy to label and distribute any internal work.
   * @return `true` if any change occurred, or `false` for a no-op.
   */
  def apply(egraph: mutable.EGraph[NodeT],
            parallelize: ParallelMap): Boolean = {

    val reification = util.IdentityHashMap[EClassSymbol.Virtual, foresight.eqsat.EClassCall]()

    var anyChanges: Boolean = false
    anyChanges = anyChanges | applyBatchZero(egraph, parallelize, reification)
    for (batch <- otherBatches) {
      anyChanges = anyChanges | applyReifiedBatch(egraph, batch, parallelize, reification)
    }

    anyChanges = anyChanges | applyUnions(egraph, unions, parallelize, reification)

    anyChanges
  }

  /**
   * Executes the command schedule against an immutable e-graph.
   *
   * @param egraph
   * Destination e-graph. Implementations may either return it unchanged or produce a new immutable
   * e-graph snapshot.
   * @param reification
   * Mapping from virtual symbols to concrete calls available before this command runs. This is used
   * to ground virtual references present in [[uses]].
   * @param parallelize
   * Parallelization strategy to label and distribute any internal work.
   * @return
   * `Some(newGraph)` if any change occurred, or `None` for a no-op.
   */
  def applyImmutable[
    Repr <: immutable.EGraphLike[NodeT, Repr] with immutable.EGraph[NodeT]
  ](
     egraph: Repr,
     parallelize: ParallelMap
   ): Option[Repr] = {
    val mutableEGraph = mutable.FreezableEGraph[NodeT, Repr](egraph)
    val updated = apply(mutableEGraph, parallelize)
    if (updated) {
      Some(mutableEGraph.freeze())
    } else {
      None
    }
  }
}
