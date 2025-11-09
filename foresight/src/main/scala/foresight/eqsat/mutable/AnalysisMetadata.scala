package foresight.eqsat.mutable

import foresight.eqsat.collections.SlotMap
import foresight.eqsat.metadata.Analysis
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.readonly
import foresight.eqsat.readonly.{EGraph => ReadOnlyEGraph}
import foresight.eqsat.{EClassCall, EClassRef, ENode}

import scala.collection.mutable.HashMap

/**
 * Incremental, queryable results of running an [[Analysis]] over an e-graph.
 *
 * This is a concrete [[Metadata]] implementation that:
 *   - Stores per-[[EClassRef]] results in `results`.
 *   - Answers analysis queries for canonicalized e-class applications and mixed trees.
 *   - Stays in sync by reacting to batched node insertions ([[onAddMany]]) and unions ([[onUnionMany]]).
 *
 * Instances are functional: updates return a new [[AnalysisMetadata]] and leave the receiver usable.
 *
 * @param analysis The analysis whose results are stored here.
 * @tparam NodeT   IR node type carried by the e-graph.
 * @tparam A       Analysis result type.
 */
final class AnalysisMetadata[NodeT, A](override val analysis: Analysis[NodeT, A])
  extends Metadata[NodeT] with readonly.AnalysisMetadata[NodeT, A] {

  private val results: HashMap[EClassRef, A] = HashMap.empty

  override def apply(call: EClassRef): A = results(call)

  override def onAddMany(added: Seq[(ENode[NodeT], EClassCall)],
                         after: ReadOnlyEGraph[NodeT],
                         parallelize: ParallelMap): Unit = {

    for ((node, call) <- added) {
      val canonicalizedNode = after.canonicalize(node)
      // Build specialization: args map to the call's slots; definitions get fresh generic slots.
      val renaming = call.args.inverse.concat(SlotMap.bijectionFromSetToFresh(canonicalizedNode.definitions.toSet))
      val genericNode = canonicalizedNode.rename(renaming).asNode
      val args = genericNode.args.map(applyPrecanonicalized)
      val result = analysis.make(genericNode, args)
      results(call.ref) = result
    }
  }

  /**
   * Update results after a batch of e-class unions.
   *
   * For each disjoint equivalence group in `equivalences`:
   *   1. Choose the representative class (as determined by `after`).
   *   2. Merge results from the other classes into the representative using [[Analysis.join]].
   *   3. Drop results for non-representative classes from the map.
   *   4. Enqueue affected nodes/classes for reprocessing.
   *
   * Then, propagate to a fixpoint using an [[AnalysisUpdater]] worklist so that any nodes whose
   * inputs changed are recomputed consistently.
   *
   * @param equivalences Disjoint groups of classes that became canonical equivalents.
   * @param after        The post-union e-graph.
   * @return A new [[AnalysisMetadata]] consistent with the unified state.
   */
  def onUnionMany(equivalences: Set[Set[EClassCall]], after: ReadOnlyEGraph[NodeT]): Unit = {
    val updater = new AnalysisUpdater(analysis, after, results)

    // First, merge class-level results within each equivalence group.
    for (equiv <- equivalences) {
      val representative = after.canonicalize(equiv.head.ref).ref
      val others = equiv.filterNot(_.ref == representative)
      val result = others.foldLeft(updater.results(representative))((acc, call) => analysis.join(acc, updater.apply(call)))

      // Drop merged classes and write back the representative.
      updater.results --= others.map(_.ref)
      updater.update(representative, result)
    }

    // Propagate changes to a fixpoint.
    updater.processPending()
  }

  /**
   * Create an empty metadata instance for an empty e-graph.
   *
   * The same [[analysis]] instance is retained, but all class results are cleared.
   */
  override def emptied: Metadata[NodeT] = {
    new AnalysisMetadata(analysis)
  }
}

/**
 * Factory for [[AnalysisMetadata]].
 */
object AnalysisMetadata {
  /**
   * Evaluate an analysis over an entire e-graph and produce its metadata snapshot.
   *
   * Evaluation strategy:
   *   1. Seed: For each e-class, compute results for **nullary** nodes (`args.isEmpty`) and
   *      initialize per-class state.
   *   2. Propagate: Use a worklist to process remaining nodes whose argument results become available,
   *      repeatedly applying [[Analysis.make]] and merging with [[Analysis.join]] until no changes occur.
   *
   * Termination relies on `join` being monotone and idempotent w.r.t. the underlying partial order.
   *
   * @param analysis The analysis to evaluate.
   * @param egraph   The e-graph to analyze.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam A     The analysis result type.
   * @return [[AnalysisMetadata]] capturing the per-class results for this analysis.
   */
  def compute[NodeT, A](analysis: Analysis[NodeT, A],
                        egraph: EGraph[NodeT]): AnalysisMetadata[NodeT, A] = {
    val result = new AnalysisMetadata[NodeT, A](analysis)

    import scala.collection.mutable

    // Worklist of nodes that are READY (all arg classes have results).
    val nodeQueue = new mutable.Queue[ENode[NodeT]]()
    // Tracks nodes currently pending in the queue; cleared on dequeue to allow re-enqueue after deps improve.
    val enqueued = new mutable.HashSet[ENode[NodeT]]()

    // Fast contains on class results.
    @inline def hasClass(ref: EClassRef): Boolean =
      result.results.contains(ref)

    // Read a class result (must exist).
    @inline def getClass(ref: EClassRef): A =
      result.results(ref)

    // Compute A for an EClassCall using already-available class result.
    @inline def evalCall(call: EClassCall): A =
      analysis.rename(getClass(call.ref), call.args)

    // Merge a nodeResult into its class; return true if the class improved.
    def mergeIntoClass(ref: EClassRef, nodeResult: A): Boolean = {
      result.results.get(ref) match {
        case Some(old) =>
          val joined = analysis.join(old, nodeResult)
          if (joined.asInstanceOf[AnyRef] ne old.asInstanceOf[AnyRef]) { // fast-path ref compare
            if (joined != old) { // fallback equals in case A is value-based
              result.results(ref) = joined
              true
            } else false
          } else false
        case None =>
          result.results(ref) = nodeResult
          true
      }
    }

    // When a class improves, try to enqueue its user nodes that are now fully ready.
    def onClassImproved(ref: EClassRef): Unit = {
      val users = egraph.users(ref)
      for (n <- users) {
        if (!enqueued.contains(n)) {
          // A node is ready if all its argument classes already have results.
          val ready = {
            var ok = true
            var ai = 0
            val as = n.args
            while (ok && ai < as.length) {
              ok = hasClass(as(ai).ref)
              ai += 1
            }
            ok
          }
          if (ready) {
            enqueued += n
            nodeQueue.enqueue(n)
          }
        }
      }
    }

    // Seed step: evaluate all nullary nodes and propagate their classes.
    // Also opportunistically enqueue any users that become ready.
    {
      val classes = egraph.classes
      for (c <- classes) {
        val canon = egraph.canonicalize(c)
        val nodes = egraph.nodes(canon)
        for (n <- nodes) {
          if (n.args.isEmpty) {
            val nodeResult = analysis.make(n, Seq.empty)
            if (mergeIntoClass(c, nodeResult)) {
              onClassImproved(c)
            }
          }
        }
      }
    }

    // Main loop: process ready nodes once.
    while (nodeQueue.nonEmpty) {
      val n = nodeQueue.dequeue()
      // Mark as no longer pending so that future upstream improvements can re-enqueue it.
      enqueued -= n
      // All arg classes are available by construction.
      val nodeResult = analysis.make(n, n.args.map(evalCall))
      val cls = egraph.find(n).get.ref
      if (mergeIntoClass(cls, nodeResult)) {
        onClassImproved(cls)
      }
    }

    result
  }
}
