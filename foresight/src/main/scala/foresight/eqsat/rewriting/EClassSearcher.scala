package foresight.eqsat.rewriting

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.EClassCall
import foresight.eqsat.readonly.EGraph
import foresight.util.collections.StrictMapOps.toStrictMapOps

import java.util.concurrent.atomic.AtomicIntegerArray
import scala.collection.compat.immutable.ArraySeq

/**
 * A searcher that searches for matches within individual e-classes.
 *
 * @tparam NodeT     Node payload type.
 * @tparam MatchT    The match type emitted by this searcher.
 * @tparam EGraphT   Concrete e-graph type (must mix in [[EGraphLike]] and [[EGraph]]).
 */
trait EClassSearcher[
  NodeT,
  MatchT,
  EGraphT <: EGraph[NodeT]
] extends Searcher[NodeT, MatchT, EGraphT]
  with SearcherLike[NodeT, MatchT, EGraphT, EClassSearcher[NodeT, MatchT, EGraphT]] {

  /**
   * Searches for matches within a single e-class
   *
   * Implementations can assume `call` refers to the class's canonical representative.
   *
   * @param call         Canonical e-class application to search within.
   * @param egraph       Read-only e-graph snapshot.
   * @param continuation Continuation to call with each match found.
   */
  protected def search(call: EClassCall, egraph: EGraphT, continuation: Continuation): Unit

  /**
   * Searches for matches within a single e-class
   * @param call        Canonical e-class application
   * @param egraph      Read-only e-graph snapshot
   */
  final def search(call: EClassCall, egraph: EGraphT): Unit = {
    search(call, egraph, continuation)
  }

  private[eqsat] def searchWithContinuation(call: EClassCall, egraph: EGraphT, cont: Continuation): Unit = {
    search(call, egraph, cont)
  }

  /**
   * Describes which e-classes to search in the e-graph. By default, searches all e-classes.
   * Implementations can override this to customize which classes are searched.
   */
  def classesToSearch: EClassesToSearch[EGraphT] = EClassesToSearch.all

  /**
   * Searches for matches within multiple e-classes in parallel.
   *
   * Implementations can assume each `call` refers to the class's canonical representative.
   * @param calls       Canonical e-class applications to search within.
   * @param egraph      Read-only e-graph snapshot.
   * @param parallelize Parallel map implementation to use for distributing work.
   */
  final def search(calls: ArraySeq[EClassCall], egraph: EGraphT, parallelize: ParallelMap): Unit = {
    EClassSearcher.searchMultiple(ArraySeq(this), calls, egraph, parallelize)
  }

  final override def search(egraph: EGraphT, parallelize: ParallelMap): Unit = {
    search(classesToSearch(egraph), egraph, parallelize)
  }
}

/**
 * Companion object for [[EClassSearcher]] containing common constants.
 */
private[eqsat] object EClassSearcher {
  /**
   * The block size to use when parallelizing searches over e-classes.
   */
  final val blockSize: Int = 64

  /**
   * The threshold size (in number of e-classes) below which we consider an e-graph "small"
   * and may choose simpler strategies that have a lower constant overhead.
   */
  final val smallEGraphThreshold: Int = 4 * blockSize

  /**
   * Searches for matches within multiple e-classes in parallel.
   *
   * Implementations can assume each `call` refers to the class's canonical representative.
   *
   * @param calls       Canonical e-class applications to search within.
   * @param egraph      Read-only e-graph snapshot.
   * @param parallelize Parallel map implementation to use for distributing work.
   */
  def searchMultiple[
    NodeT,
    MatchT,
    EGraphT <: EGraph[NodeT]
  ](searchers: ArraySeq[EClassSearcher[NodeT, MatchT, EGraphT]],
    calls: ArraySeq[EClassCall],
    egraph: EGraphT,
    parallelize: ParallelMap): Unit = {

    object HaltSearchException extends Throwable

    // Initialize all searchers as still searching
    val stillSearching = new AtomicIntegerArray(searchers.size)
    for (i <- 0 until stillSearching.length()) {
      stillSearching.set(i, 1)
    }

    val cont = (i: Int) => (m: MatchT, e: EGraphT) => {
      if (!searchers(i).continuation(m, e)) {
        // If this searcher is halting, mark it as such. If all searchers are halting, throw to stop the search.
        stillSearching.set(i, 0)

        var allHalted = true
        for (j <- 0 until stillSearching.length()) {
          if (stillSearching.get(j) != 0) {
            allHalted = false
          }
        }
        if (allHalted) {
          throw HaltSearchException
        }
      }
      true
    }

    val searchClass = (call: EClassCall) => {
      for (i <- searchers.indices) {
        if (stillSearching.get(i) != 0) {
          searchers(i).search(call, egraph, cont(i))
        }
      }
    }

    try {
      parallelize.processBlocks(calls, EClassSearcher.blockSize, searchClass)
    } catch {
      case HaltSearchException => // Swallow the exception to halt the search early
    }
  }

  /**
   * Result of partitioning rules by shared EClassesToSearch instances.
   *
   * @param rulesPerSharedEClassToSearch Map from shared EClassesToSearch instances to the rules that use them.
   * @param regularRules                  Rules that either don't use EClassSearcher or use unique EClassesToSearch.
   */
  final case class PartitionedRules[
    NodeT,
    MatchT,
    EGraphT <: EGraph[NodeT]
  ](
     rulesPerSharedEClassToSearch: Map[EClassesToSearch[EGraphT], Seq[Rewrite[NodeT, MatchT, EGraphT]]],
     regularRules: Seq[Rewrite[NodeT, MatchT, EGraphT]]
   )

  /**
   * Partitions a sequence of rules into those that share EClassesToSearch instances and those that do not.
   *
   * This is useful for optimizing rule application by grouping rules that search the same e-classes.
   * @param rules Sequence of rules to partition.
   * @tparam NodeT   Node payload type.
   * @tparam MatchT  Match element type.
   * @tparam EGraphT E-graph type.
   * @return A [[PartitionedRules]] instance containing the partitioned rules.
   */
  def partitionRulesBySharedEClassesToSearch[
    NodeT,
    MatchT,
    EGraphT <: EGraph[NodeT]
  ](rules: Seq[Rewrite[NodeT, MatchT, EGraphT]]): PartitionedRules[NodeT, MatchT, EGraphT] = {
    // Collect the EClassesToSearch instances from all EClassSearcher rules.
    val eclassesToSearchPerRule = rules.collect {
      case Rule(name, searcher: EClassSearcher[NodeT, MatchT, _], _) =>
        name -> searcher.classesToSearch
    }.toMap

    // Count how many times each unique EClassesToSearch instance is used across all rules.
    val usageCounts =
      eclassesToSearchPerRule
        .values
        .groupBy(identity) // groups by unique EClassesToSearch instance
        .mapValuesStrict(_.size) // count how many times each one appears

    // For shared EClassesToSearch, group the corresponding rules together.
    val rulesPerSharedEClassToSearch =
      usageCounts
        .filter { case (_, count) => count > 1 }
        .keys
        .map { eclassesToSearch =>
          eclassesToSearch -> rules.collect {
            case rule if eclassesToSearchPerRule.get(rule.name).contains(eclassesToSearch) =>
              rule
          }
        }.toMap

    // Rules that either don't use EClassSearcher or use a unique EClassesToSearch.
    val regularRules: Seq[Rewrite[NodeT, MatchT, EGraphT]] =
      rules.filter { rule =>
        !eclassesToSearchPerRule.contains(rule.name) ||
          usageCounts(eclassesToSearchPerRule(rule.name)) == 1
      }

    PartitionedRules(rulesPerSharedEClassToSearch, regularRules)
  }
}
