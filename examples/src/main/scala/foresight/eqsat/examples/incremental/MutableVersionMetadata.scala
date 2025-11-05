package foresight.eqsat.examples.incremental

import foresight.eqsat.mutable.Metadata
import foresight.eqsat.{EClassCall, EClassRef, ENode, MixedTree}
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.readonly.EGraph

import scala.collection.compat.immutable.ArraySeq
import scala.collection.mutable.HashMap

/**
 * Metadata that tracks a global version number for the e-graph and a version number for each e-class.
 * The global version number is incremented whenever new terms are added to the e-graph.
 * Each e-class' version number is updated to the current global version number when a new term
 * containing that e-class is added to the e-graph.
 *
 * This implements the versioning scheme described in "Incremental Equality Saturation"
 * by Soi, Driscoll, Wang, and Aiken. EGRAPHS 2025.
 *
 * @param version The current global version number of the e-graph.
 * @tparam NodeT The type of the nodes in the e-graph.
 */
final class MutableVersionMetadata[NodeT] private(version: Int)
  extends Metadata[NodeT] with VersionMetadata[NodeT] {

  private val data = HashMap.empty[EClassRef, Int]

  /**
   * Retrieves all e-classes that are at the latest version.
   * @return An iterable of e-classes at the latest version.
   */
  def latestVersionClasses: Iterable[EClassRef] = {
    data.collect { case (eclass, v) if v == version => eclass }
  }

  /**
   * Checks if the given e-class is at the latest version.
   * @param eclass The e-class to check.
   * @return true if the e-class is at the latest version, false otherwise.
   */
  def isLatestVersion(eclass: EClassRef): Boolean = {
    data.getOrElse(eclass, -1) == version
  }

  /**
   * Handles the addition of a new term to the e-graph. Increments the global version number
   * and updates the version of all e-classes in the term to the new global version number.
   * @param term The newly added term.
   * @param egraph The e-graph to which the term was added.
   * @return An updated [[VersionMetadata]] with the new global version and updated e-class versions.
   */
  def onNewTermAdded(term: MixedTree[NodeT, EClassCall], egraph: EGraph[NodeT]): Unit = {
    // Increment the global version number
    val newVersion = version + 1

    // Update the version of all e-classes in the term to the new global version
    val eclassesInTerm = findEClassesInTerm(term, egraph)
    for (eclassRef <- eclassesInTerm._2) {
      data.update(eclassRef, newVersion)
    }
  }

  override def onAddMany(added: Seq[(ENode[NodeT], EClassCall)],
                         after: EGraph[NodeT],
                         parallelize: ParallelMap): Unit = {

    for ((_, eclass) <- added) {
      data.update(eclass.ref, version)
    }
  }

  override def onUnionMany(equivalences: Set[Set[EClassCall]], after: EGraph[NodeT]): Unit = {
    for (eqClassCalls <- equivalences) {
      // Find the canonical representative of the equivalence class after the unions
      val canonical = after.canonicalize(eqClassCalls.head)

      // Get the minimum version among all classes in the equivalence group
      val minVersion = eqClassCalls.flatMap(ec => data.get(ec.ref)).fold(version)(math.min)

      // Discard all classes except for the canonical representative
      // and update the representative's version to the minimum version found
      for (ec <- eqClassCalls) {
        if (ec.ref == canonical.ref) {
          data.update(ec.ref, minVersion)
        } else {
          data.remove(ec.ref)
        }
      }
    }
  }

  override def emptied: MutableVersionMetadata[NodeT] = {
    new MutableVersionMetadata(version)
  }

  private def findEClassesInTerm(term: MixedTree[NodeT, EClassCall], egraph: EGraph[NodeT]): (EClassCall, Set[EClassRef]) = {
    term match {
      case MixedTree.Atom(call: EClassCall) =>
        call -> Set(egraph.canonicalize(call).ref)
      case MixedTree.Node(n: NodeT, defs, uses, args: Seq[MixedTree[NodeT, EClassCall]]) =>
        val argCallsBuf = ArraySeq.newBuilder[EClassCall]
        val argEClassesSet = Set.newBuilder[EClassRef]
        for (arg <- args) {
          val (ac, ecs) = findEClassesInTerm(arg, egraph)
          argCallsBuf += ac
          argEClassesSet ++= ecs
        }
        val argCalls = argCallsBuf.result()
        val eNode = ENode(n, defs, uses, argCalls)
        val eClassCall = egraph.find(eNode).getOrElse(throw new IllegalStateException("Node in term not found in e-graph"))
        argEClassesSet += egraph.canonicalize(eClassCall).ref
        eClassCall -> argEClassesSet.result()
    }
  }
}

object MutableVersionMetadata {
  def empty[NodeT]: MutableVersionMetadata[NodeT] = new MutableVersionMetadata(0)
}
