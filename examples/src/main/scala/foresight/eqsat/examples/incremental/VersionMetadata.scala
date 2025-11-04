package foresight.eqsat.examples.incremental

import foresight.eqsat.{EClassCall, EClassRef, ENode, MixedTree}
import foresight.eqsat.metadata.Metadata
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.readonly.EGraph

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
 * @param data The per-e-class version mapping.
 * @tparam NodeT The type of the nodes in the e-graph.
 */
final case class VersionMetadata[NodeT] private(version: Int,
                                                private val data: Map[EClassRef, Int])
  extends Metadata[NodeT, VersionMetadata[NodeT]] {

  private lazy val _latestVersionClasses = {
    data.collect { case (eclass, v) if v == version => eclass }
  }

  /**
   * Retrieves all e-classes that are at the latest version.
   * @return An iterable of e-classes at the latest version.
   */
  def latestVersionClasses: Iterable[EClassRef] = _latestVersionClasses

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
  def onNewTermAdded(term: MixedTree[NodeT, EClassCall], egraph: EGraph[NodeT]): VersionMetadata[NodeT] = {
    // Increment the global version number
    val newVersion = version + 1

    // Update the version of all e-classes in the term to the new global version
    val eclassesInTerm = findEClassesInTerm(term, egraph)
    val updatedData = eclassesInTerm._2.foldLeft(data) { (currentData, eclassRef) =>
      currentData + (eclassRef -> newVersion)
    }

    VersionMetadata(newVersion, updatedData)
  }

  override def onAddMany(added: Seq[(ENode[NodeT], EClassCall)],
                         after: EGraph[NodeT],
                         parallelize: ParallelMap): VersionMetadata[NodeT] = {
    VersionMetadata(
      version,
      data ++ added.map { case (_, eclass) => eclass.ref -> version }
    )
  }

  override def onUnionMany(equivalences: Set[Set[EClassCall]], after: EGraph[NodeT]): VersionMetadata[NodeT] = {
    val updatedData = equivalences.foldLeft(data) { (currentData, eqClassCalls) =>
      // Find the canonical representative of the equivalence class after the unions
      val canonical = after.canonicalize(eqClassCalls.head)

      // Get the minimum version among all classes in the equivalence group
      val minVersion = eqClassCalls.flatMap(ec => currentData.get(ec.ref)).fold(version)(math.min)

      // Discard all classes except for the canonical representative
      // and update the representative's version to the minimum version found
      eqClassCalls.foldLeft(currentData) { (dataAcc, ec) =>
        if (ec.ref == canonical.ref) {
          dataAcc + (ec.ref -> minVersion)
        } else {
          dataAcc - ec.ref
        }
      }
    }

    VersionMetadata(
      version,
      updatedData
    )
  }

  override def emptied: VersionMetadata[NodeT] = {
    VersionMetadata(version, Map.empty)
  }

  private def findEClassesInTerm(term: MixedTree[NodeT, EClassCall], egraph: EGraph[NodeT]): (EClassCall, Set[EClassRef]) = {
    term match {
      case MixedTree.Atom(call: EClassCall) =>
        call -> Set(egraph.canonicalize(call).ref)
      case MixedTree.Node(n: NodeT, defs, uses, args: Seq[MixedTree[NodeT, EClassCall]]) =>
        val (argCalls, argEClasses) = args.map(arg => findEClassesInTerm(arg, egraph)).unzip
        val eNode = ENode(n, defs, uses, argCalls)
        val eClassCall = egraph.find(eNode).getOrElse(throw new IllegalStateException("Node in term not found in e-graph"))
        eClassCall -> (argEClasses.flatten.toSet + egraph.canonicalize(eClassCall).ref)
    }
  }
}

object VersionMetadata {
  def empty[NodeT]: VersionMetadata[NodeT] = VersionMetadata(0, Map.empty)
}
