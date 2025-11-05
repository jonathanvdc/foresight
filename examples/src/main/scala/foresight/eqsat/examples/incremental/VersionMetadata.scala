package foresight.eqsat.examples.incremental

import foresight.eqsat.immutable.Metadata
import foresight.eqsat.{EClassCall, EClassRef, ENode, MixedTree}
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
 * @tparam NodeT The type of the nodes in the e-graph.
 */
trait VersionMetadata[NodeT] {
  /**
   * Checks if the given e-class is at the latest version.
   * @param eclass The e-class to check.
   * @return true if the e-class is at the latest version, false otherwise.
   */
  def isLatestVersion(eclass: EClassRef): Boolean

  /**
   * Retrieves all e-classes that are at the latest version.
   * @return An iterable of e-classes at the latest version.
   */
  def latestVersionClasses: Iterable[EClassRef]
}
