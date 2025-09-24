package foresight.eqsat.saturation

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.rewriting.PortableMatch
import foresight.eqsat._

/**
 * An e-graph that records the set of matches that have been applied to it.
 *
 * @param egraph The e-graph.
 * @param applied The set of matches that have been applied to the e-graph.
 * @tparam Node The type of the nodes described by the e-nodes in the e-graph.
 * @tparam Repr The type of the underlying e-graph.
 * @tparam Match The type of the matches that have been applied to the e-graph.
 */
final case class EGraphWithRecordedApplications[Node, Repr <: EGraphLike[Node, Repr] with EGraph[Node], Match <: PortableMatch[Node, Match]](egraph: Repr,
                                                                                                                                             applied: Map[String, Set[Match]])
  extends EGraphLike[Node, EGraphWithRecordedApplications[Node, Repr, Match]] with EGraph[Node] {

  /**
   * Migrates this e-graph to a new representation, preserving the recorded match applications.
   *
   * @param newEgraph The new e-graph representation to migrate to.
   * @tparam NewRepr The type of the new e-graph representation.
   * @return A new e-graph with the same recorded applications.
   */
  def migrateTo[NewRepr <: EGraphLike[Node, NewRepr] with EGraph[Node]](newEgraph: NewRepr): EGraphWithRecordedApplications[Node, NewRepr, Match] = {
    EGraphWithRecordedApplications(newEgraph, applied)
  }

  override def canonicalizeOrNull(ref: EClassRef): EClassCall = egraph.canonicalizeOrNull(ref)
  override def canonicalize(node: ENode[Node]): ShapeCall[Node] = egraph.canonicalize(node)
  override def classes: Iterable[EClassRef] = egraph.classes
  override def nodes(call: EClassCall): Iterable[ENode[Node]] = egraph.nodes(call)
  override def users(ref: EClassRef): Set[ENode[Node]] = egraph.users(ref)
  override def findOrNull(node: ENode[Node]): EClassCall = egraph.findOrNull(node)
  override def areSame(first: EClassCall, second: EClassCall): Boolean = egraph.areSame(first, second)

  override def tryAddMany(nodes: Seq[ENode[Node]],
                          parallelize: ParallelMap): (Seq[AddNodeResult], EGraphWithRecordedApplications[Node, Repr, Match]) = {
    val (results, newEgraph) = egraph.tryAddMany(nodes, parallelize)

    // Construct a new EGraphWithRecordedApplications with the new e-graph and the same applied matches. The applied
    // matches do not need to be updated because they are not affected by adding new nodes to the graph.
    (results, EGraphWithRecordedApplications(newEgraph, applied))
  }

  override def unionMany(pairs: Seq[(EClassCall, EClassCall)],
                         parallelize: ParallelMap): (Set[Set[EClassCall]], EGraphWithRecordedApplications[Node, Repr, Match]) = {
    val (newClasses, newEgraph) = egraph.unionMany(pairs, parallelize)

    // Construct a new EGraphWithAppliedMatches with the new e-graph and the same applied matches. The applied matches
    // need to be updated because they may be affected by the union operation.
    val portedApplications = parallelize[(String, Set[Match]), (String, Set[Match])](applied, {
      case (ruleName, matches) =>
        ruleName -> matches.map(_.port(newEgraph))
    }).toMap
    (newClasses, EGraphWithRecordedApplications(newEgraph, portedApplications))
  }

  /**
   * Gets the set of matches that have been applied to the e-graph.
   * @param ruleName The name of the rule.
   * @return The set of matches that have been applied to the e-graph.
   */
  def applications(ruleName: String): Set[Match] = applied.getOrElse(ruleName, Set.empty)

  /**
   * Records that a set of matches have been applied to the e-graph.
   * @param appliedMatches The match applications to record.
   * @return The e-graph with the recorded match applications.
   */
  def record(appliedMatches: Map[String, Set[Match]]): EGraphWithRecordedApplications[Node, Repr, Match] = {
    EGraphWithRecordedApplications(egraph, applied ++ appliedMatches.map {
      case (ruleName, newApplications) => ruleName -> (applications(ruleName) ++ newApplications)
    })
  }

  override def emptied: EGraphWithRecordedApplications[Node, Repr, Match] = {
    EGraphWithRecordedApplications(egraph.emptied)
  }
}

/**
 * The companion object for the [[EGraphWithRecordedApplications]] class.
 */
object EGraphWithRecordedApplications {
  /**
   * Constructs an e-graph with no applied matches.
   * @param egraph The e-graph.
   * @tparam Node The type of the nodes described by the e-nodes in the e-graph.
   * @tparam Repr The type of the underlying e-graph.
   * @tparam Match The type of the matches that have been applied to the e-graph.
   * @return The e-graph with no applied matches.
   */
  def apply[Node, Repr <: EGraphLike[Node, Repr] with EGraph[Node], Match <: PortableMatch[Node, Match]](egraph: Repr): EGraphWithRecordedApplications[Node, Repr, Match] = {
    EGraphWithRecordedApplications(egraph, Map())
  }
}
