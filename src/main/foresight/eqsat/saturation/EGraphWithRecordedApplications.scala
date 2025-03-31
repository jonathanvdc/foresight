package foresight.eqsat.saturation

import foresight.eqsat.{EClassCall, EClassRef, EGraph, EGraphLike, ENode, ShapeCall}
import foresight.eqsat.rewriting.PortableMatch

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

  override def tryCanonicalize(ref: EClassRef): Option[EClassCall] = egraph.tryCanonicalize(ref)
  override def canonicalize(node: ENode[Node]): ShapeCall[Node] = egraph.canonicalize(node)
  override def classes: Iterable[EClassRef] = egraph.classes
  override def nodes(call: EClassCall): Set[ENode[Node]] = egraph.nodes(call)
  override def users(ref: EClassRef): Set[ENode[Node]] = egraph.users(ref)
  override def find(node: ENode[Node]): Option[EClassCall] = egraph.find(node)
  override def areSame(first: EClassCall, second: EClassCall): Boolean = egraph.areSame(first, second)

  override def add(node: ENode[Node]): (EClassCall, EGraphWithRecordedApplications[Node, Repr, Match]) = {
    val (ref, newEgraph) = egraph.add(node)
    // Construct a new EGraphWithAppliedMatches with the new e-graph and the same applied matches. The applied matches
    // do not need to be updated because they are not affected by adding a new node to the graph.
    (ref, EGraphWithRecordedApplications(newEgraph, applied))
  }

  override def unionMany(pairs: Seq[(EClassCall, EClassCall)]): (Set[Set[EClassCall]], EGraphWithRecordedApplications[Node, Repr, Match]) = {
    val (newClasses, newEgraph) = egraph.unionMany(pairs)
    // Construct a new EGraphWithAppliedMatches with the new e-graph and the same applied matches. The applied matches
    // need to be updated because they may be affected by the union operation.
    (newClasses, EGraphWithRecordedApplications(newEgraph, applied.mapValues(_.map(_.port(newEgraph)).view.force)))
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
