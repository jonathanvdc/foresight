package foresight.util

import foresight.eqsat.lang.{AtomDecoder, Language, LanguageOp}
import foresight.eqsat.readonly.ReadOnlyEGraph
import foresight.eqsat.{EClassCall, EClassRef, MixedTree}

import java.nio.file.{Files, Paths}

/**
 * A utility class for rendering e-graphs in DOT format.
 *
 * At some point, this class may take configuration options as parameters for styling the output.
 * This implementation ignores slots for now.
 */
final case class DotPrinter[E]()(using L: Language[E]) {

  private def print(egraph: ReadOnlyEGraph[LanguageOp[E]])(using dec: AtomDecoder[E, EClassCall]): String = {
    val sb = new StringBuilder
    sb.append("digraph EGraph {\n")
    sb.append("  compound=true\n")
    sb.append("  clusterrank=local\n")

    // All e-class clusters with e-nodes
    val clsRefs = egraph.classes
    val clsRefToId = clsRefs.zipWithIndex.toMap

    for ((clsRef, clsId) <- clsRefToId) {
      val clsCall = egraph.canonicalize(clsRef)

      val nodes = egraph.nodes(clsCall)
      val nodesToId = nodes.zipWithIndex.toMap

      sb.append(s"  subgraph cluster_$clsId {\n")
      sb.append(s"    style=dotted;\n")

      for ((node, nodeId) <- nodesToId) {
        val nodeTag = s"node_${clsId}_$nodeId"
        val mixedTree = MixedTree.fromENode(node)
        val expr = L.fromTree[EClassCall](mixedTree)
        sb.append(s"    $nodeTag [label=\"${expr.getClass.getSimpleName}\"];\n")
      }

      sb.append("  }\n")
    }

    for ((clsRef, clsId) <- clsRefToId) {
      val clsCall = egraph.canonicalize(clsRef)

      val nodes = egraph.nodes(clsCall)
      val nodesToId = nodes.zipWithIndex.toMap

      for ((node, nodeId) <- nodesToId) {
        val nodeTag = s"node_${clsId}_$nodeId"

        for (childClsCall <- node.args) {
          val childClsRef = childClsCall.ref
          val childClsId = clsRefToId(childClsRef)

          sb.append(s"    $nodeTag -> node_${childClsId}_0 [lhead=cluster_$childClsId];\n")
        }
      }
    }

    sb.append("}\n")
    sb.toString()
  }

  def apply(egraph: ReadOnlyEGraph[LanguageOp[E]], filename: String)(using dec: AtomDecoder[E, EClassCall]): Unit = {
    val dotRepr = print(egraph)
    Files.writeString(Paths.get(filename), dotRepr)
  }
}
