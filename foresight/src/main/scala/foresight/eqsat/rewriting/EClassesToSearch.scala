package foresight.eqsat.rewriting

import foresight.eqsat.EClassCall
import foresight.eqsat.readonly.EGraph
import foresight.util.collections.UnsafeSeqFromArray

import scala.collection.compat.immutable.ArraySeq

/**
 * Strategy object describing which e-classes to search within the given e-graph.
 */
trait EClassesToSearch[-EGraphT] {
  def apply(egraph: EGraphT): ArraySeq[EClassCall]
}

/**
 * Companion object for [[EClassesToSearch]] containing common implementations.
 */
object EClassesToSearch {
  /**
   * An [[EClassesToSearch]] that searches all e-classes in the e-graph.
   */
  val all: EClassesToSearch[EGraph[_]] = new EClassesToSearch[EGraph[_]] {
    override def apply(egraph: EGraph[_]): ArraySeq[EClassCall] = {
      UnsafeSeqFromArray(egraph.classes.view.map(egraph.canonicalize).toArray)
    }
  }
}
