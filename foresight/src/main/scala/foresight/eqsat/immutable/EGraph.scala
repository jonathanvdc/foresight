package foresight.eqsat.immutable

import foresight.eqsat.hashCons.immutable.HashConsEGraph
import foresight.eqsat.{EClassCall, MixedTree}

/**
 * An e-graph compactly represents many equivalent expressions at once by grouping structurally
 * * compatible nodes into e-classes. This enables fast equality saturation and rewrite exploration
 * * without duplicating common substructure.
 *
 * `EGraph` is a convenience alias of [[EGraphLike]] where the self type equals the trait itself:
 * `EGraph[NodeT]` extends `EGraphLike[NodeT, EGraph[NodeT]]`. Use this when you don’t need a custom
 * concrete e-graph subtype in your method signatures.
 *
 * Semantics:
 *   - **Immutable:** All mutating operations (add/union/etc.) return a new `EGraph[NodeT]`.
 *   - **Canonicalization:** Queries and updates are expressed in terms of canonical e-classes.
 *   - **Total API:** All core operations (query, add, union, find) are inherited from [[EGraphLike]].
 *
 * @tparam NodeT The domain-specific node type stored in e-nodes.
 * @example Typical usage
 * {{{
 * val g0: EGraph[MyNode] = EGraph.empty
 * val (c1, g1) = g0.add(ENode(Add, defs = SlotSet.empty, uses = SlotSet.empty, args = Seq(...)))
 * val (c2, g2) = g1.add(ENode(Mul, defs = ..., uses = ..., args = Seq(...)))
 * val g3 = g2.union(c1, c2).rebuilt
 * }}}
 */
trait EGraph[NodeT] extends EGraphLike[NodeT, EGraph[NodeT]]

/**
 * Constructors and helpers for [[EGraph]].
 *
 * Functions here return a plain `EGraph[NodeT]` backed by a hash-consing implementation.
 */
object EGraph {

  /**
   * Creates a new, empty e-graph for the given node type backed by a hash-consing implementation.
   *
   * @tparam NodeT The node type that this e-graph will store.
   * @return An empty `EGraph[NodeT]` backed by hash-consing.
   */
  def empty[NodeT]: EGraph[NodeT] = HashConsEGraph.empty[NodeT]

  /**
   * Builds a new e-graph from a single tree, returning the e-class of its root.
   *
   * This is equivalent to:
   * {{{
   * val g = EGraph.empty[NodeT]
   * g.add(tree)
   * }}}
   *
   * The input is a `MixedTree[NodeT, Nothing]`, i.e., a tree that does not contain pre-existing
   * `EClassCall`s. All children are inserted (or found) first, then the root e-node is inserted (or
   * found), yielding the root e-class.
   *
   * @param tree The tree to insert into a fresh e-graph.
   * @tparam NodeT The node type of the tree and resulting e-graph.
   * @return (Root e-class of `tree`, new e-graph containing exactly the nodes added by `tree`.)
   */
  def from[NodeT](tree: MixedTree[NodeT, Nothing]): (EClassCall, EGraph[NodeT]) = {
    val egraph = empty[NodeT]
    egraph.add(tree)
  }
}
