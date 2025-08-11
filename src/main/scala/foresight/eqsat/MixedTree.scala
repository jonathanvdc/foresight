package foresight.eqsat

import foresight.eqsat.rewriting.ReversibleSearcher
import foresight.eqsat.rewriting.patterns.{CompiledPattern, MachineSearcherPhase, Pattern, PatternApplier, PatternMatch}

/**
 * A heterogeneous term tree that interleaves node-typed interiors with leaf "calls".
 *
 * `MixedTree[NodeT, CallT]` represents a recursive structure where each branch is either a [[MixedTree.Node]]
 * containing a `nodeType`, slot information, and child subtrees, or a [[MixedTree.Call]] containing a single
 * leaf payload of type `CallT`. While every [[MixedTree.Call]] is necessarily a leaf, a [[MixedTree.Node]] can
 * also be a leaf if it has no children. In such cases, the [[MixedTree.Node]] still participates in slot
 * definitions and uses, but terminates the recursion.
 *
 * The type parameter `CallT` abstracts over the kind of leaf the tree carries. When `CallT` is
 * [[EClassCall]], leaves reference e-classes already present in an e-graph. When `CallT` is
 * [[foresight.eqsat.rewriting.patterns.Pattern]], the structure represents a rewrite pattern where leaves can
 * be pattern variables or other atomic constructs. This design allows the same tree shape to serve multiple
 * purposes: representing extracted terms from an e-graph, rewrite patterns for e-matching, or
 * other specialized term-like structures.
 *
 * Slot semantics match those in [[Tree]] and [[ENode]]. The `definitions` sequence lists slots
 * introduced by a [[MixedTree.Node]] that are not visible to its parents, while the `uses` sequence lists
 * slots referenced by the [[MixedTree.Node]] that are defined elsewhere in the tree.
 *
 * This abstraction is useful when some subtrees must be treated as opaque handles rather than
 * fully expanded node structures, allowing mixed granularity in the same representation.
 *
 * @tparam NodeT  The type used to represent interior nodes.
 * @tparam CallT  The type used to represent leaf payloads.
 *
 * @example
 * {{{
 * // Mixed tree whose leaves are e-class calls:
 * val cA: EClassCall = ...
 * val cB: EClassCall = ...
 * val t1: MixedTree[String, EClassCall] =
 *   MixedTree.Node("add", Seq.empty, Seq.empty,
 *     Seq(MixedTree.Call(cA), MixedTree.Call(cB)))
 *
 * // A Node leaf (no children):
 * val lit: MixedTree[String, EClassCall] =
 *   MixedTree.Node("const42", Seq.empty, Seq.empty, Seq.empty)
 *
 * // Mixed tree whose leaves are pattern fragments:
 * val x: Pattern[String] = Pattern.Var.fresh()
 * val y: Pattern[String] = Pattern.Var.fresh()
 * val pat: MixedTree[String, Pattern[String]] =
 *   MixedTree.Node("mul", Seq.empty, Seq.empty,
 *     Seq(MixedTree.Call(x), MixedTree.Call(y)))
 *
 * // Transform just the node labels:
 * val upper = pat.mapNodes(_.toUpperCase)
 *
 * // Transform just the leaves:
 * val renamed = t1.mapCalls(_.rename(SlotMap.identity(_.slotSet)))
 * }}}
 */
trait MixedTree[+NodeT, +CallT] {

  /**
   * Applies a transformation to every interior node's `nodeType`, preserving slots and structure.
   * Leaves remain leaves: `Call` leaves are untouched, and `Node` leaves are mapped in place.
   *
   * @param f         Function applied to each `nodeType`.
   * @tparam NewNodeT The resulting node type.
   */
  def mapNodes[NewNodeT](f: NodeT => NewNodeT): MixedTree[NewNodeT, CallT] = this match {
    case MixedTree.Node(nodeType, definitions, uses, children) =>
      MixedTree.Node[NewNodeT, CallT](f(nodeType), definitions, uses, children.map(_.mapNodes(f)))

    case MixedTree.Call(call) =>
      MixedTree.Call(call)
  }

  /**
   * Applies a transformation to every `CallT` leaf, preserving the node structure and slots.
   * Node leaves (empty `children`) are unaffected because they do not carry a `CallT`.
   *
   * @param f         Function applied to each leaf payload.
   * @tparam NewCallT The resulting leaf type.
   */
  def mapCalls[NewCallT](f: CallT => NewCallT): MixedTree[NodeT, NewCallT] = this match {
    case MixedTree.Node(nodeType, definitions, uses, children) =>
      MixedTree.Node[NodeT, NewCallT](nodeType, definitions, uses, children.map(_.mapCalls[NewCallT](f)))

    case MixedTree.Call(call) =>
      MixedTree.Call(f(call))
  }
}

/**
 * Companion object for [[MixedTree]], containing constructors, cases, and utility operations.
 */
object MixedTree {
  import scala.language.implicitConversions

  /** Constructs a node with no bound or used slots. */
  def unslotted[NodeT, CallT](nodeType: NodeT, children: Seq[MixedTree[NodeT, CallT]]): MixedTree[NodeT, CallT] = {
    MixedTree.Node(nodeType, Seq.empty, Seq.empty, children)
  }

  /** A `Node` in a mixed tree, which may be an interior or a leaf depending on whether `children` is empty. */
  final case class Node[NodeT, CallT](nodeType: NodeT,
                                      definitions: Seq[Slot],
                                      uses: Seq[Slot],
                                      children: Seq[MixedTree[NodeT, CallT]]) extends MixedTree[NodeT, CallT]

  /** A `Call` in a mixed tree, which is always a leaf and wraps a `CallT` payload. */
  final case class Call[NodeT, CallT](call: CallT) extends MixedTree[NodeT, CallT]

  /** Implicitly converts an `ENode` into a `MixedTree` whose leaves are `EClassCall`s. */
  implicit def fromENode[NodeT](node: ENode[NodeT]): MixedTree[NodeT, EClassCall] = {
    MixedTree.Node[NodeT, EClassCall](node.nodeType, node.definitions, node.uses, node.args.map(Call[NodeT, EClassCall]))
  }

  /** Implicitly converts a `Tree` into a `MixedTree` with the same shape, keeping all nodes as `Node`s. */
  implicit def fromTree[NodeT, A](tree: Tree[NodeT]): MixedTree[NodeT, A] = {
    val args = tree.args.map(fromTree[NodeT, A])
    MixedTree.Node[NodeT, A](tree.nodeType, tree.definitions, tree.uses, args)
  }

  /** Extension methods for `MixedTree`s whose leaves are `EClassCall`s. */
  implicit class MixedTreeOfEClassCallOps[NodeT](val tree: MixedTree[NodeT, EClassCall]) extends AnyVal {
    /** Returns the set of all slots appearing in the tree, including those in leaves and node definitions/uses. */
    def slotSet: Set[Slot] = tree match {
      case MixedTree.Node(_, defs, uses, children) =>
        defs.toSet ++ uses ++ children.flatMap(_.slotSet)
      case MixedTree.Call(call) =>
        call.slotSet
    }
  }

  /** Extension methods for `MixedTree`s whose leaves are patterns. */
  implicit class MixedTreeOfPatternOps[NodeT](val tree: MixedTree[NodeT, Pattern[NodeT]]) extends AnyVal {
    def compiled[EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]]: CompiledPattern[NodeT, EGraphT] =
      CompiledPattern(tree)
    def toSearcher[EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]]: ReversibleSearcher[NodeT, PatternMatch[NodeT], EGraphT] =
      ReversibleSearcher(MachineSearcherPhase(compiled))
    def toApplier[EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]]: PatternApplier[NodeT, EGraphT] =
      PatternApplier(tree)
  }
}
