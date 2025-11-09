package foresight.eqsat

import foresight.eqsat.collections.{SlotMap, SlotSeq, SlotSet}
import foresight.eqsat.readonly.EGraph

import scala.collection.compat.immutable.ArraySeq

/**
 * Represents the application of an [[EClassRef]] to a set of argument slots.
 *
 * An e-class application combines:
 *   - A reference to an e-class (`ref`), which may have parameter slots.
 *   - A [[SlotMap]] mapping each parameter slot to a corresponding argument slot.
 *
 * Slots are a generalization of term variables that allow the e-graph to represent
 * parameterized e-classes and maintain consistent argument binding across multiple
 * occurrences. This enables advanced rewriting techniques such as those described in
 * Schneider et al., "E-Graphs with Classes and Slots: Reasoning with Parameterized
 * Equivalence Classes" (Proc. ACM Program. Lang., OOPSLA 2024).
 *
 * The argument slot map (`args`) specifies, for each parameter slot in the e-class,
 * which argument slot should be used when this e-class is applied. This supports
 * sharing, renaming, and partial instantiation of e-classes within larger expressions.
 *
 * @param ref  The reference to the e-class being applied.
 * @param args The mapping from the e-class's parameter slots to argument slots.
 * @example
 * {{{
 * // EClassRef `subXY` has parameter slots (x, y) representing "x - y"
 * val call1 = EClassCall(subXY, SlotMap(x -> a, y -> b)) // represents "a - b"
 * val call2 = EClassCall(subXY, SlotMap(x -> b, y -> a)) // represents "b - a"
 * }}}
 */
final case class EClassCall(ref: EClassRef, args: SlotMap) extends EClassSymbol with CallTree[Nothing] {
  /**
   * The set of slots used as arguments in this application, in sequence order.
   * These are the slots referenced by the argument values, not the parameter slots.
   */
  def slots: Seq[Slot] = args.values

  /**
   * The set of distinct slots used as arguments in this application.
   */
  override def slotSet: SlotSet = args.valueSet

  /**
   * Renames all argument slots in this application according to a given mapping.
   * All argument slots must be present in the renaming map.
   *
   * @param renaming The mapping from old argument slots to new argument slots.
   * @return A new application with arguments renamed.
   */
  def rename(renaming: SlotMap): EClassCall = {
    if (args.isEmpty) return this

    assert(args.valueSet.subsetOf(renaming.keySet), "Argument slots must be in the renaming.")
    EClassCall(ref, args.composePartial(renaming))
  }

  /**
   * Renames argument slots in this application according to a given mapping.
   * Argument slots not in the mapping are dropped.
   *
   * @param renaming The mapping from old argument slots to new argument slots.
   * @return A new application with arguments renamed (possibly dropping some).
   */
  def renamePartial(renaming: SlotMap): EClassCall = {
    if (args.isEmpty) return this

    val newArgs = args.composePartial(renaming)
    if (newArgs eq args) this else EClassCall(ref, newArgs)
  }

  /**
   * Renames argument slots in this application according to a given mapping.
   * Argument slots not in the mapping are retained as-is.
   *
   * @param renaming The mapping from old argument slots to new argument slots.
   * @return A new application with arguments renamed (retaining unmapped ones).
   */
  def renameRetain(renaming: SlotMap): EClassCall = {
    if (args.isEmpty || renaming.isEmpty) return this

    val newArgs = args.composeRetain(renaming)
    if (newArgs eq args) this else EClassCall(ref, newArgs)
  }

  /**
   * Checks whether this application is well-formed in the given e-graph.
   * An application is well-formed if all parameter slots of the target e-class
   * are bound in its argument slot map.
   *
   * @param egraph The e-graph in which to check.
   * @return True if well-formed, false otherwise.
   */
  def isWellFormed(egraph: EGraph[_]): Boolean = {
    val slots = egraph.canonicalize(ref).args.valueSet
    slots.subsetOf(args.keySet)
  }
}

/**
 * Symbolic handle for an e-class in an e-graph.
 *
 * An [[EClassSymbol]] can be:
 *   - a concrete reference to an existing e-class ([[EClassCall]]), or
 *   - a placeholder for an e-class not yet added ([[EClassSymbol.Virtual]]).
 *
 * Symbols are used by [[Command]] instances to refer to e-classes in a
 * portable, reifiable way.
 */
sealed trait EClassSymbol {

  /**
   * Returns `true` if this symbol refers to an existing e-class
   * (i.e. is an [[EClassCall]]).
   */
  final def isReal: Boolean = this match {
    case _: EClassCall => true
    case _: EClassSymbol.Virtual => false
  }

  /**
   * Returns `true` if this symbol is a placeholder (i.e. an [[EClassSymbol.Virtual]]).
   */
  final def isVirtual: Boolean = !isReal

  /**
   * Resolves this symbol to its [[EClassCall]].
   *
   * If this symbol is real, its call is returned directly.
   * If it is virtual, the corresponding entry in `reification` must exist.
   *
   * @param reification Mapping from virtual symbols to concrete calls.
   * @throws NoSuchElementException if this symbol is virtual and not found in `reification`.
   * @return The concrete call for this symbol.
   *
   * @example
   * {{{
   * val v = EClassSymbol.virtual()
   * val call = EClassCall(...)
   * val realCall = v.reify(Map(v -> call)) // returns call
   * }}}
   */
  final def reify(reification: EClassSymbol.Virtual => EClassCall): EClassCall = this match {
    case call: EClassCall => call
    case virtual: EClassSymbol.Virtual => reification(virtual)
  }

  /**
   * Optionally resolves this symbol to its [[EClassCall]].
   *
   * If this symbol is real, its call is wrapped in `Some`.
   * If it is virtual, returns the matching entry in `reification` if present,
   * or `None` if missing.
   *
   * @param reification Mapping from virtual symbols to concrete calls.
   * @return The resolved call, or `None` if unresolved.
   */
  final def tryReify(reification: collection.Map[EClassSymbol.Virtual, EClassCall]): Option[EClassCall] = this match {
    case call: EClassCall => Some(call)
    case virtual: EClassSymbol.Virtual => reification.get(virtual)
  }

  /**
   * Replaces this symbol with an [[EClassSymbol.Real]] if resolvable.
   *
   * - If already real, returns an equivalent [[EClassSymbol.Real]].
   * - If virtual and present in `reification`, returns a new [[EClassSymbol.Real]].
   * - Otherwise, returns `this` unchanged.
   *
   * @param reification Mapping from virtual symbols to concrete calls.
   * @return A real symbol if resolvable, else the original symbol.
   */
  final def refine(reification: collection.Map[EClassSymbol.Virtual, EClassCall]): EClassSymbol = this match {
    case call: EClassCall => EClassSymbol.real(call)
    case virtual: EClassSymbol.Virtual => reification.get(virtual) match {
      case Some(call) => EClassSymbol.real(call)
      case None => virtual
    }
  }
}

/**
 * Constructors and concrete types for [[EClassSymbol]].
 */
object EClassSymbol {
  /**
   * A symbol referring to an existing e-class.
   *
   * Wraps an [[EClassCall]] directly.
   */
  type Real = EClassCall

  /**
   * Placeholder reference for an e-class not yet added to the e-graph.
   *
   * Virtual symbols allow [[Command]] instances to describe edits that will
   * produce new e-classes, without knowing their final IDs or calls in advance.
   */
  final class Virtual extends EClassSymbol

  /**
   * Creates a fresh [[Virtual]] symbol.
   */
  def virtual(): Virtual = new Virtual

  /**
   * Indicates that the given call is a real e-class symbol.
   */
  def real(call: EClassCall): Real = call
}

/**
 * A tree of nodes and e-class calls, representing expressions in an e-graph.
 * @tparam NodeT The type of the nodes in the e-graph.
 */
sealed trait CallTree[+NodeT] {
  /**
   * The set of distinct slots used in this call tree.
   */
  def slotSet: Set[Slot] = this match {
    case call: EClassCall => call.slotSet
    case CallTree.Node(_, defs, uses, children) =>
      defs.toSet ++ uses ++ children.flatMap(_.slotSet)
  }

  /**
   * Converts this call tree into a mixed tree of nodes and e-class calls.
   * @return The resulting mixed tree.
   */
  final def toMixedTree: MixedTree[NodeT, EClassCall] = this match {
    case call: EClassCall => MixedTree.Atom(call)
    case CallTree.Node(n, defs, uses, children) =>
      MixedTree.Node(n, defs, uses, children.map(_.toMixedTree))
  }

  /**
   * Maps all [[EClassCall]] nodes in this call tree using a given function.
   * @param f The mapping function.
   * @return The resulting call tree with mapped calls.
   */
  final def mapCalls(f: EClassCall => EClassCall): CallTree[NodeT] = this match {
    case call: EClassCall => f(call)
    case CallTree.Node(n, defs, uses, children) =>
      CallTree.Node(n, defs, uses, children.map(_.mapCalls(f)))
  }
}

/**
 * Constructors for [[CallTree]] nodes.
 */
object CallTree {
  /**
   * Represents a node in the call tree.
   * @param node The node.
   * @param definitions The slots defined by this node.
   * @param uses The slots used by this node.
   * @param children The child call trees.
   * @tparam NodeT The type of the nodes in the e-graph.
   */
  final case class Node[+NodeT](node: NodeT, definitions: SlotSeq, uses: SlotSeq, children: ArraySeq[CallTree[NodeT]])
    extends CallTree[NodeT]

  /**
   * Converts a [[MixedTree]] of [[EClassCall]]s into a [[CallTree]].
   * @param tree The mixed tree to convert.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @return The resulting call tree.
   */
  def from[NodeT](tree: MixedTree[NodeT, EClassCall]): CallTree[NodeT] = tree match {
    case MixedTree.Atom(call) => call
    case MixedTree.Node(n, defs, uses, args) =>
      Node(n, defs, uses, args.map(arg => from(arg)))
  }

  /**
   * Converts a [[Tree]] into a [[CallTree]].
   * @param tree The tree to convert.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @return The resulting call tree.
   */
  def from[NodeT](tree: Tree[NodeT]): CallTree[NodeT] = tree match {
    case Tree(n, defs, uses, children) =>
      Node(n, defs, uses, children.map(child => from(child)))
  }
}
