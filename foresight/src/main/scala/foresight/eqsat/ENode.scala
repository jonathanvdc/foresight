package foresight.eqsat

import foresight.eqsat.collections.{SlotMap, SlotSeq, SlotSet}
import foresight.util.Debug
import foresight.util.collections.UnsafeSeqFromArray

import scala.collection.compat._

/**
 * A node in a slotted e-graph.
 *
 * An e-node represents one operator application together with its slot usage and child e-class applications.
 * Slots are partitioned into:
 *   - `definitions`: slots introduced locally by this node (binder-internal slots that are not visible outside)
 *   - `uses`: slots referenced by this node that come from its surrounding context (free with respect to this node)
 * Child expressions are referenced via `args` as [[EClassCall]]s, which carry their own parameter-to-argument
 * slot maps.
 *
 * Node types (`NodeT`) supply the operator and any non-structural payload. Slots and arguments are provided here.
 *
 * @param nodeType     Operator or symbol for this node.
 * @tparam NodeT       The domain-specific node type. It defines operator identity and payload but not slots/children.
 */
final class ENode[+NodeT] private (
  val nodeType: NodeT,
  private val _definitions: Array[Slot],
  private val _uses: Array[Slot],
  private val _args: Array[EClassCall]
) extends Node[NodeT, EClassCall] {
  /**
   * Slots introduced by this node that are scoped locally and invisible to parents. These are
   * redundant by construction at the boundary of this node and exist to model binders such as
   * lambda-abstraction or let.
   *
   * @return Sequence of definition slots.
   */
  def definitions: SlotSeq = SlotSeq.unsafeWrapArray(_definitions)

  /**
   * Slots referenced by this node that are visible to its parent and must be satisfied by the
   * surrounding e-class application.
   *
   * @return Sequence of use slots.
   */
  def uses: SlotSeq = SlotSeq.unsafeWrapArray(_uses)

  /**
   * Child e-class applications, each with its own parameter-to-argument [[SlotMap]].
   *
   * @return Sequence of child e-class calls.
   */
  def args: immutable.ArraySeq[EClassCall] = UnsafeSeqFromArray(_args)

  /**
   * The total number of slots occurring in this node: definitions, uses, and children’s argument slots.
   * Includes slots that may be duplicated across these categories.
   *
   * @return The total slot count.
   */
  private def slotCount: Int = {
    var count = _definitions.length + _uses.length
    var i = 0
    while (i < _args.length) {
      count += _args(i).args.size
      i += 1
    }
    count
  }

  /**
   * Collects all slots occurring in this node into a single array, preserving order and duplicates.
   * Order preserves definitions, then uses, then the values of each child's argument map in child
   * @return An array of all slots used by this node.
   */
  private def slotArray: Array[Slot] = {
    val arr = new Array[Slot](slotCount)
    var idx = 0
    var i = 0
    while (i < _definitions.length) { arr(idx) = _definitions(i); idx += 1; i += 1 }
    i = 0
    while (i < _uses.length) { arr(idx) = _uses(i); idx += 1; i += 1 }
    i = 0
    while (i < _args.length) {
      val it = _args(i).args.values.iterator
      while (it.hasNext) { arr(idx) = it.next(); idx += 1 }
      i += 1
    }
    arr
  }

  /**
   * Collects all distinct slots occurring in this node into a single array, preserving encounter order.
   * Order preserves definitions, then uses, then the values of each child's argument map in child
   * order.
   *
   * @param slotCount The total number of slots (including duplicates) in this node.
   * @return An array of distinct slots used by this node.
   */
  private def distinctSlotArray(slotCount: Int): Array[Slot] = {
    val arr = new Array[Slot](slotCount)
    var nDistinct = 0

    def addIfNew(s: Slot): Unit = {
      var j = 0
      var seen = false
      while (!seen && j < nDistinct) { if (arr(j) eq s) seen = true; j += 1 }
      if (!seen) { arr(nDistinct) = s; nDistinct += 1 }
    }

    var i = 0
    while (i < _definitions.length) { addIfNew(_definitions(i)); i += 1 }
    i = 0
    while (i < _uses.length) { addIfNew(_uses(i)); i += 1 }
    i = 0
    while (i < _args.length) {
      val it = _args(i).args.values.iterator
      while (it.hasNext) addIfNew(it.next())
      i += 1
    }

    if (nDistinct == arr.length) arr
    else java.util.Arrays.copyOf(arr, nDistinct)
  }

  /**
   * All slots that appear syntactically in this node: local definitions, free uses, and all argument slots of children.
   * Order preserves definitions, then uses, then the values of each child's argument map in child order.
   *
   * @return An ordered sequence of slots used by this node.
   */
  def slots: Seq[Slot] = UnsafeSeqFromArray(slotArray)

  /**
   * The set of all distinct slots occurring in this node: definitions, uses, and children’s argument slots.
   *
   * @return A set of slots used by this node.
   */
  def slotSet: SlotSet = {
    SlotSet.fromUnsortedMutableArrayUnsafe(slotArray)
  }

  /**
   * Whether this node contains any slots at all: definitions, uses, or children’s argument slots.
   *
   * @return True if this node has any slots; false if it is completely ground.
   */
  def hasSlots: Boolean = _definitions.nonEmpty || _uses.nonEmpty || _args.exists(!_.args.isEmpty)

  private def containsSlot(slot: Slot): Boolean = {
    _definitions.contains(slot) ||
    _uses.contains(slot) ||
    _args.exists(_.args.valueSet.contains(slot))
  }

  /**
   * Renames every occurrence of the given slots throughout this node.
   *
   * The renaming applies uniformly to:
   *   - definitions and uses
   *   - each child's argument-slot map, composed via `composeRetain` so unmapped child entries are preserved
   *
   * All keys of `renaming` must occur in this node; extra keys are allowed but ignored.
   *
   * @param renaming Mapping from old to new slots.
   * @return A node with slots renamed.
   */
  def rename(renaming: SlotMap): ENode[NodeT] = {
    if (Debug.isEnabled) {
      require(renaming.keySet.forall(containsSlot), "All slots in the renaming must be present in the e-node.")
    }

    if (!hasSlots) return this

    val newDefs = ENode.renameSlotsOrNull(_definitions, renaming)
    val newUses = ENode.renameSlotsOrNull(_uses, renaming)

    var newArgs: Array[EClassCall] = null
    var i = 0
    while (i < _args.length) {
      val renamedArg = _args(i).renameRetain(renaming)
      if (newArgs == null && renamedArg != _args(i)) {
        newArgs = new Array[EClassCall](_args.length)
        // We must actually copy the prefix if we allocated newArgs
        Array.copy(_args, 0, newArgs, 0, i)
      }
      if (newArgs != null) newArgs(i) = renamedArg
      i += 1
    }

    // If nothing changed, return this; otherwise, construct with new-or-old arrays
    if ((newDefs eq null) && (newUses eq null) && (newArgs eq null)) this
    else new ENode(
      nodeType,
      if (newDefs eq null) _definitions else newDefs,
      if (newUses eq null) _uses else newUses,
      if (newArgs eq null) _args else newArgs
    )
  }

  /**
   * Returns the canonical shape of this node together with the inverse renaming from canonical to original slots.
   *
   * Canonicalization replaces every slot that appears in this node with a numeric slot in lexicographic order
   * (e.g., `$0`, `$1`, ...) producing a name-independent normal form. The accompanying [[SlotMap]] maps the canonical
   * numeric slots back to the original slots. This decomposition underlies hash-consing and equality modulo renaming.
   *
   * @return A [[ShapeCall]] whose shape is this node in canonical slot form, and whose renaming maps canonical
   *         slots back to the original slots of this node.
   */
  def asShapeCall: ShapeCall[NodeT] = {
    // Fast path for slotless nodes: they are already in canonical form
    val numberOfSlots = slotCount
    if (numberOfSlots == 0) return ShapeCall(this, SlotMap.empty)

    // Single preallocated buffer for distinct slots (encounter order)
    val distinct = distinctSlotArray(numberOfSlots)

    // Build another buffer for the numeric slots
    val numerics = ENode.numericArray(distinct.length)

    // Building the inverse renaming (from numeric to original) is easy from these two arrays
    val invRenaming = SlotMap.fromArraysUnsafe(numerics, distinct)
    val renaming = invRenaming.inverse

    // Apply renaming on the fly via existing fast-paths in ENode.rename
    val shaped = this.rename(renaming)

    ShapeCall(shaped, invRenaming)
  }

  /**
   * Checks whether this node is already in canonical shape-normal form.
   *
   * @return True if equal to `asShapeCall.shape`; false otherwise.
   */
  def isShape: Boolean = this == asShapeCall.shape

  /**
   * Applies a function to each argument e-class call, returning a new node if any argument changes.
   *
   * If the function does not change any arguments, returns this node unchanged.
   *
   * @param f Function to apply to each argument e-class call.
   * @return A node identical to this one but with each argument replaced by `f(arg)`.
   */
  def mapArgs(f: EClassCall => EClassCall): ENode[NodeT] = {
    var newArgs: Array[EClassCall] = null
    var i = 0
    while (i < _args.length) {
      val newArg = f(_args(i))
      if (newArgs == null && newArg != _args(i)) {
        newArgs = new Array[EClassCall](_args.length)
        Array.copy(_args, 0, newArgs, 0, i)
      }
      if (newArgs != null) newArgs(i) = newArg
      i += 1
    }
    if (newArgs == null) return this

    new ENode(nodeType, _definitions, _uses, newArgs)
  }

  /**
   * Returns a copy of this node with the given argument e-class calls.
   *
   * If the new arguments are identical to the current ones, returns this node unchanged.
   *
   * @param newArgs New argument e-class calls.
   * @return A node identical to this one but with `newArgs` as its arguments.
   */
  def withArgs(newArgs: Seq[EClassCall]): ENode[NodeT] = {
    if (newArgs == args) return this

//    newArgs match {
//      case calls: ArraySeq[EClassCall] =>
//        new ENode(nodeType, _definitions, _uses, calls.unsafeArray.asInstanceOf[Array[EClassCall]])
//
//      case _ =>
//        new ENode(nodeType, _definitions, _uses, newArgs.toArray)
//    }
    new ENode(nodeType, _definitions, _uses, newArgs.toArray)
  }

  // --- case-class-like API preservation ---
  override def toString: String = s"ENode($nodeType, $definitions, $uses, $args)"

  //noinspection ComparingUnrelatedTypes
  override def equals(other: Any): Boolean = other match {
    case that: ENode[_] =>
      this.nodeType == that.nodeType &&
      ENode.arraysEqual(this._definitions, that._definitions) &&
      ENode.arraysEqual(this._uses, that._uses) &&
      ENode.arraysEqual(this._args, that._args)
    case _ => false
  }

  private lazy val _hash: Int = {
    var h = 1
    h = 31 * h + (if (nodeType == null) 0 else nodeType.hashCode)
    h = 31 * h + java.util.Arrays.hashCode(_definitions.asInstanceOf[Array[AnyRef]])
    h = 31 * h + java.util.Arrays.hashCode(_uses.asInstanceOf[Array[AnyRef]])
    h = 31 * h + java.util.Arrays.hashCode(_args.asInstanceOf[Array[AnyRef]])
    h
  }
  override def hashCode(): Int = _hash
}

/**
 * Constructors and helpers for [[ENode]].
 */
object ENode {
  private[eqsat] def arraysEqual[T](a: Array[T], b: Array[T]): Boolean = {
    if (a eq b) return true
    if (a.length != b.length) return false
    var i = 0
    while (i < a.length) {
      if (a(i) != b(i)) return false
      i += 1
    }
    true
  }

  private val emptySlotArray: Array[Slot] = Array.empty
  private val emptyCallArray: Array[EClassCall] = Array.empty

  private def arrayFromSlotSeq(s: Seq[Slot]): Array[Slot] = s match {
    case slotSeq: SlotSeq => slotSeq.unsafeArray
    case as: immutable.ArraySeq[Slot] if as.unsafeArray.isInstanceOf[Array[Slot]] =>
      as.unsafeArray.asInstanceOf[Array[Slot]]
    case _ if s.isEmpty =>
      emptySlotArray
    case _ =>
      s.toArray[Slot]
  }

  private def arrayFromCallSeq(s: Seq[EClassCall]): Array[EClassCall] = s match {
    case as: immutable.ArraySeq[EClassCall] if as.unsafeArray.isInstanceOf[Array[EClassCall]] =>
      as.unsafeArray.asInstanceOf[Array[EClassCall]]
    case _ if s.isEmpty =>
      emptyCallArray
    case _ =>
      s.toArray[EClassCall]
  }

  def apply[NodeT](nodeType: NodeT, definitions: Seq[Slot], uses: Seq[Slot], args: Seq[EClassCall]): ENode[NodeT] = {
    new ENode(nodeType, arrayFromSlotSeq(definitions), arrayFromSlotSeq(uses), arrayFromCallSeq(args))
  }

  private[foresight] def unsafeWrapArrays[NodeT](nodeType: NodeT, definitions: Array[Slot], uses: Array[Slot], args: Array[EClassCall]): ENode[NodeT] = {
    new ENode(nodeType, definitions, uses, args)
  }

  private[foresight] def unsafeWrapArrays[NodeT](nodeType: NodeT, definitions: Seq[Slot], uses: Seq[Slot], args: Array[EClassCall]): ENode[NodeT] = {
    new ENode(nodeType, arrayFromSlotSeq(definitions), arrayFromSlotSeq(uses), args)
  }

  def unapply[NodeT](n: ENode[NodeT]): Option[(NodeT, Seq[Slot], Seq[Slot], Seq[EClassCall])] =
    Some((n.nodeType, n.definitions, n.uses, n.args))

  /**
   * Builds an e-node that declares no local or free slots.
   *
   * Useful for ground nodes in languages without variables or when slot usage is expressed entirely in children.
   *
   * @param nodeType Operator or symbol.
   * @param args     Child e-class applications.
   * @tparam NodeT   Domain-specific node type.
   * @return A slotless node with the given operator and children.
   *
   * @example
   * {{{
   * val n: ENode[Op] = ENode.unslotted(Add, Seq(leftCall, rightCall))
   * }}}
   */
  def unslotted[NodeT](nodeType: NodeT, args: Seq[EClassCall]): ENode[NodeT] =
    new ENode(nodeType, Array.empty, Array.empty, args.toArray)

  private[eqsat] def renameSlotsOrNull(orig: Array[Slot], renaming: SlotMap): Array[Slot] = {
    var newArr: Array[Slot] = null
    var i = 0
    while (i < orig.length) {
      val mapped = renaming.apply(orig(i))
      if (newArr == null) {
        if (mapped != orig(i)) {
          newArr = java.util.Arrays.copyOf(orig, orig.length)
          newArr(i) = mapped
        }
      } else {
        newArr(i) = mapped
      }
      i += 1
    }
    newArr
  }

  /**
   * Returns an array of numeric slots for the given arity.
   *
   * For small arities (0 to 16), returns a shared preallocated array of numeric slots to avoid repeated allocations.
   * For larger arities, allocates a new array of numeric slots on each call.
   *
   * @param arity The desired number of numeric slots.
   * @return An array of `arity` numeric slots: `0`, `1`, ..., `arity-1`.
   */
  private[eqsat] def numericArray(arity: Int): Array[Slot] = {
    if (arity >= 0 && arity < sharedNumericArrays.length) {
      sharedNumericArrays(arity)
    } else {
      Array.tabulate(arity)(Slot.numeric)
    }
  }

  /**
   * Preallocated arrays of numeric slots for small arities to avoid repeated allocations.
   * Indexed by arity, each entry is an array of that many numeric slots.
   * Slots are `0`, `1`, ..., `n-1` for arity `n`.
   */
  private[eqsat] val sharedNumericArrays: Array[Array[Slot]] = {
    val max = 16
    Array.tabulate(max + 1)(n => Array.tabulate(n)(Slot.numeric))
  }
}
