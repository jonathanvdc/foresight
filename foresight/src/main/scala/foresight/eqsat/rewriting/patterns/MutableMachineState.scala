package foresight.eqsat.rewriting.patterns

import foresight.eqsat.{EClassCall, ENode, MixedTree, Slot}
import foresight.util.collections.ArrayMap

import scala.collection.compat._

/**
 * A mutable machine state that preallocates fixed-size arrays
 * for registers, bound vars, bound slots, and bound nodes.
 */
final class MutableMachineState[NodeT] private(val effects: Instruction.Effects,
                                               private val registersArr: Array[EClassCall],
                                               private val boundVarsArr: Array[MixedTree[NodeT, EClassCall]],
                                               private val boundSlotsArr: Array[Slot],
                                               private val boundNodesArr: Array[ENode[NodeT]],
                                               private val homePool: MutableMachineState.Pool[NodeT]) {

  private var regIdx: Int = 0
  private var varIdx: Int = 0
  private var slotIdx: Int = 0
  private var nodeIdx: Int = 0

  /**
   * Access a register by index.
   * @param idx The index of the register to access.
   * @return The e-class call in the register.
   */
  def registerAt(idx: Int): EClassCall = {
    require(idx >= 0 && idx < regIdx, s"Register index $idx out of bounds [0, $regIdx)")
    registersArr(idx)
  }

  /**
   * Lookup the node bound at a given index.
   * @param idx The index of the bound node to look up.
   * @return The bound node at the given index.
   */
  def boundNodeAt(idx: Int): ENode[NodeT] = {
    require(idx >= 0 && idx < nodeIdx, s"Bound node index $idx out of bounds [0, $nodeIdx)")
    boundNodesArr(idx)
  }

  /**
   * Lookup the value bound to a slot.
   * @param slot The slot to look up.
   * @param default The value bound to the slot.
   * @return The value bound to the slot, or `default` if the slot is not bound.
   */
  def boundSlotOrElse(slot: Slot, default: => Slot): Slot = {
    val i = effects.boundSlots.indexOf(slot)
    if (i >= 0 && i < slotIdx) boundSlotsArr(i)
    else default
  }

  /** Return this instance to its originating pool. */
  def release(): Unit = homePool.release(this)

  /**
   * Create a copy of this state, borrowed from the same pool.
   * The copy has identical registers, bound vars/slots/nodes and indices.
   */
  def fork(): MutableMachineState[NodeT] = {
    val root = registersArr(0)
    val dest = homePool.borrow(root)
    dest.assignFrom(this)
    dest
  }

  /** Internal: overwrite this instance with the contents of `src`. */
  private[patterns] def assignFrom(src: MutableMachineState[NodeT]): Unit = {
    require(this.effects eq src.effects, "Cannot assign from a state with different effects")
    // Copy counts first to bound arraycopy lengths correctly
    this.regIdx  = src.regIdx
    this.varIdx  = src.varIdx
    this.slotIdx = src.slotIdx
    this.nodeIdx = src.nodeIdx

    if (this.regIdx > 0)
      System.arraycopy(src.registersArr, 0, this.registersArr, 0, this.regIdx)
    if (this.varIdx > 0)
      System.arraycopy(src.boundVarsArr, 0, this.boundVarsArr, 0, this.varIdx)
    if (this.slotIdx > 0)
      System.arraycopy(src.boundSlotsArr, 0, this.boundSlotsArr, 0, this.slotIdx)
    if (this.nodeIdx > 0)
      System.arraycopy(src.boundNodesArr, 0, this.boundNodesArr, 0, this.nodeIdx)
  }

  /** Reinitialize indices and set a new root so this instance can be reused. */
  private[patterns] def reinit(newRoot: EClassCall): Unit = {
    varIdx = 0
    slotIdx = 0
    nodeIdx = 0
    init(newRoot)
  }

  /** Initialize the first register (root). */
  private[patterns] def init(root: EClassCall): Unit = {
    registersArr(0) = root
    regIdx = 1
  }

  /**
   * Number of registers created so far (including root).
   * @return The number of registers created so far.
   */
  def createdRegisters: Int = regIdx

  /**
   * Number of bound variables so far.
   * @return The number of bound variables so far.
   */
  def boundVarsCount: Int = varIdx

  /**
   * Number of bound slots so far.
   * @return The number of bound slots so far.
   */
  def boundSlotsCount: Int = slotIdx

  /**
   * Number of bound nodes so far.
   * @return The number of bound nodes so far.
   */
  def boundNodesCount: Int = nodeIdx

  /**
   * Bind an e-node: appends its args to registers, records slot bindings, and stores the node.
   * @param node The e-node to bind.
   */
  def bindNode(node: ENode[NodeT]): Unit = {
    // Append node arguments to registers
    val args = node.unsafeArgsArray
    var i = 0
    while (i < args.length) {
      registersArr(regIdx) = args(i)
      regIdx += 1
      i += 1
    }
    // Record slot bindings (definitions then uses)
    val defs = node.definitions
    if (defs.nonEmpty) {
      var j = 0
      while (j < defs.length) {
        boundSlotsArr(slotIdx) = defs(j)
        slotIdx += 1
        j += 1
      }
    }
    val uses = node.uses
    if (uses.nonEmpty) {
      var k = 0
      while (k < uses.length) {
        boundSlotsArr(slotIdx) = uses(k)
        slotIdx += 1
        k += 1
      }
    }
    // Store node
    boundNodesArr(nodeIdx) = node
    nodeIdx += 1
  }

  /**
   * Bind a variable to a value.
   * @param value The value to bind the variable to.
   */
  def bindVar(value: MixedTree[NodeT, EClassCall]): Unit = {
    boundVarsArr(varIdx) = value
    varIdx += 1
  }

  private def boundVars: ArrayMap[Pattern.Var, MixedTree[NodeT, EClassCall]] = {
    ArrayMap.unsafeWrapArrays(effects.boundVars.unsafeArray, java.util.Arrays.copyOf(boundVarsArr, varIdx), varIdx)
  }

  private def boundSlots: ArrayMap[Slot, Slot] = {
    ArrayMap.unsafeWrapArrays(effects.boundSlots.unsafeArray, java.util.Arrays.copyOf(boundSlotsArr, slotIdx), slotIdx)
  }

  /** Convert to an immutable MachineState snapshot. */
  def freeze(): MachineState[NodeT] = {
    val regs  = immutable.ArraySeq.unsafeWrapArray(registersArr.slice(0, regIdx))
    val nodes = immutable.ArraySeq.unsafeWrapArray(boundNodesArr.slice(0, nodeIdx))
    MachineState(regs, boundVars, boundSlots, nodes)
  }

  /**
   * Convert to a PatternMatch snapshot.
   */
  def toPatternMatch: PatternMatch[NodeT] = {
    PatternMatch(registersArr(0), boundVars, boundSlots)
  }
}

/**
 * Companion object for [[MutableMachineState]].
 */
object MutableMachineState {

  // Preallocate single empty arrays of the right types to avoid repeated allocations
  // when effects report zero bound vars/slots/nodes.
  // Unsafe casts are safe because these arrays are never written to.
  private val emptyVars  = new Array[MixedTree[_, EClassCall]](0)
  private val emptySlots = new Array[Slot](0)
  private val emptyNodes = new Array[ENode[_]](0)

  /** Internal: construct a state wired to a given pool. */
  private def makeWithPool[NodeT](root: EClassCall,
                                  effects: Instruction.Effects,
                                  pool: MutableMachineState.Pool[NodeT]): MutableMachineState[NodeT] = {
    val varsLen  = effects.boundVars.length
    val slotsLen = effects.boundSlots.length
    val nodesLen = effects.boundNodes

    val m = new MutableMachineState[NodeT](
      effects,
      new Array[EClassCall](1 + effects.createdRegisters),
      if (varsLen  > 0) new Array[MixedTree[NodeT, EClassCall]](varsLen) else emptyVars.asInstanceOf[Array[MixedTree[NodeT, EClassCall]]],
      if (slotsLen > 0) new Array[Slot](slotsLen) else emptySlots,
      if (nodesLen > 0) new Array[ENode[NodeT]](nodesLen) else emptyNodes.asInstanceOf[Array[ENode[NodeT]]],
      pool
    )
    m.init(root)
    m
  }

  /**
   * Allocate a MutableMachineState with exact capacities inferred from given effects.
   * Registers capacity includes the root register plus any created registers reported by effects.
   *
   * @param root The root e-class call.
   * @param effects The effects to infer capacities from.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @return A new MutableMachineState with preallocated arrays.
   */
  def apply[NodeT](root: EClassCall, effects: Instruction.Effects): MutableMachineState[NodeT] = {
    val p = new Pool[NodeT](effects, initialCapacity = 0)
    makeWithPool(root, effects, p)
  }

  /**
   * A simple pool for reusing `MutableMachineState` instances that all share the same `effects`.
   * The capacities are fixed by `effects`, and each borrowed instance is initialized with the
   * provided root. Returned instances are kept for reuse.
   */
  final class Pool[NodeT](val effects: Instruction.Effects, initialCapacity: Int = 0) {
    private val deque = new java.util.ArrayDeque[MutableMachineState[NodeT]](math.max(0, initialCapacity))

    /** Number of currently available instances in the pool. */
    def available: Int = deque.size()

    /**
     * Obtain a `MutableMachineState` initialized with `root`.
     * If the pool is empty, a new instance is allocated.
     * @param root The root e-class call to initialize the instance with.
     * @return A `MutableMachineState` instance ready for use.
     */
    def borrow(root: EClassCall): MutableMachineState[NodeT] = {
      if (deque.isEmpty) makeWithPool(root, effects, this)
      else { val inst = deque.pop(); inst.reinit(root); inst }
    }

    /**
     * Return an instance to the pool for reuse.
     * The instance must have been obtained from this pool (same effects).
     * @param state The instance to return to the pool.
     */
    def release(state: MutableMachineState[NodeT]): Unit = {
      // Ensure the state's sizing matches this pool.
      require(state.effects eq effects, "Releasing state into a Pool with different effects")
      deque.push(state)
    }
  }

  /**
   * Convenience constructor for a `Pool` with fixed `effects`.
   */
  object Pool {
    /**
     * Create a pool for a fixed `effects` profile.
     * @param effects The effects that determine the sizing of pooled instances.
     * @param initialCapacity Optional initial capacity of the pool.
     * @tparam NodeT The type of the nodes in the e-graph.
     * @return A new pool for `MutableMachineState` instances with the given effects.
     */
    def apply[NodeT](effects: Instruction.Effects, initialCapacity: Int = 0): Pool[NodeT] =
      new Pool[NodeT](effects, initialCapacity)
  }
}
