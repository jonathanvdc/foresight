package foresight.eqsat.rewriting.patterns

import foresight.eqsat.{EClassCall, ENode, MixedTree, ReadOnlyEGraph, Slot}

/**
 * A mutable machine state that preallocates fixed-size arrays
 * for registers, bound vars, bound slots, and bound nodes.
 */
final class MutableMachineState[NodeT] private(private val registersArr: Array[EClassCall],
                                               private val boundVarsArr: Array[MixedTree[NodeT, EClassCall]],
                                               private val boundSlotsArr: Array[Slot],
                                               private val boundNodesArr: Array[ENode[NodeT]]) {

  private var regIdx: Int = 0
  private var varIdx: Int = 0
  private var slotIdx: Int = 0
  private var nodeIdx: Int = 0

  /** Initialize the first register (root). */
  private[patterns] def initRoot(root: EClassCall): Unit = {
    registersArr(0) = root
    regIdx = 1
  }

  /** Current counts (useful for assertions in tests). */
  def createdRegisters: Int = regIdx
  def boundVarsCount: Int = varIdx
  def boundSlotsCount: Int = slotIdx
  def boundNodesCount: Int = nodeIdx

  /**
   * Bind an e-node: appends its args to registers, records slot bindings, and stores the node.
   * @param node The e-node to bind.
   */
  def bindNode(node: ENode[NodeT]): Unit = {
    // Append node arguments to registers
    val args = node.args
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

  /** Convert to an immutable MachineState snapshot. */
  def freeze(effects: Instruction.Effects): MachineState[NodeT] = {
    val regs  = registersArr.slice(0, regIdx).toIndexedSeq
    val varsM = scala.collection.mutable.Map.empty[Pattern.Var, MixedTree[NodeT, EClassCall]]
    var i = 0
    while (i < varIdx) { val v = effects.boundVars(i); val mt = boundVarsArr(i); varsM.update(v, mt); i += 1 }
    val slotsM = scala.collection.mutable.Map.empty[Slot, Slot]
    var j = 0
    while (j < slotIdx) { val s = effects.boundSlots(j); val s2 = boundSlotsArr(j); slotsM.update(s, s2); j += 1 }
    val nodes = boundNodesArr.slice(0, nodeIdx).toIndexedSeq
    MachineState(regs, varsM.toMap, slotsM.toMap, nodes)
  }
}

/**
 * Companion object for [[MutableMachineState]].
 */
object MutableMachineState {
  /**
   * Allocate a MutableMachineState with exact capacities inferred from given effects.
   * Registers capacity includes the root register plus any created registers reported by effects.
   *
   * @param root The root e-class call.
   * @param effects The effects to infer capacities from.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EG The type of the e-graph.
   * @return A new MutableMachineState with preallocated arrays.
   */
  def apply[NodeT, EG <: ReadOnlyEGraph[NodeT]](root: EClassCall, effects: Instruction.Effects): MutableMachineState[NodeT] = {
    val m = new MutableMachineState[NodeT](
      new Array[EClassCall](1 + effects.createdRegisters),
      new Array[MixedTree[NodeT, EClassCall]](effects.boundVars.length),
      new Array[Slot](effects.boundSlots.length),
      new Array[ENode[NodeT]](effects.boundNodes)
    )

    m.initRoot(root)
    m
  }
}
