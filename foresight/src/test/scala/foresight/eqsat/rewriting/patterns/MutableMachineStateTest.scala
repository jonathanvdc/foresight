package foresight.eqsat.rewriting.patterns

import org.junit.Test
import org.junit.Assert._
import foresight.eqsat._

class MutableMachineStateTest {

  // Helpers to make fresh values
  private def vars(n: Int): Seq[Pattern.Var] = List.fill(n)(Pattern.Var.fresh())

  private def slots(n: Int): Seq[Slot] = List.fill(n)(Slot.fresh())

  @Test
  def allocateInitializesRootAndCounts(): Unit = {
    val v = Pattern.Var.fresh()
    val s = Slot.fresh()

    // Effects: 0 created registers beyond root, 1 var slot, 1 slot, 0 nodes
    val effects = Instruction.Effects(
      createdRegisters = 0,
      boundVars = Seq(v),
      boundSlots = Seq(s),
      boundNodes = 0
    )

    val root: EClassCall = null.asInstanceOf[EClassCall] // not dereferenced
    val m = MutableMachineState[Any, ReadOnlyEGraph[Any]](root, effects)

    // After initRoot: one register used (the root), others zero
    assertEquals(1, m.createdRegisters)
    assertEquals(0, m.boundVarsCount)
    assertEquals(0, m.boundSlotsCount)
    assertEquals(0, m.boundNodesCount)

    // Freezing without any binds should reflect just the root register
    val snap = m.freeze()
    assertEquals(Seq(root), snap.registers)
    assertTrue(snap.boundVars.isEmpty)
    assertTrue(snap.boundSlots.isEmpty)
    assertTrue(snap.boundNodes.isEmpty)
  }

  @Test
  def bindVarAppendsAndFreezeUsesEffectsVarsAsKeys(): Unit = {
    val v1 +: v2 +: Nil = vars(2)

    // Effects declares the variable binding order (keys); values are supplied later via bindVar
    val effects = Instruction.Effects(
      createdRegisters = 0,
      boundVars = Seq(v1, v2),
      boundSlots = Seq.empty,
      boundNodes = 0
    )

    val root: EClassCall = null.asInstanceOf[EClassCall]
    val m = MutableMachineState[Any, ReadOnlyEGraph[Any]](root, effects)

    // Values for the variables
    val mt1: MixedTree[Any, EClassCall] = null.asInstanceOf[MixedTree[Any, EClassCall]]
    val mt2: MixedTree[Any, EClassCall] = null.asInstanceOf[MixedTree[Any, EClassCall]]

    m.bindVar(mt1)
    m.bindVar(mt2)

    assertEquals(1, m.createdRegisters) // root is present
    assertEquals(2, m.boundVarsCount)
    assertEquals(0, m.boundSlotsCount)
    assertEquals(0, m.boundNodesCount)

    val snap = m.freeze()
    assertEquals(Set(v1, v2), snap.boundVars.keySet)
    // We can only assert mapping size / keys; values are opaque here
    assertEquals(2, snap.boundVars.size)
    assertEquals(Seq(root), snap.registers)
  }

  @Test
  def bindNodeAppendsArgsAndMapsSlotsInOrderDefsThenUses(): Unit = {
    // Pattern-side slots (keys in freeze)
    val patDefs@Seq(pd1, pd2) = slots(2)
    val patUses@Seq(pu1) = slots(1)

    // Actual slots provided by the node (values captured at bind time)
    val actDefs@Seq(ad1, ad2) = slots(2)
    val actUses@Seq(au1) = slots(1)

    // Effects: createdRegisters must include the number of argument registers that will be appended
    // by bindNode (since capacity is root + createdRegisters).
    val argCount = 3
    val effects = Instruction.Effects(
      createdRegisters = argCount, // preallocate for args
      boundVars = Seq.empty, // no vars in this test
      boundSlots = patDefs ++ patUses, // order must match bindNode recording: defs then uses
      boundNodes = 1
    )

    val root: EClassCall = null.asInstanceOf[EClassCall]
    val m = MutableMachineState[Any, ReadOnlyEGraph[Any]](root, effects)

    // Dummy node: we never dereference op/args beyond length and identity
    val args: Seq[EClassCall] = Seq(
      null.asInstanceOf[EClassCall],
      null.asInstanceOf[EClassCall],
      null.asInstanceOf[EClassCall]
    )
    val node = ENode[Any](
      nodeType = null.asInstanceOf[Any],
      args = args,
      definitions = actDefs.toIndexedSeq,
      uses = actUses.toIndexedSeq
    )

    m.bindNode(node)

    // After one bindNode with 3 args:
    assertEquals(1 + argCount, m.createdRegisters) // root + args appended
    assertEquals(patDefs.size + patUses.size, m.boundSlotsCount)
    assertEquals(1, m.boundNodesCount)
    assertEquals(0, m.boundVarsCount)

    val snap = m.freeze()

    // Registers: root + 3 args captured
    assertEquals(1 + argCount, snap.registers.size)

    // Slot mapping: pattern keys -> actual slots provided by the node
    // Order: definitions then uses
    assertEquals(Some(ad1), snap.boundSlots.get(pd1))
    assertEquals(Some(ad2), snap.boundSlots.get(pd2))
    assertEquals(Some(au1), snap.boundSlots.get(pu1))
    assertEquals(3, snap.boundSlots.size)

    // One node recorded
    assertEquals(1, snap.boundNodes.size)
  }

  @Test
  def mixedProgramMultipleBindsAndFreezeAggregatesAll(): Unit = {
    val (v1 +: v2 +: Nil) = vars(2)

    val patDefs1@Seq(pd1a, pd1b) = slots(2)
    val patUses1@Seq(pu1a) = slots(1)
    val actDefs1@Seq(ad1a, ad1b) = slots(2)
    val actUses1@Seq(au1a) = slots(1)

    val patDefs2@Seq(pd2a) = slots(1)
    val patUses2@Seq(pu2a, pu2b) = slots(2)
    val actDefs2@Seq(ad2a) = slots(1)
    val actUses2@Seq(au2a, au2b) = slots(2)

    val args1Count = 2
    val args2Count = 1

    // Effects describe full program footprint (registers = args sum; vars and slots in order)
    val effects = Instruction.Effects(
      createdRegisters = args1Count + args2Count,
      boundVars = Seq(v1, v2),
      boundSlots = (patDefs1 ++ patUses1) ++ (patDefs2 ++ patUses2),
      boundNodes = 2
    )

    val root: EClassCall = null.asInstanceOf[EClassCall]
    val m = MutableMachineState[Any, ReadOnlyEGraph[Any]](root, effects)

    // Bind two variables
    val mt1: MixedTree[Any, EClassCall] = null.asInstanceOf[MixedTree[Any, EClassCall]]
    val mt2: MixedTree[Any, EClassCall] = null.asInstanceOf[MixedTree[Any, EClassCall]]
    m.bindVar(mt1)
    m.bindVar(mt2)

    // First node (2 args)
    val node1 = ENode[Any](
      nodeType = null.asInstanceOf[Any],
      args = IndexedSeq(null.asInstanceOf[EClassCall], null.asInstanceOf[EClassCall]),
      definitions = actDefs1.toIndexedSeq,
      uses = actUses1.toIndexedSeq
    )
    m.bindNode(node1)

    // Second node (1 arg)
    val node2 = ENode[Any](
      nodeType = null.asInstanceOf[Any],
      args = IndexedSeq(null.asInstanceOf[EClassCall]),
      definitions = actDefs2.toIndexedSeq,
      uses = actUses2.toIndexedSeq
    )
    m.bindNode(node2)

    // Counters
    assertEquals(1 + args1Count + args2Count, m.createdRegisters)
    assertEquals(2, m.boundVarsCount)
    assertEquals((patDefs1.size + patUses1.size) + (patDefs2.size + patUses2.size), m.boundSlotsCount)
    assertEquals(2, m.boundNodesCount)

    val snap = m.freeze()

    // Registers: root + 3 args
    assertEquals(1 + args1Count + args2Count, snap.registers.size)

    // Vars map (keys from effects)
    assertEquals(Set(v1, v2), snap.boundVars.keySet)
    assertEquals(2, snap.boundVars.size)

    // Slots mapping: program order grouped by instruction (defs then uses per node)
    assertEquals(Some(ad1a), snap.boundSlots.get(pd1a))
    assertEquals(Some(ad1b), snap.boundSlots.get(pd1b))
    assertEquals(Some(au1a), snap.boundSlots.get(pu1a))

    assertEquals(Some(ad2a), snap.boundSlots.get(pd2a))
    assertEquals(Some(au2a), snap.boundSlots.get(pu2a))
    assertEquals(Some(au2b), snap.boundSlots.get(pu2b))

    assertEquals(6, snap.boundSlots.size)

    // Two nodes recorded
    assertEquals(2, snap.boundNodes.size)
  }
}