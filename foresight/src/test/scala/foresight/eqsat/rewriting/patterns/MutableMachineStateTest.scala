package foresight.eqsat.rewriting.patterns

import org.junit.Test
import org.junit.Assert._
import foresight.eqsat.{Slot, EClassCall, ENode, MixedTree}
import foresight.eqsat.collections.SlotSeq
import foresight.util.collections.UnsafeSeqFromArray

import scala.collection.compat._

class MutableMachineStateTest {

  // Helpers to make fresh values
  private def vars(n: Int): immutable.ArraySeq[Pattern.Var] = UnsafeSeqFromArray(Seq.fill(n)(Pattern.Var.fresh()))

  private def slots(n: Int): SlotSeq = SlotSeq.from(Seq.fill(n)(Slot.fresh()))

  @Test
  def allocateInitializesRootAndCounts(): Unit = {
    val v = Pattern.Var.fresh()
    val s = Slot.fresh()

    // Effects: 0 created registers beyond root, 1 var slot, 1 slot, 0 nodes
    val effects = Instruction.Effects(
      createdRegisters = 0,
      boundVars = immutable.ArraySeq(v),
      boundSlots = SlotSeq(s),
      boundNodes = 0
    )

    val root: EClassCall = null.asInstanceOf[EClassCall] // not dereferenced
    val m = MutableMachineState[Any](root, effects)

    // After initRoot: one register used (the root), others zero
    assertEquals(1, m.createdRegisters)
    assertEquals(0, m.boundVarsCount)
    assertEquals(0, m.boundSlotsCount)
    assertEquals(0, m.boundNodesCount)

    // Freezing without any binds reflects just the root register
    val snap = m.freeze()
    assertEquals(Seq(root), snap.registers)
    assertTrue(snap.boundVars.isEmpty)
    assertTrue(snap.boundSlots.isEmpty)
    assertTrue(snap.boundNodes.isEmpty)
  }

  @Test
  def bindVarAppendsAndFreezeUsesEffectsVarsAsKeys(): Unit = {
    val vs = vars(2)
    val v1 = vs(0)
    val v2 = vs(1)

    // Effects declares the variable binding order (keys); values are supplied later via bindVar
    val effects = Instruction.Effects(
      createdRegisters = 0,
      boundVars = immutable.ArraySeq(v1, v2),
      boundSlots = SlotSeq.empty,
      boundNodes = 0
    )

    val root: EClassCall = null.asInstanceOf[EClassCall]
    val m = MutableMachineState[Any](root, effects)

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
      boundVars = immutable.ArraySeq.empty, // no vars in this test
      boundSlots = patDefs ++ patUses, // order must match bindNode recording: defs then uses
      boundNodes = 1
    )

    val root: EClassCall = null.asInstanceOf[EClassCall]
    val m = MutableMachineState[Any](root, effects)

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
    val Seq(v1, v2) = vars(2)

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
      boundVars = immutable.ArraySeq(v1, v2),
      boundSlots = (patDefs1 ++ patUses1) ++ (patDefs2 ++ patUses2),
      boundNodes = 2
    )

    val root: EClassCall = null.asInstanceOf[EClassCall]
    val m = MutableMachineState[Any](root, effects)

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

  @Test
  def poolBorrowInitializesRootAndCounts(): Unit = {
    val effects = Instruction.Effects(
      createdRegisters = 0,
      boundVars = immutable.ArraySeq.empty,
      boundSlots = SlotSeq.empty,
      boundNodes = 0
    )

    val pool = MutableMachineState.Pool[Any](effects, initialCapacity = 0)
    assertEquals(0, pool.available)

    val root1: EClassCall = null.asInstanceOf[EClassCall]
    val ms1 = pool.borrow(root1)

    // On fresh borrow: only root register is counted, others zero
    assertEquals(1, ms1.createdRegisters)
    assertEquals(0, ms1.boundVarsCount)
    assertEquals(0, ms1.boundSlotsCount)
    assertEquals(0, ms1.boundNodesCount)

    val snap1 = ms1.freeze()
    assertEquals(Seq(root1), snap1.registers)

    // Return to pool
    pool.release(ms1)
    assertEquals(1, pool.available)
  }

  @Test
  def poolReusesInstanceAndReinitResetsCounts(): Unit = {
    // Make effects allocate space for 2 vars so we can mutate counters
    val Seq(v1, v2) = List.fill(2)(Pattern.Var.fresh())
    val effects = Instruction.Effects(
      createdRegisters = 0,
      boundVars = immutable.ArraySeq(v1, v2),
      boundSlots = SlotSeq.empty,
      boundNodes = 0
    )

    val pool = MutableMachineState.Pool[Any](effects, initialCapacity = 1)

    val root1: EClassCall = null.asInstanceOf[EClassCall]
    val ms1 = pool.borrow(root1)

    // Mutate state to non-zero indices
    val mt: MixedTree[Any, EClassCall] = null.asInstanceOf[MixedTree[Any, EClassCall]]
    ms1.bindVar(mt)
    ms1.bindVar(mt)
    assertEquals(2, ms1.boundVarsCount)

    pool.release(ms1)
    assertEquals(1, pool.available)

    val root2: EClassCall = null.asInstanceOf[EClassCall]
    val ms2 = pool.borrow(root2)

    // Same instance and fully reset to just-root
    assertTrue(ms1 eq ms2)
    assertEquals(1, ms2.createdRegisters)
    assertEquals(0, ms2.boundVarsCount)
    assertEquals(0, ms2.boundSlotsCount)
    assertEquals(0, ms2.boundNodesCount)

    val snap2 = ms2.freeze()
    assertEquals(Seq(root2), snap2.registers)

    pool.release(ms2)
  }

  @Test
  def poolAvailableTracksReleases(): Unit = {
    val effects = Instruction.Effects(0, immutable.ArraySeq.empty, SlotSeq.empty, 0)
    val pool = MutableMachineState.Pool[Any](effects)

    val a = pool.borrow(null.asInstanceOf[EClassCall])
    val b = pool.borrow(null.asInstanceOf[EClassCall])
    assertEquals(0, pool.available) // nothing returned yet

    pool.release(a)
    assertEquals(1, pool.available)
    pool.release(b)
    assertEquals(2, pool.available)

    // Borrow twice reduces available accordingly
    val x = pool.borrow(null.asInstanceOf[EClassCall])
    assertEquals(1, pool.available)
    val y = pool.borrow(null.asInstanceOf[EClassCall])
    assertEquals(0, pool.available)

    // Clean up
    pool.release(x)
    pool.release(y)
  }

  @Test
  def poolRejectsMismatchedEffectsOnRelease(): Unit = {
    val effects1 = Instruction.Effects(0, immutable.ArraySeq.empty, SlotSeq.empty, 0)
    val effects2 = Instruction.Effects(1, immutable.ArraySeq.empty, SlotSeq.empty, 0) // different profile

    val p1 = MutableMachineState.Pool[Any](effects1)
    val p2 = MutableMachineState.Pool[Any](effects2)

    val s = p1.borrow(null.asInstanceOf[EClassCall])

    // Attempting to release into a pool with different effects will throw
    try {
      p2.release(s)
      fail("Expected IllegalArgumentException due to effects mismatch")
    } catch {
      case _: IllegalArgumentException => // expected
    } finally {
      // Properly return to original pool
      p1.release(s)
    }
  }

  @Test
  def poolUsesLifoOrderOnReuse(): Unit = {
    val effects = Instruction.Effects(0, immutable.ArraySeq.empty, SlotSeq.empty, 0)
    val pool = MutableMachineState.Pool[Any](effects)

    val s1 = pool.borrow(null.asInstanceOf[EClassCall])
    val s2 = pool.borrow(null.asInstanceOf[EClassCall])

    // Return in order: s1 then s2 (s2 will be on top)
    pool.release(s1)
    pool.release(s2)

    val a = pool.borrow(null.asInstanceOf[EClassCall])
    assertTrue("Expected LIFO reuse to return s2 first", a eq s2)
    val b = pool.borrow(null.asInstanceOf[EClassCall])
    assertTrue("Next is s1", b eq s1)

    // Clean up
    pool.release(a)
    pool.release(b)
  }

  @Test
  def instanceReleaseDelegatesToPool(): Unit = {
    val effects = Instruction.Effects(0, immutable.ArraySeq.empty, SlotSeq.empty, 0)
    val pool = MutableMachineState.Pool[Any](effects)

    val ms = pool.borrow(null.asInstanceOf[EClassCall])
    assertEquals(0, pool.available)
    // Call the instance method; should put it back in the same pool
    ms.release()
    assertEquals(1, pool.available)
  }

  @Test
  def forkProducesIndependentEqualCopy(): Unit = {
    val Seq(v1, v2) = List.fill(2)(Pattern.Var.fresh())
    val pd = Slot.fresh()
    val effects = Instruction.Effects(
      createdRegisters = 2, // enough capacity for two args appended by a node
      boundVars = immutable.ArraySeq(v1, v2),
      boundSlots = SlotSeq(pd),
      boundNodes = 1
    )

    val pool = MutableMachineState.Pool[Any](effects)
    val root: EClassCall = null.asInstanceOf[EClassCall]
    val ms = pool.borrow(root)

    // Populate state: two vars and one node with one slot use
    val mt1: MixedTree[Any, EClassCall] = null.asInstanceOf[MixedTree[Any, EClassCall]]
    val mt2: MixedTree[Any, EClassCall] = null.asInstanceOf[MixedTree[Any, EClassCall]]
    ms.bindVar(mt1)
    ms.bindVar(mt2)

    val actualSlot = Slot.fresh()
    val node = ENode[Any](
      nodeType = null.asInstanceOf[Any],
      args = IndexedSeq(null.asInstanceOf[EClassCall], null.asInstanceOf[EClassCall]),
      definitions = IndexedSeq.empty,
      uses = IndexedSeq(actualSlot)
    )
    ms.bindNode(node)

    val snap = ms.freeze()

    // Fork and compare contents
    val copy = ms.fork()
    assertTrue(copy ne ms)
    assertEquals(ms.createdRegisters, copy.createdRegisters)
    assertEquals(ms.boundVarsCount, copy.boundVarsCount)
    assertEquals(ms.boundSlotsCount, copy.boundSlotsCount)
    assertEquals(ms.boundNodesCount, copy.boundNodesCount)

    val snapCopy = copy.freeze()
    assertEquals(snap.registers, snapCopy.registers)
    assertEquals(snap.boundVars.keySet, snapCopy.boundVars.keySet)
    assertEquals(snap.boundSlots.keySet, snapCopy.boundSlots.keySet)
    assertEquals(snap.boundSlots.size, snapCopy.boundSlots.size)
    assertEquals(snap.boundNodes.size, snapCopy.boundNodes.size)

    // Mutate original (reinit) and ensure fork-copy remains unchanged
    val newRoot: EClassCall = null.asInstanceOf[EClassCall]
    ms.reinit(newRoot)
    val snapAfter = ms.freeze()
    assertEquals(Seq(newRoot), snapAfter.registers)
    // copy still has old snapshot contents
    assertEquals(snap.registers, snapCopy.registers)

    // Return both to pool
    ms.release()
    copy.release()
  }

  @Test
  def standaloneApplyCreatesPrivatePoolReleaseDoesNotThrow(): Unit = {
    val effects = Instruction.Effects(0, immutable.ArraySeq.empty, SlotSeq.empty, 0)
    val root: EClassCall = null.asInstanceOf[EClassCall]
    val lone = MutableMachineState[Any](root, effects)
    // Safe to call, even though we don't have a handle to its private pool
    lone.release()
  }
}
