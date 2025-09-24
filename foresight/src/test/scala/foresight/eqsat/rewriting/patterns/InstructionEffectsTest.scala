package foresight.eqsat.rewriting.patterns

import org.junit.Test
import org.junit.Assert._
import foresight.eqsat._
import foresight.eqsat.collections.SlotSeq
import foresight.util.collections.UnsafeSeqFromArray

import scala.collection.compat._

class InstructionEffectsTest {

  // Small helper to count lengths by constructing fresh Pattern.Var / Slot values.
  private def vars(n: Int): immutable.ArraySeq[Pattern.Var] = UnsafeSeqFromArray(Seq.fill(n)(Pattern.Var.fresh()))

  private def slots(n: Int): SlotSeq = SlotSeq.from(Seq.fill(n)(Slot.fresh()))

  @Test
  def noneIsIdentity(): Unit = {
    val a = Instruction.Effects(createdRegisters = 2, boundVars = vars(1), boundSlots = slots(3), boundNodes = 5)
    assertEquals(a, a ++ Instruction.Effects.none)
    assertEquals(a, Instruction.Effects.none ++ a)
  }

  @Test
  def combineSumsCountsAndConcatenatesSequences(): Unit = {
    val e1 = Instruction.Effects(createdRegisters = 1, boundVars = vars(2), boundSlots = slots(1), boundNodes = 3)
    val e2 = Instruction.Effects(createdRegisters = 4, boundVars = vars(1), boundSlots = slots(2), boundNodes = 7)
    val c = e1 ++ e2

    assertEquals(1 + 4, c.createdRegisters)
    assertEquals(2 + 1, c.boundVars.length)
    assertEquals(1 + 2, c.boundSlots.length)
    assertEquals(3 + 7, c.boundNodes)
  }

  @Test
  def combineIsAssociative(): Unit = {
    val a = Instruction.Effects(1, vars(1), slots(0), 2)
    val b = Instruction.Effects(3, vars(2), slots(2), 5)
    val c = Instruction.Effects(7, vars(0), slots(4), 11)

    val left = (a ++ b) ++ c
    val right = a ++ (b ++ c)
    assertEquals(left.createdRegisters, right.createdRegisters)
    assertEquals(left.boundVars.length, right.boundVars.length)
    assertEquals(left.boundSlots.length, right.boundSlots.length)
    assertEquals(left.boundNodes, right.boundNodes)
  }

  @Test
  def aggregateEffectsMatchesFoldWithIdentity(): Unit = {
    val effects = List(
      Instruction.Effects(1, vars(0), slots(1), 2),
      Instruction.Effects(0, vars(3), slots(0), 0),
      Instruction.Effects(2, vars(1), slots(2), 4)
    )
    val agg = Instruction.Effects.aggregate(effects)
    val manual = effects.foldLeft(Instruction.Effects.none)(_ ++ _)

    assertEquals(manual.createdRegisters, agg.createdRegisters)
    assertEquals(manual.boundVars.length, agg.boundVars.length)
    assertEquals(manual.boundSlots.length, agg.boundSlots.length)
    assertEquals(manual.boundNodes, agg.boundNodes)
  }

  @Test
  def aggregateFromInstructionsMapsThenAggregates(): Unit = {
    // Dummy instruction that only supplies a pre-baked effects summary; execute is never used here.
    final case class DummyInstr(e: Instruction.Effects)
      extends Instruction[Any, ReadOnlyEGraph[Any]] {
      override def effects: Instruction.Effects = e

      override def execute(ctx: Instruction.Execution[Any, ReadOnlyEGraph[Any]]): Boolean = {
        ctx.continue()
      }
    }

    val instrs = List(
      DummyInstr(Instruction.Effects(1, vars(2), slots(0), 3)),
      DummyInstr(Instruction.Effects(0, vars(0), slots(5), 0)),
      DummyInstr(Instruction.Effects(4, vars(1), slots(1), 2))
    )

    val viaFrom = Instruction.Effects.from(instrs)
    val viaAgg = Instruction.Effects.aggregate(instrs.map(_.effects))

    assertEquals(viaAgg.createdRegisters, viaFrom.createdRegisters)
    assertEquals(viaAgg.boundVars.length, viaFrom.boundVars.length)
    assertEquals(viaAgg.boundSlots.length, viaFrom.boundSlots.length)
    assertEquals(viaAgg.boundNodes, viaFrom.boundNodes)
  }

  @Test
  def bindNodeEffects(): Unit = {
    val defs = SlotSeq(Slot.fresh(), Slot.fresh())
    val uses = SlotSeq(Slot.fresh())
    val instr = Instruction.BindNode(
      register   = 0,
      nodeType   = "dummy",
      definitions = defs,
      uses        = uses,
      argCount    = 3
    )
    val eff = instr.effects
    assertEquals(3, eff.createdRegisters)
    assertTrue(eff.boundVars.isEmpty)
    assertEquals(defs ++ uses, eff.boundSlots)
    assertEquals(1, eff.boundNodes)
  }

  @Test
  def bindVarEffects(): Unit = {
    val v = Pattern.Var.fresh()
    val instr = Instruction.BindVar[Any, ReadOnlyEGraph[Any]](
      register = 2,
      variable = v
    )
    val eff = instr.effects
    assertEquals(0, eff.createdRegisters)
    assertEquals(Seq(v), eff.boundVars)
    assertTrue(eff.boundSlots.isEmpty)
    assertEquals(0, eff.boundNodes)
  }

  @Test
  def compareEffects(): Unit = {
    val instr = Instruction.Compare[Any, ReadOnlyEGraph[Any]](register1 = 1, register2 = 4)
    val eff = instr.effects
    assertEquals(0, eff.createdRegisters)
    assertTrue(eff.boundVars.isEmpty)
    assertTrue(eff.boundSlots.isEmpty)
    assertEquals(0, eff.boundNodes)
  }
}
