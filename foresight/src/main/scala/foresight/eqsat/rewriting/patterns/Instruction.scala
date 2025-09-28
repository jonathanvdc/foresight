package foresight.eqsat.rewriting.patterns

import foresight.eqsat.{EClassCall, ENode, MixedTree, Slot}
import foresight.eqsat.collections.SlotSeq
import foresight.eqsat.readonly.ReadOnlyEGraph

import java.util
import scala.collection.compat.immutable.ArraySeq

/**
 * An instruction for the pattern-matching virtual machine.
 *
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam EGraphT The type of the e-graph.
 */
trait Instruction[NodeT, -EGraphT <: ReadOnlyEGraph[NodeT]] {
  /** A compact summary of what this instruction will create and bind. */
  def effects: Instruction.Effects

  /**
   * Executes the instruction on the given machine state.
   * @param ctx The execution context for running instructions.
   * @return true to continue search, false to abort.
   */
  def execute(ctx: Instruction.Execution[NodeT, EGraphT]): Boolean
}

/**
 * A companion object for the `Instruction` trait.
 */
object Instruction {
  /**
   * Summarizes the static effects of an instruction on the matching VM.
   * @param createdRegisters The number of virtual-machine registers created.
   * @param boundVars The sequence of pattern variables bound.
   * @param boundSlots The sequence of slots bound (ordered).
   * @param boundNodes The number of e-nodes bound.
   */
  final case class Effects(
    createdRegisters: Int,
    boundVars: ArraySeq[Pattern.Var],
    boundSlots: SlotSeq,
    boundNodes: Int
  ) {
    /**
     * Combine this effect summary with another by summing counts and concatenating sequences.
     * @param other The other effect summary to combine with.
     * @return The combined effect summary.
     */
    def ++(other: Effects): Effects = Effects(
      createdRegisters = this.createdRegisters + other.createdRegisters,
      boundVars = this.boundVars ++ other.boundVars,
      boundSlots = this.boundSlots ++ other.boundSlots,
      boundNodes = this.boundNodes + other.boundNodes
    )
  }

  /** A companion object for [[Effects]]. */
  object Effects {
    /** An effect summary for an instruction that does nothing. */
    val none: Effects = Effects(0, ArraySeq.empty, SlotSeq.empty, 0)

    /**
     * Aggregate a collection of effect summaries into a single summary.
     * @param effects The collection of effect summaries to aggregate.
     * @return The aggregated effect summary.
     */
    def aggregate(effects: Iterable[Effects]): Effects =
      effects.foldLeft(none)(_ ++ _)

    /**
     * Aggregate the effects of a collection of instructions.
     * @param instructions The collection of instructions to aggregate the effects of.
     * @return The aggregated effect summary.
     */
    def from(instructions: Iterable[Instruction[_, _]]): Effects =
      aggregate(instructions.map(_.effects))
  }

  /**
   * An execution context that manages running a sequence of instructions without extra allocations.
   * Reused via a user-creatable pool. Each instruction advances execution by calling `continue()`
   * or reports errors via `error(...)`.
   */
  sealed trait Execution[NodeT, +EGraphT <: ReadOnlyEGraph[NodeT]] {
    /** The current e-graph. */
    def graph: EGraphT

    /** The current machine state. */
    def machine: MutableMachineState[NodeT]

    /** The current instruction pointer. This is the index of the instruction that will be executed next. */
    def instructionPointer: Int

    /** Continue execution with the current machine, advancing the instruction pointer by one. */
    def continue(): Boolean

    /**
     * Continue execution with a different machine (used for forked search paths).
     * Restores the original machine afterward.
     * @param newMachine The new machine state to use for the next instruction.
     * @return true to continue search, false to abort (as determined by continuations and instruction logic).
     */
    def continueWith(newMachine: MutableMachineState[NodeT]): Boolean

    /**
     * Report an error and delegate to the failure continuation.
     * @param err The error to report.
     * @return true to continue search, false to abort (as determined by continuations and instruction logic).
     */
    def error(err: MachineError[NodeT]): Boolean

    /** Run starting at the current instruction pointer. */
    def run(): Boolean

    /**
     * Terminate this execution context and release the machine.
     */
    def terminate(): Unit
  }

  /**
   * An execution context that manages running a sequence of instructions without extra allocations.
   * Reused via a user-creatable pool. Each instruction advances execution by calling `continue()`
   * or reports errors via `error(...)`.
   */
  private final class ExecutionImpl[NodeT, EGraphT <: ReadOnlyEGraph[NodeT]](private val pool: Execution.Pool[NodeT, EGraphT])
    extends Execution[NodeT, EGraphT] {
    // Mutable fields intentionally use null-sentinels for cross-version compatibility and low overhead.
    private var _graph: EGraphT = null.asInstanceOf[EGraphT]
    private var _machine: MutableMachineState[NodeT] = null
    private var _instructions: ArraySeq[Instruction[NodeT, EGraphT]] = null
    private var _onSuccess: MutableMachineState[NodeT] => Boolean = null
    private var _onFailure: (Execution[NodeT, EGraphT], MachineError[NodeT]) => Boolean = null
    private var _ip: Int = 0

    override def instructionPointer: Int = _ip

    /** Initialize this execution context for a fresh run. */
    def init(graph: EGraphT,
             machine: MutableMachineState[NodeT],
             instructions: ArraySeq[Instruction[NodeT, EGraphT]],
             onSuccess: MutableMachineState[NodeT] => Boolean,
             onFailure: (Execution[NodeT, EGraphT], MachineError[NodeT]) => Boolean): Unit = {
      _graph = graph
      _machine = machine
      _instructions = instructions
      _onSuccess = onSuccess
      _onFailure = onFailure
      _ip = 0
    }

    /** The current e-graph. */
    def graph: EGraphT = _graph

    /** The current machine state. */
    def machine: MutableMachineState[NodeT] = _machine

    /** Transfer ownership of the current machine to the caller. The caller is responsible for releasing it. */
    private def transferMachine(): MutableMachineState[NodeT] = {
      val m = _machine
      _machine = null
      m
    }

    /**
     * Continue execution with the current machine, advancing the instruction pointer by one.
     * @return true to continue search, false to abort (as determined by continuations and instruction logic)
     */
    def continue(): Boolean = {
      _ip += 1
      if (_ip >= _instructions.length) {
        // Successful completion of all instructions.
        _onSuccess(transferMachine())
      } else {
        _instructions(_ip).execute(this)
      }
    }

    /**
     * Continue execution with a different machine (used for forked search paths),
     * restoring the original machine afterwards.
     */
    def continueWith(newMachine: MutableMachineState[NodeT]): Boolean = {
      val saved = _machine
      val savedIp = _ip
      _machine = newMachine
      val res = continue()
      _machine = saved
      _ip = savedIp
      res
    }

    /** Report an error and delegate to the failure continuation. */
    def error(err: MachineError[NodeT]): Boolean = {
      if (_onFailure == null) {
        // No failure continuation; just continue search. Release the machine.
        terminate()
        true
      } else {
        _onFailure(this, err)
      }
    }

    /** Run starting at the current instruction pointer. */
    def run(): Boolean = {
      if (_instructions == null) true
      else if (_ip >= _instructions.length) _onSuccess(_machine)
      else _instructions(_ip).execute(this)
    }

    override def terminate(): Unit = {
      transferMachine().release()
    }

    /** Reset all internal state before pooling. */
    private[Instruction] def reset(): Unit = {
      _graph = null.asInstanceOf[EGraphT]
      _machine = null
      _instructions = null
      _onSuccess = null
      _onFailure = null
      _ip = 0
    }
  }

  object Execution {
    private val threadLocalPool: ThreadLocal[Pool[_, _ <: ReadOnlyEGraph[_]]] = new ThreadLocal[Pool[_, _ <: ReadOnlyEGraph[_]]] {
      override def initialValue(): Pool[_, _ <: ReadOnlyEGraph[_]] = {
        new Pool()
      }
    }

    /** Get the thread-local pool of execution contexts. */
    def pool[NodeT, EGraphT <: ReadOnlyEGraph[NodeT]]: Pool[NodeT, EGraphT] = {
      threadLocalPool.get().asInstanceOf[Pool[NodeT, EGraphT]]
    }

    /** Pool for reusing `Execution` contexts to avoid allocations. */
    final class Pool[NodeT, EGraphT <: ReadOnlyEGraph[NodeT]](initialCapacity: Int = 0) {
      private val stack = new util.ArrayDeque[ExecutionImpl[NodeT, EGraphT]](math.max(1, initialCapacity))

      /** Borrow an execution context from the pool. */
      private def borrow(): ExecutionImpl[NodeT, EGraphT] = {
        val ctx = stack.pollFirst()
        if (ctx != null) ctx else new ExecutionImpl[NodeT, EGraphT](this)
      }

      /** Release a previously borrowed execution context back to the pool. */
      private def release(ctx: ExecutionImpl[NodeT, EGraphT]): Unit = {
        if (ctx ne null) {
          ctx.reset()
          stack.addFirst(ctx)
        }
      }

      /**
       * Run a sequence of instructions on a machine and graph, reusing an execution context from the pool.
       *
       * @param graph        The e-graph to match against.
       * @param machine      The initial machine state. This method takes ownership of the machine and will release it.
       * @param instructions The instructions to apply to the machine.
       * @param onSuccess    A continuation called for each successful termination of the machine.
       *                     If it returns false, the search is aborted.
       * @param onFailure    A continuation called for each unsuccessful termination of the machine.
       *                     If it returns false, the search is aborted.
       * @return true if the search completed (all continuations returned true); false if aborted early.
       */
      def run(
               graph: EGraphT,
               machine: MutableMachineState[NodeT],
               instructions: ArraySeq[Instruction[NodeT, EGraphT]],
               onSuccess: MutableMachineState[NodeT] => Boolean,
               onFailure: (Execution[NodeT, EGraphT], MachineError[NodeT]) => Boolean
             ): Boolean = {
        val ctx = borrow()
        try {
          ctx.init(graph, machine, instructions, onSuccess, onFailure)
          ctx.run()
        } finally {
          release(ctx)
        }
      }

      /**
       * Run a sequence of instructions on a machine and graph, reusing an execution context from the pool.
       *
       * @param graph        The e-graph to match against.
       * @param machine      The initial machine state. This method takes ownership of the machine and will release it.
       * @param instructions The instructions to apply to the machine.
       * @param onSuccess    A continuation called for each successful termination of the machine.
       * @return true if the search completed (all continuations returned true); false if aborted early.
       */
      def run(
               graph: EGraphT,
               machine: MutableMachineState[NodeT],
               instructions: ArraySeq[Instruction[NodeT, EGraphT]],
               onSuccess: MutableMachineState[NodeT] => Boolean
             ): Boolean = {
        run(graph, machine, instructions, onSuccess, null)
      }
    }
  }

  /**
   * An instruction that binds an e-node to a register.
   * @param register The index of the register to bind the e-node to.
   * @param nodeType The type of the e-node to bind.
   * @param definitions The slot variables that the e-node defines.
   * @param uses The slot variables that the e-node uses.
   * @param argCount The number of arguments the e-node has.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph.
   */
  final case class BindNode[NodeT, EGraphT <: ReadOnlyEGraph[NodeT]](register: Int,
                                                                     nodeType: NodeT,
                                                                     definitions: SlotSeq,
                                                                     uses: SlotSeq,
                                                                     argCount: Int)
    extends Instruction[NodeT, EGraphT] {

    override val effects: Instruction.Effects = Instruction.Effects(
      createdRegisters = argCount,
      boundVars = ArraySeq.empty[Pattern.Var],
      boundSlots = definitions ++ uses,
      boundNodes = 1
    )

    override def execute(ctx: Instruction.Execution[NodeT, EGraphT]): Boolean = {
      val machine = ctx.machine
      val call = machine.registerAt(register)

      // Local helpers moved to use ctx.graph / ctx.machine
      def matchesSlot(expected: Slot, actual: Slot): Boolean = {
        machine.boundSlotOrElse(expected, null) match {
          case null => true
          case bound if bound == actual => true
          case _ => false
        }
      }
      def allSlotsMatch(expected: SlotSeq, actual: SlotSeq): Boolean = {
        if (expected.length != actual.length) return false
        var i = 0
        while (i < expected.length) {
          if (!matchesSlot(expected(i), actual(i))) return false
          i += 1
        }
        true
      }

      // Stream over candidates without allocating intermediate collections.
      // If we have one match, bind it and continue; if we have multiple matches,
      // fork the machine for each additional match and continue each one.
      // If we have no matches, report an error.
      val it = ctx.graph.nodes(call, nodeType).iterator
      var firstMatch: ENode[NodeT] = null
      var continueSearch = true
      while (continueSearch && it.hasNext) {
        val node = it.next()
        if (node.args.size == argCount &&
            allSlotsMatch(definitions, node.definitions) &&
            allSlotsMatch(uses, node.uses)) {
          if (firstMatch eq null) {
            firstMatch = node
          } else {
            val forked = machine.fork()
            forked.bindNode(node)
            continueSearch = ctx.continueWith(forked)
          }
        }
      }

      if (!continueSearch) {
        continueSearch
      } else if (firstMatch eq null) {
        ctx.error(MachineError.NoMatchingNode(this, call))
      } else {
        machine.bindNode(firstMatch)
        ctx.continue()
      }
    }
  }

  /**
   * An instruction that binds a pattern variable to an e-class call in register.
   * @param register The index of the register to bind the variable to.
   * @param variable The variable to bind.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph.
   */
  final case class BindVar[NodeT, EGraphT <: ReadOnlyEGraph[NodeT]](register: Int,
                                                                    variable: Pattern.Var)
    extends Instruction[NodeT, EGraphT] {

    override val effects: Instruction.Effects = Instruction.Effects(
      createdRegisters = 0,
      boundVars = ArraySeq(variable),
      boundSlots = SlotSeq.empty,
      boundNodes = 0
    )

    override def execute(ctx: Instruction.Execution[NodeT, EGraphT]): Boolean = {
      val value = MixedTree.Atom[NodeT, EClassCall](ctx.machine.registerAt(register))
      ctx.machine.bindVar(value)
      ctx.continue()
    }
  }

  /**
   * An instruction that compares two registers for equality.
   * @param register1 The index of the first register to compare.
   * @param register2 The index of the second register to compare.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph.
   */
  final case class Compare[NodeT, EGraphT <: ReadOnlyEGraph[NodeT]](register1: Int, register2: Int)
    extends Instruction[NodeT, EGraphT] {

    override def effects: Instruction.Effects = Instruction.Effects.none

    override def execute(ctx: Instruction.Execution[NodeT, EGraphT]): Boolean = {
      val m = ctx.machine
      if (ctx.graph.areSame(m.registerAt(register1), m.registerAt(register2))) {
        ctx.continue()
      } else {
        ctx.error(MachineError.InconsistentVars(this, m.registerAt(register1), m.registerAt(register2)))
      }
    }
  }
}
