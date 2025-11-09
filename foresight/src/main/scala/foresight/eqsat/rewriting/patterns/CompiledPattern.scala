package foresight.eqsat.rewriting.patterns

import foresight.eqsat.readonly.EGraph
import foresight.eqsat.{EClassCall, MixedTree}
import foresight.util.collections.UnsafeSeqFromArray

import scala.collection.compat._

/**
 * A compiled pattern.
 *
 * @param instructions The instructions of the compiled pattern.
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam EGraphT The type of the e-graph that the pattern is compiled for.
 */
final case class CompiledPattern[NodeT, EGraphT <: EGraph[NodeT]](pattern: MixedTree[NodeT, Pattern.Var],
                                                                  instructions: immutable.ArraySeq[Instruction[NodeT, EGraphT]]) {

  private val effects = Instruction.Effects.from(instructions)

  private val threadLocalMachinePool = new ThreadLocal[MutableMachineState.Pool[NodeT]] {
    override def initialValue(): MutableMachineState.Pool[NodeT] = {
      MutableMachineState.Pool[NodeT](effects)
    }
  }

  /**
   * Gets the thread-local pool of mutable machine states.
   * @return The thread-local pool of mutable machine states.
   */
  private def machinePool: MutableMachineState.Pool[NodeT] = threadLocalMachinePool.get()

  /**
   * Searches for matches of the pattern in an e-graph, calling a continuation for each match.
   * If the continuation returns false, the search is stopped.
   * @param call The e-class application to search for.
   * @param egraph The e-graph to search in.
   * @param continuation A continuation that is called for each match of the pattern. If the continuation returns false,
   *                     the search is stopped.
   */
  def search(call: EClassCall, egraph: EGraphT, continuation: (PatternMatch[NodeT], EGraphT) => Boolean): Unit = {
    val state = machinePool.borrow(call)
    Machine.run(egraph, state, instructions, (state: MutableMachineState[NodeT]) => {
      val m = state.toPatternMatch
      state.release()
      continuation(m, egraph)
    })
  }

  /**
   * Searches for matches of the pattern in an e-graph, calling a continuation for each match.
   * If the continuation returns false, the search is stopped.
   *
   * Borrows the machine state and passes it to the continuation. The continuation must not
   * retain the state after returning.
   *
   * @param call         The e-class application to search for.
   * @param egraph       The e-graph to search in.
   * @param continuation A continuation that is called for each match of the pattern. If the continuation returns false,
   *                     the search is stopped.
   */
  def searchBorrowed(call: EClassCall, egraph: EGraphT, continuation: (AbstractPatternMatch[NodeT], EGraphT) => Boolean): Unit = {
    val state = machinePool.borrow(call)
    Machine.run(egraph, state, instructions, (state: MutableMachineState[NodeT]) => {
      val result = continuation(state, egraph)
      state.release()
      result
    })
  }

  /**
   * Searches for matches of the pattern in an e-graph.
   * @param call The e-class application to search for.
   * @param egraph The e-graph to search in.
   * @return The matches of the pattern in the e-graph.
   */
  def search(call: EClassCall, egraph: EGraphT): Seq[PatternMatch[NodeT]] = {
    val state = machinePool.borrow(call)
    val matches = Machine.run(egraph, state, instructions).map { state =>
      PatternMatch(call, state.boundVars, state.boundSlots)
    }

    if (matches.size == 1) {
      // If there's only one match, skip the distinct call for performance
      matches
    } else {
      matches.distinct
    }
  }

  /**
   * Checks if an e-class application in an e-graph contains at least one match of the pattern.
   * @param call The e-class application to check.
   * @param egraph The e-graph to check in.
   * @return True if the pattern matches the e-class application, false otherwise.
   */
  def matches(call: EClassCall, egraph: EGraphT): Boolean = {
    val state = machinePool.borrow(call)
    var anyMatches = false
    Machine.run(egraph, state, instructions, (state: MutableMachineState[NodeT]) => {
      anyMatches = true
      state.release()
      false // Stop searching after the first match
    })
    anyMatches
  }
}

/**
 * A companion object for [[CompiledPattern]].
 */
object CompiledPattern {
  /**
   * Creates a compiled pattern from a pattern and a sequence of instructions.
   * @param pattern The pattern to compile.
   * @param instructions The instructions of the compiled pattern.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph that the pattern is compiled for.
   * @return The compiled pattern.
   */
  def apply[NodeT, EGraphT <: EGraph[NodeT]](pattern: MixedTree[NodeT, Pattern.Var],
                                             instructions: Seq[Instruction[NodeT, EGraphT]]): CompiledPattern[NodeT, EGraphT] = {
    new CompiledPattern(pattern, UnsafeSeqFromArray(instructions))
  }

  /**
   * Compiles a pattern into a compiled pattern.
   * @param pattern The pattern to compile.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph that the pattern is compiled for.
   * @return The compiled pattern.
   */
  def apply[NodeT, EGraphT <: EGraph[NodeT]](pattern: MixedTree[NodeT, Pattern.Var]): CompiledPattern[NodeT, EGraphT] = {
    CompiledPattern(pattern, PatternCompiler.compile[NodeT, EGraphT](pattern))
  }
}
