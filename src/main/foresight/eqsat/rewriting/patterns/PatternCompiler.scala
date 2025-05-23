package foresight.eqsat.rewriting.patterns

import foresight.eqsat.{EGraph, EGraphLike, MixedTree}

import scala.collection.mutable

/**
 * A compiler that compiles patterns into a list of instructions for the pattern-matching virtual machine.
 */
object PatternCompiler {
  /**
   * Compiles a pattern into a list of instructions for the pattern-matching virtual machine.
   * @param pattern The pattern to compile.
   * @return The list of instructions.
   */
  def compile[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](pattern: MixedTree[NodeT, Pattern[NodeT]]): List[Instruction[NodeT, EGraphT]] = {
    val compiler = new PatternCompiler[NodeT, EGraphT]()
    compiler.compile(pattern, 0)
  }
}


/**
 * A compiler that compiles patterns into a list of instructions for the pattern-matching virtual machine.
 */
private final class PatternCompiler[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]] {
  /**
   * A mapping from expression variable IDs to registers.
   */
  private val varToReg = new mutable.ListMap[Pattern.Var[NodeT], Int]()

  /**
   * The current length of the tape.
   */
  private var tapeLength: Int = 0

  def compile(pattern: MixedTree[NodeT, Pattern[NodeT]], out: Int): List[Instruction[NodeT, EGraphT]] = {
    pattern match {
      case MixedTree.Call(p) => p match {
        // If we encountered a wildcard, then we want to either bind the wildcard to a concrete expression *or*
        // ensure that the wildcard is bound consistently.
        case w: Pattern.Var[NodeT] =>
          varToReg get w match {
            case None =>
              varToReg.put(w, out)
              List(Instruction.BindVar(out, w))
            case Some(i) => List(Instruction.Compare(out, i))
          }
      }

      // If we encountered any other type of expression, we want to unpack it and compile its children.
      case MixedTree.Node(nodeType, definitions, uses, args) =>
        val intro = Instruction.BindNode[NodeT, EGraphT](out, nodeType, definitions, uses, args.length)
        val tapeLengthAtIntro = tapeLength + 1
        tapeLength += args.length
        val argInstructions = args
          .zipWithIndex
          .map(pair => compile(pair._1, tapeLengthAtIntro + pair._2))
          .foldLeft(List.empty[Instruction[NodeT, EGraphT]])(_ ++ _)

        intro :: argInstructions
    }
  }
}
