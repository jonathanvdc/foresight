package foresight.eqsat.lang

import foresight.eqsat.EClassCall
import foresight.eqsat.immutable.{EGraph, EGraphLike}

import scala.util.NotGiven

/**
 * Provides extension methods for `EGraphLike` to operate on surface expressions.
 */
extension [E, This <: EGraphLike[LanguageOp[E], This] with EGraph[LanguageOp[E]]](g: This)
  /**
   * Adds an expression to the EGraph, converting it to an `EClassCall` using the provided language and encoder.
   * @param expr The expression to add.
   * @param L Implicit language instance for converting the expression to a tree.
   * @param enc Implicit encoder for converting the expression to an `EClassCall`.
   * @return A tuple containing the `EClassCall` created from the expression and the updated EGraph.
   */
  def add(expr: E)(using L: Language[E], enc: AtomEncoder[E, EClassCall]): (EClassCall, This) =
    g.add(L.toTree[EClassCall](expr))

  /**
   * Extract a surface expression from this e-graph starting at a given root e-class.
   *
   * This is a thin convenience wrapper around [[Language.extract]],
   * using the current e-graph `g`. It:
   *   - Builds an extraction analysis from the provided cost function,
   *   - Enriches `g` with that analysis (without mutating `g`),
   *   - Reconstructs the minimal-cost surface expression rooted at `call`.
   *
   * @tparam C
   * The cost type used to rank candidate extractions.
   * @param call
   * The root e-class to extract from.
   * @param costFunction
   * Function assigning costs to surface ASTs.
   * @param L
   * (using) The `Language[E]` instance describing how to encode/decode `E`.
   * @param ord
   * (using) Ordering on costs of type `C`.
   * @return
   * The extracted surface AST `E` of minimal cost according to `costFunction`.
   * @example
   * {{{
   *   val cf: LanguageCostFunction[Expr, Int] = ...
   *   val program: Expr = g.extract(rootCall, cf)
   * }}}
   */
  def extract[C](call: EClassCall, costFunction: LanguageCostFunction[E, C])(using L: Language[E],
                                                                             ord: Ordering[C]): E =
    L.extract[C](call, g, costFunction)(using ord)

  /**
   * Insert a surface expression into this e-graph and immediately extract its minimal-cost equivalent.
   *
   * This is a thin convenience wrapper around [[Language.extract]], using the current e-graph `g`.
   *
   * @tparam C
   * The cost type used to rank candidate exactions.
   * @param exprWithCalls
   * Surface expression to add and then extract from.
   * @param costFunction
   * Function assigning costs to surface ASTs.
   * @param L
   * (using) The `Language[E]` instance describing how to encode/decode `E`.
   * @param enc
   * (using) Encoder from surface AST atoms to `EClassCall` for insertion.
   * @param ord
   * (using) Ordering on costs of type `C`.
   * @return
   * The minimal-cost expression equivalent to `exprWithCalls` under `costFunction`.
   * @example
   * {{{
   *   val cf: LanguageCostFunction[Expr, BigDecimal] = ...
   *   val best: Expr = g.extract(myExpr, cf)
   * }}}
   */
  def extract[C](exprWithCalls: E, costFunction: LanguageCostFunction[E, C])(using L: Language[E],
                                                                             enc: AtomEncoder[E, EClassCall],
                                                                             ord: Ordering[C],
                                                                             ev: NotGiven[E =:= EClassCall]): E =
    L.extract[C](exprWithCalls, g, costFunction)(using enc, ord)
