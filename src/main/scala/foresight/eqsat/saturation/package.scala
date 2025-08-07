package foresight.eqsat

/**
 * Defines core abstractions and concrete strategies for saturation-based rewriting
 * over e-graphs. Saturation refers to the process of repeatedly applying rewrite rules until
 * no new terms can be discovered or until a budget is exhausted.
 *
 * The key component is the [[Strategy]] trait, which encapsulates a step-by-step transformation
 * over an e-graph with user-defined data and iteration control. Strategies can be composed,
 * repeated, rebased, and extended with metadata.
 *
 * ## Key Concepts
 *
 *  - [[Strategy]]: Core trait representing a saturation step that transforms an e-graph.
 *  - [[SearchAndApply]]: Separates search from application logic for fine-grained control.
 *  - [[MaximalRuleApplication]]: Applies every match for each rule once per iteration (no fixpoint).
 *  - [[MaximalRuleApplicationWithCaching]]: Same as above, but avoids reapplying matches across iterations.
 *  - [[BackoffRuleApplication]]: Applies rules with dynamic quotas and cooldown periods to balance rule usage.
 *  - [[StochasticRuleApplication]]: Applies a batch of matches selected probabilistically based on priorities.
 *  - [[StochasticRuleApplicationWithCaching]]: Same as above, but avoids reapplying matches across iterations.
 *
 * ## Typical Usage
 *
 * More sophisticated strategies are constructed using combinators such as [[foresight.eqsat.saturation.Strategy.thenApply]],
 * [[foresight.eqsat.saturation.Strategy.repeatUntilStable]], [[foresight.eqsat.saturation.Strategy.withIterationLimit]],
 * [[foresight.eqsat.saturation.Strategy.withTimeout]] and [[foresight.eqsat.saturation.Strategy.thenRebase]].
 * For example, to run a maximal application strategy to convergence with a timeout and
 * periodic rebasing:
 *
 * {{{
 *   val strategy = MaximalRuleApplication(myRules)
 *     .withTimeout(30.seconds)
 *     .repeatUntilStable
 *     .thenRebase(extractor)
 *     .withIterationLimit(5)
 *     .repeatUntilStable
 *
 *   val result = strategy.run(myEGraph)
 * }}}
 */
package object saturation
