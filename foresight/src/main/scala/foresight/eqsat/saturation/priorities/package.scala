package foresight.eqsat.saturation

/**
 * Provides strategies for prioritizing rule matches in equality saturation.
 *
 * In saturation-based rewrite systems, many rule matches may be discovered in a single iteration.
 * Applying all of them can be expensive, redundant, or counterproductive. This package defines
 * abstractions and implementations for prioritizing and selecting a subset of matches
 * to apply per iteration.
 *
 * The core abstraction is [[MatchPriorities]], which defines a trait for assigning priority scores to rule matches and
 * determining the batch size to apply. These priorities are consumed by stochastic strategies like
 * [[foresight.eqsat.saturation.StochasticRuleApplication]], which use them to bias sampling.
 *
 * ## Included prioritization strategies
 *
 *   - [[UniformPriorities]] assigns equal priority to all matches, applying up to a fixed batch size.
 *   - [[ReweightedPriorities]] is a compositional trait for transforming priorities produced by another strategy.
 *   - [[CurveFittedPriorities]] reweights matches using a [[foresight.util.random.DiscreteDistribution]].
 *
 * ## Usage example
 * {{{
 * val base = MyDomainPriorities
 * val curve = ShiftedGeometricDistribution(0.5)
 * val priorities = CurveFittedPriorities(base, curve)
 * }}}
 *
 * These prioritizers can be plugged into [[StochasticRuleApplication]] to guide rule application.
 */
package object priorities
