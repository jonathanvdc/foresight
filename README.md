# Foresight

![Build](https://github.com/jonathanvdc/foresight/actions/workflows/ci.yml/badge.svg)
![Scala](https://img.shields.io/badge/scala-2.11%20|%202.13%20|%203-red?logo=scala)
![License](https://img.shields.io/github/license/jonathanvdc/foresight)
[![Docs](https://img.shields.io/badge/docs-latest-blue)](https://jonathanvdc.github.io/foresight/latest/api)
![GitHub package](https://img.shields.io/badge/GitHub%20Packages-published-blue?logo=github)

Foresight is a Scala library for **equality saturation**, a technique that enables compilers and program transformers to explore many semantically equivalent rewrites of a program simultaneously.
By separating the application of rewrite rules from the decision‑making process, equality saturation allows for more principled and comprehensive optimization.

Whereas existing equality saturation frameworks such as [egg](https://egraphs‑good.github.io/egg/) rely on sequential rule execution, Foresight introduces a new architectural model built around **parallelism**, **flexibility**, and **extensibility**.
It offers both immutable (purely functional) and mutable e‑graph implementations, making it well‑suited to a wide range of compiler infrastructure scenarios.

Foresight’s architecture includes slotted e‑graph representations with native support for bound variables, a parallel rewriting pipeline, a modular strategy interface, and a suite of built‑in saturation strategies.
These innovations enable safe speculative rewriting, efficient parallel execution, and flexible rewrite control.

---

## Features

### Immutable, Thread‑Safe E‑Graphs
Foresight offers an immutable e‑graph built using purely functional data structures. Every operation returns a new, structurally consistent e‑graph without a rebuild phase. This makes reasoning about transformations simpler and enables speculative or parallel execution without side effects.

### High‑Performance Mutable E‑Graphs
For performance‑critical workloads, Foresight also provides a mutable e‑graph backed by a dedicated hash‑consing implementation. It supports in‑place addition and merging of e‑classes, delivering substantially lower latency than the immutable variant for sequential saturation workloads. Both variants share the same interface and are interchangeable across all strategies and analyses.

### Slotted E‑Graphs with Native Binding Support
E‑classes are parameterized by [slots](https://dl.acm.org/doi/10.1145/3729326), a mechanism that encodes variable binding directly in the e‑graph. This allows Foresight to correctly and efficiently unify expressions that differ only in variable names, making it ideal for functional intermediate representations with bound variables (e.g., lambdas).

### Parallel Rule Matching and Application
Rewriting in Foresight is divided into four fine‑grained stages: pattern matching, command generation, simplification, and graph mutation. The first three stages are embarrassingly parallel, and the final mutation stage benefits from deferred command batching and optimized application.

### Metadata and Analyses
Users can attach domain‑specific metadata (such as types, constants, or cost estimates) to e‑classes. Metadata is updated in parallel and remains consistent with the evolving graph. This enables type‑safe rewriting, cost‑guided extraction, and analysis‑informed rule application.

### Composable Strategy Interface
Rewrite control is decoupled from the engine using a modular strategy interface. Users can define custom saturation strategies with iteration limits, timeouts, stopping conditions, or metadata integration. Built‑in strategy combinators make it easy to experiment with complex workflows declaratively.

### Flexible Built‑In Saturation Strategies
Foresight ships with several ready‑to‑use saturation strategies:

- **`MaximalRuleApplication`** – performs a single exhaustive pass over all rules per iteration.
- **`BackoffRuleApplication`** – assigns each rule a dynamic match quota and a cooldown period, preventing any single rule from dominating the search.
- **`StochasticRuleApplication`** – uses weighted random sampling guided by user‑defined `MatchPriorities` to apply a batch of matches each iteration, enabling efficient exploration of large rule sets.

Caching variants of the last two strategies avoid reapplying matches across iterations.

### Cost‑Guided Extraction
Foresight includes a built‑in analysis that tracks the lowest‑cost term in each equivalence class using a user‑defined cost model. Extraction is incremental and can occur during or after saturation, supporting efficient selection of optimized expressions.

### Configurable Parallel Execution
All parallel behavior in Foresight is implemented via a pluggable `ParallelMap` interface. Users can select or implement custom concurrency backends for fine‑grained control over resource usage, instrumentation, or cancellation behavior.
