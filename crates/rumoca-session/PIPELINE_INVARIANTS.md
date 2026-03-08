# rumoca-session Pipeline Invariants and Failure Contracts

This document defines the runtime and API invariants used by the session orchestrator.
It is the implementation companion for the active specs (not a replacement for them).

## Phase Ordering Invariant

`rumoca-session` compiles a model in this fixed order:

1. `Resolve`
2. `Instantiate`
3. `Typecheck`
4. `Flatten`
5. `ToDae`

`Typecheck` intentionally runs **after** instantiation so dimensions/modifiers are validated
with full modification context (MLS behavior for evaluated dimensions).

## Resolution Contract

- `build_resolved_with_diagnostics()` is the canonical tree-build API for diagnostic-first flows.
- Single-document sessions use strict unresolved-name/function behavior.
- Multi-document/library indexing flows use tolerant resolve flags to avoid aborting on unrelated
  external library symbols.

## Per-Model Compile Contract

- `compile_model()` is strict: returns `Result<CompilationResult>`.
  - `NeedsInner` is surfaced as an error with explicit missing inner names.
  - `Failed` returns a phase-qualified message (`Instantiate`, `Typecheck`, `Flatten`, `ToDae`).
- `compile_model_phases()` is structured: returns `PhaseResult` so callers can classify
  success vs `NeedsInner` vs phase failure.
- `compile_model_internal()` is the single phase pipeline entry point used by strict and
  strict-reachable-with-recovery flows.

## Mode Vocabulary Contract

- `CompilationMode::StrictReachable` and
  `CompilationMode::StrictReachableWithRecovery` are the compile-mode vocabulary.
- `IndexingMode::Tolerant` is the indexing-mode vocabulary.
- Strict-reachable variants currently share the same behavior path.

## Strict-Reachable-With-Recovery Contract

- `compile_model_strict_reachable_with_recovery()` must preserve requested-model status while
  collecting failures from the requested model's reachable transitive dependency closure.
- Parse failures from documents outside that closure must not be surfaced as strict-compile
  failures.
- Parse and resolve diagnostics are included as `ModelFailureDiagnostic` entries.
- The requested model is still treated strictly (`requested_result` and `requested_succeeded()`).

## Diagnostics Contract

- Phase failures must stay phase-local in `PhaseResult::Failed`.
- `error_code` is preserved when available.
- `CompilationSummary` must be computable from `PhaseResult` values without re-running phases.

## Experiment Annotation Contract

`CompilationResult` carries normalized simulation metadata extracted from root
`annotation(experiment(...))`:

- `experiment_start_time`
- `experiment_stop_time`
- `experiment_tolerance`
- `experiment_interval`
- `experiment_solver`

Normalization rules:

- Non-finite values are dropped.
- `StopTime < StartTime` is treated as invalid (`stop_time = None`).
- Tolerance/interval must be positive finite values.
- OpenModelica flags (`__OpenModelica_simulationFlags`) and common solver keys are accepted.

## Performance/Concurrency Contract

- Resolved tree and model names are cached and invalidated on document mutation.
- Parallel compile helpers (`compile_models_parallel`, `compile_all_parallel`) must share
  the same resolved snapshot.
- Global phase timing counters are additive, resettable, and must remain non-normative
  instrumentation only (no behavior changes).
