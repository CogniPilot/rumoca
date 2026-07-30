# SPEC_0040: IR Stage Contract Catalog

## Status
REFERENCE

## Summary

Lookup catalog of the per-stage DAE, Solve, and structural-lowering contract
rows referenced by [SPEC_0007](SPEC_0007_IR_PIPELINE.md).

## How To Use This Catalog

This annex holds no rules of its own. Every row below is a SPEC_0007 stage
contract and is **normative by reference from SPEC_0007**; the owning stage
section in SPEC_0007 states the governing requirement and links here. Cite rows
by their catalog ID (`DAE-C07`, `SOLVE-C13`, `STRUCT-T04`) so a reviewer can
find the exact obligation without loading the whole pipeline spec.

## Specification

### 1. DAE Stage Contract Catalog (SPEC_0007 Stage 3)

| ID | Rule | Where | Why |
|---|---|---|---|
| DAE-C01 | No source temporal operators (`pre`, `edge`, `change`, `sample`, `previous`) survive in f_x, f_z, f_m, f_c, relations, or initialization equations | DAE lowering rewrites them into Appendix B constructs: explicit `__pre__.*` inputs, relation/c variables, scheduled events, clock metadata, and ordinary equations over `v` | MLS Appendix B states the DAE as functions over `v` and `relation(v)`; source temporal operators are not computable DAE/Solve graph nodes |
| DAE-C02 | `der(x)` denotes a state-derivative coordinate | MLS Appendix B includes `ẋ` in `v`; DAE keeps that coordinate explicit and structural analysis matches it to the owning state | Source derivative syntax must resolve to a declared state coordinate |
| DAE-C03 | No `initial()` in f_x/f_z/f_m/f_c | initial phase is handled separately | Avoids mixing initialization into runtime equations |
| DAE-C04 | `pre(z)` / `pre(m)` are typed coordinates paired with their current `z` / `m` identities | runtime writes their Solve slots at event entry, and clock-associated history only at its owning tick | `pre()` exists only in AST and Flat; MLS §16 `previous()` history must not advance at unrelated events |
| DAE-C05 | Discrete Real `when` equations remain condition-activated B.1b residuals | ToDAE retains trigger/guard ownership; Solve may reject unsupported coupled systems | An explicit assignment action would silently assume a solved form that B.1b does not guarantee |
| DAE-C06 | `edge(b)` and `change(v)` are equations over typed current/pre coordinates | DAE lowering expands source operators before checked construction | Leaves no event operator for Solve lowering to interpret |
| DAE-C07 | `sample(...)` and clocked `previous(...)` are represented by typed DAE event, clock, and temporal identities plus ordinary equations over current/history coordinates | Runtime scheduling data is explicit DAE ownership; `PreviousId` and typed pre coordinates retain the owning clock without generated-name recovery | Keeps clock semantics at DAE level, compute functions ordinary, and clock history advancing once per owning tick |
| DAE-C08 | `terminal()` is represented by one typed `TerminalCoordinate` and terminal condition activation | The simulation driver writes the derived Solve slot only for the final event at the configured stop time | Preserves MLS §8.6 terminal-event behavior without embedding a phase-sensitive operator or generated parameter name in DAE graphs |
| DAE-C09 | `delay(expr, delayTime[, delayMax])` is represented by a typed `DelayId` plus a checked delay owner containing the source and proof-bearing timing form | Solve lowering derives the runtime slot; the simulation runtime refreshes it from accepted-solution history before evaluating compute graphs | Keeps transport-delay state and interpolation in the runtime while enforcing MLS delay contracts at DAE construction |
| DAE-C10 | `reinit(x, expr)` is lowered into checked guarded state updates during DAE construction | DAE lowering converts state resets into typed target/value updates over current/pre coordinates | Keeps state reset semantics in the event system instead of exposing a source operator to runtimes |
| DAE-C11 | `assert(...)` and `terminate(...)` are represented as `events.event_actions`, not as residual/value expressions | DAE lowering converts integration-flow statements into guarded event actions with source spans | Keeps Appendix B compute graphs pure while preserving solver-visible runtime actions |
| DAE-C12 | Ordinary discrete Real assignments are prohibited in `events.event_actions` | They construct B.1b residual owners instead | Event actions must not bypass coupled/nonlinear B.1b semantics |
| DAE-C13 | Checked DAE expression constructors cannot represent source temporal or flow-action operators | `rumoca-ir-dae` construction API | The Appendix-B boundary is enforced when a node is introduced, not by a later tree scan |

### 2. Solve Stage Contract Catalog (SPEC_0007 Stage 4)

| ID | Rule | Why |
|---|---|---|
| SOLVE-C01 | All ops are pure functions of `(y[], p[], t)`; only `LoadY`, `LoadP`, `Const`, math ops | No Modelica-specific ops remain |
| SOLVE-C02 | No source temporal operators (`pre`, `edge`, `change`, `sample`, `previous`) in Solve-IR | Eliminated or represented as explicit DAE metadata before Solve lowering; surviving source temporal operators are upstream bugs |
| SOLVE-C03 | No flow-action calls (`assert`, `terminate`, `reinit`) in Solve-IR scalar programs | `reinit` is already a guarded discrete update; `assert` and `terminate` lower from DAE `events.event_actions` into action metadata plus pure action-condition scalar programs |
| SOLVE-C04 | Solve slots projected from typed DAE pre/previous coordinates hold history values | Runtime writes event-owned pre slots at event entry and clock-owned history slots only when their `ClockId` ticks; generated display names do not define ownership |
| SOLVE-C05 | `initial()` is a typed activation node, never a generic P load in value programs | Runtime phase inputs cannot become ordinary values |
| SOLVE-C06 | A B.1c discrete-valued definition owns source-priority activation/value branches | Preserve assignment priority |
| SOLVE-C07 | No active branch means hold the current target | Inactive assignment semantics cannot be omitted or replaced |
| SOLVE-C08 | B.1b residuals, B.1c definitions, reinit, and condition memory are distinct typed owners | Row-role tags cannot conflate semantics |
| SOLVE-C09 | Pre policy, observation policy, and clock owner derive from each typed owner | Parallel metadata cannot disagree |
| SOLVE-C10 | A B.1c definition computes its first active value or leaves its target unchanged | Gives one local refinement obligation |
| SOLVE-C11 | Event timing is partitioned into root conditions, static arbitrary time instants, dynamic time-event rows, and periodic clock schedules | `events` owns zero-crossing and one-shot/dynamic time events; `clocks` owns periodic schedules derived from `sample`/clock metadata |
| SOLVE-C12 | Terminal-event and transport-delay inputs use explicit runtime-managed P-slots | Solve metadata names and computes these inputs; numeric runtimes activate the terminal slot at the stop-time event and refresh delay slots from accepted-solution history |
| SOLVE-C13 | Valid `ComputeBlock`s scalarize via fallible `rumoca-eval-solve::to_scalar_program_block(&block)` | Tensor-agnostic adapters call it and propagate span-bearing metadata errors |
| SOLVE-C14 | Scalarization is a backend/evaluator choice, not an IR or lowering choice | Do not flatten tensor nodes in `rumoca-ir-solve` / `rumoca-phase-solve`; IR crates must not define scalarization helpers |
| SOLVE-C15 | Forward and reverse AD products are Solve artifacts, not base Solve IR fields | Keeps base Solve payloads lean while allowing Rumoca-owned JVP/VJP/adjoint paths for runtime and generated targets |
| SOLVE-C16 | Jacobian products live in `SolveArtifacts`, not base `SolveProblem` | Avoids unconditional AD materialization for codegen/IDE paths that do not consume them |
| SOLVE-C17 | Structural sparsity is derived with `SolveArtifacts`, never accepted as a raw hint | A false-negative pattern can corrupt compressed AD and sparse solves |
| SOLVE-C18 | Mass-matrix form lives in `ContinuousSolveArtifacts`, not DAE | It is solver-facing derived metadata, not canonical Modelica DAE semantics |
| SOLVE-C19 | BLT orderings from DAE-IR MAY drive `ComputeBlock` layout | Reuses upstream structural analysis |

### 3. Structural Lowering Transformation Catalog (SPEC_0007 Structural Lowering Scope)

These are the transformations that are in scope for DAE-to-DAE structural
lowering. Anything absent from this catalog requires a SPEC_0007 update.

| ID | Transformation | Owning module | Notes |
|---|---|---|---|
| STRUCT-T01 | Source pre-lowering (`pre(v)` → typed paired pre-coordinate) | `rumoca-phase-dae` | Runs before finalized DAE construction and applies to every Appendix-B partition. See `DAE-C01`/`DAE-C04`. |
| STRUCT-T02 | Alias elimination | `rumoca-phase-dae` | Folds trivial equalities into the variable graph. |
| STRUCT-T03 | Structural index reduction (Pantelides-style) | `rumoca-phase-structural` | For states without a `der(state)` equation, differentiate a non-ODE constraint referencing that state and substitute. Index-1 lift is supported; higher-index lifts are an explicit subset of Pantelides. |
| STRUCT-T04 | State demotion | `rumoca-phase-structural` | Demote over-classified states whose derivative is structurally unreachable. |
| STRUCT-T05 | BLT ordering | `rumoca-phase-structural` | Block-lower-triangular ordering of equations for sequential solve. |
| STRUCT-T06 | Algebraic-loop tearing (Greedy Cellier) | `rumoca-phase-structural::tearing` | Identifies tear variables for cyclic algebraic blocks. |
| STRUCT-T07 | State selection | `rumoca-phase-structural` | Pick a consistent state set. |

## References

- [SPEC_0007](SPEC_0007_IR_PIPELINE.md) — owning stage contracts and placement
  requirements for every row in this catalog.
- [SPEC_0036](SPEC_0036_VALID_BY_CONSTRUCTION_IR.md) — construction rules that
  make these contracts unrepresentable to violate.
- [MLS Appendix B](https://specification.modelica.org/maint/3.6/modelica-dae-representation.html)
