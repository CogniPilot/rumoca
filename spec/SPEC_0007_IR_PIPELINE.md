# SPEC_0007: IR Pipeline (AST → Flat → DAE → Solve)

## Status
ACCEPTED

## Summary

Rumoca transforms Modelica through AST → Flat → DAE → Solve IRs. Each stage
defines its contents, ownership, and boundary.

## Specification

```
Modelica source (.mo)
        │
        ▼  rumoca-phase-parse
  ┌──────────┐
  │   AST    │  rumoca-ir-ast        ◄─ codegen: formatters, pretty-printers,
  └────┬─────┘                           documentation generators
       │  rumoca-phase-resolve, rumoca-phase-typecheck,
       │  rumoca-phase-instantiate
       ▼
  ┌──────────┐
  │   Flat   │  rumoca-ir-flat       ◄─ codegen: flat Modelica export
  └────┬─────┘
       │  rumoca-phase-flatten, rumoca-phase-dae
       ▼
  ┌──────────┐
  │   DAE    │  rumoca-ir-dae        ◄─ codegen: FMI export, DAE-level
  └────┬─────┘                           symbolic/array backends
       │  rumoca-phase-solve              (CasADi, SymPy, JAX)
       ▼
  ┌──────────┐
  │  Solve   │  rumoca-ir-solve      ◄─ codegen/JIT: numeric C/Rust,
  └──────────┘                           CUDA C/NVRTC, MLIR/LLVM, kernels
```

**Codegen targets the lowest proven-valid IR it needs — no lower.**

| Backend | IR level | Why |
|---|---|---|
| Formatter, doc generator | AST | Needs syntax + spans |
| Flat Modelica export | Flat | Original expression structure |
| FMI export, DAE-readable C/Fortran, CasADi, SymPy, JAX-style symbolic/array targets | DAE | MLS B.1 form, source traceability |
| Numeric sim, C/Rust kernels, JIT, MLIR, CUDA/GPU | Solve | Register-machine plus tensor bytecode |

`rumoca-phase-codegen` renders text; execution adapters wrap toolchains and
runtimes without owning compiler semantics.

Every IR that crosses the code-generation boundary MUST already satisfy its
stage invariants by construction. A target manifest selects the exact canonical
or checked export IR it consumes; the compiler supplies a typed, read-only
semantic view of that artifact to MiniJinja. Rendering MUST NOT resolve names,
infer types or shapes, lower to another IR, mutate its input, or repair an
invalid artifact.

The code-generation architecture is therefore:

```text
proven-valid IR -> typed semantic template view -> target.toml + MiniJinja -> artifacts
```

This boundary applies uniformly to syntax, Resolve, Flat, DAE, Solve, and
checked export IRs. Adding a target for an already-supported IR requires only a
target directory. Supporting a new IR requires one target-neutral semantic view
and capability vocabulary, never a target-language renderer in Rust. Export IRs
remain projections and do not become canonical pipeline stages merely because a
target manifest can select them.

---

### Stage 1 — AST (`rumoca-ir-ast`)

**What it is:** Parser output: concrete syntax, comments, and spans.

**Contract:**
- Represents source text structure, not language semantics.
- No name resolution, type information, or class lookup.
- Every node carries a source `Span`; later AST merges must preserve parser
  provenance instead of rewriting source ids.

**What to do here:** Parsing, formatting, early syntax diagnostics.

**What NOT to do here:** Name lookup, class instantiation, type inference,
equation manipulation.

---

### Stage 2 — Flat (`rumoca-ir-flat`)

**What it is:** The instantiated class hierarchy with fully-qualified names.

**Contract:**
- No unresolved class references.
- No modification chains; all modifications have been applied.
- Virtual connection graphs satisfy MLS §9.4 forest and root invariants.
- Arrays remain symbolic (not scalarized).
- Function bodies remain structured in `functions`.
- `pre()`, `der()`, `initial()`, and other Modelica built-ins are still present
  as expression nodes — semantic lowering has not occurred.

**Do here:** Resolution, instantiation, post-instantiation type checking, and
flattening. **Do not:** solve equations, eliminate Modelica operators, or
generate simulation code.

**Cross-cutting rules (Flat through DAE):**

| Rule | Why |
|---|---|
| Instantiation and flattening are separate logical phases | Instantiation applies modifications + builds `InstanceOverlay`/`InstancedTree`; production then runs `typecheck_instanced` before flattening traverses the overlay, expands connections, and produces `flat::Model`. |
| Arrays stay symbolic through Flat and DAE | Backends requesting scalar form call scalarization in structural/solver layers with shape metadata, not via display-string parsing |
| Function algorithms remain structured in `Flat.functions`/`Dae.functions` | Function bodies are not lowered into solver equation buckets |
| Model algorithms lower to DAE only when they fit the declarative subset | Unsupported forms fail explicitly with `ED013` |
| Post-resolution declaration identity is keyed by `DefId`, not strings | Hashing rendered names, `VarName`, flat names, cached display strings, rendered `ComponentPath`, or rendered `ComponentReference` after resolution is a phase-boundary bug. Carry `DefId` for declarations and structured instance identity where one declaration has multiple instantiated meanings. |
| Flat `TypeId` is the resolved effective type of that concrete instance | Two instances originating from one `DefId` may have different effective types after redeclare or modification. DAE type catalogs key by this identity and retain `DefId` only as declaration provenance. |
| Semantic phases do not recover name hierarchy by tokenizing flattened strings | The AST, `QualifiedName`, `ComponentReference`, `DefId`, scope tree, and phase metadata carry name structure. Splitting `a.b.c` text inside compiler/evaluator/lowering logic means structure was lost too early. Textual path parsing is allowed only at source/protocol/config/display boundaries while structured IR replaces it. |

---

### Stage 3 — DAE (`rumoca-ir-dae`)

**What it is:** The computable MLS Appendix B canonical DAE after eliminating
Modelica-specific operators: pure functions over
`v := [p; t; ẋ; x; y; z; m; pre(z); pre(m)]`.

**The four MLS B.1 functions:**

| ID   | Function           | Role                        |
|------|--------------------|-----------------------------|
| B.1a | `fx(v, c) = 0`    | Continuous DAE residual     |
| B.1b | `fz(v, c) = 0`    | Coupled discrete Real residual |
| B.1c | `m := fm(v, c)`   | Solved discrete-valued assignment |
| B.1d | `fc(relation(v))` | Event conditions            |

**DAE representation rule:** DAE is the canonical MLS Appendix B model, not a
solver cache. One canonical variable catalog owns stable variable identity;
typed views classify `p`, `x`, `y`, `z`, and `m`, while input/output causality
is orthogonal metadata. Dedicated continuous, initialization, discrete,
condition, event, and clock systems own their respective behavior. Schema
version 11 is the only supported wire version; every other version is rejected
without superseded readers or adapters.

Finalized DAE is valid by construction. Invariant-bearing fields are private,
checked child constructors establish local expression/type/shape/domain
contracts, and root construction establishes catalog membership and
cross-object contracts. Production phases do not receive a weaker DAE-shaped
draft and do not run a whole-root validation pass.

`Dae::construct` lends sequential semantic-owner closures one generatively
branded aggregate. All expressions use one DAE-wide dense arena with parallel
node, provenance, and type columns plus packed variadic operands. Every source
node carries its exact occurrence span; generated nodes carry typed generation
and the nearest responsible source span.

DAE fields represent Modelica semantics, source identity, or stable Appendix B
partitions. Mass matrices, Jacobians, BLT orderings, tearing choices,
state-selection reports, and scalarized variants belong in structural results
or Solve artifacts.

The condition system independently owns typed relation and condition catalogs.
Conditions refer to relation leaves by typed identity; relation and condition
counts are not required to match. Runtime metadata passes must not rediscover
roots from continuous equations. Non-Appendix-B event-generating numeric
surfaces introduced by lowering belong to the event system as synthetic roots.
The event-free MLS `abs(...)` and `sign(...)` functions do not create roots.

Only private current-version wire records derive `Deserialize`. Decoding
constructs checked children and then the checked root; derived counts and
indexes are recomputed rather than accepted as wire inputs.

**Contract:**

| Rule | Where | Why |
|---|---|---|
| No source temporal operators (`pre`, `edge`, `change`, `sample`, `previous`) survive in f_x, f_z, f_m, f_c, relations, or initialization equations | DAE lowering rewrites them into Appendix B constructs: explicit `__pre__.*` inputs, relation/c variables, scheduled events, clock metadata, and ordinary equations over `v` | MLS Appendix B states the DAE as functions over `v` and `relation(v)`; source temporal operators are not computable DAE/Solve graph nodes |
| `der(x)` denotes a state-derivative coordinate | MLS Appendix B includes `ẋ` in `v`; DAE keeps that coordinate explicit and structural analysis matches it to the owning state | Source derivative syntax must resolve to a declared state coordinate |
| No `initial()` in f_x/f_z/f_m/f_c | initial phase is handled separately | Avoids mixing initialization into runtime equations |
| `pre(z)` / `pre(m)` are typed coordinates paired with their current `z` / `m` identities | runtime writes their Solve slots at event entry, and clock-associated history only at its owning tick | `pre()` exists only in AST and Flat; MLS §16 `previous()` history must not advance at unrelated events |
| Discrete Real `when` equations remain condition-activated B.1b residuals | ToDAE retains trigger/guard ownership; Solve may reject unsupported coupled systems | An explicit assignment action would silently assume a solved form that B.1b does not guarantee |
| `edge(b)` and `change(v)` are equations over typed current/pre coordinates | DAE lowering expands source operators before checked construction | Leaves no event operator for Solve lowering to interpret |
| `sample(...)` and clocked `previous(...)` are represented by typed DAE event, clock, and temporal identities plus ordinary equations over current/history coordinates | Runtime scheduling data is explicit DAE ownership; `PreviousId` and typed pre coordinates retain the owning clock without generated-name recovery | Keeps clock semantics at DAE level, compute functions ordinary, and clock history advancing once per owning tick |
| `terminal()` is represented by one typed `TerminalCoordinate` and terminal condition activation | The simulation driver writes the derived Solve slot only for the final event at the configured stop time | Preserves MLS §8.6 terminal-event behavior without embedding a phase-sensitive operator or generated parameter name in DAE graphs |
| `delay(expr, delayTime[, delayMax])` is represented by a typed `DelayId` plus a checked delay owner containing the source and proof-bearing timing form | Solve lowering derives the runtime slot; the simulation runtime refreshes it from accepted-solution history before evaluating compute graphs | Keeps transport-delay state and interpolation in the runtime while enforcing MLS delay contracts at DAE construction |
| `reinit(x, expr)` is lowered into checked guarded state updates during DAE construction | DAE lowering converts state resets into typed target/value updates over current/pre coordinates | Keeps state reset semantics in the event system instead of exposing a source operator to runtimes |
| `assert(...)` and `terminate(...)` are represented as `events.event_actions`, not as residual/value expressions | DAE lowering converts integration-flow statements into guarded event actions with source spans | Keeps Appendix B compute graphs pure while preserving solver-visible runtime actions |
| Ordinary discrete Real assignments are prohibited in `events.event_actions` | They construct B.1b residual owners instead | Event actions must not bypass coupled/nonlinear B.1b semantics |
| Checked DAE expression constructors cannot represent source temporal or flow-action operators | `rumoca-ir-dae` construction API | The Appendix-B boundary is enforced when a node is introduced, not by a later tree scan |

**Do here:** DAE lowering, structural transformation, and separately returned
structural analysis. **Do not:** allocate registers, lower bytecode, emit
templates, or store backend artifacts in DAE.

**Prohibited:** mutable cache fields, merged variable-kind maps, solver row
bytecode/layout, model-level `when_clauses`, and unlowered synchronous
operators in solver equation partitions.

---

### Stage 4 — Solve (`rumoca-ir-solve`)

**What it is:** A register-machine format for DAE functions. MLS B.1 functions
become `ComputeBlock` graphs of scalar programs and tensor nodes without new
mathematical content. `ComputeNode` preserves matrix, stencil, reduction, map,
and broadcast structure for native backends or scalar fallback.

Canonical terminology:

| Term | Current type/name | Meaning |
|---|---|---|
| `ScalarProgram` | `Vec<LinearOp>` | A flat register program that produces one scalar output |
| `ScalarProgramBlock` | `ScalarProgramBlock` | A group of scalar programs with one output per program |
| `TensorProgramNode` | `ComputeNode::{MatMul, LinSolve, AffineStencil, ...}` | A tensor-level kernel with explicit shape/layout metadata and scalar fallback |
| `ComputeBlock` | `ComputeBlock` | Ordered mix of scalar program blocks and tensor program nodes |

New Solve-IR APIs use `ScalarProgram` / `ScalarProgramBlock` terminology, not
`RowBlock` / `ScalarRows`.

`ComputeNode::AffineStencil` is source-proven: it comes from preserved DAE
structured-family domains plus affine operand proofs. It carries the compact
iteration domain and strides; Solve lowering must not recover stencils by
scanning unstructured scalar rows after structured-family metadata is discarded.

The root `schema_version` is mandatory on serialized Solve payloads.
Deserializers reject unsupported versions and pre-versioned `ComputeBlock` row
payloads.

`SolveProblem` is the base lowered problem. Backend products that are expensive
or outside the canonical MLS DAE (mass-matrix form, Jacobian-vector
scalar-program blocks) live in `SolveArtifacts`, materialized by
`rumoca-phase-solve` only when a backend/template/runtime boundary asks.
`lower_solve_problem` must not eagerly populate them.

**Contract:**

| Rule | Why |
|---|---|
| All ops are pure functions of `(y[], p[], t)`; only `LoadY`, `LoadP`, `Const`, math ops | No Modelica-specific ops remain |
| No source temporal operators (`pre`, `edge`, `change`, `sample`, `previous`) in Solve-IR | Eliminated or represented as explicit DAE metadata before Solve lowering; surviving source temporal operators are upstream bugs |
| No flow-action calls (`assert`, `terminate`, `reinit`) in Solve-IR scalar programs | `reinit` is already a guarded discrete update; `assert` and `terminate` lower from DAE `events.event_actions` into action metadata plus pure action-condition scalar programs |
| Solve slots projected from typed DAE pre/previous coordinates hold history values | Runtime writes event-owned pre slots at event entry and clock-owned history slots only when their `ClockId` ticks; generated display names do not define ownership |
| `initial()` is a typed activation node, never a generic P load in value programs | Runtime phase inputs cannot become ordinary values |
| A B.1c discrete-valued definition owns source-priority activation/value branches | Preserve assignment priority |
| No active branch means hold the current target | Inactive assignment semantics cannot be omitted or replaced |
| B.1b residuals, B.1c definitions, reinit, and condition memory are distinct typed owners | Row-role tags cannot conflate semantics |
| Pre policy, observation policy, and clock owner derive from each typed owner | Parallel metadata cannot disagree |
| A B.1c definition computes its first active value or leaves its target unchanged | Gives one local refinement obligation |
| Event timing is partitioned into root conditions, static arbitrary time instants, dynamic time-event rows, and periodic clock schedules | `events` owns zero-crossing and one-shot/dynamic time events; `clocks` owns periodic schedules derived from `sample`/clock metadata |
| Terminal-event and transport-delay inputs use explicit runtime-managed P-slots | Solve metadata names and computes these inputs; numeric runtimes activate the terminal slot at the stop-time event and refresh delay slots from accepted-solution history |
| Valid `ComputeBlock`s scalarize via fallible `rumoca-eval-solve::to_scalar_program_block(&block)` | Tensor-agnostic adapters call it and propagate span-bearing metadata errors |
| Scalarization is a backend/evaluator choice, not an IR or lowering choice | Do not flatten tensor nodes in `rumoca-ir-solve` / `rumoca-phase-solve`; IR crates must not define scalarization helpers |
| Forward and reverse AD products are Solve artifacts, not base Solve IR fields | Keeps base Solve payloads lean while allowing Rumoca-owned JVP/VJP/adjoint paths for runtime and generated targets |
| Jacobian products live in `SolveArtifacts`, not base `SolveProblem` | Avoids unconditional AD materialization for codegen/IDE paths that do not consume them |
| Structural sparsity is derived with `SolveArtifacts`, never accepted as a raw hint | A false-negative pattern can corrupt compressed AD and sparse solves |
| Mass-matrix form lives in `ContinuousSolveArtifacts`, not DAE | It is solver-facing derived metadata, not canonical Modelica DAE semantics |
| BLT orderings from DAE-IR MAY drive `ComputeBlock` layout | Reuses upstream structural analysis |

Steady-state objectives, adjoints, parameter sensitivities, and
optimizer-facing projections are runtime or generated-target products layered
over Solve artifacts, not canonical `SolveProblem` payload fields.

**Do here:** lower DAE-IR expression trees + for-loops to `LinearOp` sequences
and preserve tensor nodes/sparsity metadata for downstream consumers.

Sparsity metadata follows [SPEC_0039](SPEC_0039_PROOF_CARRYING_SPARSITY.md):
it is a constructor-derived may-depend certificate, distinct from numerical
zeros and target storage format. Compact affine patterns originate from
SPEC_0032 owners rather than scalar-row recovery.
**Do NOT do here:** DAE-level structural transformations, MLS semantics changes,
expression-level symbolic rewrites, concrete JIT/toolchain invocation, CUDA
runtime compilation, native object loading, or Jinja/minijinja template
rendering (those live in DAE-IR/upstream lowering, `rumoca-exec-*`, or
`rumoca-phase-codegen`, respectively).

---

### Structural Lowering Scope

Rumoca performs OpenModelica-class structural lowering between DAE and Solve.
Structural lowering is DAE-to-DAE: each pass consumes a finalized DAE and
returns another finalized DAE through root-owned checked changes. Partial
mutation, independently replayable proof receipts, and mutable partition
callbacks are prohibited. The supported transformations are listed here to
keep scope and ownership clear.

**In scope:**

| Transformation | Owning module | Notes |
|---|---|---|
| Source pre-lowering (`pre(v)` → typed paired pre-coordinate) | `rumoca-phase-dae` | Runs before finalized DAE construction and applies to every Appendix-B partition. See Stage 3 Contract. |
| Alias elimination | `rumoca-phase-dae` | Folds trivial equalities into the variable graph. |
| Structural index reduction (Pantelides-style) | `rumoca-phase-structural` | For states without a `der(state)` equation, differentiate a non-ODE constraint referencing that state and substitute. Index-1 lift is supported; higher-index lifts are an explicit subset of Pantelides. |
| State demotion | `rumoca-phase-structural` | Demote over-classified states whose derivative is structurally unreachable. |
| BLT ordering | `rumoca-phase-structural` | Block-lower-triangular ordering of equations for sequential solve. |
| Algebraic-loop tearing (Greedy Cellier) | `rumoca-phase-structural::tearing` | Identifies tear variables for cyclic algebraic blocks. |
| State selection | `rumoca-phase-structural` | Pick a consistent state set. |

**Placement requirement:**

DAE structural transformations live in `rumoca-phase-structural`, return a
finalized DAE, and keep analysis products outside DAE. `rumoca-phase-solve`
only lowers finalized DAE. General dummy derivatives, unrelated symbolic
simplification, and control-design linearization require a spec update.

## References

- `SPEC_0029` — Crate boundary rules
- `SPEC_0021` — Maintainability and deterministic collection rules
