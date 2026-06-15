# SPEC_0007: IR Pipeline (AST → Flat → DAE → Solve)

## Status
ACCEPTED

## Summary

Rumoca transforms Modelica through AST → Flat → DAE → Solve IRs. Each
stage owns contract.

## The Four IR Stages

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

**Codegen targets the lowest IR it needs — no lower.**

| Backend | IR level | Why |
|---|---|---|
| Formatter, doc generator | AST | Needs syntax + spans |
| Flat Modelica export | Flat | Original expression structure |
| FMI export, DAE-readable C/Fortran, CasADi, SymPy, JAX-style symbolic/array targets | DAE | MLS B.1 form, source traceability |
| Numeric sim, C/Rust kernels, JIT, MLIR, CUDA/GPU | Solve | Register-machine plus tensor bytecode |

**Template/codegen ownership:** `rumoca-phase-codegen` renders text. Execution
adapters wrap toolchains, packaging, runtime calls, or JIT APIs, not semantics,
DAE lowering, structural rewrites, or template policy.

**Neutral codegen:** IR; no shims.

---

### Stage 1 — AST (`rumoca-ir-ast`)

**What it is:** The parser output: concrete syntax with comments and spans.

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

**What it is:** The instantiated, modified class hierarchy: variables,
equations, and algorithms with fully-qualified names.

**Contract:**
- No unresolved class references.
- No modification chains; all modifications have been applied.
- Arrays remain symbolic (not scalarized).
- Function bodies remain structured in `functions`.
- `pre()`, `der()`, `initial()`, and other Modelica built-ins are still present
  as expression nodes — semantic lowering has not occurred.

**What to do here:** Name resolution, instantiation, post-instantiation
type checking, flattening, structural transformations that preserve Modelica
expression form.

**What NOT to do here:** Solving equations, eliminating Modelica-specific
operators, generating simulation code.

**Cross-cutting rules (Flat through DAE):**

| Rule | Why |
|---|---|
| Instantiation and flattening are separate logical phases | Instantiation applies modifications + builds `InstanceOverlay`/`InstancedTree`; production then runs `typecheck_instanced` before flattening traverses the overlay, expands connections, and produces `flat::Model`. |
| Arrays stay symbolic through Flat and DAE | Backends requesting scalar form call scalarization in structural/solver layers with shape metadata, not via display-string parsing |
| Function algorithms remain structured in `Flat.functions`/`Dae.functions` | Function bodies are not lowered into solver equation buckets |
| Model algorithms lower to DAE only when they fit the declarative subset | Unsupported forms fail explicitly with `ED013` |
| Post-resolution compiler identity is keyed by `DefId`, not strings | Hashing rendered names, `VarName`, flat names, cached display strings, rendered `ComponentPath`, or rendered `ComponentReference` after resolution is a phase-boundary bug. Carry `DefId`; semantic keys may be `DefId` or structured keys whose identity fields are all `DefId` values. |
| Semantic phases do not recover name hierarchy by tokenizing flattened strings | The AST, `QualifiedName`, `ComponentReference`, `DefId`, scope tree, and phase metadata carry name structure. Splitting `a.b.c` text inside compiler/evaluator/lowering logic means structure was lost too early. Textual path parsing is allowed only at source/protocol/config/display boundaries while structured IR replaces it. |

---

### Stage 3 — DAE (`rumoca-ir-dae`)

**What it is:** A computable mathematical representation matching the MLS
Appendix B canonical DAE. Modelica-specific operators have been eliminated.
The result is a set of pure functions over the variable vector
`v := [p; t; ẋ; x; y; z; m; pre(z); pre(m)]`.

**The four MLS B.1 functions:**

| ID   | Function           | Role                        |
|------|--------------------|-----------------------------|
| B.1a | `fx(v, c) = 0`    | Continuous DAE residual     |
| B.1b | `fz(v, c) = 0`    | Discrete real update        |
| B.1c | `fm(v, c) = 0`    | Discrete-valued update      |
| B.1d | `fc(relation(v))` | Event conditions            |

**DAE representation rule:** DAE is the lean canonical MLS Appendix B model,
not a solver work cache. Variables stay partitioned by kind under
`DaeVariables`; event behavior lives in `discrete`, `conditions`, `events`, and
`clocks`. Serialized DAE exposes MLS/root template keys through serde
flattening; Rust stays partitioned. The root `schema_version` is mandatory and
unsupported versions are rejected.

DAE fields represent Modelica semantics, source identity, or stable MLS Appendix
B partitions. Backend products such as mass matrices, Jacobians, BLT orderings,
tearing choices, state-selection reports, and scalarized variants belong in
structural analysis results or Solve artifacts.

`conditions.relations` owns MLS Appendix B relation surfaces. Runtime metadata
passes must not rediscover roots from continuous equations. Non-Appendix-B
runtime surfaces, such as numeric roots from `abs(...)` or `sign(...)`, belong
in `events.synthetic_root_conditions`.

Optional same-version DAE fields may use `#[serde(default)]` only when absence
has the same meaning as the default. Incompatible schema changes bump
`schema_version`.

**Contract:**

| Rule | Where | Why |
|---|---|---|
| No source temporal operators (`pre`, `edge`, `change`, `sample`, `previous`) survive in f_x, f_z, f_m, f_c, relations, or initialization equations | DAE lowering rewrites them into Appendix B constructs: explicit `__pre__.*` inputs, relation/c variables, scheduled events, clock metadata, and ordinary equations over `v` | MLS Appendix B states the DAE as functions over `v` and `relation(v)`; source temporal operators are not computable DAE/Solve graph nodes |
| No `der()` on RHS | derivatives flow via `dae.states` + equation structure | Inline `der()` would hide state identity |
| No `initial()` in f_x/f_z/f_m/f_c | initial phase is handled separately | Avoids mixing initialization into runtime equations |
| `pre(z)` / `pre(m)` are `__pre__.*` entries in `dae.parameters` | runtime writes slots at event entry per Stage 4 (`SolveLayout::pre_param_bindings`) | The Modelica `pre()` operator exists only in AST and Flat |
| `edge(b)` and `change(v)` are equations over current values and `__pre__.*` inputs | DAE lowering expands source operators before validation | Leaves no event operator for Solve lowering to interpret |
| `sample(...)` and clocked `previous(...)` are represented by DAE event/clock metadata plus ordinary equations over current/pre slots | Runtime scheduling data is explicit DAE metadata; sampled values are `__pre__.*` reads where needed | Keeps clock semantics at DAE level while keeping compute functions ordinary |
| `reinit(x, expr)` is lowered into guarded discrete state-update equations before DAE validation | DAE lowering converts state resets into ordinary Appendix B update equations over current/pre slots | Keeps state reset semantics in the numeric update system instead of exposing a source operator to runtimes |
| `assert(...)` and `terminate(...)` are represented as `events.event_actions`, not as residual/value expressions | DAE lowering converts integration-flow statements into guarded event actions with source spans | Keeps Appendix B compute graphs pure while preserving solver-visible runtime actions |
| `appendix_b_validation` rejects any surviving source temporal operator | `phase-dae/src/appendix_b_validation.rs::validate_no_source_temporal_operator_survives` | Positive enforcement gate, not defensive code |

**What to do here:** DAE-level passes such as pre-lowering and alias
elimination; structural lowering/transformation such as index reduction and
state selection; and structural analysis products that are returned separately
from the DAE value.

**What NOT to do here:** Register allocation, lowering expression trees to
bytecode, FMI/MLIR template emission, or storing backend-specific solver
artifacts in DAE.

**Prohibited:** mutable cache fields, merged variable-kind maps, solver row
bytecode/layout, model-level `when_clauses`, and unlowered synchronous
operators in solver equation partitions.

---

### Stage 4 — Solve (`rumoca-ir-solve`)

**What it is:** A register-machine representation of the DAE-IR functions.
MLS B.1 functions lower into `ComputeBlock` graphs of scalar programs and
tensor program nodes.
Solve-IR does not add new mathematical content; it changes format. Tensor
structure (matrix multiply, linear solve, affine stencils, future
reductions/maps/broadcasts) is preserved as `ComputeNode` variants above the
scalar layer so backends can choose scalar expansion or native tensor ops
(BLAS/faer, Cranelift/LLVM kernels, CUDA, MLIR `linalg`).

Canonical terminology:

| Term | Current type/name | Meaning |
|---|---|---|
| `ScalarProgram` | `Vec<LinearOp>` | A flat register program that produces one scalar output |
| `ScalarProgramBlock` | `ScalarProgramBlock` | A group of scalar programs with one output per program |
| `TensorProgramNode` | `ComputeNode::{MatMul, LinSolve, AffineStencil, ...}` | A tensor-level kernel with explicit shape/layout metadata and scalar fallback |
| `ComputeBlock` | `ComputeBlock` | Ordered mix of scalar program blocks and tensor program nodes |

`ScalarProgramBlock` and `ComputeNode::ScalarPrograms` are the public source-code
names. New Solve-IR APIs must use `ScalarProgram` / `ScalarProgramBlock`
terminology and must not reintroduce `RowBlock` / `ScalarRows` naming.

`ComputeNode::AffineStencil` is source-proven: it comes from preserved DAE
`for_equations` domains plus affine operand proofs. It carries the source
iteration domain and strides; Solve lowering must not recover stencils by
scanning unstructured scalar rows after loop metadata is discarded.

The root `schema_version` field is mandatory on serialized Solve payloads.
Deserializers reject unsupported versions and the Solve wire format does not
accept pre-versioned `ComputeBlock` row payloads.

`SolveProblem` is the base lowered problem. Backend products that are expensive
or not part of the canonical MLS DAE, such as mass-matrix form and
Jacobian-vector scalar-program blocks, live in `SolveArtifacts` and are materialized by
`rumoca-phase-solve` only when a backend/template/runtime boundary asks for
them. Ordinary `lower_solve_problem` must not eagerly populate backend-specific
Jacobian products.

**Contract:**

| Rule | Why |
|---|---|
| All ops are pure functions of `(y[], p[], t)`; only `LoadY`, `LoadP`, `Const`, math ops | No Modelica-specific ops remain |
| No source temporal operators (`pre`, `edge`, `change`, `sample`, `previous`) in Solve-IR | Eliminated or represented as explicit DAE metadata before Solve lowering; surviving source temporal operators are upstream bugs |
| No flow-action calls (`assert`, `terminate`, `reinit`) in Solve-IR scalar programs | `reinit` is already a guarded discrete update; `assert` and `terminate` lower from DAE `events.event_actions` into action metadata plus pure action-condition scalar programs |
| `__pre__.*` parameters in `p[]` hold discrete/continuous pre-values | Runtime writes slots at event entry via `SolveLayout::pre_param_bindings` |
| Event timing is partitioned into root conditions, static arbitrary time instants, dynamic time-event rows, and periodic clock schedules | `events` owns zero-crossing and one-shot/dynamic time events; `clocks` owns periodic schedules derived from `sample`/clock metadata |
| Every `ComputeBlock` scalarizable via `rumoca-eval-solve::to_scalar_program_block(&block)` | Execution adapters that can't consume tensor ops call the Solve-IR evaluator utility at their boundary |
| Scalarization is a backend/evaluator choice, not an IR or lowering choice | Do not flatten tensor nodes in `rumoca-ir-solve` / `rumoca-phase-solve`; IR crates must not define scalarization helpers |
| Forward and reverse AD products are Solve artifacts, not base Solve IR fields | Keeps base Solve payloads lean while allowing Rumoca-owned JVP/VJP/adjoint paths for runtime and generated targets |
| Jacobian products live in `SolveArtifacts`, not base `SolveProblem` | Avoids unconditional AD materialization for codegen/IDE paths that do not consume them |
| Mass-matrix form lives in `ContinuousSolveArtifacts`, not DAE | It is solver-facing derived metadata, not canonical Modelica DAE semantics |
| BLT orderings from DAE-IR MAY drive `ComputeBlock` layout | Reuses upstream structural analysis |

Steady-state objectives, adjoints, parameter sensitivities, and
optimizer-facing projections are runtime or generated-target products layered
over Solve artifacts, not canonical `SolveProblem` payload fields.

**Do here:** lower DAE-IR expression trees + for-loops to `LinearOp` sequences
and preserve tensor nodes/sparsity metadata for downstream consumers.
**Do NOT do here:** DAE-level structural transformations, MLS semantics changes,
expression-level symbolic rewrites, concrete JIT/toolchain invocation, CUDA
runtime compilation, native object loading, or Jinja/minijinja template
rendering (those live in DAE-IR/upstream lowering, `rumoca-exec-*`, or
`rumoca-phase-codegen`, respectively).

---

## Key Invariants for Agents

1. **Source temporal operators are eliminated at the DAE boundary.** Any `pre`,
   `edge`, `change`, `sample`, or `previous` callable expression past
   `phase-dae` is a bug; Solve-IR is also free of these operators.

1. **Runtime flow actions are not expression graph nodes.** DAE-IR lowers
   `reinit` into guarded updates and stores `assert`/`terminate` as guarded
   event actions. Numeric DAE or Solve compute graphs must never contain these
   source calls.

2. **IR crates are pure data.** No evaluation logic, phase logic, or side
   effects in `rumoca-ir-ast`, `rumoca-ir-flat`, `rumoca-ir-dae`, or
   `rumoca-ir-solve`. See `SPEC_0029`.

3. **Scalarization happens at the backend/evaluator boundary.** Call
   `rumoca_eval_solve::to_scalar_program_block(&compute_block)` from the backend
   or evaluator crate that needs scalar programs. Do not define scalarization
   methods/functions in IR crates, and do not flatten tensor nodes in
   `rumoca-phase-solve` lowering.

4. **Each stage's output is serializable.** All IR types implement
   `serde::Serialize` / `Deserialize`. Serialized DAE and Solve root payloads
   carry a mandatory `schema_version`; deserializers reject unsupported
   versions. `#[serde(default)]` is allowed only for documented optional
   same-version fields where the default is semantically valid.

5. **The dependency direction is strictly downward.** AST → Flat → DAE → Solve.
   No stage imports from a later stage.

6. **DAE-IR owns symbolic math; Solve-IR lowers format only.** Do not add
   expression rewrites or new mathematical content in Solve-IR or solve
   lowering; add them to DAE-IR first.

7. **Optional IR fields are explicit same-version omissions, not hidden work
   caches.** DAE optional fields must still be canonical MLS/diagnostic data.
   Solver-facing optional products belong in structural analysis results or
   Solve artifacts. If a field changes the meaning of existing payloads, bump
   `schema_version` instead of adding a defaulted field.

## Structural Lowering Scope

Rumoca performs OpenModelica-class structural lowering between DAE and Solve.
Structural lowering is DAE-to-DAE: it rewrites or annotates mathematical
structure for downstream lowering without changing IR stage. The supported
transformations are listed here to keep scope and ownership clear.

**In scope:**

| Transformation | Owning module | Notes |
|---|---|---|
| Pre-lowering (`pre(v)` → `__pre__.v`) | `rumoca-phase-dae::pre_lowering` | Runs at DAE entry; applies to every partition (f_x, f_z, f_m, f_c). See Stage 3 Contract. |
| Alias elimination | `rumoca-phase-dae` | Folds trivial equalities into the variable graph. |
| Structural index reduction (Pantelides-style) | `rumoca-phase-structural` | For states without a `der(state)` equation, differentiate a non-ODE constraint referencing that state and substitute. Index-1 lift is supported; higher-index lifts are an explicit subset of Pantelides. |
| State demotion | `rumoca-phase-structural` | Demote over-classified states whose derivative is structurally unreachable. |
| BLT ordering | `rumoca-phase-structural` | Block-lower-triangular ordering of equations for sequential solve. |
| Algebraic-loop tearing (Greedy Cellier) | `rumoca-phase-structural::tearing` | Identifies tear variables for cyclic algebraic blocks. |
| State selection | `rumoca-phase-structural` | Pick a consistent state set. |

**Out of scope (require an explicit spec update before adding):**

- Full dummy-derivative method (Mattsson-Söderlind). The current
  Pantelides-style approach may add dummy derivatives in restricted forms,
  but a general dummy-derivative pass is not implemented.
- Higher-order symbolic simplification beyond what serves index reduction
  and alias elimination.
- Symbolic linearization for control-design output (codegen-level concern,
  not pipeline-level).

**Placement requirement:**

All DAE structural lowering/transformation MUST live in
`rumoca-phase-structural` per SPEC_0029 §12. A structural lowering pass's IR
output is another finalized DAE. Separate structural analysis products may
accompany that DAE, but they are not stored as backend convenience fields on
`ir-dae::Dae`. `rumoca-phase-solve` only lowers a finalized DAE to Solve-IR; it
does not mutate DAE mathematical structure.

## Relevant Specs

- `SPEC_0029` — Crate boundary rules
- `SPEC_0021` — Maintainability and deterministic collection rules
