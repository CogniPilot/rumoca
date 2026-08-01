# SPEC_0007: IR Pipeline (AST → Flat → DAE → Solve)

## Status
ACCEPTED

## Summary

Rumoca transforms Modelica through AST → Flat → DAE → Solve IRs. Each stage
defines its contents, ownership, and boundary.

The per-stage contract rows and the structural-lowering transformation list are
catalogued in [SPEC_0040](SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md). Every row
there is normative by reference from the stage section that links it.

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
| `initial algorithm` sections are replayed symbolically into declarative owners: sequential scalar assignments and `if` conditionals determine a `parameter` declared `fixed = false` as a calculated-parameter binding, and `assert` becomes an assertion owner carrying its enclosing branch conditions | The replayed value reads only parameters and constants, so the parameter set computes exactly the number initialization would. Every other target — discrete, algebraic, state, output — and every loop, `when`, or non-`assert` call statement keeps `ED013`, because no checked initialization owner determines it |
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
version 12 is the only supported wire version; every other version is rejected
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

**Contract:** rows `DAE-C01`–`DAE-C13` in
[SPEC_0040 §1](SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md#1-dae-stage-contract-catalog-spec_0007-stage-3).

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

**Contract:** rows `SOLVE-C01`–`SOLVE-C19` in
[SPEC_0040 §2](SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md#2-solve-stage-contract-catalog-spec_0007-stage-4).

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
callbacks are prohibited.

**In scope:** exactly rows `STRUCT-T01`–`STRUCT-T07` in
[SPEC_0040 §3](SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md#3-structural-lowering-transformation-catalog-spec_0007-structural-lowering-scope).
A transformation absent from that catalog is out of scope until this spec is
amended.

**Placement requirement:**

DAE structural transformations live in `rumoca-phase-structural`, return a
finalized DAE, and keep analysis products outside DAE. `rumoca-phase-solve`
only lowers finalized DAE. General dummy derivatives, unrelated symbolic
simplification, and control-design linearization require a spec update.

## References

- [SPEC_0040](SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md) — stage contract catalog
- `SPEC_0029` — Crate boundary rules
- `SPEC_0021` — Maintainability and deterministic collection rules
