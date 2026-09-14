# SPEC_0007: IR Pipeline (AST → Flat → DAE → Solve)

## Status
ACCEPTED

## Summary

Each Modelica stage — AST → Flat → DAE → Solve — defines contents and ownership.

[SPEC_0040](SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md) catalogs stage contracts and
structural transformations; each linked row is normative.

## Specification

```
Modelica source (.mo)
        │
        ▼  rumoca-phase-parse
  ┌──────────┐
  │   AST    │  rumoca-ir-ast        ◄─ consumers: formatters, source-aware
  └────┬─────┘                           documentation tools
       │  rumoca-phase-resolve, rumoca-phase-instantiate,
       │  rumoca-phase-typecheck, rumoca-phase-flatten
       ▼
  ┌──────────┐
  │   Flat   │  rumoca-ir-flat       ◄─ codegen: flat Modelica export
  └────┬─────┘
       │  rumoca-phase-dae
       ▼
  ┌──────────┐
  │   DAE    │  rumoca-ir-dae        ◄─ codegen: DAE-level symbolic/array
  └────┬─────┘                           backends
       │  rumoca-phase-solve              (CasADi, SymPy, JAX)
       ▼
  ┌──────────┐
  │  Solve   │  rumoca-ir-solve      ◄─ codegen/JIT: numeric C/Rust,
  └──────────┘                           MLIR/LLVM, CUDA C and WGSL kernels
```

**Codegen targets the lowest proven-valid IR it needs — no lower.**

| Backend | IR level | Why |
|---|---|---|
| Formatter, doc generator | AST | Needs syntax + spans; it is a target only when it preserves every supported construct or fails closed |
| Flat Modelica export | Flat | Original expression structure |
| DAE residual and symbolic-analysis targets | DAE | MLS B.1 form, residual ownership, source traceability |
| Numeric simulation and explicit-ODE products | `SolveProblem` | Register-machine plus tensor bytecode |
| eFMI Algorithm Code | checked `AlgorithmCodePackage` derived from DAE | Causal GALEC lifecycle and language semantics |
| eFMI Production Code and GALEC-derived embedded execution | checked `SolveAlgorithmBlock` derived from `AlgorithmCodePackage` (pending: 2026-08-08 plan, M3-4) | Typed executable lifecycle, storage, effects, and ABI obligations |
| FMI 2/3 components | checked FMI component export IR derived from DAE + Solve | DAE metadata and tensor shape plus one executable checked kernel |

`rumoca-phase-codegen` renders text; execution adapters wrap toolchains and
runtimes without owning compiler semantics.

Code-generation inputs MUST satisfy stage invariants by construction. Target
manifests select the exact canonical or checked export IR; MiniJinja receives
its typed, read-only semantic view. Rendering MUST NOT resolve names, infer
types/shapes, lower IRs, mutate inputs, or repair invalid artifacts.

Code-generation architecture:

```text
proven-valid IR -> typed semantic template view -> target.toml + MiniJinja -> artifacts
```

This boundary covers syntax, Flat, DAE, Solve, and checked export IRs. Existing
IRs need only a target directory; new IRs require a target-neutral semantic view
and capability vocabulary, never a target-language Rust renderer. Export IRs
remain projections, not canonical stages.

Checked FMI component export is the sole FMI 2/3 deployment projection. Its
constructor binds DAE variable identity, causality, type, shape, units, and
provenance to the Solve kernel. Version adapters may scalarize only required
external value references; they MUST NOT repeat equation lowering,
initialization, events, or state-machine semantics. Derivative-only C kernels
MUST NOT be advertised as FMI components or deployment substitutes.

### Built-in Target Product Contract

Every target registered below `rumoca-phase-codegen/src/templates/` MUST be an
executable or inspectable product satisfying these rules:

The complete product and evidence requirements are normative in
[SPEC_0040 §4](SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md#4-built-in-target-product-contract-spec_0007).

---

### Stage 1 — AST (`rumoca-ir-ast`)

**What it is:** Parser output: concrete syntax, comments, and spans.

**Contract:**
- Represents source text structure, not language semantics.
- No name resolution, type information, or class lookup.
- Every node carries a source `Span`; later AST merges must preserve parser
  provenance instead of rewriting source ids.

**Do here:** Parsing, formatting, early syntax diagnostics.

**Do not:** Name lookup, class instantiation, type inference, equation
manipulation.

---

### Stage 2 — Flat (`rumoca-ir-flat`)

**What it is:** The instantiated class hierarchy with fully-qualified names.

**Contract:**
- No unresolved class references.
- No modification chains; all modifications have been applied.
- Virtual connection graphs satisfy MLS §9.4 forest and root invariants.
- Arrays remain symbolic (not scalarized).
- Array construction retains its source operation: `{...}` adds an element
  axis, bracket commas concatenate along dimension 2, and bracket semicolons
  concatenate along dimension 1, with MLS §10.4.2.1 promotion. AST and Flat
  carry this distinction explicitly through rewrites and function bodies;
  consumers MUST NOT infer the concatenation axis from child nesting. Expanded
  comprehensions and materialized array values remain element constructors.
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
| Function algorithms remain structured; conditional joins retain checked shared-branch correlation | Downstream projections preserve call cardinality without reconstructing control flow |
| A function-algorithm `assert` is a flow action, not an ordinary call or a value expression | A value-proven function specialization may erase the statement only when its exact specialization environment proves the condition `true`. An unsettled condition may lower only through the call-specialized guarded root/action schedule in SOLVE-C25; a proven-false or otherwise unrepresentable schedule is typed-rejected. The action is never silently discarded or routed through multi-result-call lowering. |
| Model algorithms lower to DAE only when they fit the declarative subset | Unsupported forms fail explicitly with `ED013` |
| Initial algorithms support sequential scalar assignments and `if` conditionals targeting `fixed=false` parameters or discrete coordinates. Initial equations `m = value` / `pre(m) = value` produce the same discrete initial-value owner. Assertions retain enclosing branch conditions. | Discrete initial definitions prove exact scalar type, initialization-settled reads, and unique ownership; Solve initializes both current and `pre` storage. Replayed calculated-parameter values read only parameters/constants. Settled dependencies permit exact parameter-set evaluation; dependencies on `fixed=false` parameters require post-projection binding updates, treating parameter-set values as seeds. Algebraic/state/output/input algorithm targets, loops, `when`, and non-`assert` call statements retain `ED013` without a checked owner. |
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
condition, event, and clock systems own their respective behavior. The current
`DAE_SCHEMA_VERSION` wire schema is the only supported version; every other
version is rejected without superseded readers or adapters.

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

**Contract:** rows `DAE-C01`–`DAE-C21` in
[SPEC_0040 §1](SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md#1-dae-stage-contract-catalog-spec_0007-stage-3).

Non-Real `fixed=false` initialization definitions have checked owners distinct
from translation-time bindings and numeric residuals. They retain exact
parameter identity, matching shape/type, provenance, and unique ownership;
values may read initialization state/algebraic unknowns. Wire replay and
structural transformation reconstruct these owners through checked construction.

**Do here:** DAE lowering, structural transformation, and separately returned
structural analysis. **Do not:** allocate registers, lower bytecode, emit
templates, or store backend artifacts in DAE.

**Prohibited:** mutable cache fields, merged variable-kind maps, solver row
bytecode/layout, model-level `when_clauses`, and unlowered synchronous
operators in solver equation partitions.

---

### Stage 4 — Solve (`rumoca-ir-solve`)

**What it is:** Typed programs with DAE and Algorithm Code roots over
shared scalar/tensor vocabulary.

Canonical terminology:

| Term | Current type/name | Meaning |
|---|---|---|
| `ScalarProgram` | `Vec<LinearOp>` | A flat register program that produces one scalar output |
| `ScalarProgramBlock` | `ScalarProgramBlock` | A group of scalar programs with one output per program |
| `TensorProgramNode` | `ComputeNode::{MatMul, LinSolve, AffineStencil, ...}` | A tensor-level kernel with explicit shape/layout metadata and scalar fallback |
| `FunctionFoldProgram` | `FunctionFoldProgram` | A finite-domain loop with an explicit loop-carried tuple and compact typed body |
| `ComputeBlock` | `ComputeBlock` | Ordered mix of scalar program blocks and tensor program nodes |
| `SolveAlgorithmBlock` | (pending: 2026-08-08 plan, M3-4) | Checked Algorithm Code execution root |

New Solve-IR APIs use `ScalarProgram` / `ScalarProgramBlock` terminology, not
`RowBlock` / `ScalarRows`.

`ComputeNode::AffineStencil` is source-proven: it comes from preserved DAE
structured-family domains plus affine operand proofs. It carries the compact
iteration domain and strides; Solve lowering must not recover stencils by
scanning unstructured scalar rows after structured-family metadata is discarded.

Structured B.1c definitions follow the same boundary: Solve preserves their
authoritative DAE domain as a compact map plus a compact target map, and phase
lowering creates no parallel scalar owner (SOLVE-C20).

Each scalar and structured discrete update owns a typed integrator-history
effect derived by Solve lowering, never recovered by a runtime from model
names, row positions, or observed numerical behavior (SOLVE-C21).

One clocked partition has one equation-shaped owner: producers proved total on
that tick exchange same-tick values through construction-issued intermediates,
guarded producers lacking that proof remain hold-fallback members under the
checked hold rows, and event-transaction, `sample`, and causally unowned rows
keep their existing owners (SOLVE-C57; pending design
SPEC_0046).

Serialized Solve roots carry a mandatory schema version; unsupported and
pre-versioned payloads are rejected.

`SolveProblem` is the numerical DAE root. Backend products that are expensive
or outside the canonical MLS DAE (mass-matrix form, Jacobian-vector
scalar-program blocks) live in `SolveArtifacts`, materialized by
`rumoca-phase-solve` only when a backend/template/runtime boundary asks.
`lower_solve_problem` must not eagerly populate them.

`SolveAlgorithmBlock` is constructed only from checked Algorithm Code under an
explicit arithmetic profile (pending: 2026-08-08 plan, M3-4). It is not a mode
of `SolveProblem`; rows SOLVE-C32–C38 define its complete obligations.

**Contract:** rows `SOLVE-C01`–`SOLVE-C57` in
[SPEC_0040 §2](SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md#2-solve-stage-contract-catalog-spec_0007-stage-4).

Solve substitutes initialization parameter definitions into that system's
residuals and commits their values from the solved initial point. Start guesses
and parameter-set values cannot discharge these definitions. A dependency
cycle without a supported coupled owner must be rejected explicitly.
Matched initialization rows are ordered by dependency; only strongly connected
rows share a simultaneous projection block. Unmatched checks remain required.

Objectives, adjoints, sensitivities, and optimizer projections are derived
products, not canonical root fields.

**Do here:** construct either checked root and preserve typed programs,
provenance, and its execution contract.

Sparsity follows [SPEC_0039](SPEC_0039_PROOF_CARRYING_SPARSITY.md); compact
affine patterns originate from SPEC_0032 owners, never scalar-row recovery.

**Do not:** work assigned to DAE/structural phases, concrete execution crates,
or `rumoca-phase-codegen` by SPEC_0029.

---

### Structural Lowering Scope

Structural lowering between DAE and Solve is DAE-to-DAE: each pass consumes a
finalized DAE and returns another through root-owned checked changes. Partial
mutation, independently replayable proof receipts, and mutable partition
callbacks are prohibited.

**In scope:** exactly rows `STRUCT-T01`–`STRUCT-T09` in
[SPEC_0040 §3](SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md#3-structural-lowering-transformation-catalog-spec_0007-structural-lowering-scope).
A transformation absent from that catalog is out of scope until this spec is
amended.

STRUCT-T03 reconstruction preserves shared expression identity during value
substitution and differentiation. Reuse requires the same source, exact call
substitutions, derivative order, reconstruction mode, and provenance; it cannot
merge unrelated call contexts or equation owners.

STRUCT-T03's linear auxiliary profile:

| Rule | Owner | Why |
|---|---|---|
| Reconstruct a continuous Real vector only from a source-owned square linear system with state/invariant coefficients independent of that unknown | structural value and derivative proofs | Establishes the exact domain of the auxiliary solve |
| Follow exact function substitutions and array operations; retain original residual owners and assertions | structural coefficient proof | Reconstruction must preserve source behavior |
| Keep identity, projection, matrix product, and outer product aggregate; never enumerate a tensor basis to obtain coefficients | structural reconstruction | Compiler representation must stay compact |
| Reject singular runtime matrices through the checked aggregate solve | native evaluation | Structural shape cannot prove numerical nonsingularity |
| Select supplied derivatives only at a supported order; a first-order annotation does not forbid a separately proved higher derivative of the checked function body (MLS §12.7.1) | structural differentiation | Annotation availability is not a smoothness bound |

**Placement requirement:**

DAE structural transformations live in `rumoca-phase-structural`, return a
finalized DAE, and keep analysis products outside DAE. `rumoca-phase-solve`
only lowers finalized DAE. STRUCT-T09 permits exact aliases for implicit
derivatives and mixed derivative/algebraic blocks; it does not choose scalar
pivots or construct numerical coefficient matrices. Other dummy-derivative transformations,
unrelated symbolic simplification, and control-design linearization require a
spec update.

## References

- [SPEC_0040](SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md) — stage contract catalog
- `SPEC_0029` — Crate boundary rules
- `SPEC_0021` — Maintainability and deterministic collection rules
