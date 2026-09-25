# SPEC_0007: IR Pipeline (AST → Flat → DAE → Solve)

## Status
ACCEPTED

## Summary

[SPEC_0040](SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md) catalogs stage contracts and
structural transformations; linked rows are normative.

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

**Codegen targets the lowest proven-valid IR it needs.**

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

**Contents:** syntax, comments, spans.

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

**Contents:** instantiated classes with fully-qualified names.

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
| Instantiation and flattening are separate logical phases | Instantiate modifications/overlay → typecheck → traverse overlay, expand connections, produce `flat::Model` |
| Scalar binding specialization follows [FLAT-C01](SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md#0-flat-stage-contract-catalog-spec_0007-stage-2) | Preserves parameter dependencies |
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
solver cache. One catalog owns stable variable identity;
typed views classify `p`, `x`, `y`, `z`, and `m`, while input/output causality
is orthogonal metadata. Continuous, initialization, discrete,
condition, event, and clock systems own behavior. The current
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

| Term | Current type/name | Meaning |
|---|---|---|
| `ScalarProgram` | `Vec<LinearOp>` | A flat register program that produces one scalar output |
| `ScalarProgramBlock` | `ScalarProgramBlock` | A group of scalar programs with one output per program |
| `TensorProgramNode` | `ComputeNode::{MatMul, LinSolve, AffineStencil, ...}` | A tensor-level kernel with explicit shape/layout metadata and scalar fallback |
| `FunctionFoldProgram` | `FunctionFoldProgram` | A finite-domain loop with an explicit loop-carried tuple and compact typed body |
| `ComputeBlock` | `ComputeBlock` | Ordered mix of scalar program blocks and tensor program nodes |
| `SolveAlgorithmBlock` | (pending: 2026-08-08 plan, M3-4) | Checked Algorithm Code execution root |

New Solve APIs use `ScalarProgram`/`ScalarProgramBlock`, never `RowBlock`/`ScalarRows`.

`ComputeNode::AffineStencil` is source-proven: it comes from preserved DAE
structured-family domains plus affine operand proofs. It carries the compact
iteration domain and strides; Solve lowering must not recover stencils by
scanning unstructured scalar rows after structured-family metadata is discarded.

Structured B.1c definitions follow the same boundary: Solve preserves their
authoritative DAE domain as a compact map plus a compact target map, and phase
lowering creates no parallel scalar owner (SOLVE-C20).

Solve lowering derives each scalar/structured discrete update's typed integrator-history
effect; runtime model names, row positions, or observed behavior cannot supply it
(SOLVE-C21).

One clocked partition has one equation-shaped owner: producers proved total on
that tick exchange same-tick values through construction-issued intermediates,
guarded producers lacking that proof remain hold-fallback members under the
checked hold rows, and event-transaction, `sample`, and causally unowned rows
keep their existing owners (SOLVE-C57; pending design
SPEC_0046).

Serialized Solve roots carry a mandatory schema version; unsupported and
pre-versioned payloads are rejected.

`SolveProblem` owns the numerical DAE. Expensive or noncanonical products
(mass-matrix form, Jacobian-vector programs) belong in `SolveArtifacts`.
`rumoca-phase-solve` materializes them only on backend/template/runtime demand;
`lower_solve_problem` must not populate them eagerly.

`SolveAlgorithmBlock` is constructed only from checked Algorithm Code under an
explicit arithmetic profile (pending: 2026-08-08 plan, M3-4). It is not a mode
of `SolveProblem`; rows SOLVE-C32–C38 define its complete obligations.

**Contract:** rows `SOLVE-C01`–`SOLVE-C57` in
[SPEC_0040 §2](SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md#2-solve-stage-contract-catalog-spec_0007-stage-4).

Initialization planning follows matched rows through algebraic and derivative
reads, preserving tensor-coordinate dependencies. Parameter definitions are
substituted into residuals and committed from the solved point; seeds cannot
discharge them. Unsupported cycles are rejected. Order matched unknowns by
dependency; only strongly connected rows share projection blocks.
Algebraic-dependent rows require total derivatives of the reconstructed residual.
Unmatched checks remain required.

Objectives, adjoints, sensitivities, and optimizer projections are derived
products, not canonical root fields.

**Do here:** construct checked roots preserving typed programs, provenance, and execution contracts.

Sparsity follows [SPEC_0039](SPEC_0039_PROOF_CARRYING_SPARSITY.md); compact
affine patterns originate from SPEC_0032 owners, never scalar-row recovery.

**Do not:** perform DAE/structural, execution, or codegen work owned elsewhere under SPEC_0029.

---

### Structural Lowering Scope

Transformations require checked, root-owned DAEs; partial mutation,
replayable proof receipts, and mutable partition callbacks are prohibited.

**In scope:** only `STRUCT-T01`–`STRUCT-T10` in
[SPEC_0040 §3](SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md#3-structural-lowering-transformation-catalog-spec_0007-structural-lowering-scope).
Other transformations require amendments.

State selection certifies signatures, tensor-uniform offsets, formal derivatives,
and candidate maps (STRUCT-T07). Proposals seed and prioritize from exact initial-value transfers within source preference classes.
Trials never replace initialization; integration requires regularity.

STRUCT-T03 reuses expressions only for identical source, call substitutions,
derivative order, reconstruction mode, and provenance. Distinct calls and
equation owners never merge. Discovery, preflight, and reconstruction share
differentiation facts per immutable source round; replacement DAEs refresh facts
without changing ordering or acceptance.

Demotion distinguishes exact values from affine derivatives; displaced definitions
preserve source value equations and cannot substitute manifold values.
Tangents and non-additive lifts replay exact anchors.
Additive lifts share acyclic source-row value/derivative proofs excluding replaced owners and lifted coordinates.
Holonomic replacement excludes value identities; undoing lifts restores original equations.

STRUCT-T03 reconstruction (aggregate rows: SPEC_0040 §3):

| Rule | Owner | Why |
|---|---|---|
| Reconstruct a continuous Real vector only from a source-owned square linear system with state/invariant coefficients independent of that unknown | structural value and derivative proofs | Establishes the exact domain of the auxiliary solve |
| Reconstruct dependent state vectors from affine scalar constraints and independent literal-array entries, including signed aliases; retain parent equations/projections and exclude the target from all anchors | structural state reconstruction | Close dependent kinematics without circular definitions |
| Prefer an admitted direct source definition over an auxiliary solve for the same state | structural state selection | Avoid obscuring explicit kinematics with redundant implicit solves |
| Follow exact function/array substitutions and independently defined derivatives; retain source residual owners and assertions | structural coefficient proof | Reconstruction must preserve source behavior |
| Structural differentiation may select an exact whole-coordinate equality with proved state/invariant value anchors when executable causal-definition uniqueness is unavailable; selection must be acyclic and retain every source equation, including alternative definitions | structural substitution facts | Multiple equations constrain a coordinate without preventing exact substitution |
| Differentiate `A*q=b` as `A*der(q)=der(b)-der(A)*q`, preserving exact zeros and only needed primal reads; manifold reconstruction uses proved state/invariant anchors on the same nonsingular domain | structural reconstruction | Preserve the original primal solve instead of recursively recomputing it |

**Placement requirement:**

`rumoca-phase-structural` reconstructs finalized DAEs; analysis stays outside DAE.
STRUCT-T02 quotients copy and negation aliases before state selection and again among formal-derivative coordinates after it, each through one checked reconstruction; eliminated members stay defined, observable variables, and every unquotiented class is explicit.
`rumoca-phase-solve` lowers finalized DAE only. STRUCT-T09 permits exact implicit-derivative
and mixed derivative/algebraic aliases, never scalar pivot selection or numerical
coefficient matrices. Other dummy-derivative transformations, unrelated symbolic
simplification, and control-design linearization require a spec update.

## References

- [SPEC_0040](SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md) — stage contract catalog
- `SPEC_0029` — Crate boundary rules
- `SPEC_0021` — Maintainability and deterministic collection rules
