# SPEC_0007: IR Pipeline (AST → Flat → DAE → Solve)

## Status
ACCEPTED

## Summary

Rumoca transforms Modelica through AST → Flat → DAE → Solve IRs. Each stage
defines its contents, ownership, and boundary.

Per-stage contract rows and the structural-lowering transformation list are
catalogued in [SPEC_0040](SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md). Every row is
normative by reference from the stage section linking it.

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
  │   Flat   │  rumoca-ir-flat       ◄─ inspection: exact current-version JSON
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
| Flat IR inspection | Flat | Exact current-version JSON including construction-state flags |
| DAE residual and symbolic-analysis targets | DAE | MLS B.1 form, residual ownership, source traceability |
| Numeric simulation and explicit-ODE products | `SolveProblem` | Register-machine plus tensor bytecode |
| eFMI Algorithm Code | checked `AlgorithmCodePackage` derived from DAE | Causal GALEC lifecycle and language semantics |
| eFMI Production Code | the prepared `SolveAlgorithmBlock` view of one checked `SolveAlgorithmProduct` that retains its source `AlgorithmCodePackage` | Only the Solve-owned block authorizes C/H; the registered readiness-zero `efmu` slice is experimental |
| FMI 2/3 components | checked projections of the sealed variable catalog retained by one `SolveModel` | One executable kernel owns identity, metadata, tensor shape, final runtime values, and version-specific interface coordinates |

`rumoca-phase-codegen` renders text; execution adapters wrap toolchains and
runtimes without owning compiler semantics.

Every IR that crosses the code-generation boundary MUST already satisfy its
stage invariants by construction. The semantic-context enum maps one-to-one,
in both directions, to every `rumoca-ir-*` crate through the exact vocabulary
`ast`, `flat`, `dae`, `galec`, and `solve`. Architecture CI enumerates the
crates and proves that equality mechanically. Adding an IR crate requires the
corresponding spec/context change.
Roots, views, output formats, target identities, packages, and products never
become manifest contexts. Each admitted file may name one
closed `view` inside that crate; absence is resolved once by construction to
the crate's canonical root (`ClassTree`, Flat `Model`, `Dae`,
`AlgorithmCodePackage`, or `SolveModel`), never by `Default`. The admitted
noncanonical Solve views are the actual checked `FmiComponent` and
`SolveAlgorithmBlock` types. Target construction derives one required product
from the checked `(context, view)` file plans without a target-wide IR field,
root selector, suffix inference, or target-name dispatch. A sole
`SolveAlgorithmBlock` rejects; only its pairing with the same product's
`AlgorithmCodePackage` files requires one `SolveAlgorithmProduct`. Mixed
`SolveModel`/`FmiComponent` or otherwise independently provisioned roots
reject. The compiler supplies only the file's typed, read-only semantic view
to MiniJinja.
Each file that observes target-issued artifact identities declares its exact
required logical keys. Every logical file ID uses the exact ASCII grammar
`[a-z_][a-z0-9_]*`; the sole template mapping is the injective top-level scalar
`__rumoca_artifact_identity_v1_<key>`. The serialized `artifact` object contains
no identity map. Target construction checks the IDs and dependencies against
the complete target-issued logical file-ID catalog before constructing any
renderer. Each exact snapshotted template must compile under the
composition-free MiniJinja grammar before MiniJinja's own all-branch
undeclared-variable AST analysis compares the exact flattened scalar names and
declarations bidirectionally. The file context then contains only those
declared identity scalars. Semantic context construction rejects the reserved
identity namespace, and the production MiniJinja global set contains no State
or context introspector. An identity reference hidden from MiniJinja analysis
by local self-shadowing remains absent unless declared and therefore fails
strictly if evaluated; it cannot recover an undeclared identity value.
Independent text scanning, the removed `artifact.identities` map, defaults,
and fallback identities are prohibited.
Rendering MUST NOT resolve names, infer types or shapes, lower to another IR,
mutate its input, or repair an invalid artifact.

Code-generation architecture:

```text
proven-valid IR -> typed semantic template view -> target.toml + MiniJinja -> artifacts
```

The Algorithm Code cutover MUST satisfy every construction and evidence row in
[SPEC_0040 §5](SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md#5-algorithm-code-cutover-catalog),
normative by reference. SPEC_0042 GAL-041 and SPEC_0049 `MatrixMultiply` remain
design sources, not promotion of their DRAFT parents. The catalog's completed
slice does not establish eFMI Production Code conformance.

This boundary applies uniformly to syntax, Flat, DAE, Solve, and checked export
IRs. Adding a target for an already-supported IR requires only a target
directory. Supporting a new IR requires one target-neutral semantic view and
capability vocabulary, never a target-language renderer in Rust. Export IRs
remain projections, never canonical pipeline stages.

The checked FMI component export is the single deployment projection for FMI 2
and FMI 3. The sole DAE-to-`SolveModel` construction atomically retains one
sealed variable catalog beside the executable kernel. Every finalized DAE
variable already carries a mandatory nonzero Flat-issued source occurrence;
structural reconstruction preserves it, and Solve construction records an
explicit occurrence-to-catalog mapping. One independent checker consumes that
mapping and eagerly materialized, closed name-free projections of the exact
prepared DAE and Solve root privately co-retained by the lowering product,
issuing a non-serializable live refinement receipt. Projections contain only
admitted facts: names, scalar labels, provenance, and spans are absent and cannot
authorize correlation. Issued once by DAE reservation and Solve catalog
construction, occurrence uniqueness survives closed projection; C60 consumes it
without rechecking. Each dense Solve declaration has exactly one catalog entry
carrying its source occurrence, shape, scalar names, value kind, causality, variability, tunability, units,
description, provenance, storage run, and constructor-evaluated final runtime
attributes. FMI construction consumes only that `SolveModel`; it never accepts
or pairs a second DAE view, metadata vector, evaluated-value vector, or wire
input. FMI-version adapters may scalarize only the external value-reference
view required by that version; they MUST NOT repeat equation lowering,
initialization, event, or state-machine semantics. A raw derivative-only C
kernel is not an FMI component and MUST NOT be advertised as an FMI deployment
substitute.

### Built-in Target Product Contract

Registered built-in targets satisfy
[SPEC_0040 §4](SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md#4-built-in-target-product-contract).
Future targets remain specifications until they emit and validate a useful
artifact. Unsupported checked semantics fail with a span-bearing error; a
template cannot omit or repair them. External directory targets are
user-authored extensions, but they still require the same closed per-file
artifact/context/view declarations as built-ins. Standalone `.jinja` targets
and invocation-selected template IRs are absent because neither can retain the
construction proof authorizing emitted bytes. Exact Flat inspection uses
`flat-json`; textual Flat/Base Modelica remains disabled until the catalogued
atomic cutover is complete.

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

### Stage 2 — Flat (`rumoca-ir-flat`)

**What it is:** The instantiated class hierarchy with fully-qualified names.

**Contract:**
- No unresolved class references.
- No modification chains; all modifications have been applied.
- Virtual connection graphs satisfy MLS §9.4 forest and root invariants.
- Typed connection and virtual-connection-graph inputs lower through one atomic
  Flat transaction; malformed, unknown, or unsupported topology fails before
  any Flat mutation.
- Arrays remain symbolic (not scalarized).
- Function bodies remain structured in `functions`.
- Record-valued function slots remain aggregate through Flat and DAE; only a
  final Solve or GALEC target boundary may project checked scalar leaves.
- `pre()`, `der()`, `initial()`, and other Modelica built-ins are still present
  as expression nodes — semantic lowering has not occurred.

**Do here:** Resolution, instantiation, post-instantiation type checking, and
flattening. **Do not:** solve equations, eliminate Modelica operators, or
generate simulation code.

The exact Flat-stage obligations are catalogued in
[SPEC_0040 §0](SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md#0-flat-stage-contract-catalog-spec_0007-stage-2).

**Cross-cutting rules (Flat through DAE):** every row in
[SPEC_0040 §6](SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md#6-flat-through-dae-cross-cutting-catalog)
is normative by reference. No phase may substitute a textual/default/partial
owner for those typed contracts.

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

**Do here:** DAE lowering, structural transformation, and separately returned
structural analysis. **Do not:** allocate registers, lower bytecode, emit
templates, or store backend artifacts in DAE.

**Prohibited:** mutable cache fields, merged variable-kind maps, solver row
bytecode/layout, model-level `when_clauses`, and unlowered synchronous
operators in solver equation partitions.

### Callable Proof Plan (`rumoca-plan-callable`)

The callable plan is the sole affine, non-wire, non-root proof aggregate over
one transferred checked DAE function graph. It retains that exact DAE behind
borrowed correlated views; no API returns/clones it, no consumer accepts a
second DAE, and the plan owns no target, presentation, storage, ABI, wire,
evaluation, or C authority. Numerical Solve refines the DAE-origin plan;
Algorithm Code construction consumes/rebrands it and drops DAE ownership;
package-rooted Solve refines only that retained package owner.

Construction and consumption are governed by SOLVE-C52/SOLVE-C58 in
[SPEC_0040 §2](SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md#2-solve-stage-contract-catalog-spec_0007-stage-4)
and by the callable rows in
[SPEC_0043 §9](SPEC_0043_CONSTRUCTION_CATALOG.md#9-solve-algorithm-block-construction-catalog-pending-2026-08-08-plan-m3-4).
Every linked row is normative by reference. The mandatory four-way
GALEC/Solve/Production-C/OMC differential remains the proof relation; only the
correlated prepared Solve block authorizes C/H.

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
| `SolveAlgorithmBlock` | implemented for the current narrow Algorithm Code refinement slice | Checked Algorithm Code execution root; broader vocabulary remains fail-closed |

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

`SolveAlgorithmBlock` is constructed only from checked Algorithm Code by
consuming the complete numeric profile already retained in its package. The
constructor and registered readiness-zero `efmu` path have landed for the
current scalar-only experimental slice and accept no second arithmetic
profile. Broader semantic coverage and the Production-C toolchain/refinement
receipts remain pending; this is not an eFMI conformance claim. The block is
not a mode of `SolveProblem`; rows SOLVE-C32–C38 and SOLVE-C58–C59 define its
complete obligations.

**Contract:** rows `SOLVE-C01`–`SOLVE-C61` in
[SPEC_0040 §2](SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md#2-solve-stage-contract-catalog-spec_0007-stage-4).

Objectives, adjoints, sensitivities, and optimizer projections are derived
products, not canonical root fields.

**Do here:** construct either checked root and preserve typed programs,
provenance, and its execution contract.

Sparsity follows [SPEC_0039](SPEC_0039_PROOF_CARRYING_SPARSITY.md); compact
affine patterns originate from SPEC_0032 owners, never scalar-row recovery.

**Do not:** work assigned to DAE/structural phases, concrete execution crates,
or `rumoca-phase-codegen` by SPEC_0029.

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
