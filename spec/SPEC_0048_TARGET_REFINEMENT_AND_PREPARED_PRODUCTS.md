# SPEC_0048: Target Refinement and Prepared Products

## Status
DRAFT

## Summary

One per-invocation build session seals each issued Solve root exactly once,
prepares one closed product plan with total coverage, and hands a passive
emitter exactly one issued variant.

## Specification

### 1. Governance, Scope, And Acceptance-Time Amendment Map

This DRAFT proposes the amendments below and claims none today. On acceptance
it amends SPEC_0007 Stage 4 with lockstep SPEC_0040 C13/C14/C20 (scalar programs
exist only at the final emitter, as an issued plan); SPEC_0032 §§2/4/5 (the
shared `rumoca-eval-solve` scalar-fallback license narrows to the final
expansion boundary); SPEC_0034 Summary, pipeline, GAL-027, and GAL-038 (TRP-030
replaces independently lowered AlgorithmCode bodies); and DRAFT SPEC_0036 with
SPEC_0043 product-root rows.

**Direct clause conflicts** are enumerated clause by clause in
[SPEC_0047 §8](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#8-acceptance-time-amendment-map); every row there is amended atomically in this vote.

It further amends SPEC_0029 §5 and §12 with these exact SPEC_0041 §4 rows, which
TRP-032 splits: *"Compilation/session orchestration"* (`rumoca-compile`) gains
atomic sibling-package orchestration; *"DAE → `SolveProblem`; checked Algorithm
Code → `SolveAlgorithmBlock` lowering"* and *"Checked DAE pure-function graph →
shared typed Solve program regions and pure-call owners"* (`rumoca-phase-solve`)
gain the shared expression and function relation construction plus profile-bound
root closure; and *"DAE/Solve → checked GALEC lowering"* (`rumoca-phase-galec`)
narrows to projection and container authority with no expression lowering.

Governed: prepared execution artifacts, the target build session, typed target
manifests, product plans, the final expansion boundary, product budgets and
digests, and the eFMI sibling contract. The Solve grammar, type algebra,
profiles, and identity ladder are
[SPEC_0045](SPEC_0045_SOLVE_EXECUTABLE_VOCABULARY_AND_PROFILES.md).

### 2. Prepared Execution Artifacts

| ID | Rule | Owner/Where | Brief Justification |
|----|------|-------------|---------------------|
| TRP-001 | The four categories in [§4.14](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs) carry separate gates; mandatory legality and refinement plans carry NO speed gate. | preparation | Four costs, four gates |
| TRP-002 | A prepared artifact holds only the contents admitted by [§4.17](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs) and MAY persist under its `PreparedDigest`; an identity-bearing op-DAG clone, a second wire form, and a stored scalar analysis graph are prohibited. | backends | Persistence, not authority |
| TRP-046 | A PERSISTED plan MUST NOT serialize a `RootHandle`, a root-local ID, or a cloned DAG. It stores `RootDigest` plus a canonical `OwnerPath` — region, block, operation, and projection path, independent of in-memory interning and construction order. Replay resolves that path through checked root construction to FRESH branded handles and rechecks owner kind and type; a forged path or a wrong-root digest REJECTS. | preparation, replay | Paths replay, handles cannot |
| TRP-003 | Backend-local DAG, SSA, and CFG structures MAY carry local identities correlated to canonical owners, with zero semantic and wire authority. | backends | Machine IR, not sibling |
| TRP-004 | A second canonical graph requires a different stage contract PLUS a product witness not representable in the grammar. | governance | Harder than dispatch |

### 3. Target Build Session And Product Plans

| ID | Rule | Owner/Where | Brief Justification |
|----|------|-------------|---------------------|
| TRP-010 | One per-invocation build session issues target identity and profile once and validates capabilities from the issued inventory. It MAY issue and correlate MULTIPLE semantic roots, sealing EACH exactly once (problem plus correlated typed call and effect tables); demand-built artifacts derive once per ARTIFACT KEY — the tuple of owning root, artifact kind, and the normalized inputs that artifact consumes. | `rumoca-compile` | Seal each root once |
| TRP-011 | Every rendered file, capability record, and prepared variant records its own layer's digest; no product mixes layers. | `rumoca-compile` | One layer per record |
| TRP-012 | `target.toml` EXTENDS the existing deny-unknown manifest schema with the product-tagged typed fields of [§4.8](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs), including `CoverageMode::{NativeRequired, HybridMigration}` — a new axis, distinct from the existing `execution_mode`. It is not a parallel universal schema, and no dtype strings appear. | `rumoca-compile` | Free text fails open |
| TRP-047 | Manifest obligations are PRODUCT-TAGGED: a Solve-executable or Production-C product REQUIRES numeric, value, operation/effect, environment, and emission profiles; a Flat, DAE, or AC-only export FORBIDS root and preparation fields; a co-issued eFMI package carries tagged sibling inputs. OMISSION of a capability key canonically means DENY, never unknown. | `rumoca-compile` | Silence denies |
| TRP-033 | The `NumericProfile` request is the closed schema of [§4.21](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs), or a compiler-known named profile expanding to exactly those normalized fields. The source Modelica `Integer` default is SIGNED; an unsigned SEMANTIC representation comes ONLY from an explicit checked conversion — never from a range refinement (SEV-018) and never from a target-wide reinterpretation. | `rumoca-compile` | No silent reinterpretation |
| TRP-037 | Unsigned machine STORAGE for a semantically signed value is a prepared physical-layout optimization at the `PreparedDigest` layer: the value stays semantic `I32`, and the layout carries the [§4.23](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs) round-trip and no-reinterpretation receipt or is not selected. It is the unsigned special case of TRP-044. | preparation | Storage is not semantics |
| TRP-044 | EVERY prepared layout refinement — field offset and order, alignment, address space, AoS versus SoA, enum mapping, empty-field mapping, and storage representation — is a checked relation that preserves the semantic type and all values, carries its receipt, and moves `PreparedDigest`. A refinement without a valid receipt is not selected. | preparation | Layout refines, never redefines |
| TRP-045 | EVERY `ExecutionEnvironmentProfile` selection carries its own receipt recording environment, allocation and scratch decision, failure disposition, and — when a handler is named — the NORMALIZED CONTENT of its [§4.27](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs) contract, never a bare label; the receipt moves `PreparedDigest`. A selection whose handler contract fails its check, or which violates a §4.25 compatibility rule such as a required handler being absent, is not selected. | preparation | Environment is receipted too |
| TRP-039 | A target additionally declares the closed `ValueCapabilityProfile` of [§4.24](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs) and the `ExecutionEnvironmentProfile` of [§4.25](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs). A numeric profile proves nothing about `Boolean[4]`, a record array, or `no_std`. A target MAY reject any family or environment need, but only through this typed preparation boundary, never in a template. Both live at the `PreparedDigest` layer and are bounded prepared-product schemas, never platform ABI imported into Solve types. | preparation | Numeric width proves nothing else |
| TRP-048 | Validation is two-phase. ROOT-INTRINSIC requirements — the value, operation, effect, and environment needs every root owner has regardless of plan — are checked transitively BEFORE plan selection. CANDIDATE-SPECIFIC needs — workspace, stack, allocator, library, ISA, and concurrency — are checked DURING selection, so a candidate that does not fit is not selectable while another for the same root may be. The union is sealed and receipted before rendering. | preparation | Some needs are per-candidate |
| TRP-042 | A target also declares the `OperationEffectCapabilityProfile` of [§4.26](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs), keyed to the [SPEC_0049 §1](SPEC_0049_SOLVE_GRAMMAR_CATALOG.md#1-value-invoke-and-effect-variants) catalog, since admitting `f32` tensors proves nothing about `Fold`, `MatrixMultiply`, `InvokeOp`, a status or action effect, or volatile and atomic semantics. Construction and preparation check the transitive NESTED-REGION and CALL closure before candidate selection; this is how a target discharges SEV-007's reject-a-declared-capability duty. | preparation | Values are not operations |
| TRP-043 | ONE admission authority: the `NumericProfile` determines which semantic representations enter a root, and the capability profiles must COVER every representation, value family, operation, and effect the selected root actually uses. A capability profile MAY be a strict superset; it may never reinterpret or narrow a root, and any disagreement REJECTS. | preparation | Cover the root, never rewrite it |
| TRP-034 | The build session normalizes the request BEFORE Solve-root construction, since f32/i32 versus f64/i64 changes `RootDigest`; a target request that mismatches an already-profiled root REJECTS rather than converts. | `rumoca-compile` | Normalize before, never after |
| TRP-035 | A target selects a typed FINAL-EMISSION POLICY from [§4.22](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs). **"Expansion" means ephemeral C, Rust, or machine instructions emitted AFTER the sealed plan** — never coordinate `SolveOp`s, stored scalar programs, per-coordinate owners, or extent-sized preparation metadata. | preparation | Expansion is emission, not lowering |
| TRP-036 | Preparation evaluates the typed predicate and records EXACTLY ONE plan drawn from the COMPLETE [§4.18](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs) union — `DirectCompact`, `Loop`, `BoundedUnroll`, `Kernel`, `Composite`, `CheckedDispatch` — with its budget and coverage receipt. MiniJinja sees only that choice and its compact operands; it never iterates manifest candidates or decides loop versus kernel. | preparation | One recorded choice |
| TRP-013 | A kernel receipt binds every field in [§4.9](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs); without one the target keeps a loop or rejects, and name recognition or scalar recollapse is prohibited. | preparation | A name is not semantics |
| TRP-014 | A product plan is the closed union in [§4.18](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs). Coverage is exact over `(owner, logical domain point)`: a `Composite` MAY cover one owner in DISJOINT subdomains, and `CheckedDispatch` predicates MAY overlap only with proved equivalence and a deterministic selected branch. The plan proves total coverage with no gap and no duplicate `(owner, point)`. | preparation | Coverage, not candidates |
| TRP-015 | Unroll, tiling, fusion, kernels, and dispatch stay `PreparedDigest`-only IFF a receipt proves the exact root relation over [§4.3](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs); anything else builds a distinct root or is rejected. | preparation | Not an optimization |
| TRP-016 | `NativeRequired` rejects incomplete coverage; `HybridMigration` is explicit, recorded, and never silent. | preparation | Fallback must be visible |
| TRP-017 | There is no universal target program: the factors in [§4.10](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs) compose into sealed product-specific plans, and a common base is promoted only when three different products share an IDENTICAL mandatory invariant and checker flow. | preparation | Invariants, not counts |
| TRP-018 | The emitter sees one sealed plan and no candidates; renderer construction takes ONLY that plan plus packaging facts, and no context offers both compact and scalarized alternatives. | `rumoca-phase-codegen` | Template choice unverifiable |
| TRP-019 | The executable COMPUTATIONAL KERNEL semantics of C, Rust, WASM, and native products end at a profile-bound Solve root. A lifecycle product MAY additionally bind checked FMI component metadata, the eFMI AC sibling, and package facts — these enter prepared, artifact, and package identity, NEVER a forged `RootDigest`. Flat, DAE, and Algorithm-Code exports stay at their lowest valid IR. Distinct product and root kinds, enumerated in [SPEC_0047 §5](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#5-product-closure-matrix): Simulation `SolveProblem`, Simulation C-ODE, FMI component, Embedded C, and AlgorithmBlock eFMI PC. Products sharing a root kind say so; none borrows another's name. | `rumoca-compile` | Kernels end at the root |
| TRP-038 | Two products share a root ONLY when their semantic root KIND, complete semantic inputs, normalized arithmetic and sensitivity profile, AND lifecycle contract are all identical. Any difference — notably a Simulation versus AlgorithmBlock lifecycle — yields distinct roots that MUST NOT be substituted for one another, even when their issued expression and function correlations match. | `rumoca-compile` | Lifecycle is root identity |

### 4. Final Expansion Boundary And Budgets

| ID | Rule | Owner/Where | Brief Justification |
|----|------|-------------|---------------------|
| TRP-020 | No IR, evaluation, query, or preparation path EXPANDS a compact owner or domain beyond the operations the source already authored; executors loop over the compact owner, and final emitters alone create budgeted ephemeral machine instructions. An authored `ConstructAggregate{elements}` with N operands is legal — N is source-authored, not extent-derived. | Solve, backends | Expansion, not size, is the defect |
| TRP-021 | Prohibited: a stored semantic scalar graph, per-coordinate metadata DERIVED from tensor extent, and implicit scalar fallback; checked interval and range ownership replaces them. Backend-private structures — liveness bitsets over genuine definitions, for instance — MAY scale with those definitions, carrying zero semantic and wire authority (TRP-003). | construction | Extent-derived is the defect |
| TRP-022 | Every product carries a checked work, code-size, and resource budget plus ONE admitted execution path from the complete §4.18 union; bounded unrolling is explicit under TRP-014, constrained by TRP-015. | preparation | Permission, not efficiency |

### 5. eFMI Co-Issued Siblings

| ID | Rule | Owner/Where | Brief Justification |
|----|------|-------------|---------------------|
| TRP-030 | **eFMI siblings are co-issued from one checked semantic construction; AlgorithmCode owns a closed final-language projection, never a second semantic lowerer.** Both issue under the correlation and checksum obligations of [§4.19](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs); neither is lowered from the other, and neither validates its own construction. | see TRP-032 | Neither validates itself |
| TRP-032 | TRP-030's ownership splits three ways: the ONE shared expression and function relation construction plus the profile-bound Solve root closure belong to `rumoca-phase-solve`; the GALEC admissibility and lifecycle PROJECTION belongs to `rumoca-phase-galec`, which owns projection and container authority only; the atomic sibling-package orchestration — one construction transaction, the correlation web, and checksum binding — belongs to the build session in `rumoca-compile`. **`rumoca-phase-galec` owns no expression or function lowering.** | three crates | No second Solve compiler |
| TRP-031 | Untouched DAE keeps an independent GALEC/eFMI admissibility receipt; `eval-galec` checks the projection, a definitional Solve evaluator checks Solve, compiled C checks refinement, and OMC remains the independent frontend leg. The AC-to-PC numeric relation is DECLARED per [§4.32](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs): `eval-galec` is PARAMETERIZED by that mapping, which gives AlgorithmCode no `RootDigest` and creates no dependence on the Solve evaluator. | oracles | Four legs, no self-proof |

### 6. Current State, Gates, And Rejected Alternatives

Every row of
[SPEC_0047 §1](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#1-current-implementation-state)
is `Partial` or `Absent` with its location and closing edge. A rule is
implemented only when every gate its `Covers` row names passes:
[SPEC_0047 §2](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#2-red-gates-and-witnesses-preregistered).
The three eFMI alternatives and the other defeated options, with costs and
reversal gates, are
[SPEC_0047 §3](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#3-rejected-alternatives).

### 7. Reversal Gates

| ID | Reopening | Requires |
|----|-----------|----------|
| TRP-040 | Retaining an OPTIONAL optimization | Preregistered metric, corpus, code-size, and peak-RSS budgets against a control at identical `RootDigest`; below any bound it is not retained |
| TRP-041 | Either eFMI extreme (TRP-030) | EITHER the chosen design fails ANY preregistered SEV-134, SEV-135a, SEV-135b, or cost gate — in which case the alternative recorded in SPEC_0047 §3 is reconsidered on that evidence — OR a named external assurance requirement plus a prototype demonstrating no duplicate semantic lowering |

## References

- [SPEC_0045](SPEC_0045_SOLVE_EXECUTABLE_VOCABULARY_AND_PROFILES.md)
  — the Solve grammar, profiles, and identity ladder this spec refines.
- [SPEC_0047](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md) — shared
  evidence and field-catalog annex; binding force lives in the rules above,
  which enumerate their field lists and gates there.
