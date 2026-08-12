# SPEC_0048: Target Refinement and Prepared Products

## Status
DRAFT

## Summary

One per-invocation build session seals one Solve root, prepares one closed
product plan with total owner coverage, and hands a passive emitter exactly one
issued variant.

## Specification

### 1. Governance, Scope, And Acceptance-Time Amendment Map

This DRAFT proposes the amendments below and claims none today. On acceptance
it amends SPEC_0007 Stage 4 with lockstep SPEC_0040 C13/C14/C20 (scalar
programs exist only at the final emitter, as an issued plan); SPEC_0029 §§5/12
with SPEC_0041 ownership rows (preparation, not templates, owns product
choice); SPEC_0032 §§2/4/5 (the shared `rumoca-eval-solve` scalar-fallback
license narrows to the final expansion boundary); SPEC_0034 Summary, pipeline,
GAL-027, and GAL-038 (TRP-030 replaces independently lowered AlgorithmCode
bodies); and DRAFT SPEC_0036 with SPEC_0043 product-root rows.

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
| TRP-003 | Backend-local DAG, SSA, and CFG structures MAY carry local identities correlated to canonical owners, with zero semantic and wire authority. | backends | Machine IR, not sibling |
| TRP-004 | A second canonical graph requires a different stage contract PLUS a product witness not representable in the grammar. | governance | Harder than dispatch |

### 3. Target Build Session And Product Plans

| ID | Rule | Owner/Where | Brief Justification |
|----|------|-------------|---------------------|
| TRP-010 | One per-invocation build session issues target identity and profile once, seals ONE semantic root (problem plus correlated typed call and effect tables), and validates capabilities from its inventory; demand-built artifacts derive once per ARTIFACT KEY. | `rumoca-compile` | Two lowerings, two roots |
| TRP-011 | Every rendered file, capability record, and prepared variant records its own layer's digest; no product mixes layers. | `rumoca-compile` | One layer per record |
| TRP-012 | `target.toml` is deny-unknown typed over [§4.8](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs), including `ExecutionMode::{NativeRequired, HybridMigration}`; no dtype strings. | `rumoca-compile` | Free text fails open |
| TRP-013 | A kernel receipt binds every field in [§4.9](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs); without one the target keeps a loop or rejects, and name recognition or scalar recollapse is prohibited. | preparation | A name is not semantics |
| TRP-014 | A product plan is the closed union in [§4.18](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs). Ordered candidates agree only on OVERLAPPING admitted predicate domains; the plan proves total root-domain coverage, no owner gap or overlap, and root-equivalent result and status on those overlaps. | preparation | Coverage, not candidates |
| TRP-015 | Unroll, tiling, fusion, kernels, and dispatch stay `PreparedDigest`-only IFF a receipt proves the exact root relation over [§4.3](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs); anything else builds a distinct root or is rejected. | preparation | Not an optimization |
| TRP-016 | `NativeRequired` rejects incomplete coverage; `HybridMigration` is explicit, recorded, and never silent. | preparation | Fallback must be visible |
| TRP-017 | There is no universal target program: the factors in [§4.10](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs) compose into sealed product-specific plans, and a common base is promoted only when three different products share an IDENTICAL mandatory invariant and checker flow. | preparation | Invariants, not counts |
| TRP-018 | The emitter sees one sealed plan and no candidates; renderer construction takes ONLY that plan plus packaging facts, and no context offers both compact and scalarized alternatives. | `rumoca-phase-codegen` | Template choice unverifiable |
| TRP-019 | Executable C, Rust, WASM, and native products end at a profile-bound Solve root; Flat, DAE, and Algorithm-Code exports stay at their lowest valid IR. One root exists per semantic input plus profile: same-profile products MAY share it, others MUST NOT. | `rumoca-compile` | No manufactured roots |

### 4. Final Expansion Boundary And Budgets

| ID | Rule | Owner/Where | Brief Justification |
|----|------|-------------|---------------------|
| TRP-020 | No IR, evaluation, query, or preparation path materializes per-element semantic nodes; executors loop over the compact owner, and final emitters alone create budgeted ephemeral machine instructions. | Solve, backends | Materialization is the defect |
| TRP-021 | Prohibited: a stored semantic scalar graph, extent-sized construction or preparation, per-register bitmaps, and implicit scalar fallback; checked interval and range ownership replaces them. | construction | O(extent) is the defect |
| TRP-022 | Every product carries a checked work, code-size, and resource budget plus an admitted loop or kernel path; bounded unrolling is explicit under TRP-014, constrained by TRP-015. | preparation | Permission, not efficiency |

### 5. eFMI Co-Issued Siblings

| ID | Rule | Owner/Where | Brief Justification |
|----|------|-------------|---------------------|
| TRP-030 | **eFMI siblings are co-issued from one checked semantic construction; AlgorithmCode owns a closed final-language projection, never a second semantic lowerer.** Both issue under the correlation and checksum obligations of [§4.19](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs); neither is lowered from the other, and neither validates its own construction. | `rumoca-phase-galec` | Neither validates itself |
| TRP-031 | Untouched DAE keeps an independent GALEC/eFMI admissibility receipt; `eval-galec` checks the projection, a definitional Solve evaluator checks Solve, compiled C checks refinement, and OMC remains the independent frontend leg. | oracles | Four legs, no self-proof |

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
| TRP-041 | Either eFMI extreme (TRP-030) | A named external assurance requirement AND a prototype demonstrating no duplicate semantic lowering |

## References

- [SPEC_0045](SPEC_0045_SOLVE_EXECUTABLE_VOCABULARY_AND_PROFILES.md)
  — the Solve grammar, profiles, and identity ladder this spec refines.
- [SPEC_0047](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md) — shared
  evidence and field-catalog annex; binding force lives in the rules above,
  which enumerate their field lists and gates there.
