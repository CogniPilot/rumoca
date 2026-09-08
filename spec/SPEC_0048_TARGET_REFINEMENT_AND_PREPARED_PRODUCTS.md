# SPEC_0048: Target Refinement and Prepared Products

## Status
DRAFT

## Summary

One build session seals each issued Solve root once, prepares one closed
total-coverage product plan, and hands a passive emitter one issued variant.

## Specification

**Sections.** 1 governance · 2 prepared artifacts · 3 build session and plans ·
4 expansion boundary · 5 eFMI refinement chain · 6 state and gates · 7 reversal gates.

### 1. Governance, Scope, And Acceptance-Time Amendment Map

This DRAFT claims none today. On acceptance it amends SPEC_0007 Stage 4 with
lockstep SPEC_0040 C13/C14/C20 and TRP-051's C51–C57 consumer edges; SPEC_0032 §§2/4/5; SPEC_0034 Summary, pipeline,
GAL-027, and GAL-038; and DRAFT SPEC_0036 with SPEC_0043 product-root rows.

**Direct clause conflicts** are enumerated clause by clause in
[SPEC_0047 §8](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#8-acceptance-time-amendment-map); every row there is amended atomically in this vote.

It further amends SPEC_0029 §5 and §12 with the exact SPEC_0041 §4 ownership
rows TRP-032 splits, also enumerated in SPEC_0047 §8.

Governed: prepared artifacts, the build session, typed manifests, product plans,
the expansion boundary, budgets, digests, and the eFMI refinement-chain contract.
Grammar, type algebra, profiles, and the identity ladder are
[SPEC_0045](SPEC_0045_SOLVE_EXECUTABLE_VOCABULARY_AND_PROFILES.md).

### 2. Prepared Execution Artifacts

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Prepared artifacts may retain only checked receipts, plans, digests, and root-independent replay paths. They cannot serialize branded handles, clone a semantic DAG, create a second wire authority, or promote backend-local graphs to compiler authority. Exact clauses: [TRP-001–004 and TRP-046](SPEC_0055_TARGET_REFINEMENT_CATALOG.md#2-prepared-execution-artifacts). | preparation + replay | Persistence cannot become authority |

### 3. Target Build Session And Product Plans

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| One session normalizes target/profile identity before root construction and issues each root and artifact key once. Exact clauses: TRP-010–012, TRP-033–034. | `rumoca-compile` | No late profile selection |
| Product/root schemas and capability profiles are closed, product-tagged, Default-free, and checked for complete value/operation/effect/environment closure. Exact clauses: TRP-039, TRP-042–050. | target construction + preparation | Omission cannot mean support |
| Every layout, environment, qualification, kernel, and optimization choice carries its own exact preservation receipt or is not selected. Exact clauses: TRP-013, TRP-015, TRP-037, TRP-044–045, TRP-052. | preparation | Selection requires proof |
| Preparation emits one sealed product plan with independent exact execution and call coverage, resolved fallback, and bounded inlining; templates receive only that plan. Exact clauses: TRP-014, TRP-016–019, TRP-035–036. | preparation + codegen | Emitters cannot choose |
| Runtime preparation assigns every reachable owner one fixed interpreter or precompiled-native arm before session exposure; compilation failure is construction failure. Exact clause: TRP-053. | runtime preparation | No runtime recovery path |

All TRP identifiers in this section link to
[SPEC_0055 §3](SPEC_0055_TARGET_REFINEMENT_CATALOG.md#3-target-build-session-and-product-plans).

### 4. Final Expansion Boundary And Budgets

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Solve remains tensor-native: no IR, evaluator, query, or preparation path may create extent-derived scalar owners or metadata. Only final emitters may create budgeted ephemeral instructions, and every executable product carries one checked work/code/resource budget and admitted execution path. Exact clauses: [TRP-020–022](SPEC_0055_TARGET_REFINEMENT_CATALOG.md#4-final-expansion-boundary-and-budgets). | Solve + final emitters | Expansion is final and budgeted |

### 5. eFMI Refinement Chain

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| The only eFMI executable chain is untouched DAE → checked Algorithm Code → profile-bound `SolveAlgorithmBlock`. Phase-galec owns the first restriction, phase-solve owns the second refinement, compile owns atomic packaging, and four independent oracle legs check the relation; direct DAE → AlgorithmBlock and Solve → GALEC are forbidden. Exact clauses: [TRP-030–032 and TRP-051](SPEC_0055_TARGET_REFINEMENT_CATALOG.md#5-efmi-refinement-chain). | phase-galec + phase-solve + compile | One ordered refinement chain |

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
- [SPEC_0055](SPEC_0055_TARGET_REFINEMENT_CATALOG.md) — exact TRP clause
  catalog, normative only through the affirmative rules above.
