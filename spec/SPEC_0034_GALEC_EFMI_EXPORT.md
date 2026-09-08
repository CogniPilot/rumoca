# SPEC_0034: eFMI/GALEC Algorithm Code Export

## Status
DRAFT

Only the Conformance Ladder records earned claims; pending rules and the
experimental slice establish no conformance.

## Summary
Rumoca renders Algorithm Code from a checked `AlgorithmCodePackage`; only its
correlated `SolveAlgorithmBlock` may authorize experimental Production C/H,
while canonical `SolveProblem` construction remains independent.

## Specification

### Pipeline Placement And Ownership

The target chain is DAE → checked `AlgorithmCodePackage` → correlated
`SolveAlgorithmBlock`; Algorithm Code rendering borrows the package, Production
C/H borrows only the block, and generic packaging owns bytes. GAL-001–GAL-014
and the remaining Rules below govern GALEC ownership; SPEC_0029 §12 governs
the shared crate/dependency boundary. The diagrams were duplicated rationale
and are not a second architecture source.

### Rules

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Algorithm Code is an auxiliary checked export; projection leaves canonical DAE and numerical Solve unchanged and runs admissibility before construction. Exact clauses: [GAL-001–004](SPEC_0042_GALEC_LANGUAGE_CATALOG.md#10-gal-rule-catalog). | DAE → GALEC boundary | Canonical semantics stay intact |
| Checked construction owns GALEC language, lifecycle, typing, names, signals, clocks, initialization, diagnostics, and syntax constraints. Exact clauses: [GAL-005–020](SPEC_0042_GALEC_LANGUAGE_CATALOG.md#10-gal-rule-catalog). | GALEC construction | Invalid GALEC cannot escape |
| Packaging owns honest conformance claims, versions, licensed assets, profile-bound Production-Code mapping, and scoped refusals. Exact clauses: [GAL-021–025](SPEC_0042_GALEC_LANGUAGE_CATALOG.md#10-gal-rule-catalog). | target/package construction | Claims follow evidence |
| GALEC and Solve remain tensor-native; evaluators, Integer proofs, C restrictions, storage, effects, and differential oracles are checked before rendering. Exact clauses: [GAL-026–040](SPEC_0042_GALEC_LANGUAGE_CATALOG.md#10-gal-rule-catalog). | evaluation + Solve preparation | Rendering makes no choices |
| Package construction issues total branded subject/provenance/call correlations and one profile-bound correlated Solve product. Exact clauses: [GAL-041–044](SPEC_0042_GALEC_LANGUAGE_CATALOG.md#10-gal-rule-catalog). | package + Solve refinement | Consumers cannot reconstruct meaning |
| Template borrowing and packaged paths derive from one checked context, role family, source map, and artifact layout. Exact clauses: [GAL-045–046](SPEC_0042_GALEC_LANGUAGE_CATALOG.md#10-gal-rule-catalog). | target construction + rendering | Bytes retain their origin |

GAL-016/GAL-024 rationale is [SPEC_0042 §3](SPEC_0042_GALEC_LANGUAGE_CATALOG.md#3-rule-rationale-spec_0034-gal-016-gal-024).

#### Resolved Decisions (Phase 1 gates)

Decisions `D1`–`D12` are [SPEC_0042 §1](SPEC_0042_GALEC_LANGUAGE_CATALOG.md#1-resolved-decisions-spec_0034-phase-1-gates);
reopening one amends this spec.

#### Conformance Ladder (GAL-021, GAL-024)

The complete claim/evidence/status rows are
[SPEC_0042 §7](SPEC_0042_GALEC_LANGUAGE_CATALOG.md#7-conformance-ladder),
normative by reference from GAL-021/GAL-024. No lower rung implies a higher one.

#### Variable Classification and Checked Construction Scope

The Modelica-to-GALEC declaration/causality table and the per-analysis checks
whole-block construction runs are lookup tables:
[SPEC_0042 §5](SPEC_0042_GALEC_LANGUAGE_CATALOG.md#5-variable-classification-and-checked-construction-scope).
GAL-020 and GAL-017 own the governing rules; both tables are normative by
reference from them.

#### Language Traps (T1–T14)

Traps `T1`–`T14` catalog the GALEC-versus-Modelica language differences and the
emitter consequence each one imposes:
[SPEC_0042 §2](SPEC_0042_GALEC_LANGUAGE_CATALOG.md#2-language-traps-t1t14).
GAL-005, GAL-015, GAL-019, and GAL-028 own the governing rules; the trap rows
are normative by reference from them.

### Testing Requirements

Every test row in
[SPEC_0042 §8](SPEC_0042_GALEC_LANGUAGE_CATALOG.md#8-testing-evidence-catalog)
is required and normative by reference from its cited GAL rule.

### Non-Goals

- GALEC does not replace canonical DAE or numerical `SolveProblem`; the
  eFMI/embedded-C path refines checked DAE → `AlgorithmCodePackage` →
  `SolveAlgorithmProduct` without changing Modelica semantics or authorizing
  canonical-DAE rewrites.
- No Behavioral Model (ch. 4; an eFMU is valid without one), FMU embedding, or
  Binary Code representation.
- An `efmu` is an eFMI AC/PC deployment container, not the FMI 3 simulation
  product. The separate `fmi3` target owns Model Exchange plus Co-Simulation;
  its ME side is host-integrated and its CS side owns the built-in solver.
- The parser never accepts Modelica input — GALEC only (GAL-014).

## References

- Ground truth: **eFMI Standard 1.0.0 Beta 1** (CC-BY-SA text not reproduced,
  GAL-023): ch. 2 container; §3.1 manifest; §3.2 analyses/signals/builtins;
  App. C reserved names; ch. 5 Production Code.
- [SPEC_0042](SPEC_0042_GALEC_LANGUAGE_CATALOG.md) — language traps and resolved
  decisions, normative by reference from the GAL-NNN rules above.
- [SPEC_0007](SPEC_0007_IR_PIPELINE.md), [SPEC_0008](SPEC_0008_PHASE_ERRORS.md),
  [SPEC_0029](SPEC_0029_CRATE_BOUNDARIES.md) (§12 template boundary).
- [MISRA Compliance:2020](https://www.misra.org.uk/app/uploads/2021/06/MISRA-Compliance-2020.pdf)
  (process and claim boundary); MISRA C:2023 guideline text is not reproduced.
- [FAA AC 20-115D](https://www.faa.gov/airports/resources/advisory_circulars/index.cfm/go/document.information/documentNumber/20-115D)
  (DO-178C with DO-330/DO-331/DO-333 as applicable).
