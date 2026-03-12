# Rumoca Specification Index

Contributor-facing workflow commands referenced by the active specs are standardized through the
`rum` developer CLI. The main groups are:

- `rum verify ...`
- `rum coverage ...`
- `rum repo ...`

For setup and day-to-day usage, see [CONTRIBUTING.md](../CONTRIBUTING.md).

## Active Specifications

| Spec | Title | Domain | Lines | Status |
|------|-------|--------|-------|--------|
| [SPEC_0000](SPEC_0000_SPEC_GUIDELINES.md) | Specification Writing Guidelines | process | ~120 | ACCEPTED |
| [SPEC_0001](SPEC_0001_DEFID.md) | DefId for Stable References | IR | ~50 | ACCEPTED |
| [SPEC_0002](SPEC_0002_SCOPE_TREE.md) | Scope Tree for Name Lookup | IR | ~100 | ACCEPTED |
| [SPEC_0003](SPEC_0003_HYBRID_DAE.md) | Complete Hybrid DAE Formulation | IR | ~115 | ACCEPTED |
| [SPEC_0004](SPEC_0004_INSTANTIATE_FLATTEN.md) | Separate Instantiation and Flattening Phases | phase | ~120 | ACCEPTED |
| [SPEC_0007](SPEC_0007_LEAN_DAE.md) | Lean DAE (No Derived Data) | IR | ~90 | ACCEPTED |
| [SPEC_0008](SPEC_0008_PHASE_ERRORS.md) | Phase-Local Error Types | error | ~220 | ACCEPTED |
| [SPEC_0009](SPEC_0009_COMMON_CRATE.md) | Single Foundation Crate (rumoca-core) | architecture | ~100 | ACCEPTED |
| [SPEC_0017](SPEC_0017_ORDERED_COLLECTIONS.md) | Ordered Collection Pattern | convention | ~100 | ACCEPTED |
| [SPEC_0018](SPEC_0018_TOOL_CONFIG.md) | Tool Configuration Loading | tooling | ~155 | ACCEPTED |
| [SPEC_0019](SPEC_0019_ARRAY_PRESERVATION.md) | Array Preservation Through Flatten Phase | phase | ~125 | ACCEPTED |
| [SPEC_0020](SPEC_0020_ALGORITHM_PRESERVATION.md) | Model Algorithm Lowering and Function Algorithm Preservation | phase | ~95 | ACCEPTED |
| [SPEC_0021](SPEC_0021_CODE_COMPLEXITY.md) | Code Complexity Guidelines | convention | ~170 | ACCEPTED |
| [SPEC_0022](SPEC_0022_MLS_COMPILER_COMPLIANCE.md) | MLS Compiler Compliance (431 contracts) | MLS | ~960 | REFERENCE |
| [SPEC_0023](SPEC_0023_CRATE_ARCHITECTURE.md) | Crate Architecture for MLS Compliance | architecture | ~270 | DRAFT |
| [SPEC_0024](SPEC_0024_DIAGNOSTIC_INSTRUMENTATION.md) | Diagnostic Instrumentation (tracing) | tooling | ~120 | DRAFT |
| [SPEC_0025](SPEC_0025_PR_REVIEW_PROCESS.md) | PR Review Process | process | ~310 | ACCEPTED |
| [SPEC_0026](SPEC_0026_SOURCE_TRACEABILITY.md) | Source Traceability | convention | ~120 | ACCEPTED |
| [SPEC_0028](SPEC_0028_CERTIFICATION_CODEGEN.md) | Designing for Safety in Code Generation | codegen | ~110 | DRAFT |
| [SPEC_0029](SPEC_0029_CRATE_BOUNDARIES.md) | Crate Boundaries as Collaboration Guardrails | architecture | ~170 | ACCEPTED |
| [SPEC_0030](SPEC_0030_COVERAGE_TRIM_PROCESS.md) | Coverage Trim and Gate Process | process | ~100 | ACCEPTED |

## Archived Specifications

Archived specs are historical context and are **not** part of the active PR compliance set.

### Deferred

- [SPEC_0012](archive/deferred/SPEC_0012_CST_AST.md) - CST vs AST Distinction
- [SPEC_0014](archive/deferred/SPEC_0014_EVAL_MEMO.md) - Eval Memoization at Phase Boundaries
- [SPEC_0015](archive/deferred/SPEC_0015_FORMATTER.md) - Token-Based Formatter (Pretty-Printer)
- [SPEC_0016](archive/deferred/SPEC_0016_COLLISION_DETECTION.md) - Collision Detection Posture
- [SPEC_0005](archive/deferred/SPEC_0005_TWO_HASH.md) - Two-Hash Strategy (not implemented)
- [SPEC_0006](archive/deferred/SPEC_0006_NARY_CANONICAL.md) - N-ary Canonical Form (not implemented)
- [SPEC_0011](archive/deferred/SPEC_0011_EQID_ORDER_INDEPENDENT.md) - Order-Independent EqId (not implemented)
- [SPEC_0013](archive/deferred/SPEC_0013_STABLE_ID.md) - Unified stable_id Contract (not implemented)
- [SPEC_0010](archive/deferred/SPEC_0010_CASE_SENSITIVE.md) - Case-Sensitive Identifiers (redundant with MLS §2.3.1)

### Rejected

(None yet.)

### Legacy

- [SPEC_0027](archive/legacy/SPEC_0027_PHASE_RESPONSIBILITIES.md) - Compilation Phase Responsibilities
- [PR_CHECKLIST](archive/legacy/PR_CHECKLIST.md) - Legacy PR checklist (superseded by SPEC_0025)
