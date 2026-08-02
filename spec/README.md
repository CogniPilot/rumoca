# Rumoca Specification Index

Contributor-facing workflow commands referenced by the active specs are standardized through the
`rum` developer CLI. The main groups are:

- `cargo xtask verify ...`
- `cargo xtask coverage ...`
- `cargo xtask repo ...`

For setup and day-to-day usage, see [CONTRIBUTING.md](../CONTRIBUTING.md).

## Active Specifications

| Spec | Title | Domain | Lines | Status |
|------|-------|--------|-------|--------|
| [SPEC_0000](SPEC_0000_SPEC_GUIDELINES.md) | Specification Writing Guidelines | process | ~220 | ACCEPTED |
| [SPEC_0001](SPEC_0001_DEFID.md) | DefId for Stable References | IR | ~50 | ACCEPTED |
| [SPEC_0002](SPEC_0002_SCOPE_TREE.md) | Scope Tree for Name Lookup | IR | ~95 | ACCEPTED |
| [SPEC_0007](SPEC_0007_IR_PIPELINE.md) | Compiler IR Graph and Contracts | architecture | ~320 | ACCEPTED |
| [SPEC_0008](SPEC_0008_PHASE_ERRORS.md) | Diagnostics, Traceability, and Phase-Local Errors | error | ~260 | ACCEPTED |
| [SPEC_0018](SPEC_0018_TOOL_CONFIG.md) | Tool Configuration Loading | tooling | ~155 | ACCEPTED |
| [SPEC_0021](SPEC_0021_CODE_COMPLEXITY.md) | Maintainability and Determinism Guidelines | convention | ~210 | ACCEPTED |
| [SPEC_0022](SPEC_0022_MLS_COMPILER_COMPLIANCE.md) | MLS Compiler Compliance (431 contracts) | MLS | ~960 | REFERENCE |
| [SPEC_0025](SPEC_0025_PR_REVIEW_PROCESS.md) | Change Review Process | process | ~350 | ACCEPTED |
| [SPEC_0029](SPEC_0029_CRATE_BOUNDARIES.md) | Crate Boundaries as Collaboration Guardrails | architecture | ~340 | ACCEPTED |
| [SPEC_0031](SPEC_0031_COMPILER_PHILOSOPHY.md) | Compiler Scope and Philosophy | architecture | ~150 | ACCEPTED |
| [SPEC_0032](SPEC_0032_RANGE_PRESERVING_TENSORS.md) | Range-Preserving Tensor IR | IR | ~85 | ACCEPTED |
| [SPEC_0034](SPEC_0034_GALEC_EFMI_EXPORT.md) | eFMI/GALEC Algorithm Code Export | target/codegen | ~205 | DRAFT |

## Deferred Specifications

Deferred specs are non-active future-work proposals. They do not gate reviews or
CI, but remain worth preserving because the design direction is likely to be
useful after the 0.9 stabilization work.

| Spec | Title | Domain | Lines | Status |
|------|-------|--------|-------|--------|
| [SPEC_0012](archive/deferred/SPEC_0012_CST_AST.md) | CST vs AST Distinction | parser/tooling | ~170 | DEFERRED |
| [SPEC_0014](archive/deferred/SPEC_0014_EVAL_MEMO.md) | Eval Memoization at Phase Boundaries | performance | ~200 | DEFERRED |
| [SPEC_0015](archive/deferred/SPEC_0015_FORMATTER.md) | Token-Based Formatter | tooling | ~250 | DEFERRED |
| [SPEC_0028](archive/deferred/SPEC_0028_CERTIFICATION_CODEGEN.md) | Safety-Oriented Code Generation | codegen | ~100 | DEFERRED |
