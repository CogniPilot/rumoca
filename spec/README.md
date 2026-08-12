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
| [SPEC_0000](SPEC_0000_SPEC_GUIDELINES.md) | Specification Writing Guidelines | process | ~258 | ACCEPTED |
| [SPEC_0001](SPEC_0001_DEFID.md) | DefId for Stable References | IR | ~128 | ACCEPTED |
| [SPEC_0002](SPEC_0002_SCOPE_TREE.md) | Scope Tree for Name Lookup | IR | ~119 | ACCEPTED |
| [SPEC_0007](SPEC_0007_IR_PIPELINE.md) | Compiler Pipeline and IR Contracts | architecture | ~313 | ACCEPTED |
| [SPEC_0008](SPEC_0008_PHASE_ERRORS.md) | Diagnostics, Traceability, and Phase-Local Errors | error | ~328 | ACCEPTED |
| [SPEC_0018](SPEC_0018_TOOL_CONFIG.md) | Tool Configuration Loading | tooling | ~329 | ACCEPTED |
| [SPEC_0021](SPEC_0021_CODE_COMPLEXITY.md) | Maintainability and Determinism Guidelines | convention | ~234 | ACCEPTED |
| [SPEC_0022](SPEC_0022_MLS_COMPILER_COMPLIANCE.md) | MLS Compiler Compliance (438 contracts) | MLS | ~1010 | REFERENCE |
| [SPEC_0025](SPEC_0025_PR_REVIEW_PROCESS.md) | Change Review Process | process | ~236 | ACCEPTED |
| [SPEC_0029](SPEC_0029_CRATE_BOUNDARIES.md) | Crate Boundaries as Collaboration Guardrails | architecture | ~344 | ACCEPTED |
| [SPEC_0031](SPEC_0031_COMPILER_PHILOSOPHY.md) | Compiler Scope and Philosophy | architecture | ~159 | REFERENCE |
| [SPEC_0032](SPEC_0032_RANGE_PRESERVING_TENSORS.md) | Range-Preserving Tensor IR | IR | ~191 | ACCEPTED |
| [SPEC_0033](SPEC_0033_DEVELOPMENT_PROCESS.md) | Development Process | process | ~172 | ACCEPTED |
| [SPEC_0034](SPEC_0034_GALEC_EFMI_EXPORT.md) | eFMI/GALEC Algorithm Code Export | target/codegen | ~186 | DRAFT |
| [SPEC_0035](SPEC_0035_COMPLEX_NUMERIC_TYPES.md) | Complex Numeric Types in Solve IR | IR | ~196 | DRAFT |
| [SPEC_0036](SPEC_0036_VALID_BY_CONSTRUCTION_IR.md) | Valid-by-Construction Compiler IR | IR | ~319 | DRAFT |
| [SPEC_0037](SPEC_0037_FORMALLY_VERIFIED_COMPILER.md) | Formally Verified Compiler | verification | ~311 | DRAFT |
| [SPEC_0038](SPEC_0038_UNIFIED_FMI_EXECUTION.md) | Unified FMI Execution | target/runtime | ~266 | DRAFT |
| [SPEC_0039](SPEC_0039_PROOF_CARRYING_SPARSITY.md) | Proof-Carrying Structural Sparsity | IR | ~153 | DRAFT |
| [SPEC_0040](SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md) | IR Stage Contract Catalog | architecture | ~129 | REFERENCE |
| [SPEC_0041](SPEC_0041_CRATE_OWNERSHIP_CATALOG.md) | Crate Ownership Catalog | architecture | ~135 | REFERENCE |
| [SPEC_0042](SPEC_0042_GALEC_LANGUAGE_CATALOG.md) | GALEC Language and Decision Catalog | target/codegen | ~67 | REFERENCE |
| [SPEC_0043](SPEC_0043_CONSTRUCTION_CATALOG.md) | Valid-by-Construction Catalog | IR | ~256 | REFERENCE |
| [SPEC_0044](SPEC_0044_FMI_EXECUTION_CATALOG.md) | FMI Execution Contract Catalog | runtime/verification | ~151 | REFERENCE |
| [SPEC_0045](SPEC_0045_SOLVE_EXECUTABLE_VOCABULARY_AND_PROFILES.md) | Solve Executable Vocabulary and Profiles | IR | ~165 | DRAFT |
| [SPEC_0047](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md) | Solve Vocabulary and Target Refinement Catalog | IR/target | ~385 | REFERENCE |
| [SPEC_0048](SPEC_0048_TARGET_REFINEMENT_AND_PREPARED_PRODUCTS.md) | Target Refinement and Prepared Products | target/codegen | ~119 | DRAFT |
| [SPEC_0049](SPEC_0049_SOLVE_GRAMMAR_CATALOG.md) | Solve Grammar and Effect Catalog | IR | ~200 | REFERENCE |

### Reference annexes

`SPEC_0040`–`SPEC_0044`, `SPEC_0047`, and `SPEC_0049` are REFERENCE annexes:
they carry the lookup catalogs split out of their parent spec under SPEC_0000
§3/§3a size budgets. Every row in an annex is normative by reference from the
parent section that links it (SPEC_0007→0040, SPEC_0029→0041, SPEC_0034→0042,
SPEC_0036→0043). `SPEC_0047` serves two parents, SPEC_0045 and SPEC_0048, and
each of its gate rows names the parent rule it covers. `SPEC_0049` also serves
both, bound by SEV-001/002/007 (grammar and coverage), SEV-011/024 (contract
classes and element-kind splits), and TRP-042 (the capability profile keyed to
its rows). Annexes add no rules of their own; edit the parent when the
requirement itself changes.

## Deferred Specifications

Deferred specs are non-active future-work proposals. They do not gate reviews or
CI, but remain worth preserving because the design direction is likely to be
useful after the 0.9 stabilization work.

| Spec | Title | Domain | Lines | Status |
|------|-------|--------|-------|--------|
| [SPEC_0012](archive/deferred/SPEC_0012_CST_AST.md) | CST vs AST Distinction | parser/tooling | ~167 | DEFERRED |
| [SPEC_0014](archive/deferred/SPEC_0014_EVAL_MEMO.md) | Eval Memoization at Phase Boundaries | performance | ~189 | DEFERRED |
| [SPEC_0015](archive/deferred/SPEC_0015_FORMATTER.md) | Token-Based Formatter | tooling | ~249 | DEFERRED |
| [SPEC_0028](archive/deferred/SPEC_0028_CERTIFICATION_CODEGEN.md) | Safety-Oriented Code Generation | codegen | ~97 | DEFERRED |
