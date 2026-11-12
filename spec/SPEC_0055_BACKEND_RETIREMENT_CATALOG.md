# SPEC_0055: Source-Bound Backend Retirement Catalog

## Status
REFERENCE

## Summary
Maps DRAFT SPEC_0037 §2a's joint retirement obligations to existing owners, evidence and refusal controls.

## Specification

This is the lookup catalog for the **non-active DRAFT**
[SPEC_0037 §2a](SPEC_0037_FORMALLY_VERIFIED_COMPILER.md#2a-source-bound-backend-retirement).
It adds no independent rule or permission. The implemented
[SPEC_0041 §7 freeze](SPEC_0041_CRATE_OWNERSHIP_CATALOG.md#7-codegen-target-and-command-ownership-catalog-spec_0029-12)
remains authoritative until a reviewed production joint checker exists.

### 1. Existing Implementation Boundary

These are current APIs, not a proposed authorization type.

| Existing API | Source | Established fact |
|---|---|---|
| `migration_ledger::run(root: &Path) -> Result<()>` | `crates/xtask/src/migration_ledger.rs` | Complete snapshot unchanged except pinned revision R001; 95 unresolved audit groups |
| `FrozenBaseline::checked`, `FrozenBaseline::source` | `crates/xtask/src/migration_ledger/baseline.rs` | Immutable ledger and exact borrowed old bytes |
| `captured::check_unchanged_capture` | `crates/xtask/src/migration_ledger/captured.rs` | Frozen directory inventory in one retained capture |
| `source_closure::capture`, `SourceClosure::tree` | `crates/xtask/src/print_boundary_check/source_closure.rs` | Bounded complete retained bytes, including unexpected descendants |
| `dependency_closure::collect` | `crates/xtask/src/print_boundary_check/dependency_closure.rs` | Cargo-resolved production dependencies, features and target edges |
| `source_complement::prove_complement` | `crates/xtask/src/migration_ledger/source_complement.rs` | Test-only byte relation outside supplied ranges |
| `SolveAlgorithmBlock::call_levels` | `crates/rumoca-ir-solve/src/typed_program/algorithm_block/call_levels.rs` | IR-issued levels; no source-replacement permission |
| `TypedProgram::print_plan` | `crates/rumoca-ir-solve/src/typed_program/program/print_plan.rs` | Construction-issued print projection |

The snapshot is revision `187eee7a`, its ledger digest starts `51f97c10`, and
its scope is the entire `rumoca-phase-codegen` directory, including tests and
assets. Capture equality is not atomic filesystem custody. The complement
module accepts supplied ranges and is not consumed by production authorization.
Making it production-visible alone cannot implement the parent contract.

### 2. Joint Evidence Map

Every row maps the parent contract; missing products remain refusals.

| Parent obligation | Evidence owner and binding | Refusal controls |
|---|---|---|
| Immutable baseline | One checked ledger owner and exact old source bytes | Changed ledger, rebaselined revision, foreign old-byte owner |
| Complete closure | Retained source plus Cargo-resolved features, target edges, templates, assets and helper inputs | Omitted/new/symlinked file, undeclared helper or feature |
| Same checked owners | Private checker-issued acceptance consumed with its original source, route, build and execution owners | Equal-copy, stale, foreign or deserialized result |
| Build/execution custody | Private captured snapshot, pinned toolchain/options and actual artifacts throughout build and execution | Live-checkout substitution, source drift, wrong artifact/toolchain |
| Rust/template API ownership | Concrete AST entry, helper edges and same-root issued facts in the actual target route | Recomputed JSON facts, wrong-root table, unknown syntax/dispatch |
| Full conformance | Actual target/evaluator executions over exhaustive instruction/operator cases and declared observable domains | Missing family, skipped run, timeout, wrong route, unexplained refusal |
| Authorized regions/complements | AST/API/conformance-issued replacement nodes plus original and replacement byte extents | Caller-supplied range authority, overlap, swallowed unrelated decision, changed comment |
| Production decision/report | Same joint result through `migration_ledger::run`, existing policy command and canonical verification | Approval flag, alternate success entry, partial success labelled complete |

Execution correspondence includes values, exceptional-value/domain behavior,
output order, ABI/storage effects and first source-ordered faults. Differential
agreement remains validation evidence, not a theorem about every input.
An instruction without a supported executable route is an explicit refusal,
not an omitted test. An audit-owned legacy branch remains unresolved and
source-bound; an unaudited branch supplies no acceptance.

The examined source roster has 37 `SolveOperation` variants and eight
`SolveBodyItem` variants. These counts are navigation, not a coverage bar.
Completeness follows the closed variants and their nested operator/domain
families; adding a case invalidates completeness until it is accounted for.
`SolveBodyItem::Legacy` includes an optional audit owner; `None` is not a
certified path. A fixed-model parity set alone cannot discharge this row.

### 3. Call-Level Migration Scope

| Audit row | Existing obligation | Candidate effect and remaining claim |
|---|---|---|
| D007 | Renderer derives call levels for scratch overlays | Replacing `assign_call_levels` with the same block's issued table is a candidate, pending joint evidence |
| D085 | Header constructs overlaid frames from the renderer hierarchy | Issued levels alone do not establish all placement, alias and budget obligations |
| D008 | Renderer selects reusable scratch pools and intervals | Unchanged by a call-level-only migration |

The consumer path includes the typed block view and the C/H template reads.
Removing a Rust helper does not alone certify its consumers. New test files
and changed test bodies belong to the complete inventory; a test pathname is
not an exemption. Source nodes come from checked AST/API/conformance products,
not an expanded permitted-range list in the immutable ledger. Unrelated bytes,
including comments and literal template bodies, retain the existing freeze.

### 4. Integration and Reporting Map

| Stage | Required evidence from the parent contract | Reported scope |
|---|---|---|
| Design and component checks | Captured route, API checker, conformance and complement contracts | No production authorization |
| Conformance families | Scalar/storage; control/call/state; tensor/index/linear solve; faults/native/ABI/operator completeness | Each unfinished family remains partial |
| Joint production integration | Same owners and complete products through the real policy entry | Only proven replacement nodes and obligations |
| Canonical verification | Build and conformance consume the captured snapshot without recursively invoking the policy entry | Actual source/options/artifacts and every refused or unmeasured part |
| Audit report | Unresolved rows, covered route and uncovered consumers | No print-only, complete migration or WCET inference |

Cranelift, external helpers and arbitrary template routes are outside the
initial frozen directory scope. A pass for one concrete route does not certify
them. The implemented freeze stays blocking while this DRAFT is designed,
tested or reviewed; merging this catalog is not its production integration.

## References

- [SPEC_0000](SPEC_0000_SPEC_GUIDELINES.md), DRAFT status and catalog ownership.
- [SPEC_0029 §12](SPEC_0029_CRATE_BOUNDARIES.md#12-runtime-backend-simulation-session-and-visualization-layering), phase and target ownership.
- [SPEC_0037 §2a](SPEC_0037_FORMALLY_VERIFIED_COMPILER.md#2a-source-bound-backend-retirement), governing proposed joint contract.
- [SPEC_0041 §7](SPEC_0041_CRATE_OWNERSHIP_CATALOG.md#7-codegen-target-and-command-ownership-catalog-spec_0029-12), implemented freeze.
