# SPEC_0032: Range-Preserving Tensor IR

## Status
ACCEPTED

## Summary

Structured array/range equations stay compact through Flat, DAE, and Solve;
scalar rows are derived views, not recovered structure.

## Specification

### 1. Ownership And Domains

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Structured equation families stay authoritative | Flat/DAE IR | Prevents parallel scalar owners |
| Domains use `rumoca-core::StructuredIndexDomain` | Flat/DAE/Solve IR | One compact domain shape |
| Domain payloads are compact | IR serialization | Avoids O(N) metadata |
| Binder ids are stable and explicit | `StructuredIndexBinder` / phase maps | Names can shadow |
| Empty domains produce zero scalar rows | Scalar views | Valid zero-iteration ranges |

Structured families include source `for` equations, whole-array equations,
slices, comprehensions, boundary ranges, and connection-generated array
equations that are naturally ranged. Domain payloads must not serialize one
entry per scalar iteration except inside an explicitly materialized scalar view.
Stage-specific structured-equation ids remain stage-owned and must be mapped
explicitly when identity crosses phase boundaries.

### 2. Scalar Views

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Scalar rows are generated views | `rumoca-eval-solve` / structural phases | Single structured owner |
| View ordering is deterministic | Domain enumeration | Backend agreement |
| Views carry provenance | Scalar-view metadata | Diagnostics and fallback |
| No scalar-row reassembly | Solve lowering | Prevents fragile recovery |

Domains enumerate in binder declaration order, lexicographic with the innermost
binder varying fastest, respecting explicit step direction. For each index
tuple, body equations emit in source/body order. Scalar views must preserve
parent structured/tensor id, index tuple, scalar row id, and instantiated
lhs/rhs or output expression.

### 3. DAE Canonical Form

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Structured DAE contains no source `der(...)` | DAE lowering | MLS Appendix B form |
| Derivative families map to canonical slots | DAE structured family | Explicit state identity |
| No parallel scalarized owner | DAE IR | Avoids drift |

A source family such as `der(u[i, j]) = w[i, j]` is represented as residuals
over canonical derivative slots/state metadata. The structured node owns the
compact index domain and maps each tuple to the corresponding derivative/output
slot.

### 4. Solve Tensor Nodes

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| `ComputeNode::Map` is elementwise | Solve IR | Pointwise tensor semantics |
| `ComputeNode::AffineStencil` is neighborhood access | Solve IR | Affine offset semantics |
| Solve grouping is semantic | `rumoca-phase-solve` | Backends do not redefine IR |
| Scalar fallback uses shared scalarization | `rumoca-eval-solve` | One ordering implementation |

`Map` represents canonical DAE residual families that are elementwise over a
compact domain, including `der(u) = w` after DAE canonicalization. `AffineStencil`
comes from structured DAE domains plus affine operand proofs; Solve lowering
must not rediscover stencils by scanning anonymous scalar rows. Backends may
fuse or split generated kernels as target-local codegen, but the reported
kernel inventory must match the generated work.

### 5. Ownership Boundaries

| Thing | Owner/Where | Brief Justification |
|---|---|---|
| `StructuredIndexDomain`, binder ids | `rumoca-core` | Cross-IR data shape |
| Domain semantic normalization/evaluation | Owning phase crate | Needs semantic context |
| Solve tensor scalar-view generation | `rumoca-eval-solve` | Shared fallback boundary |
| Native tensor rendering | backend/codegen target | Target-local optimization |

Name resolution, parameter-bound evaluation, zero-size handling, and ordering
normalization are phase/evaluation behavior, not IR-crate behavior.
