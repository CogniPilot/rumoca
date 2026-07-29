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
| Compacted component arrays record a family descriptor | Instance IR | Preserves the compact domain |
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

Instantiation may compact an array of structured components: it resolves one
template domain point and derives the others by reindexing instance paths
(`rumoca-phase-instantiate/src/array_expansion/`). This is an instantiation
optimization, not an ownership change — Instance IR's authoritative
representation of the array is still the per-element `components`/`classes`
entries, and every later phase reads those.

`InstanceOverlay::component_families` records the descriptor
(`InstanceComponentFamily`: domain, root path, subscript depth, template
instance ids) for each array that was compacted. Today it has no readers; it is
recorded so a future phase can consume the compact domain without re-deriving
it. **REQUIRED:** a compacted array must produce byte-identical per-element
entries — including `InstanceId` allocation order, on which flat variable
identity depends (SPEC_0001) — to element-by-element expansion.
**PROHIBITED:** treating the descriptor as the array's owner while per-element
entries remain the consumed representation.

Compaction is refused whenever any per-element rewrite in
`prepare_element_declaration` would fire: a non-`each` `start`, an array-level
binding, or any non-`each` modification that indexes per element — which
includes every nested class modification, even an index-independent one. The
gate is one-sided by design: refusing costs only instantiation time, while
compacting a genuinely per-element array would produce a wrong model.

### 2. Scalar Views

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Scalar rows are generated views | `rumoca-eval-solve` / structural phases | Single structured owner |
| View ordering is deterministic | Domain enumeration | Backend agreement |
| Views carry provenance | Structured equation/tensor scalar views | Diagnostics and fallback |
| No scalar-row reassembly | Solve lowering | Prevents fragile recovery |

Domains enumerate in binder declaration order, lexicographic with the innermost
binder varying fastest, respecting explicit step direction. For each index
tuple, body equations emit in source/body order. Scalar views must preserve
parent structured/tensor id, index tuple, scalar row id, and instantiated
lhs/rhs or output expression.

This section governs views derived from a structured *equation* or tensor owner.
It does not apply to the per-element instance entries of a compacted component
array (§1): those are ordinary instances carrying ordinary instance provenance
(qualified name, span, source scopes) and no parent-family metadata, because the
family descriptor is not their owner. It does apply to the scalar view of a
compact `InstanceConnectionFamily` (`rumoca-eval-ast::connection`), which is a
derived view of a structured owner.

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
| Instance family/connection scalar views | `rumoca-eval-ast` | AST IR stays data-only |
| Native tensor rendering | backend/codegen target | Target-local optimization |

Name resolution, parameter-bound evaluation, zero-size handling, and ordering
normalization are phase/evaluation behavior, not IR-crate behavior.

### 6. Structural Sparsity

Compact domains and affine access maps are also the authoritative source for
compact dependency patterns. Solve lowering derives affine sparsity directly
from these owners; it must not materialize scalar rows and then rediscover a
pattern. Pattern soundness, coloring, storage-policy separation, and complex
block expansion are specified by
[SPEC_0039](SPEC_0039_PROOF_CARRYING_SPARSITY.md).
