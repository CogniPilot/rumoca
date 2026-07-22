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
| Unmaterialized interior rows are non-semantic placeholders | Flat/DAE structural metadata | Corner proof remains authoritative |

Domains enumerate in binder declaration order, lexicographic with the innermost
binder varying fastest, respecting explicit step direction. For each index
tuple, body equations emit in source/body order. Scalar views must preserve
parent structured/tensor id, index tuple, scalar row id, and instantiated
lhs/rhs or output expression.
Function projection derives slice shape from selector kind: `:` preserves the
axis, a confirmed scalar selector removes it without evaluating its value, and
compatible elementwise binary array operands retain that shape. Function-
projection shape inference declines unknown or array-valued selectors and
ranges with unknown compile-time length.

For a regular family whose interiors are not materialized, only the base and
per-binder neighbor rows carry the reconstruction proof. Structural rewrites of
an interior placeholder do not invalidate that proof; rewrites of a corner row
must discard the family metadata unless a new proof is produced.

An extent-1 binder has no distinct neighbor row: the base row is sufficient and
that binder contributes zero stride to every affine access and output map. Corner
selection skips such binders while retaining base/neighbor proof for all other
dimensions. An all-singleton non-empty domain therefore uses only its base row;
an empty domain produces zero rows and no corner proof.

### 3. DAE Canonical Form

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Structured DAE contains no source `der(...)` | DAE lowering | MLS Appendix B form |
| Derivative families map to canonical slots | DAE structured family | Explicit state identity |
| No parallel scalarized owner | DAE IR | Avoids drift |
| Orphan pruning counts exact scalar references on both equation sides | Structural phases | An explicit scalar lhs is a live owner use; a shaped slice/base lhs owns only the exact scalar projection proven by DAE dimensions and `scalar_count`; an aggregate base alias alone does not keep unrelated scalar leaves |

DAE lowers colon-slice multiplication to a scalar dot product only when both
operands are proven rank-one vectors of equal width. Proven scalar operands,
including scalar compound expressions, retain elementwise vector scaling. A
conditional is proven scalar only when every condition, branch value, and the
else value are proven scalar; unresolved function/builtin calls, unknown widths,
and higher-rank shapes remain unprojected rather than acquiring broadcast
semantics.

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
| Unprojectable direct single-array-output calls stay whole | Solve projection / array runtime boundary | Never duplicate one array call as scalar lanes |

`Map` represents canonical DAE residual families that are elementwise over a
compact domain, including `der(u) = w` after DAE canonicalization. `AffineStencil`
comes from structured DAE domains plus affine operand proofs; Solve lowering
must not rediscover stencils by scanning anonymous scalar rows. Backends may
fuse or split generated kernels as target-local codegen, but the reported
kernel inventory must match the generated work.

For direct structured initialization, the same domain can pair a residual `Map`
with a compact target `TensorOutputMap`. The target map is the sole scalar-view
mapping for that family; creating parallel `row_targets`, `StructuredProgram`,
or `Vec<Vec<LinearOp>>` ownership is forbidden on the compact path. The
`ComputeBlock` remains the sole owner of the Map; initialization metadata refers
to it by node index. `rumoca-eval-solve` executes the base program and affine
strides natively over the domain, without per-cell `LinearOp` construction.
Direct and fixed-start target ranges form an exact, non-overlapping affine
partition. Fixed-start array coverage is derived from the resolved contiguous
layout base and shape without scalar row-target materialization. Descending
source binders are normalized to an ascending execution domain by selecting the
corresponding source base and corners; target maps therefore remain canonical
positive-stride maps without changing source-index semantics.
Corner-derived load, constant, and target strides are admissible only after
Solve lowering proves the reconstructed program against every materialized
family cell. A family whose interiors are unavailable, whose values are not
affine, or whose initializer contains random/impure operations fails closed at
the first source row that breaks the proof; executing a self-consistent but
unproven affine reconstruction is forbidden.

### 5. Ownership Boundaries

| Thing | Owner/Where | Brief Justification |
|---|---|---|
| `StructuredIndexDomain`, binder ids | `rumoca-core` | Cross-IR data shape |
| Domain semantic normalization/evaluation | Owning phase crate | Needs semantic context |
| Solve tensor scalar-view generation | `rumoca-eval-solve` | Shared fallback boundary |
| Native tensor rendering | backend/codegen target | Target-local optimization |

Name resolution, parameter-bound evaluation, zero-size handling, and ordering
normalization are phase/evaluation behavior, not IR-crate behavior.
