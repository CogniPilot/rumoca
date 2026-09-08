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
| Structured event-relation families stay authoritative | DAE event system | Preserve every per-point zero crossing without making scalar roots a second owner |
| Compacted component arrays leave no descriptor | Instance IR | Compaction is not an ownership change |
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

An event-generating relation inside a structured family is itself a compact
family over the same checked domain. DAE owns continuously monitored state
relations as compact root families, and Solve derives one root program for each
domain point in canonical order. A structured event kind without its own
compact owner MUST be rejected at the exact relation; lowering MUST NOT retain
the conditional value while silently omitting its event boundary.

Instantiation may compact an array of structured components: it resolves one
template domain point and derives the others by reindexing instance paths
(`rumoca-phase-instantiate/src/array_expansion/`). This is an instantiation
optimization, not an ownership change — Instance IR's authoritative
representation of the array is still the per-element `components`/`classes`
entries, and every later phase reads those.

**REQUIRED:** a compacted array must produce byte-identical per-element
entries — including `InstanceId` allocation order, on which flat variable
identity depends (SPEC_0001) — to element-by-element expansion. The declared
extents of every expanded array, compacted or not, are recorded once in
`InstanceOverlay::array_parent_dims`; that is the only array-level record
Instance IR keeps, and the per-element entries remain the array's
representation.

**PROHIBITED:** recording *in Instance IR* that an array was compacted.
Instantiation kept a `component_families` descriptor for a possible future
reader and acquired none; while it existed a diagnostic read it and reported
whether the compiler had compacted the array rather than what the model
declared. A compaction record in the IR is by construction a second owner for
state the per-element entries already own, and it makes the two paths
distinguishable to consumers, which this section forbids. A future phase that
wants the compact domain re-derives it from the declared extents.

This prohibition is on IR content, not on observability as such: a counter or
other out-of-band signal that no consumer can read is still permitted, and one
is needed. Compaction currently has no liveness witness — the differential
tests prove only that the compact and scalar overlays agree, which stays
trivially true if the homogeneity gate silently stops compacting anything, so
the optimization could regress to element-by-element expansion undetected. The
follow-up is a non-IR compaction counter that the instantiate tests can assert
on.

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
| Structured B.1c views retain assignment identity | DAE/Solve scalar views | Preserve target and update policy |
| Structured root rows are generated views | Event evaluator/backend boundary | Root location sees every domain point in canonical order |

Domains enumerate in binder declaration order, lexicographic with the innermost
binder varying fastest, respecting explicit step direction. For each index
tuple, body equations emit in source/body order. Scalar views must preserve
parent structured/tensor id, index tuple, scalar row id, and instantiated
lhs/rhs or output expression.

This section governs views derived from a structured *equation* or tensor owner.
It does not apply to the per-element instance entries of a compacted component
array (§1): those are ordinary instances carrying ordinary instance provenance
(qualified name, span, source scopes) and no parent-family metadata, because
compaction leaves no owner for them to point at. It does apply to the scalar
view of a compact `InstanceConnectionFamily` (`rumoca-eval-ast::connection`),
which is a derived view of a structured owner.

One compact `InstanceConnectionFamily` remains one Flat connection-plan owner;
its scalar projections are views identified by domain ordinal. A zero domain is
consumed vacuously. A partially active domain must retain a compact checked
subdomain or fail before plan construction; lowering may not silently discard
inactive coordinates or enumerate active ones into independent semantic owners.

A scalar view of a structured B.1c owner additionally retains its parent B.1c
owner id, domain index tuple, body ordinal, derived target scalar, value
expression, and exact provenance. Ordering is domain lexicographic order and
then source body order. These are derived view values; no per-scalar target or
row list is stored in DAE or canonical Solve IR.

A scalar root view is derived by substituting its canonical domain index tuple
into the compact relation expression and carries the family's zero-domain
policy and provenance into its program row. The compact DAE event family, not
the expanded Solve root list, is authoritative.

### 3. DAE Canonical Form

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Structured DAE contains no source `der(...)` | DAE lowering | MLS Appendix B form |
| Derivative families map to canonical slots | DAE structured family | Explicit state identity |
| No parallel scalarized owner | DAE IR | Avoids drift |
| Structured B.1c target coverage is constructor-derived | DAE discrete system | No holes, overlaps, or caller counts |
| Structured event relations own their domain once | DAE event system | No dropped or parallel per-point roots |

A source family such as `der(u[i, j]) = w[i, j]` is represented as residuals
over canonical derivative slots/state metadata. The structured node owns the
compact index domain and maps each tuple to the corresponding derivative/output
slot.

A structured discrete-valued family remains one B.1c owner. It carries the
existing `StructuredIndexDomain` and `ComprehensionScalarView`; its checked
target projection and value bodies determine the scalar view. Construction
derives the scalar count and proves that the target projection is injective and
exhaustive for every owned target. A partial, overlapping, shape-incompatible,
or scalar-type-incompatible projection is rejected at this owner.

### 4. Solve Tensor Nodes

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| `ComputeNode::Map` is elementwise | Solve IR | Pointwise tensor semantics |
| `ComputeNode::AffineStencil` is neighborhood access | Solve IR | Affine offset semantics |
| Runtime tensor access/update stays aggregate-native | Solve IR typed programs | Compact affine indexing instead of extent-sized scalar selection |
| Solve grouping is semantic | `rumoca-phase-solve` | Backends do not redefine IR |
| Scalar fallback uses shared scalarization | `rumoca-eval-solve` | One ordering implementation |
| Structured B.1c uses compact map and target map | Solve IR | Preserve the authoritative discrete family |

`Map` represents canonical DAE residual families that are elementwise over a
compact domain, including `der(u) = w` after DAE canonicalization. `AffineStencil`
comes from structured DAE domains plus affine operand proofs; Solve lowering
must not rediscover stencils by scanning anonymous scalar rows. Backends may
fuse or split generated kernels as target-local codegen, but the reported
kernel inventory must match the generated work.

Structured B.1c lowering uses `ComputeNode::Map` (or a stronger proven tensor
node) together with a compact affine target map. Discrete row role, pre mode,
observation policy, and clock owner derive from the structured owner. A runtime
that needs scalar programs requests the shared scalar view from
`rumoca-eval-solve`; `rumoca-phase-solve` does not scalarize the owner.

A runtime tensor index, slice, or update is a first-class shape-checked
aggregate operation. Its dimensions, index operands, one-based Modelica index
conversion, row-major affine address, result shape, and out-of-range behavior
are constructed once. Phase lowering and canonical Solve construction do not
enumerate coordinates into scalar registers, repeated operations, or select
chains. A scalar consumer receives a projection of this aggregate owner; it
does not become the owner. Evaluators and native backends execute the compact
operation directly. Text backends may materialize target-language coordinate
operations only while rendering the final checked template view.

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

### 7. Compile-Time Materialization Budgets

Compact owners are preferred; when a compiler path must materialize a finite
semantic view, its budget is named here rather than introduced as an untracked
local policy. The unit is part of the contract: callers MUST NOT compare a
different quantity to the same limit, and a refusal MUST identify the named
bound and preserve the unmodified input IR.

| Named code constant | Value | Unit and transaction scope | Typed refusal |
|---|---:|---|---|
| `DEFAULT_EVAL_BUDGET` | 100,000 | Retained `Value` nodes and default interpreter/comprehension work units in one Flat constant-evaluation request | `EvalError::UnsupportedExpression`; an optional fold defers, while a phase that requires the value maps the typed error at its own boundary (for example `EF034`) |
| `DEFAULT_MATERIALIZED_RANK_BUDGET` | 256 | Axes in one Flat constant value or materializing builtin request | `EvalError::UnsupportedExpression`, with the same optional-versus-required ownership rule as `DEFAULT_EVAL_BUDGET` |
| `MAX_EAGER_RANGE_ELEMENTS` | 100,000 | Scalar elements retained by one eager Flat structural range/connection scalar-view transaction | spanful `EF037` before Flat or connection-set mutation |
| `MAX_MATERIALIZED_CONNECTION_ITERATIONS` | 1,000,000 | Total fallback loop iterations reserved across one Instance connection-extraction transaction | spanful `EI004`; the diagnostic names both this limit and the remaining transaction budget |

These are compile-time semantic materialization budgets. Target-language text
emission and runtime numerical iteration limits are separate policies and MUST
NOT reuse these constants or units. Adding another semantic materialization
limit requires adding a row here in the same change.
