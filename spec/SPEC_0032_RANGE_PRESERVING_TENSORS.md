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
| Scalar refresh replay demands one logical block receipt from its canonical stage owner: explicit blocks are borrowed, singleton runs derive only the requested source-ordered row/target pair without tearing or alternate charts. Full source/output/paired-target checks remain required | Solve scalar execution boundary | Ranged owners need neither authored explicit blocks nor an eager scalar catalog to establish replay |
| Structured equation families stay authoritative | Flat/DAE IR | Prevents parallel scalar owners |
| Structured event-relation families stay authoritative | DAE event system | Preserve every per-point zero crossing without making scalar roots a second owner |
| Compacted component arrays leave no descriptor | Instance IR | Compaction is not an ownership change |
| Domains use `rumoca-core::StructuredIndexDomain` | Flat/DAE/Solve IR | One compact domain shape |
| Domain payloads are compact | IR serialization | Avoids O(N) metadata |
| Binder ids are stable and explicit | `StructuredIndexBinder` / phase maps | Names can shadow |
| Empty domains produce zero scalar rows | Scalar views | Valid zero-iteration ranges |
| Each body of a loop nest owns one family over the whole nest | Flatten | An unrolled outer binder scalarizes every grid row |

Structured equation and event-relation families stay authoritative, domains are compact and use `rumoca-core::StructuredIndexDomain`, and a compacted array is indistinguishable from its unrolled form per element. [SPEC_0043 catalog §17](SPEC_0043_CONSTRUCTION_CATALOG.md#17-range-preserving-ownership-and-domains-catalog) is normative by reference.

### 2. Scalar Views

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Scalar rows are generated views | `rumoca-eval-solve` / structural phases | Single structured owner |
| View ordering is deterministic | Domain enumeration | Backend agreement |
| Views carry provenance | Structured equation/tensor scalar views | Diagnostics and fallback |
| Derivative substitution retains the matched state, defining expression, and enclosing lowering context. A component index selects an output, not a new definition environment. A construction-issued empty binder domain proves that the definition's scalar-view domain point cannot affect expression reuse; otherwise its exact domain point remains part of the context. Original evaluation and derivative-cycle rejection remain authoritative. | Solve derivative scalar projection | MLS §3.7.4 and §8.3.1 preserve one tensor definition without conflating binder-dependent component equations |
| Product incidence may omit a Real factor only when the other selected scalar is proven invariant zero through literal structure (literals, arrays, unary signs, `transpose`, and selections by literal subscripts), non-tunable, fixed parameter bindings, and continuous algebraic declarations their own residual owner binds to such a literal ([SPEC_0043 §4](SPEC_0043_CONSTRUCTION_CATALOG.md)), or to a scalar expression that is zero wherever it has a value: a product with a proven-zero factor, a quotient with a proven-zero numerator, a sum or difference of proven-zero terms, or `smooth`/`noEvent` of one, whatever the other operands are. The owner row still evaluates that whole expression, calls and divisions included, and a non-finite value fails there, so the declaration is zero at every instant it has a value (MLS 3.7 §3.3 would even let a tool skip the other operand). The omitted factor is a coordinate read, a literal, or a sum, difference, product, array, transpose, or literal-subscript selection of these; division, calls, and every other operation with domain or validation effects are never omitted. Tunable parents, initialization unknowns, and unsupported binding operations cannot establish this proof. The original DAE tensor expression remains authoritative. Solve lowering omits exactly the product terms this same proof omits (one proof, `rumoca_eval_dae::ZeroCoefficients`, two consumers), so executed rows read only the coordinates the block ordering matched and a non-finite factor of an omitted term does not propagate. A target isolator whose coefficient is zero at construction (a zero scale or a literal-zero coefficient register) is not issued, and a causal step whose row reads its unknown only through such a coefficient is a construction error, because the incidence proof omits that term and matching cannot have chosen it; the run time carries no check for this case. | DAE scalar dependency projection, Solve scalar lowering, and Solve isolator certification | MLS §7.2.6 and §10.6.4 permit exact zero coefficients without freezing parameter defaults or erasing evaluation obligations |
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

Solve keeps compact Map and AffineStencil tensor nodes; scalar rows are derived views, never the authority. [SPEC_0043 catalog §18](SPEC_0043_CONSTRUCTION_CATALOG.md#18-solve-tensor-nodes-catalog) is normative by reference.

### 5. Ownership Boundaries

| Thing | Owner/Where | Brief Justification |
|---|---|---|
| `StructuredIndexDomain`, binder ids | `rumoca-core` | Cross-IR data shape |
| Domain semantic normalization/evaluation | Owning phase crate | Needs semantic context |
| Solve tensor scalar-view generation | `rumoca-eval-solve` | Shared fallback boundary |
| Instance family/connection scalar views | `rumoca-eval-ast` | AST IR stays data-only |
| Native tensor rendering | backend/codegen target | Target-local optimization |
| A memory-backed contiguous output range, including a Binary64 pure-call payload whose encoding is the identity, is transferred by a bounded emitted loop; its extent does not issue one native store instruction per cell. Other typed payloads retain their exact conversion and refusal rules | Native scalar-program output and call boundaries | Compact source ranges must not become functions whose code size grows with array width |
| Event-history execution may borrow the checked event-iteration owner's contiguous current/pre ranges. It validates the complete typed input before publication, preserves the issued clock policy and numeric equality, and creates no temporary per-cell history inventory | Shared event-history kernel and its runtime adapter | Array-sized data transfers do not require array-sized transient metadata |

Name resolution, parameter-bound evaluation, zero-size handling, and ordering
normalization are phase/evaluation behavior, not IR-crate behavior.

### 6. Structural Sparsity

Compact domains and affine access maps are also the authoritative source for
compact dependency patterns. Solve lowering derives affine sparsity directly
from these owners; it must not materialize scalar rows and then rediscover a
pattern. Pattern soundness, coloring, storage-policy separation, and complex
block expansion are specified by
[SPEC_0039](SPEC_0039_PROOF_CARRYING_SPARSITY.md).

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| A checked structural pattern retains one private immutable owner for its shape, relation, provenance and construction-issued coloring. Clone shares that owner; an independent checked derivation, projection or replay issues a fresh owner. Explicit colored construction derives through the existing exhaustive algorithm at most once per owner, including upstream pattern clones and concurrent construction. Coloring storage is excluded from structural equality and wire data. | `rumoca-ir-solve::StructuralPattern` | Exact owner sharing prevents repeated derivation without substituting equal geometry for source ownership. |
| Artifact construction determines coloring obligations before computation: global implicit, manifold, derivative and initialization-residual structures retain patterns only; algebraic, manifold and initialization projection blocks require a constructor-issued colored structure. Consumers borrow that required product and cannot trigger coloring analysis; an unplanned execution request receives the existing typed missing-artifact refusal. No caller flag or empty coloring substitutes for the required product. | Solve structural artifact construction and projection adapters | Required block coloring remains exact; unused global patterns never materialize coloring coordinates. |
| Register-range version issuance has one canonical interval inventory, shared by structural dependency families and prepared execution. An overwrite splits only intersecting range pieces; captured operand views retain their exact earlier owners and offsets. Structural tensor loads, broadcasts, and elementwise unions do not allocate one dependency cell per tensor lane or absent register. Scalar and range queries derive from this inventory through the existing exhaustive dependency rules. Execution operand receipts reuse the canonical exhaustive source-register rules and preserve native range operations. | `rumoca-ir-solve` register-range inventory, consumed by `structural_pattern`, `refresh::dependency`, and prepared evaluation | Metadata cost follows operation and range structure; overwrites preserve earlier reads, and a missing source is still refused even if no requested result depends on it. |
| A projection block's selected-output degree queries share one ordered walk for each canonical node/program and its complete exact target set. Queries read degrees at the original stores, retain conservative refusal of each required prefix, and construct no dense inventory of unselected outputs or register snapshots. Different target sets remain independent proofs. | `rumoca-ir-solve::affinity` | A tensor owner's residual rows must not each replay its whole degree-analysis prefix; this construction-time query sharing authorizes no runtime invocation reuse. |


### 7. Compact Visible Projections

SolveModel owns a compact `VisibleProjection` in place of scalar visible inventories. [SPEC_0043 catalog §19](SPEC_0043_CONSTRUCTION_CATALOG.md#19-compact-visible-projections-catalog) is normative by reference.

