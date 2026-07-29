# SPEC_0039: Proof-Carrying Structural Sparsity

## Status
DRAFT

## Summary

Rumoca derives structural sparsity from checked programs and compact domains.
A structural pattern is a sound may-depend relation: entries outside the
pattern are proven zero for every valid execution. Numerical zeros and storage
formats are separate runtime or target choices.

## Specification

### Meaning and Ownership

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| A pattern is a may-depend relation | structural/Solve artifacts | False positives are safe |
| A false negative is a construction defect | all producers | Missing derivatives can corrupt solves |
| Numerical zeros do not change structural patterns | runtime | Values are point-dependent |
| Dense, CSR, CSC, interleaved, and planar are storage policies | runtime/codegen | Layout is not mathematics |
| DAE retains expressions and compact domains, not Jacobian patterns | DAE IR | Sparsity is derived solver metadata |
| Solve artifacts own Jacobian and mass-matrix patterns | phase-solve | Optional solver-facing products |
| Tensor operand patterns stay on checked tensor operations | Solve IR | Kernel semantics need local structure |

`Full` is the explicit conservative pattern. It is never an implicit default.
An unavailable or opaque dependency becomes `Full`, or artifact construction
fails when the derivative operation itself is unavailable. It never becomes
`Empty`.

### Canonical Pattern

A finalized structural pattern has private fields, a checked rectangular
shape, generated provenance, and one canonical representation:

| Representation | Contract |
|---|---|
| `Empty` | No output row depends on an input column |
| `Full` | Every entry may be nonzero |
| `Diagonal` | Only `(i, i)` for the rectangular diagonal may be nonzero |
| `Banded` | Entries are bounded by checked lower/upper bandwidths |
| `Csr` | Row offsets and strictly increasing bounded columns are canonical |
| `Affine` | A compact index domain plus checked row/column affine maps |

CSR uses `u32` indices. Its row-offset count is `rows + 1`, begins at zero,
ends at the column-index count, and is monotone. Columns are in bounds and
strictly increasing within each row. Duplicate and unsorted entries are
rejected. Affine patterns remain O(domain rank + access count); materializing
one entry per domain point in canonical IR is prohibited.

Pattern fields do not derive `Default` or `Deserialize`. Private current-wire
records decode through the same pattern operations. Derived patterns are absent
from canonical Solve wire data and are reconstructed deterministically from
the decoded programs and domains.

### Dependency Derivation

Dependency state is explicitly either `Known(set)` or `Unknown`; absence from
a register table means `Unknown`.

| Operation | Derived dependency |
|---|---|
| Literal/time/ordinary parameter load | Empty with respect to solver seeds |
| Seed or selected unknown coordinate | Its checked singleton column |
| Move/unary | Operand dependencies |
| Binary/comparison | Union of both operands |
| Select/conditional | Union of condition and every value branch |
| Static array index | Exact selected dependency |
| Bounded dynamic index | Union of the complete selectable run |
| Function call | Checked argument substitution into the function summary |
| Recursive function group | Least monotone fixed point over finite summaries |
| Map/stencil | Compact affine propagation over its owner domain |
| Matrix multiply | Algebraic propagation from both operand patterns |
| Linear solve | All matrix/RHS dependencies unless a proved diagonal or block rule applies |
| Opaque runtime operation | Unknown, becoming Full at the artifact boundary |

Algebraic cancellation does not remove a dependency unless a separately proved
rewrite establishes the identity under the selected numeric semantics.

### Differentiation, Coloring, and Execution

`SolveArtifacts` may contain structural patterns for continuous, implicit,
manifold, initialization, and requested sensitivity Jacobians. Patterns use
typed row and layout identities during construction; finalized ordinals are
layout only.

Column coloring is derived from a finalized pattern. Construction proves:

- every column occurs in exactly one color;
- columns in one color have disjoint affected-row sets;
- ordering is deterministic.

Compressed AD is selected when the color count is lower than the column count.
Otherwise ordinary AD is used. A backend may choose dense execution for a
sparse pattern, but may not discard or weaken the certificate.

The versioned execution policy compares deterministic operation, storage, and
symbolic-fill estimates. A tie selects dense execution. The first native
consumer is the Diffsol/Faer Model Exchange host; generated and additional
runtime targets consume the same artifact rather than re-deriving sparsity.

### Complex Values

Complex sparsity is first expressed at semantic element-block granularity.
Checked derivative rules expand each complex dependency to a real-lane block.
The conservative expansion is a full 2-by-2 block. Internal block zeros are
removed only by a proved derivative rule. This section is governed jointly
with [SPEC_0035](SPEC_0035_COMPLEX_NUMERIC_TYPES.md).

### Correct Construction and Evidence

Raw pattern claims, unchecked constructors, mutable pattern fields, and
post-construction root validation are prohibited. Solve construction derives
patterns rather than accepting caller annotations.

Certification receipts retain the semantic owner, source/generated
provenance, derivation rule, input/output identities, and resulting pattern
identity. Runtime IR need not retain a full proof tree. The intended formal
theorem is that dependency derivation never omits a coordinate that can affect
an output.

## Verification

- Unit tests cover every Solve operation and dependency rule.
- Negative tests cover malformed CSR/affine data, overflow, duplicates,
  missing provenance, and cross-owner identities.
- Property tests compare optimized derivation with an independent conservative
  reference interpreter.
- Sparse/color-compressed and dense executions produce equivalent results.
- Scaling tests prove compact affine metadata is independent of domain
  cardinality.
- Representative structured MSL models exercise the Faer consumer and record
  deterministic policy decisions.

## Implementation Plan

| Phase | Delivers | Promotion evidence |
|---|---|---|
| 1 | Checked canonical patterns and sound dependency lattice | Operation and negative tests |
| 2 | Constructor-derived Solve patterns and coloring | Artifact and wire round trips |
| 3 | Compressed AD and tensor-kernel consumption | Dense/sparse equivalence |
| 4 | Diffsol/Faer native consumer and deterministic policy | MSL simulations and benchmarks |

**Promotion:** ACCEPTED after phases 1-4 are implemented and all production
pattern entry points construct rather than deserialize or mutate raw fields.

## References

- [SPEC_0007](SPEC_0007_IR_PIPELINE.md) — phase and artifact ownership
- [SPEC_0032](SPEC_0032_RANGE_PRESERVING_TENSORS.md) — compact domains
- [SPEC_0035](SPEC_0035_COMPLEX_NUMERIC_TYPES.md) — complex element blocks
- [SPEC_0036](SPEC_0036_VALID_BY_CONSTRUCTION_IR.md) — construction rules
