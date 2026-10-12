# Checked and prepared native tensor snapshots

The checked scalar-program issuer admits initialized aggregate operand aliases.
An aggregate reads its operands before writing its result. The native prepared
path previously overwrote destinations while later components still read those
operands. This is an evaluator divergence, not a reason to reject legal aliases
or to scalarize canonical tensor owners.

## Actual before evidence

Source `340b5cb99476378f323c17e1f62fe0ebdf24eee1`, attempt
`pr427-combined-native-before-20261011`, executed four unchanged public
constructor/prepared controls. The original checked evaluator and disjoint
positive cases were reached before each alias failure. Cross passed because its
native kernel already copies both operands to fixed stack arrays. Three failed:

| Operation | Prepared result | Independent expected result |
| --- | --- | --- |
| In-place transpose | `[1,4,4,5,4,6]` | `[1,4,2,5,3,6]` |
| Matmul overwriting its left operand | `[19,130,43,290]` | `[19,22,43,50]` |
| Binary add overwriting its broadcast operand | `[3,6,7]` | `[3,4,5]` |

The attempt terminated with exit 101 after 41.9708 seconds. Its log SHA-256 is
`53168fa8c024531603c8a6584f924a1b905e43de3f605997e275d9462e654b66`;
guardian SHA-256 is
`ba22af8d112163c141cba7cfd6727c72709bbf5888978d60fda9cde056d8a5b0`.
Raw evidence remains under `$HOME/scratch/rumoca/slam-integration-cycle-20261010/`
in that attempt directory.

Test-only source `2c188dd6f` adds the larger ordered-alias counterexample:
64 constants, in-place transpose, Select, and one output. The original checked
value is 4. Attempt `pr427-native-long-select-before-20261011` compiled
successfully and entered its single test, then terminated at the unchanged RSS
guard with exit 137 after 45.317 seconds and peak 4,353,392 KiB. The last-writer
dependency map makes an aggregate depend on its own overwritten input registers;
the demand scheduler cannot complete that cycle. This is a resource failure,
not an assertion-result receipt.

The larger probe log SHA-256 is
`2924718bc406c20e1e71b86c948a5623ddfeae042f6e7e4b97f1799811626a94`;
guardian SHA-256 is
`ad34eafe84fce3f511299ca9e4b444d571d2ef03867b2341fe25486482189599`.

## Owner repair and cost

`rumoca-eval-solve::tensor_snapshot` privately issues a compact overlap witness
before writes. It intersects destination ranges with strided source element
blocks, including lane widths and broadcast strides, without enumerating a
register list. Checked products issue its result width, avoiding saturated
output-count metadata. Both the existing final scalar projection and native execution
consume this geometry. No new public API or stage representation is introduced.

`native_tensors` owns the matrix, binary and transpose arithmetic and layout
rules. Checked and prepared execution read through that same numeric owner.
Prepared execution stages every overlapping result in `RowEvalScratch` before
committing any destination write. Fallible reservation requests the issued
result width; later evaluations reuse the vector's capacity. Disjoint operations
allocate no result buffer. Scratch storage follows the largest overlapping
result seen by this row scratch, rather than the sum of operations. The existing
checked register-flow certificate supplies the operand and destination bounds.
Native tensor op counts and compact shapes are unchanged. The scalar projection
retains its separate, cumulative final-template expansion budget.

The overlap query is constant work per compact opcode at the shared kernel
entry. Persisting this fact across every prepared owner is deferred until
measurement establishes that this query is a bottleneck; partial cached plans
would miss other existing fast consumers. Kernel dispatch and arithmetic are
shared across fast evaluation and both fresh and cached lazy traces.

Lazy demand scheduling now admits only unique register writers. An ordered
program with legal aliases keeps its checked owner and uses sequential native
execution. The unchanged larger before probe exercises this fallback. A separate
positive control retains a unique-writer lazy row and changes its runtime
selection across repeated evaluations, covering cache guard invalidation.

Governing contracts: ACCEPTED SPEC_0007 stage preservation, SPEC_0029 shared
owner discipline, SPEC_0032 compact native tensors and final scalar views,
SPEC_0033 actual first-divergence evidence, and SPEC_0021 maintainability.
SPEC_0036 remains DRAFT; this repair does not change its status.

## Qualification still required

The four original controls remain unchanged. Supplemental public controls cover
dual-lane matrix/broadcast operands and transpose trailing axes; a checked
constructor-backed control verifies scratch reuse and that strided gaps do not
allocate staging storage. All tests are normal, unignored eval-solve lib tests,
selected by the existing workspace CI.

This candidate has only passed rustfmt and `git diff --check`. Required focused
after gates select `prepared_tensor_snapshot` (9 tests),
`native_tensor_staging` (1 test), and `scalar_ops::snapshot_tests::` (5 tests).
No native after success, runtime performance improvement, or SLAM qualification
is claimed yet.
