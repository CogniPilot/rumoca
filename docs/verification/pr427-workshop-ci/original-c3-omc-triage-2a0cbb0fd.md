# Original C3 comprehension classification

Evidence source: `2a0cbb0fd510afefa7685b9070fd7c114efe8100`.
The root agent executed the guarded attempt
`$HOME/scratch/rumoca/slam-integration-cycle-20261010/pr427-native-copies-after-and-original-comprehension-omc-20261011`.
This record adds documentation only; it does not modify a model or execute a gate.
The adjacent JSON binds exact commands, sources, logs, guardians, and actual CSVs
by SHA-256. Machine-local artifacts remain under `$HOME/scratch`.

The original #424 fixture declares `m[3, 2]` but binds it to
`{i + 10*j for i in 1:3, j in 1:2}`. [MLS 3.7 §10.4.1.2](https://specification.modelica.org/maint/3.7/arrays.html#constructor-with-several-iterators)
defines multiple iterators through nested constructors: the first iterator is
inside the next iterator. The original binding therefore has shape `[2, 3]`,
which disagrees with its declared `[3, 2]`. The earlier compiler test and census
assumption that this original must be a positive `[3, 2]` model was incorrect.

| Actual OMC source | Process exit | Admission and trace |
| --- | --- | --- |
| Original fixture, byte-identical | 0 | Rejected: expected `[3, 2]`, got `[2, 3]`; failed model build; no CSV |
| Explicit nesting `{{i + 10*j for j in 1:2} for i in 1:3}`, original declaration/read | 0 | Admitted; actual CSV retained |
| Original constructor, declared `m[2, 3]`, read `m[2, 3]` | 0 | Admitted; actual CSV retained |

OMC's original process exit zero is **not** model admission. Its diagnostic and
empty simulation result explicitly reject the model. The two controls are
different sources and do not establish that the original works. Preserve the
original fixture verbatim as a typed-negative regression; correct the mistaken
positive expectation at the owner instead of changing the fixture or relaxing
dimension checks (SPEC_0033 §§2–4; SPEC_0022 ARR-027).

Both admitted controls have coefficient 23 and `y(0)=1`, so their independent
analytical solution is `exp(-23*t)`. Each retained CSV has seven actual rows
covering exact start time zero through 0.1. The last row at time zero has `y=1`;
both final values are `0.1002588441638353`. Read-only comparison of every actual
sample gives maximum absolute analytical difference `5.548803083677001e-10`.
No missing observable is defaulted and no interpolation is used. These are
short OMC controls, not a full original C3 simulation or full CI qualification.

The same attempt qualified the native copy repair on its original alias probes:

| Gate | Actual Rust results | Guardian/wrapper outcome |
| --- | --- | --- |
| `prepared_tensor_additional_alias_` | 3 passed, 0 failed, 0 ignored | Success |
| `prepared_tensor_snapshot` | 15 passed, 0 failed, 0 ignored | Refused: configured expected count was 12 |
| `native_tensor_staging` | Unrun | Previous count refusal stopped the attempt |
| `scalar_ops::snapshot_tests::` | Unrun | Previous count refusal stopped the attempt |
| Resolve `comprehension_range_scope_` | Unrun | No compiler-scope result from this attempt |

The broad snapshot substring also matches the enclosing
`prepared_tensor_snapshots::` module and therefore includes the three additional
alias probes. Its correct cohort count is 15. The strict wrapper refusal must
remain visible; it is not a semantic failure among those 15 executed tests.
The earlier count-12 command in the native copy source note needs correction
before preparing a new recipe. Original frozen recipes and logs remain intact.

Source review of repair `7fc4d03fde6356541ed50e5ffd71b237fc7a1bd7`
found no additional concrete behavior bug. Fast and cached/fresh lazy copy
execution share `native_tensors`; compact overlap admission shares the existing
private `tensor_snapshot::Snapshot` owner. Runtime index and slice-coordinate
reads are included before destination writes. Checked Solve construction proves
nonzero lanes, equal projection rank, and operand ranges before private native
execution. Concatenation traversal and update projection reuse the existing
helpers. The new private helpers stay within SPEC_0021's function/argument
limits, retain compact operations under SPEC_0032 §4, and add no parallel IR or
public construction escape under SPEC_0029 §§3b/5. This source review is not a
replacement for the unrun cohort, Clippy, or broader CI gates.
