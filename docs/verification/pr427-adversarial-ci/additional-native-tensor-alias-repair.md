# Additional aggregate snapshot repair

The root agent executed the original three probes at `df495d74b` in
`$HOME/scratch/rumoca/slam-integration-cycle-20261010/pr427-native-staging-and-additional-alias-before-20261011`.
All checked, disjoint prepared, and legal unique-writer lazy controls reached;
all three alias cases failed in ordinary prepared execution:

| Operation | Expected | Actual before |
| --- | --- | --- |
| Concatenate | `[3,4,1,2]` | `[3,4,3,4]` |
| Update | `[1,1,2]` | `[1,1,1]` |
| Packed fill | `[1,2,1,2]` | `[1,2,2,2]` |

The before gate selected three tests, exited 101 after 4.119 seconds, and recorded
peak RSS 538884 KiB. Its log SHA-256 is
`63abbc215f5b6e2bc7d991e7f5a7efbde1b88d463898c5f208a79823f6cc9a2a`;
guardian SHA-256 is
`ebda79e41a0b2d2d6160a34b9ff4abf8e4c1a09a69fee2c2ee8dfe8682a62b9e`.
The staging reuse control passed separately in that same attempt.

The repair extends the existing private `tensor_snapshot::Snapshot` owner.
Checked products derive output width and compact read ranges before writes;
concatenation examines each source descriptor once, while update derives the
replacement width from Whole/Index/Slice projections and includes runtime index
and slice-coordinate registers. Admission work follows metadata size, independent
of the tensor extent. It never enumerates destination registers to detect overlap.

Fast execution and both fresh/cached lazy paths share `native_tensors` execution.
Disjoint copies write directly and allocate no result scratch. Overlapping copies
fallibly reserve reusable `RowEvalScratch.tensor_results`, compute the complete
result from the original registers, and then commit it. Concatenation traversal
and update index projection reuse the same private helpers as checked evaluation;
allocation also shares the native owner's existing fallible reservation helper.
The operations stay compact Solve IR; no scalar expansion, additional public
API, model-name path, or relaxed construction certificate is introduced.

The three original probes and nine original prepared snapshot tests are unchanged.
Three added prepared controls check runtime index aliases, slice-coordinate
aliases, and a two-dimensional Whole/constant-index replacement projection.
Each includes checked reference, disjoint prepared, and admitted lazy controls.
One added native storage test checks repeat-use pointer/capacity stability and
zero allocation on disjoint destinations for all three repaired operations.

Focused after commands and selected counts:

```sh
cargo test --locked --offline -p rumoca-eval-solve --lib prepared_tensor_additional_alias_ # 3
cargo test --locked --offline -p rumoca-eval-solve --lib prepared_tensor_snapshot # 15
cargo test --locked --offline -p rumoca-eval-solve --lib native_tensor_staging # 2
cargo test --locked --offline -p rumoca-eval-solve --lib scalar_ops::snapshot_tests:: # 5
```

The second selector also matches the module path of the three additional-alias
tests: it selects nine original snapshots, three new update controls, and those
three original alias probes. At root source `2a0cbb0fd`, the separate alias gate
passed all three and the full module selector passed all fifteen. Its exact-count
wrapper then refused the incorrectly proposed count twelve; this was a manifest
count mismatch after successful test execution. Staging and scalar snapshot
qualification were not executed in that first after attempt.

All controls are unignored ordinary library tests in the existing workspace CI
selection. Source review and rustfmt/diff checks are complete. The governing accepted specs are
SPEC_0007 Stage 4, SPEC_0032 sections 2 and 4, SPEC_0029 sections 3b and 5, and
SPEC_0033 sections 2 and 3, as recorded in the original before-probe document.
