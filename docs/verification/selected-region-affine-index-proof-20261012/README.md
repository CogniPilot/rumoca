# Selected-region affine index ownership

Qualified source: `a0dd6fc7d3811b3439b4d633732d4d23798f50a0`, based on `86c3bc1a6128e3d3174fefa169ae7bd5e03243e9`. These results qualify this isolated source, not a later combined candidate. [receipts.json](receipts.json) binds the retained recipes, raw logs, guardians, exact input inventory, worker executables and output trace by SHA256.

The unchanged MSL `ThreePhaseTwoLevel_PWM` originally failed the structured B.1c affine proof at operation 2: four corresponding constant column indices differed inside nested selected regions. Binding explicit iterator coordinates fixed two controls but did not fix the original implicit row-major projection. The final repair retains those selected index/literal leaves as distinct captures and then runs the unchanged complete-domain affine checker.

The capture normalizer consumes the complete issued point family. It requires corresponding checked region topology and exact operation/destination identities. Captures contain only immutable index/literal values; index reads, calls and arithmetic remain in their original lazy branches. Existing reusable owned regions still refuse differing contents. Each varying leaf has a separate capture, and all points must satisfy the existing affine certificate; the `{1,2,2}` negative protects against inferring a proof from two points. No new IR layout, schema, model exception, evaluator or relaxed checker is introduced.

This follows SPEC0007's Solve stage and structural scope, SPEC0032's compact-source/scalar-view/affine-Map ownership, SPEC0043's structured B.1c and checked conditional capture contracts, and SPEC0029's single-source helper rule. MLS 3.7 §3.6.5 requires selected if-expression evaluation; §10.1 defines one-based array indices. The source domain binding is shared with the existing quotient owner.

## Actual gates

| Stage | Outcome |
| --- | --- |
| Corrected input-binder BEFORE (`9d5ae01e7`) | 0 passed, 2 failed: nested selected program mismatch |
| Explicit-domain binding AFTER (`755f45797`) | 2 passed; full phase-Solve 190 passed; scoped Clippy passed; original PWM still failed |
| Row-major BEFORE (`e584d6752`) | 3 passed, 2 failed; both positive row-major controls rejected at operation 2 |
| Index-only normalization (`cfaa267a6`) | 3 passed, 2 failed; differing selected Const leaves still rejected |
| Literal capture repair (`18c95b4a5`) | 5 passed, full 193 passed; Clippy found one test-only nesting violation |
| Final helper extraction (`a0dd6fc7d`) | 5 passed, full 193 passed, scoped all-target Clippy passed, unchanged original PWM checked Solve passed |
| Original authored PWM simulation (`a0dd6fc7d`) | `ic_ok` / `sim_ok`, full 0–2 second horizon, 4,569 points across 402 finite rectangular trace channels |

The focused five are a subset of the 193, not 198 unique tests. Their controls preserve source binder identity, row-major columns, inactive invalid-index laziness, active index diagnostics, and complete-domain nonaffine refusal.

Exact Rust gates, in the repository's `nix develop .#full` shell:

```sh
cargo test -p rumoca-phase-solve --lib structured_selected_indices -- --nocapture
cargo test -p rumoca-phase-solve --lib
cargo clippy -p rumoca-phase-solve --all-targets -- -D warnings
```

Original simulation used the existing public ModelWorkerDaemon and unchanged authored request, with canonical source-root startup 60 seconds, model phase 10 seconds, solver 12 seconds and parent 14 seconds. Model compilation took 0.7605 seconds; Solve lowering 0.1584 seconds; solver run 4.8896 seconds; recorded simulation wall time 10.1130 seconds. The final outer guardian exited 0 in 38.7206 seconds, including helper build and source-root startup, with peak aggregate resident-plus-swap 1,682,708 KiB, zero swap, and no resource stop. The original simulator selected BDF and recorded 1,184 events. The retained worker response does not establish a separate explicit backend-policy receipt.

An earlier launch failed at Bash environment-variable syntax before execution. The next launch incorrectly used a 10-second startup budget and failed during source-root loading before model phases. Both are retained separately; neither is counted as a model semantic failure. The final launch uses the canonical startup budget from the MSL pipeline owner while keeping all model/solver/parent budgets unchanged.

The separately sealed canonical comparator from source `117b2490b79a081fd21e87e028f19a29e5636296` completed against this worker's full trace. OMC actually simulated the unchanged model over 0–2 seconds, 2,000 authored intervals and tolerance 1e-5. The canonical all-channel comparison reports 298/298 compared channels high, zero minor/deviation/severe channels, model bounded normalized L1 score 2.675764309674647e-7, and worst channel bounded L1 0.001411865674747083. Initial-condition comparisons are 297/297 high; both tools' seven selected states match exactly. The canonical comparator retains its normal channel matching and scoring rules; the worker trace itself has 402 channels.

The first comparison exited 0 in 15.6123 seconds with peak resident-plus-swap 609,500 KiB and zero swap, recorded in the tool transcript. Its full guardian stdout was not archived to a file. A second cached-reference comparison captures a durable guardian JSON separately; both report the same channel verdict, and the first full output snapshot is retained unchanged. OMC simulation took 0.9692 seconds versus worker solver run 4.8896 seconds, so this result makes no performance superiority claim. The comparator reads the canonical pinned MSL source but writes only the isolated one-model carrier; cohort results are unchanged. Its tool source identity is distinct from the worker source and recorded explicitly.

Combined-head qualification and the seven independently reported structural singularities remain pending. This local original-model restoration does not close an issue or replace the final 566-model gate.
