# Selected-region unit coverage reachability

Source-only audit of root final source
`e437fa83c121554cde08c5ef4700103f4285085a` found the needed unit replay already
present through `0be5834a5d9d2e3d989b2f968549dc5b3fddd66b`.
No duplicate regression, production change, ignored test, coverage exemption,
baseline promotion or threshold waiver is added.

The CI `coverage-gate` job invokes `xtask coverage run`. Its actual argument
builder in `crates/xtask/src/coverage_run_cmd.rs` runs
`cargo llvm-cov --workspace --tests` and enables
`rumoca-bind-wasm/native-assignments`. It does not enable
`rumoca/template-runtime-tests`, whose separate target requires that feature.
Therefore the seven scalar template regressions are meaningful backend CI
coverage but do not alone establish coverage-lane execution of new Rust methods.

`rumoca-eval-solve::typed_program::tests` is gated only by `cfg(test)`, with no
external tool, backend feature, or ignored fixture. Its existing
`conditional_projections_select_lazily_between_identical_calls` test constructs
the original owners through `SolvePureCallTable::construct` and
`TypedProgramBuilder::conditional`. Two conditional regions either call the
nested doubled-value owner with their Real capture or return that capture.
The independently specified results at input `3` are `12` for the true
predicate and `6` for the false predicate.

The test now calls its shared `assert_scalar_call_projection` helper for both
predicate values. The helper derives aggregate input starts and result width
from the issued call site, calls the actual shared `lower_scalar_ops`, validates
both programs with `ScalarProgramBlock::with_program_spans`, and executes both
through `eval_scalar_program_block_with_context`. It compares each to the same
independent expected values, rather than only comparing the two implementations.
The actual API has no `lower_scalar_program_block` entry point.

Because the predicate is a runtime input register during projection, this path
reaches all four new methods in `scalar_ops/pure_calls/conditional.rs`:
`selected_regions`, `project_region`, `region_captures`, and `capture`. Each
selected region is projected with a private register file and the shared
budget; the nested called owner reads the same captured value. Both selected
results then execute in the canonical scalar evaluator. This is nested call
capture coverage, not a claim that this particular fixture contains a nested
conditional; the separate vector backend regressions exercise that case.

No test count changes. Source declarations give 31 tests beneath
`typed_program::tests::`, or 33 beneath all `typed_program::` including the two
payload tests. These are source counts; this audit ran no Cargo or coverage
instrumentation and claims no measured coverage percentage. The seven template
tests remain unchanged, and the separate batched JAX probe remains unqualified.
Root's final focused and coverage lanes must establish actual execution.

SPEC_0025 §4 requires every newly added function to execute in the workspace
coverage lane and forbids unreviewed coverage exemptions; SPEC_0033 requires
focused evidence before broad qualification. The existing unit path supplies
the required reachability without weakening either gate.
