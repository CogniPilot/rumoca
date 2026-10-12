# Coverage failure: current semantic owners and ordinary test reachability

This source-only audit starts at
`718d131d52af04150c259df298468e1d38aeff2e`. The actual old
[CoverageGate failure](https://github.com/CogniPilot/rumoca/actions/runs/38114997762/job/114398094730)
reported **31 newly unexecuted functions**. Its retained raw log SHA-256 is
`ab5070ae1f82b469cc1fb40024fa6b3b869778d7f5169dedfb756942f77c1806`.
[The exact 31-row inventory](coverage-31-owner-reachability.json) retains every
old function/path/line and maps its current owner and ordinary CI test.
These are source reachability findings, **not a current coverage pass**.

The actual `coverage-gate` job runs `xtask coverage run`, whose argument builder
in `crates/xtask/src/coverage_run_cmd.rs` selects `cargo llvm-cov --workspace
--tests --features rumoca-bind-wasm/native-assignments`. It does not select
`rumoca/template-runtime-tests`. Backend runtime fixtures therefore do not
establish execution in this coverage lane.

| Old functions | Count | Current disposition and source-selected evidence |
|---|---:|---|
| JavaScript target API wrappers | 5 | `cfg(target_arch = "wasm32")`; FullWeb's actual Node target API smoke runs through the PR `playground` job's `cargo xtask playground test`. They are absent from native instrumentation, without coverage exemptions. |
| Algorithm Code source preview helper | 1 | Retained native helper; eight ordinary `source_preview_tests` exercise real generated source, declared capability, empty output, artifact refusal, checked source identity and rendered path. |
| Explicit initialization presence/capability helpers | 2 | Canonical presence query generalized to parameter/state assignments; capability query now projects through the shared evaluator and retained pure-call table. Native `source_render_produces_solve_target_files` calls the capability path for four actual Solve targets. |
| Pure-call inliner functions | 19 | Old target-owned file removed; shared scalar owner in `rumoca-eval-solve` now handles checked numbers, shapes, folds and selected regions. Existing constructor-backed unit projections cover matrix algebra, concatenation, continued fold and nested-call conditionals. Three focused tests below add supported static view/update/selection paths. |
| Owned Solve renderer without DAE | 1 | Public `CompilationResult::render_solve_template_str_without_dae` now has an ordinary `suite_core` positive/refusal/reuse test. |
| Compact tensor identity/transpose/cross | 3 | Shared `scalar_ops.rs` inline tests already call the actual `lower_scalar_ops`, execute canonical/projected checked programs, and compare independent numeric outputs, including overlap. No duplicate test added. |

The old `select_value` speculated both conditional regions and then selected
each aggregate element. It has been removed, rather than retained for coverage;
its replacement projects the existing checked `FunctionConditionalProgram`
through private region registers. Existing
`conditional_projections_select_lazily_between_identical_calls` reaches that
owner with both predicates and independent expected values.

The new `typed_program::tests::scalar_projection` tests use the existing
`SolvePureCallTable`/`TypedProgramBuilder` constructors and shared
`assert_scalar_call_projection` helper. Each executes the canonical checked
call and the actual `lower_scalar_ops` result against independent numbers:

- Runtime Boolean selection returns `-7` or `3.5`; the negative constant uses
  checked Integer unary evaluation and explicit Integer-to-Real conversion.
- Static views read and replace each column of a 3-by-2 matrix, checking both
  row-major result orders and untouched columns.
- Static element/slice updates retain an original SSA read, select the updated
  bottom-right value `13`, and return the declared `-99` fallback for row zero.

Coordinates are construction-known Integer constants. Existing runtime Integer
input fixtures and scalar-profile refusal remain unchanged. No unchecked typed
operations, model-name exception, ignored test, feature broadening, coverage
baseline promotion, or threshold waiver is introduced.

The public renderer test compiles a function-call Modelica model and requests
its checked scalar derivative output through the exact public no-DAE API. It
checks one output and successive caller-supplied model names, refuses an
unavailable DAE field, and proves the retained context still renders afterward.

Focused commands prepared for the qualification owner, with exact new counts:

```sh
cargo test -p rumoca-eval-solve --lib typed_program::tests::scalar_projection:: -- --nocapture
# 3 new tests
cargo test -p rumoca --test suite_core solve_renderer_without_dae:: -- --nocapture
# 1 new test
```

All four new tests are ordinary unignored tests selected by workspace CI and
the native coverage workspace command. Only `rustfmt` and `git diff --check`
were run while authoring this followup. Cargo, OMC and coverage instrumentation
were not run. Fresh focused execution and the actual coverage gate must still
prove the resulting source. SPEC_0007/SPEC_0029 keep projection with the shared
semantic owner; SPEC_0032 preserves checked shape and coordinate ownership;
SPEC_0033 and SPEC_0025 require honest execution evidence without waivers.
