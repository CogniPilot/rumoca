# Initial clock visibility for an ordinary sample

At source `614109f2eab2b3702ab32de2f65a546caf6ae4dc`, the unchanged
`periodic_source_counter_regression::` suite ran seventeen tests: fifteen
passed and two failed. The zero-phase RK-like accumulator reported `count=2`
at `t=0`, where its independent source oracle requires one tick. The other
failure, an extra regular condition-memory row, has a separate lowering fix.

The first divergent runtime owner is
`SolveRuntime::apply_projected_event_update`. Initialization settles an
`UnownedOnly` pass before the phase-zero `All` pass. An ordinary Boolean
`sample` expression has an unowned row and a typed clock leaf; the canonical
parameter view previously activated that leaf during both passes. This does
not require a source `initial()` call, so an optional initial flag cannot
provide the event-pass timing authority. Committing `pre` between the two
passes exposes the first increment to the second increment.

The repair applies the existing event-action clock-view discipline to the
canonical parameter view through one private helper. A pass that admits a
clock tick uses the existing schedule writer; a pass that does not uses the
existing observation writer to clear clock leaves. It changes no schedule,
row ownership, initialization ordering, pulse rearming, IR or wire shape.
SPEC0007's Solve event-pass ownership and the MLS initialization/transient
boundary govern this distinction; source `when true` and `initial()` remain
their existing independent controls.

The unchanged periodic suite now passes all seventeen tests at
`9aec6edc9fd68ef422566ba9b83462916402987e`, including its independent
`sample(0, 0.1)` oracle of one increment at initialization and one per later
tick. The thirty-two backward time-activation controls, five guarded-deadline
controls and all 188 phase-Solve tests also pass on that source.

The additional runtime owner witness passes separately at
`8f4a4cce477b8142c31524226f136323c5050dbb`: one test, zero ignored, with all
assertions for initialization zero, post-initial one, next-tick two and pulse
rearming reached. Its inactive arm holds the current target, matching the
checked when producer (SOLVE-C07/C10), while only its active arm reads
`pre(counter)`. Production commit
`aa1c23ab26e2a1e01d906774a706963e15d327b7` is present in both qualified sources.

Earlier versions of this supplemental fixture were incomplete: the Unary
field first failed compilation; the event-iteration storage/run was missing;
and the inactive arm restored `pre(counter)`, undoing an increment when the
pulse buffer settled. The earlier one-observation failures reached no numeric
assertions and do not establish the original double-increment defect. The
original public seventeen-test BEFORE/AFTER pair establishes that defect;
fixture corrections preserve all expected values and observation counts.

Actual source/log/guardian bindings are in
[`unowned-phase-zero-clock-view-after.json`](unowned-phase-zero-clock-view-after.json).
The isolated witness's complete 4,024-entry source seal and HEAD were identical
before and after execution. It used the unchanged 600-second, 4-GiB RSS,
8-GiB cache and scratch-floor guards with four jobs; execution completed in
120.691 seconds including a fresh offline build, with peak RSS 2,075,180 KiB
and no resource stop. Formatting and diff checks passed. These results do
not establish complete solver/workspace or CI success.

Focused qualification commands:

```sh
cargo test --locked --offline --jobs 4 -p rumoca-solver --lib \
  runtime::solve_runtime::tests::visibility::phase_zero_unowned_sample_waits_for_its_single_post_initial_tick \
  -- --exact --nocapture
cargo test --locked --offline --jobs 4 -p rumoca-solver --lib \
  runtime::solve_runtime::tests::visibility::phase_zero_clock_tick_executes_once_after_initialization \
  -- --exact --nocapture
cargo test --locked --offline --jobs 4 -p rumoca --test suite_core \
  periodic_source_counter_regression:: -- --nocapture
cargo test --locked --offline --jobs 4 -p rumoca --test suite_core \
  time_event_when_activation:: -- --nocapture
```

Source counts are respectively one, one, seventeen and thirty-two. The
original periodic/time models, values, tolerances and time budgets are
unchanged. Actual BEFORE evidence is under attempt
`pr427-combined-arm-owners-full-controls-614109-20261012`:

- recipe SHA256: `f9b4dfbc37183d4db3146a7a605febf72e1ab2160c84d9a9e8ec0f2a001b41ce`
- periodic log SHA256: `0682ee1ed1a2dad4ffdf4e641e78fcfcb55fd4c7185bf2ae90e5e81054d38c47`
- periodic guardian SHA256: `903d1a8ce4c4247a1b805d98cbbbe7d0b3214538e715c104c85e8a46f81054eb`

The qualified scopes above are runtime results; broader final gates remain
separate obligations.
