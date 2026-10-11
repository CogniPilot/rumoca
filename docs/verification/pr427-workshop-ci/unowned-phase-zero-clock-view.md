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

Test-only commit `f60e082000b6366ba884f7f8b0d98493572efa19` adds a runtime
owner witness with an unowned counter, pulse memory, a zero-phase periodic
schedule and no initial-flag slot. It requires a zero initialization value,
one post-initial tick, rearmed pulse memory, and one further increment at the
next tick. Production commit
`aa1c23ab26e2a1e01d906774a706963e15d327b7` shares the checked pass's clock
visibility between canonical updates and event actions. Both are unrun here;
formatting and `git diff --check` passed, and independent source review found
no blocking defect.

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

No AFTER runtime success or complete CI success is inferred from source
review.
