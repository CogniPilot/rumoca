# Original issue 425: 38x38 qualification candidate

Test-only source based on `673ce6ec74418fb0043d3ad7354e0ceb2c7543da`.
This is the current workshop source, not a claim of complete PR421 composition.
Both new tests are **UNRUN**. No Cargo, benchmark, or simulation was executed
while preparing this candidate. No issue-fix or memory-improvement claim is made.

The unchanged retained original is
`crates/rumoca-compile/src/session/tests/fixtures/reported_bugs/issue_425.mo`.
Its SHA256 is `dccdafed12d656db2c6758b6e0e5f7ca5d85accb89847d6715c4c6f5f5c4b89d`.
The only substitutions are `parameter Integer nx = 30;` to `38` and
`parameter Integer ny = 30;` to `38`, exactly once each. Model name remains
E30x30 and every equation, algorithm, declaration, and initial value stays
unchanged. The derived source SHA256 is
`1bd65c874537a23acd43cde3d5d1879d84bf1b1c2affa685df1646d84e8c6a5d`.
One shared test-owned helper pins both texts with the existing sha1 dependency
and asserts the two substitution counts; no hashing dependency was added.

Run the focused gates sequentially with frozen source/binary/tool hashes and
existing resource guards. Each command must execute exactly one test, with zero
ignored. Local output/cache/TMPDIR stay below `$HOME/scratch/rumoca`.

```sh
cargo test --locked --offline --jobs 4 -p rumoca --test suite_core \
  process_allocator_address_space::issue_425_original_38_bench_fits_a_two_gib_address_space \
  -- --exact --nocapture

cargo test --locked --offline --jobs 4 -p rumoca --test suite_core \
  reported_bug_runtime::issue_425_original_38_preserves_quiescent_states_and_native_receipts \
  -- --exact --nocapture
```

The memory gate requires the actual CLI binary: use the normal default feature
set, including the existing process allocator. A no-default-feature library
build is not this gate. Both commands use the same feature set to avoid a
second compiler link. The semantic gate alone can also use the previously
qualified `--no-default-features --features fmu-packaging` feature set.
The memory test creates one CLI child with RLIMIT_AS=2097152 KiB and
RAYON_NUM_THREADS=1, then runs the original `sim bench` options: rk-like,
t_end=0.1, dt=0.01, one iteration, zero warmups. It retains stdout/stderr/status
in the test log and asserts JSON identity/options, eleven points, final time
0.1, and finite nonnegative compilation/preparation/backend/hot timings.
AS=2 GiB is not a peak-RSS measurement; the outer guardian must retain resources
and the independent original CLI/time receipt when measuring peak RSS. There
is no arbitrary speed ceiling, estimated timeout claim, or alternate benchmark.

The semantic gate uses the checked DAE's scalar names at the observation
boundary, not name reconstruction or compiler tensor expansion. It checks all
4*38*38 original states: r=1, mu=0, mv=0, E=2.5, finite and exactly quiescent,
at 0, 0.01, ..., 0.1. It uses the existing public native-table compilation owner
to refuse hidden helper compilation fallback; that preflight is dropped before
session construction. Every sample requires Cranelift/no refusal/no declines.
This gate is separate from the address-space benchmark and cannot be used as
its peak-memory measurement. It does not assert a nonempty table: legitimately
absent retained helpers do not invalidate the original model.

Existing accepted SPEC0007/0029/0032 preserve checked owners, shared execution,
and compact source families; SPEC0033 requires original-path proof and honest
unrun boundaries. No product API, compiler behavior, validator, spec status,
model exception, tolerance, or cap changes here.
