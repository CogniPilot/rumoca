# Compact ordered reverse invalidation

Base: `fa8de17c66c22689d01f286d22f8e5fa34ef9dcf`.
Branch: `fix/slam-compact-reverse-invalidations`.

The application's unchanged full-depth Modelica source has 14400 pixels,
28800 output Y values and 57604 P values. Its source SHA-256 is
`5348000691eb9fb878f902c3aa867d8cd65e58505994ec2759dd88066837e604`.
The reviewed compiler at `94cff417cec61c256b5ea307d5d2a7f8213437ac`
prepared two certified complete native families, 675 and 381 bytes. The
application's `dev/modelica-depth-frame-verification.json` records 90 frames,
2592000 independently checked outputs, source-edit checks and reset/JSON checks.
This is a full-depth proof, not full SLAM admission or throughput evidence.

## Source and profile evidence

Native preparation calls the complete `lower_solve_model` pipeline. It does
not initialize a solver, but still derives the solver artifacts before runtime
defaults, metadata and the native module/JSON payload.

In `rumoca-eval-solve/src/sparsity.rs`, multiple Map/AffineStencil nodes derive
a conservative Full structural pattern. That pattern's representation is
compact. The old `derive_algebraic_reverse_invalidations` immediately called
`StructuralPattern::column_rows`, however, which copies every row into every
column of a Full pattern, even if the projection plan contains no blocks.
28800 squared wasm32 `usize` entries alone occupy 3317760000 bytes (3.09 GiB).
The two source owners are byte-for-byte unchanged between reviewed94 and
main `37a875980a8e7478129b93a6dec960cca193eb68`.

The actual reviewed-WASM preparation CPU profile took 61.83 seconds overall,
60.913 seconds for native preparation, and reached 3.663 GiB RSS. Of 43570
samples over 61.249 seconds, V8 garbage collection accounted for 48.3569
seconds exclusively (78.95%). `column_rows` accounted for 1.7238 seconds
exclusively and 3.1674 seconds inclusively. Garbage collector samples do not
retain the allocating Rust owner, so assigning all that time to one call would
require the unchanged-source measurement after this fix.

The raw application-owned profile is
`/tmp/slam-depth-map-preparation.cpuprofile`; its summary is
`/tmp/slam-depth-map-preparation-profile-summary.json`. This is the baseline
profile; the later integrated package/source gate is recorded below.

## Exact consumer change

The consumer retains a selected-row bitmap and an affected-column bitmap.
After checking each block's target columns against the earlier selection, it
commits that block's rows through the existing certified
`StructuralPattern::visit_row_columns` API. Duplicate committed rows need no
second visit. A Full pattern retains one all-columns flag rather than visiting
its dense relation; an empty plan returns immediately.

The result is exactly the old relation: a block invalidates an earlier row
if and only if any of its target columns occurs in any previously committed
row of the existing structural pattern. Full remains conservative Full;
Empty, Diagonal, Banded, CSR and Affine keep their certified edges. No producer
claims a new pattern or removes an edge, and no module, assignment target,
Jacobian output extent or algebraic storage is collapsed.

Every target column is still checked after a prior intersection or the Full
flag becomes true. Every row is checked before committing it. Refusals retain
the exact diagnostic text, owner span and validation order; no partial
invalidation inventory is published. Missing-source behavior is preserved.

Storage is O(rows + columns + projection blocks), including the result vector.
Full processing is O(rows + target occurrences), excluding input plan storage.
Other representations visit each selected row's certified columns once. A
truly dense Banded/CSR relation can still require many edge visits, but this
consumer never allocates the inverse rows-times-columns relation.

## Verification status and remaining owners

Staged tests compare small ordered plans with both an independent explicit
dense Boolean matrix and the unchanged old column-row consumer. They cover
Full, Empty, rectangular Diagonal, CSR, shifted Affine domains with signed
strides, overlapping rows, target ranges, duplicates, empty blocks, zero
extents and exact typed refusals after an earlier Full intersection. A separate
unchanged 28800-row Full fixture exercises the empty plan and all 28800 ordered
blocks without a dense test oracle.

The seven focused tests pass, including exact diagnostic text and source-span
comparison against the original consumer. All 222 evaluation-library tests
pass. Strict scoped Clippy passes for all targets and features. Source was
unchanged between the focused test, complete library and Clippy gates.
Rustfmt and `git diff --check` pass. The commands are:

```sh
cargo test -p rumoca-eval-solve --lib sparsity::reverse_invalidations
cargo test -p rumoca-eval-solve --lib
cargo clippy -p rumoca-eval-solve --all-targets --all-features -- -D warnings
```

The focused command took 52.457 seconds including 51.00 seconds of compilation
and 0.01 seconds of tests. The warm complete library command took 1.974 seconds
including 0.04 seconds of tests; strict Clippy took 15.159 seconds. Across those
build/test process groups, the watchdog observed 843668 KiB peak aggregate RSS,
5059252224 bytes peak private target/tmp cache and at least 55164088 KiB
available RAM. Every command used nice 10, CPU affinity 6/7, two build/test
threads, a hard 8 GiB memory cgroup with swap disabled, an aggregate-RSS
watchdog capped at 8 GiB, a 7.5 GiB cache watchdog and a 600-second deadline.
These are native focused gates, not full-model browser memory measurements.

The production consumer source SHA-256 is
`3be3f88a3919b2bf5b1160c89c4c61e25d5743408cf096f825e74f421b3aa549`;
the tests SHA-256 is
`de541333779557d8a6f508c929be3204dca244c29d1d07b4c0a6091f57d71a27`.
Both hashes were recorded before and after the gates. The original consumer
remains executable as a separate test-only oracle, with the old bitmap,
column-row expansion, fold validation order and owner spans intact; small
before/after results are compared inside the same actual test process.

The architecture checker must be compiled for this worktree's manifest path;
that gate and broad workspace/MSL/ModelicaTest checks remain pending integration.
No wrong-worktree cached checker pass is claimed.

After integration, the unchanged full-depth actual-WASM preparation and complete
numerical/source-edit probes passed with compiler revision
`943aa7d7b745eccc63ceb0f6a8f678f1542b60c7`. The compiler WASM SHA-256 is
`a2310458a4723af3c43d1934b50d8ad7509e247f58465b2c1beb7e3b0eb5f135`.
The 675/381-byte module hashes remain exactly
`426ddca60ac3541b05123551abdcef5b0200efe44ee4fb7c1761997e9589334d` and
`64a1e37bab13cd072120520e911159fe7c1faf078a75700d5c72cc47719fee7e`.

The durable application record is
`slam_web/dev/modelica-depth-frame-compact-verification.json`. The complete
14,400-pixel source, source edit/default/reset/JSON checks and all 2,592,000
outputs across 90 changing frames pass. The entire original-plus-edited source
gate takes 31.112 seconds and reaches 1,216,172 KiB peak aggregate RSS under its
180-second/8-GiB limits on cores 4/5. This executes actual WASM under Node.
The gate includes both preparation runs and assertions; it is not isolated cold
preparation time. Several compiler fixes are integrated, so the elapsed difference
from the reviewed baseline is not a causal timing measurement of this consumer.
Mean stage execution plus complete target copies is 0.705 ms (maximum 4.077 ms),
excluding compilation, input generation, workers, GPU rendering/readback and SLAM.
The production pin and runtime depth path remain unchanged. Complete browser
acceptance and combined architecture/workspace/MSL gates remain pending.

Composite affine pattern derivation remains separate. Repeated scalar fallback
projection/AD in `phase-solve/artifacts.rs`, scalar reporting metadata in
`phase-solve/model_values.rs`, repeated scalar layout names in
`phase-solve/layout.rs`, and the complete native preparation root also remain
separate owners. The 57604-value P vector and 28800-value Y vector do not
themselves explain a multi-GiB allocation. They must retain exact input order,
declaration defaults, typed storage runs and reporting semantics when optimized.
