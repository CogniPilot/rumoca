# PlanarFourbar Linux perf investigation

## Accepted storage reuse and canonical recovery

The allocation profile below led to worker `16f744833`, integrated as
`7fed0a809`. Shared affine projection now exclusively leases initialized matrix
storage from the exact issued block through solving and certification. Every
call still evaluates fresh derivatives and clears the destination; native
transactional publication is unchanged. This removes repeated allocation, not
zeroing or numerical work. Retained seed linearizations keep independent storage.
One lazy matrix remains resident per exercised block until runtime destruction.

The old implementation fails the actual refresh allocation-count control after
its first call. New source passes that control and poisoned-matrix, native-error,
mixed fallback, reentry, owner/extent, rollback, and dense-retry controls.
Independent review found no blocker in the frozen patch. Main passes 553 shared
tests, 12 source tests, simulation clippy, and formatting; canary has no change.

Canonical PlanarFourbar now completes in 8.956 seconds in the unchanged recovery
list and 9.128 seconds in full `tier2-affine-storage-7fed0a809/`. Both compare high
against OMC across all 872 comparable channels, with zero deviation channels.
The complete published traces have SHA-256
`863c9039b40734fcdb1e2f3eb411bfdb4090bf171e6e576b2f0d2f88faf1c3b7`, identical
to the earlier complete diagnostic. The old canonical timeout did not finish,
so these results are not an exact before/after speedup measurement. The full run
has 190 high/190 compared, 17 exclusions, zero missing. It preserves the prior
189 high identities, but two excluded models lose raw completion; those losses
remain under investigation. Six historical high models still time out.

## Canonical worker settings at e7c61f60

One frozen capture at `e7c61f60` (production source identical to `2a0ea3aa`)
reuses the canonical request, changing only the output directory. It retains
the 12-second budget, disables IR emission, uses the same library root, and
applies jobs=1, Rayon=1, CPU core 0, and the canonical mimalloc overrides.
Observed simulation thread affinity is core 0. The optimized `msl-fast` build
adds unwind tables, frame pointers, and line tables for attribution. The capture
does not reproduce four-process contention or persistent daemon reuse and is
diagnostic evidence, not a replacement canonical gate attempt.

The simulation times out at 12.059 seconds. A strictly interior 11-second window
has 4.78 seconds user CPU, 6.13 seconds system CPU, 3,766,492 minor page faults,
and zero major faults. CPU accounting nearly spans the window; sample timing
and tick quantization limits are retained. User-only sampling does not identify
the kernel cost or prove that allocator policy caused it.

Among 2,350 user samples, 697 identify fresh dense-matrix zero allocation in
`projection/initial.rs::algebraic_block_jacobian`; 298 identify the native
Jacobian's transactional scratch-to-output publication. Roughly 90% of samples
decode at least two distinct program counters. These are user CPU shares,
not shares of wall time. The source-bound proposal is initialized, block-owned
Jacobian storage reuse with fresh derivatives on every call. It keeps the
publication copy, fallback initialization, rollback, and all certificates.
It makes no numerical-cache or allocator-policy change. Independent direction
review precedes implementation; this capture establishes no recovered model.

Frozen executable SHA-256:
`5ec45883dcd26eb2aed26f98a341649772ec23eaca8bee57ad18ed1761ad82a1`.
Raw perf data, executable, requests, source/build provenance, process samples,
resolved caller lines, interval proof, and proposal are retained under
`/tmp/rumoca-fluid-speed-bdf/target/planar-perf-canonical-e7c61f60/`.
Main's compact copy is `target/fluid-campaign/planar-canonical-perf-review/`.

The user requested Linux `perf` to isolate the remaining performance problems.
This investigation follows SPEC_0033's first-producer evidence requirement;
optimization must preserve the shared Model Exchange contract and unchanged
OMC comparator. Seven original preservation targets still time out in the
canonical `recovery-residual-batch-32e020219/` run. Its official transition diff
reports no stage, execution, or band changes; ArmatureStroke remains high.

## First capture: frozen residual-batch candidate

Source `d5f466bc58a1d1d2eaf07f2361e780a40471d7af`, release worker SHA-256
`17485b9e443a138127637edc64d3f23cd68e2a48481050f76cd38760853881ff`.
Artifacts, exact command, analysis scripts, frozen executable, request, raw
`perf.data`, JIT symbol map, and trace:
`/tmp/rumoca-fluid-speed-bdf/target/planar-perf-d5f466bc/`.

One capture used perf 7.1.4, `cpu-clock:u`, 499 Hz, and 16 KiB DWARF stack
capture. The original xtask worker request retained the 12-second budget and
5-second source stop time. Main waited for its builds and simulations to finish
before sampling. No kernel settings or source were changed. There were 19,303
samples and zero reported lost samples across the complete worker lifetime.

The conservative interior interval `[94250, 94260]` on the perf clock contains
4,985 samples. Alignment uses source-load and per-model progress durations;
the phase clocks are not synchronized absolute timestamps. Margins of roughly
0.29 and 0.58 seconds and adjacent sampled instructions support this interval
as simulation work. Whole-process reports also include compilation and must
not be quoted as simulation costs.

| Symbol | Sampled self CPU share |
|---|---:|
| `eval_row_prepared_fast` | 12.38% |
| libc `memmove` | 10.91% |
| `project_algebraic_singleton_assignment` | 7.10% |
| `SparseNewtonCache::solve_torn_scaled` | 6.48% |
| `mark_projection_indices` | 4.95% |
| libc `memset` | 4.13% |
| `eval_target_assignment_row_inner` | 3.19% |
| `eval_target_assignment_row_with_scratch` | 3.03% |
| nalgebra `LU::new` | 2.97% |

This supports investigating projection and interpreted assignment execution.
It does not identify the source callers responsible for memory copying or
establish redundant work. The existing validated-plan guard also means that
`mark_projection_indices` samples alone cannot justify removing validation.

Stack quality limits attribution: 82.25% of samples decoded no frames, 17.71%
decoded only the leaf, and two samples decoded two frames. All 544 `memmove`
samples contain only the leaf. Sampled instruction addresses still resolve
well enough for self costs; 0.72% are unresolved. All 247 named JIT samples
lack caller frames. The nominal inclusive report is therefore not trustworthy
as inclusive attribution. A separate optimized build with explicit unwind
tables, frame pointers, and debug information is needed to resolve callers.

## OMC comparison and timing limits

The first profiled worker completed in 10.869 seconds. Main used unchanged
`cargo xtask repo msl -- plot-compare --reuse-traces` on that saved trace and
the retained pinned OMC reference from `tier2-67978a5d`: all 872 comparable
channels, full 0–5 seconds, 501 aligned points. Worst bounded normalized L1
is `6.904e-5`; mean channel score is `1.205e-5`.
`target/fluid-campaign/planar-perf-d5f466bc-omc.{log,html}` retains the result.
Neither simulator was rerun for comparison; no channel, tolerance, or horizon
was changed.

This is diagnostic agreement, not canonical recovery. The worker source is
the isolated candidate, and its release profile uses ThinLTO; canonical runs
use the integrated commit and `msl-fast`, which disables LTO and uses 16 codegen
units. Instrumentation adds another timing difference. No source, profile, or
parallel-load effect is isolated by comparing these two runs. The next perf
build uses integrated `32e020219` and `msl-fast` with diagnostic unwind flags.

The OMC reference's independently retained statistics remain in
[planar-fourbar-profile-7ebb.md](planar-fourbar-profile-7ebb.md): 798 steps,
1,179 ODE calls, 38 error-test failures, zero convergence failures, full 5-second
completion. Its timer scopes differ from Rumoca's; no exact speed ratio is claimed.

## Second capture: source attribution at integrated 32e020219

The optimized `msl-fast` worker adds only diagnostic build settings:
`RUSTFLAGS='-C force-unwind-tables=yes -C force-frame-pointers=yes'`,
`CARGO_PROFILE_MSL_FAST_DEBUG=line-tables-only`, and
`CARGO_PROFILE_MSL_FAST_STRIP=none`. Source is clean `32e020219`; executable
SHA-256 is `be089405f0795ba6e88ae314bff1b8ec0b31756c25156b89cd48e35113ccc531`.
Artifacts and commands are under
`/tmp/rumoca-fluid-speed-bdf/target/planar-perf-unwind-32e020219/`.
The build took 5m21s; there was one capture, with a 32 KiB DWARF stack buffer.
It retains 24,077 samples with no reported loss. The instrumented simulation
times out at 12 seconds, so this capture has no new trace-parity claim.

The interior window `[95032, 95043]` contains 5,481 samples. The same approximate
phase-alignment method leaves 0.36/0.67-second margins. Source loading and its
automatic cache pruning are excluded. Now 93.03% of samples decode at least two
distinct program counters, versus 0.040% decoding any caller in the first run.
4.78% decode no frames; JIT unwind gaps and retained addr2line warnings remain.
Native symbol reports without inline expansion independently support these
inclusive scopes. They overlap and are observed lower bounds:

| Scope | Inclusive sampled CPU share |
|---|---:|
| `project_refresh_slots` | 65.64% |
| `SolveMeKernel::observe` | 56.10% |
| `project_algebraic_singleton_assignment` | 43.02% |
| `eval_target_assignment_output_unchecked_with_context` | 32.37% |

The exact caller evidence narrows the direction:

- **Jacobian publication:** 346 of 513 `memmove` samples (6.31% of all samples)
  come from `CompiledProjectionJacobian::call`, reached through prepared affine
  projection Jacobian evaluation. Address-to-line decoding identifies
  `emit/projection_jacobian.rs:98`, the final scratch-to-output copy. It preserves
  failure atomicity; the next change leaves it intact.
- **Repeated structural validation:** all 188 `mark_projection_indices` samples
  (3.43% self) decode through refresh-stage projection, primarily observation
  and secondarily derivative refresh. The existing validation guard is present;
  `refresh_execution.rs::project_refresh_slots` constructs its model with
  `plan_validated:false`. The next candidate must validate both issued stage-plan
  variants at preparation and preserve an exact plan/dimensions proof through
  the guard. A blanket boolean change would not establish safety.
- **Interpreted target isolation:** the 32.37% scope comes through singleton
  projection. Existing native admission restricts output offset and program
  output cardinality. This profile does not establish which restriction explains
  every call. Changing aggregate ownership needs separate proof and is deferred.

Main reviewed the caller paths and selected prepared structural validation as
the next bounded general fix. An independent reviewer is checking constructor
ownership and dimension-change counterexamples before admission changes. No
numerical cache, solver policy, tolerance, or residual certificate change is
authorized by these measurements. The measured 3.43% is not a promised speedup.

The independent review found a concrete counterexample to unconditional admission:
`SolveRuntime` exposes mutable `state_count` and `solver_count`. Increasing the
state count can turn a previously valid algebraic unknown into a state; expression
read-bound checks alone do not reject that partition change. Prepared validation
must retain captured layout extents and check them against each invocation and
its coordinate slice. Both stage-plan variants and remainders need coverage at
the constructor where the runtime binds the issued owners. This review is source
evidence; the candidate must supply executable rejection controls.

## Recurring stage-seed failure: concrete producer

One bounded temporary probe at `b685d26e3` identifies canonical block 1026:
algebraic sequence 5 fails all 9,843 seed attempts, and derivative sequence
4294967301 fails all 3,934 attempts. Every complete-plan fallback succeeds,
from source time 0 through 5. These are callback counts across runtime lifetimes,
not native solver steps. Only the first failure per owner records operands;
later failure reasons were not separately sampled.

The first failing seed entry is position 235, issued target Y375, source
node 0/program 325, equation/output 1043, output offset 0. Its tensor-affine
residual is `0 - dot(P[55..57], Y[373..375])`. Recorded parameters are `[1,0,0]`,
and all three incoming coordinates are zero. Isolating Y375 therefore uses
coefficient `-P57 = -0`, producing `0/0`. This is a computed invalid seed, not
an initially uninitialized coordinate. The following assignment to Y374 has a
nonzero own coefficient but reads the newly NaN Y375, so it can also become NaN.
The existing finite-value check then rejects the seed, restores the whole
incoming coordinate, and invokes complete projection.

This explains why otherwise grouped native/static assignments are repeatedly
revisited by the slower complete-projection path. It does not establish a safe
optimization by itself: a structural tensor-affine isolator is not a proof of a
numerically nonzero coefficient for every admitted parameter value. Independent
direction review is checking seed admission and the scope of rollback before
any production repair. Full residual and correction checks remain required.

Evidence, issued mappings, first operand snapshots, counts, probe patch,
source/binary/request hashes, and cleanup record:
`/tmp/rumoca-fluid-speed-bdf/target/planar-seed-census-b685d26e3/`.
There was one instrumented 12-second-budget run, completing in 11.965 seconds;
this is not canonical recovery. Probes were removed, and the worker tree is clean.
Its build output remains instrumented and must be rebuilt before ordinary use.

Main compared the saved complete trace through unchanged
`cargo xtask repo msl -- plot-compare --reuse-traces`: 872 comparable channels,
full 0–5 seconds, 501 aligned points, worst bounded normalized L1 `6.904e-5`.
The retained result is
`target/fluid-campaign/planar-seed-census-b685d26e3-omc.{log,html}`.
No simulator was rerun for this comparison. Latest canonical cohort results and
the subsequent IMS_Start recovery are recorded in
[fluid-review.md](fluid-review.md).

## Implemented repair and measured effect

`0d9b08d0c` integrates worker `8b684b72`; `2a0ea3aa` completes the simulation
error conversion. An allowed failed seed now restores its whole stage-entry
coordinate and tries the existing issued block. Upstream values and all numerical
certificates remain intact. A typed numerical nonconvergence outcome preserves
the old whole-plan recovery route from the original call entry; native/evaluator
errors do not authorize replay. Independent review challenged both poisoned
secondary writes and loss of the old nonconvergence recovery path. Tests cover
both. Main passed 546 shared tests, 12 source-level tests, simulation-layer lint,
and formatting after repairing an exhaustive error-conversion build failure.

One sequential pair uses frozen ordinary `msl-fast` production binaries, with
no instrumentation. Planar simulation time falls from **11.7585 to 5.2740 seconds**
(55.15%); setup is measured separately. Both complete traces are byte-identical:
SHA-256 `863c9039b40734fcdb1e2f3eb411bfdb4090bf171e6e576b2f0d2f88faf1c3b7`,
3,494 published channels, 501 samples, full 0–5 seconds. Main verified hashes and
reran the supported `cargo xtask repo msl -- plot-compare --reuse-traces` route:
872 comparable OMC channels, worst bounded normalized L1 `6.904e-5`.

Artifacts: `/tmp/rumoca-fluid-speed-bdf/target/planar-ab-b685-2a0ea3aa/`, with
compact independent evidence under
`target/fluid-campaign/stage-seed-planar-ab-review/` and main's comparison in
`planar-stage-seed-2a0ea3aa-omc.{log,html}` under that campaign root.

The diagnostic requests differ from the actual canonical request in output
directory, timeout, IR emission, and source-root path. All 4,213 library entries
match. Canonical execution uses four pinned worker processes, Rayon=1, and
allocator overrides; the diagnostic pair uses one sequential worker with
Rayon=4 and no explicit affinity. Actual diagnostic CPU placement was not
recorded. Thus the pair establishes an isolated benefit, not canonical recovery
or a causal explanation of the timing gap. Canonical Planar still times out.
Next profiling must use canonical settings; no gate or budget is relaxed.

The full `2a0ea3aa` cohort preserves all 189 compared high models and recovers
two raw completions, including IMS_Start. Seven preservation targets still time
out. Exact exclusions, identities and remaining obligations are in
[fluid-review.md](fluid-review.md).
