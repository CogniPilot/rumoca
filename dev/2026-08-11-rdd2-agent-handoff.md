# RDD2 tensor-native performance and mission handoff

Date: 2026-08-11  
Branch: `msl-trace-parity-50`  
Repository: `/home/jgoppert/git/rumoca`

## Active goal

Complete the RDD2 roadmap end to end:

1. Keep Rumoca tensor-native without premature array or expression expansion.
2. Preserve valid/correct-by-construction IR suitable for later formal proof.
3. Fix remaining estimator, refresh, event, and runtime performance issues.
4. Reach at least 10x realtime for the RDD2 pure Modelica missions: a 0.5 s
   slice must run in at most 0.05 s.
5. Validate full GPS and optical-flow missions against OMC traces.
6. Share one DAE/Solve lowering with Production C and GALEC as a subset.
7. Build `cerebri_rdd2`, validate eFMU code generation, and run equivalent
   firmware missions.

The long-term goal is active. It is not complete or blocked. Overall roadmap
completion is approximately 45%: the tensor-native typed owner and continuous
refresh foundations are substantially implemented, but the 10x gate, full
mission parity, GALEC cleanup, and firmware/eFMU qualification remain open.

The live roadmap is:

- `dev/2026-08-10-rdd2-tensor-native-performance-roadmap.md`

## Mandatory design rules

Read these before changing the compiler or runtime:

- `spec/SPEC_0007_IR_PIPELINE.md` and catalog `SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md`
- `spec/SPEC_0036_VALID_BY_CONSTRUCTION_IR.md` and catalog `SPEC_0043_CONSTRUCTION_CATALOG.md`
- `spec/SPEC_0032_RANGE_PRESERVING_TENSORS.md`
- `spec/SPEC_0029_CRATE_BOUNDARIES.md`
- `spec/SPEC_0033_DEVELOPMENT_PROCESS.md`
- `spec/SPEC_0034_GALEC_EFMI_EXPORT.md` and `SPEC_0042_GALEC_LANGUAGE_CATALOG.md`
- `spec/SPEC_0021_CODE_COMPLEXITY.md`

The key row is SOLVE-C56 in `SPEC_0040`. Do not identify work through body,
name, span, hash, pointer, or slice equality. Do not expand and later recollapse
arrays or expression graphs. Semantic IDs, schedules, compact ranges,
directional relations, coverage, and remainders must be issued by checked
constructors and replayed from wire data. Scalar execution views may be
materialized only at the final backend/template boundary.

## Verified committed state

Recent commits:

- `95315052 Store compact refresh row selections`
- `e72f9dd3 Reuse event-settled clock refresh owners`
- `ae636cce Issue canonical continuous refresh schedules`
- `8c0e28d1 Document rejected Cranelift optimization experiments`
- `e5ddf351 Issue exact causal refresh remainders`

Earlier relevant commits:

- `0e1d7d15 Select exact typed region captures`
- `7c4de0c7 Execute continuous calls through checked directional owners`
- `522613a6 Execute checked event transactions atomically`
- `489f7fd3 Implement tensor-native Solve owners and event transactions`

At `e5ddf351`:

- Canonical continuous refresh owners are issued from `ComputeBlock`, before
  evaluator scalar preparation.
- Exact assignment owners retain source/row identities, targets, and checked
  isolator shapes. Scalar programs are materialized only in final adapters.
- The register-to-Y dependency proof is centralized in Solve IR.
- Runtime and Production C consume construction-issued assignment sequences.
- The runtime fixture fallback was removed.
- Root-after-derivative now constructs an actual filtered ordered causal
  remainder when both plans carry causal-solution certificates. Otherwise it
  drops the full causal certificate and uses uncovered stages.
- Full affected gates passed: Solve IR 171/171 and solver 288/288. The earlier
  full schema-58 gate also passed evaluator 139, Cranelift 59, Phase Solve 87,
  codegen 99, solver 288, and diffsol 107.

## Current performance evidence

Exact baseline command:

```bash
./target/release/rumoca sim bench \
  /home/jgoppert/git/modelica_models/Vehicles/package.mo \
  --model Vehicles.Rdd2.Test.WaypointMission \
  --source-root /home/jgoppert/git/modelica_models \
  --t-end 0.5 --dt 0.005 --solver rk-like \
  --iterations 10 --warmups 2 --json
```

Latest verified result after `e5ddf351`:

- 101 output points, final time exactly 0.5 s.
- Average `0.151910842 s`.
- Best `0.150515297 s`.
- About 3.32x realtime; still approximately 3.01x slower than the 0.05 s gate.

The committed clock-after-event relation slice has also completed an exact
release canary from the rebuilt binary:

- 101 output points, final time exactly 0.5 s.
- Average `0.1474226518 s`.
- Best `0.146777937 s`.
- About 3.41x realtime and still approximately 2.93x slower than the gate.
- Short-slice native counts remain owner 30 = 143, owner 22 = 107, and owner
  145 = 3, so the improvement is reduced covered assignment work rather than
  a changed call-cache policy.

The schema-59 compact-selection release rebuild is performance-neutral and
preserves the same mission endpoint:

- 101 output points, final time exactly 0.5 s.
- Average `0.149262073 s`.
- Best `0.147334356 s`.
- About 3.39x realtime; the 0.05 s acceptance gate remains open.

Fresh delayed runtime-only perf capture:

- `/tmp/rdd2-schema58-clock-after-event-runtime-only.perf.data`
- 5,863 samples at 499 Hz, zero lost samples.
- 80-run average `0.148941707 s`, best `0.147759861 s`.
- Owner 30: 13.10%.
- Owner 22: 5.89%.
- Assignment schedules 3 and 5: 5.14% and 4.76%.
- `memmove`: 4.35%.

Direct RK timing from a temporary diagnostic build, recorded in
`/tmp/rdd2-rk-breakdown.stderr`:

- 4,555 derivative evaluations: about 28 ms.
- 1,154 root evaluations: about 29 ms.
- 500 legitimate scheduled-event updates: about 89 ms.
- Event update work, not the Kalman owner by itself, is now the largest
  category. The event update body accounts for nearly all of those 89 ms.

Native call counts over a 0.01 s slice:

- owner 30: 143 calls.
- owner 22: 107 calls.
- owner 145 (large estimator `solveSPD`): 3 calls.

The complete estimator/Kalman owner is not the order-of-magnitude gap. The
controller estimator executes at a plausible per-tick cost; duplicated
continuous/event/clock refresh closure around it is the main problem.

## Rejected experiments

Do not repeat these without a new representation:

1. Cranelift `opt_level=speed` instead of `speed_and_size` moved hot time only
   0.64% and increased backend preparation. It was reverted.
2. Keeping every typed scalar register in one giant Cranelift SSA graph caused
   a clean release build to take `14.607840854 s` for the 0.5 s slice, roughly
   100x slower than the control. The earlier neutral measurement was from a
   stale release artifact. It was fully reverted. Compact Solve IR remains the
   correct representation; bounded final tape storage is currently necessary
   until checked native region boundaries constrain SSA live ranges.
3. Merely disabling the inherited causal certificate on a remainder split
   typed owner 22 across staged schedules, raised its short-slice calls from
   107 to 117, and regressed best time to `0.155554216 s`. The committed
   constructor instead filters the already ordered causal schedule and restores
   107 calls and baseline performance.

## Latest completed implementation slice

Commit `95315052` advances the Solve wire schema to 59 and removes cloned row
metadata from every stored causal/stage schedule:

- `RefreshPlan.rows` remains the sole canonical `AlgebraicRefreshRow` catalog.
- `causal_seed_rows`, static/dynamic partitions, exact-assignment stages, and
  projection seeds now store checked ordered `u32` catalog positions in
  `RefreshRowSelection`.
- Construction rejects out-of-range or repeated positions. Outer checked wire
  replay validates every selection and stage before exposing the model.
- Evaluator, solver, native schedule preparation, root planning, and
  continuation use a zero-allocation borrowed `RefreshRows` view.
- Remainder construction filters compact positions and never clones row
  metadata or expression graphs.
- Exact-assignment construction uses temporary row references only; final
  scalar materialization remains at the backend boundary.
- A wire regression proves `[0]` is serialized rather than a duplicated row
  object. The golden schema fixture is updated to 59.

Verification completed:

- Solve IR 172/172 plus three doctests.
- Evaluator 139/139.
- Cranelift 59/59.
- Phase Solve 87/87.
- Production codegen 99/99.
- Solver 288/288.
- Diffsol 107/107.
- Clean release canary: 101 points through exactly 0.5 s, with the timing above.
- `git diff --check` is clean for the committed slice.
- The scheduled R2 review and measurements are in the live roadmap.

Commit `e72f9dd3` immediately before it owns the event-settled clock remainder.
That relation remains construction-issued and wire-replayed; runtime performs
no union, filtering, graph comparison, expansion, or recollapse.

One unrelated uncommitted edit currently exists:

```text
 M crates/rumoca-exec-wasm/src/emit.rs
 M crates/rumoca-phase-codegen/src/templates/embedded-c-galec/model.c.jinja
 M crates/rumoca-phase-codegen/src/templates/embedded-c-galec/model.h.jinja
 M crates/rumoca-phase-codegen/src/templates/galec-production/pc_manifest.xml.jinja
```

These appeared concurrently and were deliberately not staged or edited. The
WASM change adds unsupported typed-pure-call match arms and is the reason
workspace-wide `cargo fmt --all -- --check` currently reports a line-wrap
diff. Preserve and coordinate all four edits rather than folding them into the
RDD2 commits accidentally.

## Remaining architectural work

SOLVE-C56 remains open:

- Add the complete coordinate/invalidation certificate over time, Y/P
  generations, event/pre/previous/history generations, external tables,
  impure state, and arithmetic/AD mode.
- Use those certificates for event/clock and callback reuse. No value hashing,
  tracing, pointer equality, body matching, or runtime schedule discovery.
- Remove the remaining independently reconstructed GALEC path. GALEC should
  consume a strict subset of the same DAE/Solve IR used by Production C.
- Audit standalone `ContinuousRefreshOwners` deserialization: today the outer
  `SolveProblem` replay reconstructs source-backed exact programs, but a
  standalone deserialize can temporarily produce an issued-looking object
  without source-backed programs. Prefer one outer constructor/wire replay that
  makes the invalid intermediate unrepresentable.
- Remove the pre-existing `Arc::ptr_eq` use in Phase Solve if it still survives
  at `lower/call_scoped_actions.rs`; replace it with issued semantic identity.

## Performance next steps

Next:

1. Construct the complete coordinate/invalidation certificate in Solve IR and
   replay it from wire data before authorizing any broader refresh/call reuse.
2. Re-run runtime-only perf with delayed collection. The command pattern is:

```bash
nix shell nixpkgs#linuxPackages.perf -c perf record \
  --delay=6500 -F 999 -g --call-graph dwarf \
  -o /tmp/rdd2-next-runtime-only.perf.data -- \
  env LD_LIBRARY_PATH="$LD_LIBRARY_PATH" \
  ./target/release/rumoca sim bench \
  /home/jgoppert/git/modelica_models/Vehicles/package.mo \
  --model Vehicles.Rdd2.Test.WaypointMission \
  --source-root /home/jgoppert/git/modelica_models \
  --t-end 0.5 --dt 0.005 --solver rk-like \
  --iterations 80 --warmups 2 --json
```

3. Measure derivative/root/event categories separately. Event work must fall
   materially; micro-optimizing `solveSPD` alone cannot reach 0.05 s.
4. Only add a first-class Cholesky/SPD tensor operation through an explicit
   source-issued semantic intrinsic and a spec update. Never recognize
   `solveSPD` by function name or expanded body pattern.
5. Preserve solver and mission semantics for acceptance. The 500 1 kHz event
   boundaries are legitimate and must not be skipped.

## Environment

Pinned tools used in this work:

```bash
export PATH=/nix/store/mhzbjdy4s49yj9rqwfdz5yslhz9bhai3-rust-nightly-2026-02-27/bin:/nix/store/xcnqqnhw9hb4j5rjgds2yjryi8qki5f3-gcc-wrapper-15.2.0/bin:/nix/store/2l3a3fhc1nyp69mlq5ifb4mkb9mryb98-pkg-config-wrapper-0.29.2/bin:$PATH
export LD_LIBRARY_PATH=/nix/store/n35z8vvlr7c5k1406n5bwd0f8h2hgj1j-gcc-15.2.0-lib/lib/lib
export PKG_CONFIG_PATH=/nix/store/jpibpgd3d0ahnn4wibdsscxigl2s0hng-systemd-260.2-dev/lib/pkgconfig
```

For running the release binary, prepend systemd runtime libraries:

```bash
export LD_LIBRARY_PATH=/nix/store/vv5bna641lxwxm0nqgy20134y7wivsvp-systemd-260.2/lib:/nix/store/n35z8vvlr7c5k1406n5bwd0f8h2hgj1j-gcc-15.2.0-lib/lib/lib
```

## End-to-end work still unverified

Do not claim completion until all of these have current evidence:

- 0.5 s slice at or below 0.05 s.
- Full optical-flow mission in pure Rumoca Modelica.
- Full GPS mission in pure Rumoca Modelica.
- Trace comparison against equivalent OMC runs using the `cargo xtask repo msl`
  comparison tooling.
- Shared Production C and GALEC lowering from the checked Solve/DAE owner.
- `cerebri_rdd2` firmware build.
- eFMU code generation.
- Firmware/eFMU simulations completing the same GPS and optical missions with
  trace parity.
