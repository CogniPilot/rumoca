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

Fresh delayed runtime-only perf capture:

- `/tmp/rdd2-schema58-runtime-only.perf.data`
- 11,864 samples, zero lost samples.
- 80-run average `0.152721013 s`, best `0.151042603 s`.
- Owner 30: 13.13%.
- Owner 22: 6.00%.
- Assignment schedules 3 and 5: 5.18% and 4.29%.
- `memmove`: 4.26%.

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

## Current uncommitted worktree

There is one intentionally incomplete, untested edit:

```text
 M crates/rumoca-ir-solve/src/refresh.rs
```

It adds 31 lines beginning a construction-issued `clock_events_after_event`
relation inventory:

- a skipped/private `Vec<RefreshRemainderRelation>` field;
- issuance of one clock-plan remainder after the event plan;
- unique sequence IDs after the normal clock owners;
- inclusion in exact-assignment schedule rebuilding;
- a read-only accessor;
- `is_issued` checks relation cardinality.

This partial edit has not been compiled or tested. Runtime has not been wired
to consume the relations, and tests have not been added. Inspect it with:

```bash
git diff -- crates/rumoca-ir-solve/src/refresh.rs
```

Do not commit it as-is. The intended completion is:

1. Add wire-replay/cardinality and coverage tests in
   `crates/rumoca-ir-solve/src/refresh/tests.rs`.
2. Add a matching runtime field in
   `crates/rumoca-solver/src/runtime/solve_runtime.rs` and clone the issued
   relations during `SolveRuntime` construction.
3. In `refresh_event_dependency_slots_certified`, execute each active clock's
   `clock_events_after_event[clock].remainder()` after the event plan instead of
   its complete clock plan. There is no mutation between these two refreshes,
   so this is the first safe same-coordinate reuse boundary.
4. Fail closed if relation count and clock plan count differ.
5. Run the focused/full gates, exact canary, native call counts, and delayed
   runtime-only perf. Revert if call count or hot time regresses.

The next extension should cover earlier active clocks as well, but only with a
construction-issued ordered coverage relation. Do not dynamically union plans
or filter rows at runtime.

## Remaining architectural work

SOLVE-C56 remains open:

- Replace cloned row metadata in `RefreshPlan` and `RefreshStage` with compact,
  construction-issued selections into a canonical row catalog.
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

After completing the clock-after-event relation:

1. Re-run runtime-only perf with delayed collection. The command pattern is:

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

2. Measure derivative/root/event categories separately. Event work must fall
   materially; micro-optimizing `solveSPD` alone cannot reach 0.05 s.
3. Only add a first-class Cholesky/SPD tensor operation through an explicit
   source-issued semantic intrinsic and a spec update. Never recognize
   `solveSPD` by function name or expanded body pattern.
4. Preserve solver and mission semantics for acceptance. The 500 1 kHz event
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

