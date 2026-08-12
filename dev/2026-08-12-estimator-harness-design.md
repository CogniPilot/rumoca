# Algorithm-neutral estimator comparison harness — design

Date: 2026-08-12
Author: Claude (design task #34, doc only — no model or code changes)
Status: PROPOSED — for James's review; implementation gated on slice plan §9
Scope: `modelica_models` (flight models repo) + rumoca GALEC measurement
tooling. This document changes nothing; it specifies what the slices build.

Authority: James's estimator direction as recorded in the coordination
mailbox (`dev/2026-08-11-rdd2-agent-handoff.md`, 2026-08-12 08:36 EDT
Codex → Claude, ACK'd 08:45 EDT). Binding requirements restated in §1.

---

## 1. Binding requirements (verbatim intent)

1. `Avionics.PartialNavigationEstimator` stays the common interface;
   every candidate implements it (directly or via the instrumented
   extension in §3). Honest naming: the current estimator is a
   **geometric error-state EKF** — no IEKF/invariant/log-linear claims.
2. OUTSIDE each filter: timestamp normalization, bounded
   measurement/history handling, current-time output prediction,
   mission inputs, and scoring.
3. Candidates compete on **identical traces**, scored on: accuracy,
   NEES/NIS consistency, delay/dropout/outlier recovery, rejection
   behavior, CPU, peak memory, generated-C size, mission completion.
   Selection **never by filter label**. Any further geometric change is
   proposed separately and must **win the same discriminator**.
4. Adopt-first (EKF-flavor-independent, clean-room from official EKF3
   documentation only — the GPL implementation is never read or
   copied): bounded IMU/observation buffers sized from the maximum
   configured sensor delay; per-observation sample-time estimation;
   delayed fusion horizon recall; a separate current-time inertial
   output whose corrections track the delayed filter.
5. Candidate set: (C1) current geometric error-state EKF; (C2) an
   EKF3-style delayed-filter/output-predictor candidate (bounded
   embedded cost); (C3) a deterministic fixed-lag/replay exact
   reference. Cyecca `rdd2.py` is a **complexity floor only** (per the
   08:36 classification: strapdown + simple GPS correction +
   complementary attitude; no bias EKF, no delayed fusion, no output
   predictor) — never an algorithmic oracle.

## 2. Current architecture (as found, 2026-08-12)

Models repo `/home/jgoppert/git/modelica_models`, branch
`estimator-numerical-health` (the parallel numerical-health stream is
actively editing `Avionics/package.mo` start-values,
`Estimation/MultiSensorInvariant/{State,initialize,predict,package}.mo`
and `Estimation/MixedInvariantNavigationEstimator.mo` uncommitted —
see ownership consequences in §9).

- **Interface** — `Avionics/package.mo:99` `partial block
  PartialNavigationEstimator`: parameter `samplePeriod` (flight value
  0.005 s = 200 Hz, `Vehicles/Rdd2/WaypointVehicleSystem.mo:46-48`),
  `input Boolean reset`, record connectors `imu` (`ImuSample`), `mocap`
  (`MocapSample`), `gps` (`GpsSample`), `opticalFlow`
  (`OpticalFlowSample`), discrete outputs `estimate`
  (`NavigationEstimate`) and `status` (`EstimatorStatus`, six Booleans).
  All four sensor records already carry `valid`, `fresh`,
  `timestamp_s` — **timestamped measurements need no record change**.
  Documented contract: covariance/bias/tangent ordering stay private to
  the filter.
- **Sole implementor** —
  `Estimation/MixedInvariantNavigationEstimator.mo`: one
  `when sample(0.0, samplePeriod)` clause calling the pure function
  `Estimation.MultiSensorInvariant.step(...)` over discrete states
  (15×15 covariance, quaternion, biases). Timestamps are *carried but
  fused at the current tick* — no history, no delayed horizon
  (confirmed by the 08:36 message and by `step.mo`). Honest naming
  already applied in its doc string ("geometric error-state EKF").
  `Vehicles/Rdd2/NavigationEstimator.mo` is the eFMU-export alias.
- **Mission stack** — `Vehicles.Rdd2.WaypointVehicleSystem` hard-codes
  `Vehicles.Rdd2.NavigationEstimator estimator(...)` (line 46) fed by
  *ideal, zero-delay* truth-derived sensors (`timestamp_s = time`,
  lines 94-131); `navigationSource ∈ {0 truth, 1 GPS, 2 optical}`.
  Missions: `Vehicles/Rdd2/Test/WaypointMission.mo` (45 s, dt 0.005,
  optical/GPS-denied box) and `GlobalWaypointMission.mo` (GPS box),
  driven by `rumoca-scenario.*.toml` + `run_waypoint_qualification.py`
  (corner-capture scoring, artifact hashing).
- **Measurement tooling already proven** (flight assessment,
  `/home/jgoppert/.claude/jobs/de80c98d/tmp/flight/`): `rumoca compile
  --target galec-production` per block; host + `arm-zephyr-eabi-gcc
  -mcpu=cortex-m7 -Os -fstack-usage` compile; `size` for text bytes;
  `.su` worst-chain stack; `bench_dostep.c` host micro-benchmark of the
  generated `dostep`. Generated state structs are **`float`
  (binary32)**.
- **Known numerical-health facts the harness must surface, not hide**
  (B-3, mailbox 09:00): unaided binary32 propagation goes non-SPD after
  ~100 s with silent permanent correction rejection. The parallel
  health stream owns the fix *inside* the filter; §3.3 reserves the
  interface fields its status bits need.

## 3. Modelica interface contract

### 3.1 Principles

- `PartialNavigationEstimator` is **unchanged** — today's flight models
  (`NavigationEstimator`, its eFMU manifest, controller wiring) compile
  and fly exactly as before. Backward compatibility is achieved by
  *extension*, not modification.
- New capability is added in a derived partial that candidates in the
  comparison implement. Consumers that only need `estimate`/`status`
  keep depending on the base partial.

### 3.2 New records (new file — see ownership §9; promoted into
`Avionics` only in the final integration slice)

```modelica
record FilterConsistency
  "Per-tick quantities the scoring blocks need; algorithm-neutral"
  Boolean fusionActive          "True on ticks where any correction was applied";
  Real fusionHorizon_s          "Timestamp the filter state refers to (delayed filters: horizon; current-tick filters: now)";
  // NEES support: marginal covariance of the OUTPUT-space error
  // {position ENU (3), velocity ENU (3), attitude small-angle body (3)}.
  // This is defined on the common output space, so every candidate can
  // produce it regardless of internal parameterization/tangent ordering.
  Real outputCovariance[9, 9];
  // NIS support: last innovation per aiding channel, padded to the
  // channel's fixed maximum dimension, plus its innovation covariance
  // and the active dimension (0 = no fusion this tick).
  Integer mocapInnovationDim(min = 0, max = 6);
  Real mocapInnovation[6];
  Real mocapInnovationCovariance[6, 6];
  Integer gpsInnovationDim(min = 0, max = 6);
  Real gpsInnovation[6];
  Real gpsInnovationCovariance[6, 6];
  Integer opticalFlowInnovationDim(min = 0, max = 2);
  Real opticalFlowInnovation[2];
  Real opticalFlowInnovationCovariance[2, 2];
end FilterConsistency;

record EstimatorHealth
  "Reserved for the estimator-numerical-health stream (B-3); the
   harness consumes it read-only for the rejection/recovery metrics"
  Boolean covarianceHealthy      "False once the SPD solve degrades";
  Boolean varianceLimited        "Variance clamp engaged this tick";
  Integer consecutiveRejections[3] "{mocap, gps, opticalFlow}";
  Integer reinitCount            "Auto-reinitializations since start";
  Real innovationGateValue[3]    "Latest chi-square NIS per channel";
  Integer lastErrorCode          "0 = nominal; nonzero = solve/limit fault";
end EstimatorHealth;
```

Field list to be reconciled with whatever the health stream actually
lands; the harness only *requires* `covarianceHealthy`,
`consecutiveRejections`, `reinitCount` (metric M6). If their branch
names differ, the wrapper (§3.4) adapts — the record above is the
harness-side contract, not a constraint on their internals.

### 3.3 The instrumented partial

```modelica
partial block PartialInstrumentedNavigationEstimator
  "Comparison-grade estimator: base contract + delayed/current pair +
   consistency + health. Extends, never replaces, the flight contract."
  extends Avionics.PartialNavigationEstimator;   // MLS 3.7 §7.1
  parameter Real maxSensorDelay_s(min = 0.0) = 0.5
    "Upper bound on any aiding-sensor latency; sizes all history buffers";
  // The delayed/current-time output pair. `estimate` (inherited)
  // REMAINS the current-time output consumed by guidance — backward
  // compatible by construction. `delayedEstimate` is the filter's
  // fusion-horizon solution (for current-tick filters the two are
  // identical and horizonLag_s = 0).
  discrete Avionics.NavigationEstimateOutput delayedEstimate;
  output Real horizonLag_s "time - fusionHorizon_s, diagnostic";
  discrete output FilterConsistency consistency;
  discrete output EstimatorHealth health;
end PartialInstrumentedNavigationEstimator;
```

Rules:

- Candidates implement the instrumented partial. The flight stack keeps
  instantiating plain `PartialNavigationEstimator` implementors; when a
  comparison winner is promoted, its instrumented block *is already* a
  `PartialNavigationEstimator`, so promotion is a one-line component
  swap with zero controller changes (exactly the property
  `Vehicles/Rdd2/NavigationEstimator.mo` documents).
- `estimate.timestamp_s` stays "now"; `delayedEstimate.timestamp_s`
  carries the fusion horizon. No consumer change.
- The `outputCovariance` 9×9 block is the only covariance a candidate
  must expose; internal state dimension (15, 24, …) stays private, per
  the existing `Avionics` documentation contract.

### 3.4 Adapting the current filter (candidate C1)

A new wrapper block
(`EstimatorHarness.Candidates.GeometricEskfCurrentTick`) extends the
instrumented partial and instantiates/duplicates the existing
`MixedInvariantNavigationEstimator` behavior by calling the same
`Estimation.MultiSensorInvariant.step` function — internals stay a
black box. It needs two small *new* pure functions in
`Estimation.MultiSensorInvariant` (new files, additive):
`outputCovariance9(covariance, quaternion) → Real[9,9]` (projects the
15×15 tangent covariance onto the output space) and
`lastInnovations(...)` exposure from `step`'s correction path — the
only internals-adjacent work, sequenced AFTER the health branch lands
(§9, S5). Until then C1 competes with `outputCovariance = 0` and is
simply excluded from NEES/NIS scoring (flagged, not failed).

## 4. Harness architecture — the outside-the-filter blocks

New top-level package `EstimatorHarness` (own directory; zero collision
with `Avionics/`, `Estimation/`, `Vehicles/` — §9). All blocks are
plain Modelica, fixed-size, GALEC-compatible: sizes are `parameter
Integer` fixed at translation, no dynamic allocation, no `delay()`.

```
sensors ──► TimestampNormalizer ──► FaultInjector ──► MeasurementBuffer ──► candidate filter ──► OutputPredictor ──► estimate (current time)
                                        │                                      ▲       │
truth ──────────────────────────────────┼──────────────────────────────────────┼───────┼──► Scoring taps
                                        └────────────► ImuHistoryBuffer ───────┘       └──► delayedEstimate, consistency, health
```

### 4.1 `TimestampNormalizer` (one per aiding stream)

Maps raw sample timestamps into the estimator timebase; implements the
EKF3-style **per-observation sample-time estimation** (clean-room, from
the official EKF3 documentation of delayed-fusion timing):

- If `sample.timestamp_s` is plausible (within
  `[time - maxSensorDelay_s, time]`), pass through.
- Maintain a low-pass estimate of the stream's arrival lag
  `lagHat := lagHat + alpha * ((time - timestamp_s) - lagHat)` on fresh
  samples; when a timestamp is missing/implausible, substitute
  `time - max(0, min(lagHat, maxSensorDelay_s))`.
- Outputs the normalized record plus `sampleTime_s` (the estimated
  true measurement epoch) and a `substituted` flag (scored under M6).

Parameters: `nominalDelay_s` (initial `lagHat`), `alpha`,
`maxSensorDelay_s`. Discrete states: `lagHat`. Pure `when`-clause
logic; MLS §8.3.5 / §3.8.5 (`sample`, `pre`, `edge`).

### 4.2 `MeasurementBuffer` (bounded observation ring, one per stream)

- Depth fixed at translation:
  `parameter Integer depth = integer(ceil(maxStreamDelay_s / streamPeriod_s)) + 1;`
  sized **per stream at the stream's own rate**, not the 200 Hz tick
  (e.g. GPS 10 Hz × 0.5 s → 6 slots; mocap 100 Hz × 50 ms → 6; flow
  50 Hz × 40 ms → 3). Structural parameters are evaluated at
  instantiate and frozen into literal dims (GAL-020) — legal for GALEC
  static allocation, but note the consequence: `maxSensorDelay_s`
  stays manifest-tunable while the buffer *size* is translation-frozen,
  so tuning it above the translated bound is clamped (asserted in S2).
- Storage is **struct-of-arrays** (parallel fixed-size arrays per record
  field, e.g. `Real gpsPosition[depth, 3]`), NOT arrays of records —
  mandatory, not just cautious: GALEC hard-rejects record state
  compartments (`ensure_c_exportable`, §10 G2). Pack/unpack helpers
  must be **multi-output array functions** (`(p, v, t) := recall(...)`,
  supported via projection inlining) — never record-returning functions
  and never `out[i] := ...` targets, which GALEC's function inliner
  rejects (§10 G5).
- Ring index: `discrete Integer head(start = 1, min = 1, max = depth)`;
  advance with `head := if pre(head) >= depth then 1 else pre(head) + 1;`
  — **not** `mod()`, which GALEC rejects as `builtin:mod` (ET017,
  SPEC_0034 trap T8). The `min`/`max` bounds are load-bearing: GALEC
  lowers a runtime subscript to an if-chain over the provable index
  range and rejects unbounded indices, so every buffer index variable
  carries tight declared bounds, and small depths (single digits, per
  the per-stream sizing above) keep the emitted select-chains from
  bloating M9 C-size. Recall is a bounded linear scan (`for` over
  `1:depth` with static-bound body) for the newest entry with
  `sampleTime_s <= fusionHorizon_s`.
- Popped-flag per slot prevents double fusion (EKF3 "recall once when
  the horizon reaches the sample time" semantics).

### 4.3 `ImuHistoryBuffer`

- Accumulates IMU into **downsampled horizon-rate frames** (EKF3
  pattern): integrate delta-angle/delta-velocity at 200 Hz into frames
  of `fusionInterval_s` (parameter, default 0.02 s), store frames in a
  ring of
  `parameter Integer frames = integer(ceil(maxSensorDelay_s / fusionInterval_s)) + 1;`
  (0.5 s / 0.02 s + 1 = 26 frames × 10 floats ≈ 1 KB f32). Frame
  count is the largest index range in the harness; the S2 canary
  measures the select-chain C-size cost at 26 and the budget table
  fixes the final `fusionInterval_s` trade (§7).
- Consumers: C2's delayed filter (predicts at horizon rate from
  recalled frames), C3's replay (re-integrates from a snapshot), and
  the `OutputPredictor`.

### 4.4 `OutputPredictor` (current-time output, common to all candidates)

Separate inertial mechanization that propagates `delayedEstimate` to
`time` using the buffered IMU frames newer than the horizon, then
applies a complementary correction so it **tracks the delayed filter**
(EKF3 output-predictor pattern, flavor-independent):

- State: current-time position/velocity/quaternion.
- Each tick: strapdown-propagate with the newest IMU sample; compute
  the tracking error between its horizon-delayed copy and
  `delayedEstimate`; feed back
  `correction = trackingError / timeConstant_s` (parameter, default
  ~0.25 s) into the current-time state.
- For `horizonLag_s = 0` candidates (C1) it degenerates to
  pass-through (tracking error 0 after transient) — one code path, no
  special case.
- The harness, not the filter, publishes `estimate` from this block —
  requirement "current-time output prediction outside each filter."
  Inside the *flight* build the winning candidate + its predictor are
  exported as one composed block, so the eFMU boundary is unchanged.

### 4.5 `FaultInjector` (scenario scripting)

Deterministic transformation of normalized streams from a fixed-size
parameter table (rows padded; `parameter Integer maxEvents = 16`):

```modelica
parameter Real eventTable[maxEvents, 6];
// columns: {startTime_s, duration_s, streamId, kind, p1, p2}
// kind: 1 delay (p1 = added latency s), 2 dropout (fresh := false),
//       3 outlier (p1 = bias magnitude, p2 = axis mask),
//       4 covariance-lie (p1 = scale on reported covariance)
```

Runs *after* the normalizer taps the raw stream and *before* buffering,
so every candidate sees byte-identical corrupted inputs. Pure function
of `(time, eventTable, sample)` — no RNG at runtime; "random" outliers
are pre-drawn into the table by the scenario generator (§5.3) so runs
are reproducible bit-for-bit.

### 4.6 `ReplayReference` mechanization (candidate C3)

The exact fixed-lag reference, itself behind the instrumented partial:

- Snapshot ring of full filter states (nominal + covariance) at
  horizon rate: `frames` deep (§4.3 sizing).
- On recall of a measurement with `sampleTime_s = τ`: rewind to the
  newest snapshot ≤ τ, apply the correction there, then **replay** the
  stored IMU frames (and any already-fused newer measurements, in
  stored order) forward to `time`. Deterministic: replay order is the
  buffer order, which is arrival order, which is fixed by the trace.
- Cost is O(frames) state copies + refilters per late measurement —
  acceptable in simulation, *disqualified from flight by construction*
  (it still gets CPU/memory/C-size measured, as the upper bound that
  makes the other candidates' costs interpretable).
- Its posterior is the "exact" answer the bounded C2 approximates;
  metric M1/M2 for C2 are additionally reported *relative to C3*.

### 4.7 `Scoring` taps

In-model blocks record per-tick raw quantities as outputs (they do NOT
compute verdicts — verdict math lives in the Python scorer §6.3, so
scoring changes never touch generated flight-adjacent code):

- `truthError[9]` (position/velocity/attitude small-angle vs plant
  truth), for both `estimate` and `delayedEstimate`.
- Pass-through of `consistency` and `health` records.
- `ScenarioClock`: exports the event table + phase markers so the
  scorer can window metrics per fault event without re-parsing TOMLs.

## 5. Trace protocol — identical inputs, guaranteed

### 5.1 Two-tier scoring split (honest about closed-loop feedback)

- **Tier A, open-loop, identical traces (metrics M1–M6):** sensor
  streams are *recorded once* and replayed to all candidates. This is
  the only way "identical traces" is literally true — in closed loop
  each candidate would fly a different trajectory and see different
  sensors.
- **Tier B, closed-loop, identical missions (metrics M7–M10):** each
  candidate flies `WaypointMission` + `GlobalWaypointMission` in the
  loop. Inputs are identical *mission definitions and initial
  conditions*, not identical sensor streams; only mission-completion
  and whole-system metrics are scored here.

### 5.2 Trace sources

1. **Recorded nominal missions:** one canonical run each of
   `WaypointMission` (optical, GPS-denied) and `GlobalWaypointMission`
   (GPS-aided) with the *current flight estimator* in the loop, 45 s,
   dt 0.005, recording truth + all four raw sensor streams to CSV
   (extends the existing `[sim] output` scenario mechanism,
   `Vehicles/Rdd2/Test/rumoca-scenario.*.toml`). The recording's SHA-256
   is pinned in the comparison scenario files
   (`run_waypoint_qualification.py` already hashes artifacts — reuse
   that code path).
2. **Synthetic fault scenarios:** the recorded traces replayed through
   `FaultInjector` tables: (a) constant GPS delay sweep 0/50/100/250/
   500 ms; (b) per-stream dropouts 0.5 s/2 s/10 s incl. the B-3 cliff
   probe (unaided ≥ 120 s synthetic extension); (c) position outliers
   5σ/20σ/100σ, single and burst; (d) covariance-lie ×0.1/×10;
   (e) combined "bad day" script. Tables are committed Modelica
   parameter files — no runtime randomness (§4.5).
3. **Flight logs (post-flight):** today's flight logs, mapped into the
   same CSV schema by a converter script; enter the corpus as
   additional Tier-A traces once available. (Open question Q3 —
   truth source for flight logs is mocap/OMC only.)

### 5.3 Replay mechanics (two equivalent paths, both specified)

- **Path 1 — in-model replay (preferred if G1 in §10 is green):** a
  `TracePlayer` block sources the recorded CSV via rumoca's table
  runtime as time-indexed sample-and-hold streams; one comparison model
  (`EstimatorHarness.Comparisons.SideBySide`) instantiates the player
  once and ALL candidates in parallel, so identity of inputs is
  structural (same connector fan-out), and one simulation yields all
  candidates' outputs on one time base. Candidate slots use
  `replaceable`/`redeclare` — verified fully supported (resolved at
  instantiate, transparent to the DAE and GALEC; §10 G7) — so
  `SideBySide` is ONE model with three constrained-by
  `PartialInstrumentedNavigationEstimator` slots. Sim-only: this model
  is never GALEC-exported (multi-instance is fine in sim; export
  admissibility constraints in §7 apply only to per-candidate export
  wrappers).
- **Path 2 — generated-C replay (no compiler-feature dependency;
  always built as it doubles as the CPU bench):** a host runner in the
  spirit of the flight assessment's `bench_dostep.c` feeds the CSV
  rows into each candidate's `galec-production` C at 200 Hz and writes
  the identical output schema. Identity of inputs is by construction
  (same file, same reader). This path is also the *authoritative* one
  for CPU/memory/C-size (§6.2) and validates GALEC/binary32 behavior —
  Tier A is scored on Path 2 outputs; Path 1 is the modeling/debug
  view and a Path1-vs-Path2 divergence is itself a finding (f64 sim vs
  binary32 flight, the B-3 lesson).

### 5.4 Determinism rules

- One `samplePeriod` for all candidates (0.005 s); all harness blocks
  tick on the same `sample(0, samplePeriod)` clock; no candidate may
  read `time` except through its inputs (enforced by review checklist —
  compiler can't enforce it).
- No RNG anywhere at runtime; all stochastic content is pre-drawn into
  committed tables with recorded seeds.
- Trace hash + scenario-table hash + rumoca version + model git rev are
  embedded in every score report row.

## 6. Scoring rubric

### 6.1 Metric definitions (exact)

Scoring window W = [5 s, end] for nominal traces (excludes convergence
transient); per-event windows for fault scenarios. e_p, e_v, e_θ are
position error [m], velocity error [m/s], attitude geodesic angle
error [rad] of the **current-time** `estimate` vs truth;
delayed-output errors reported separately at the horizon timestamp.

- **M1 Accuracy:** RMSE and max-|·| of e_p, e_v, e_θ over W, per trace.
  For C2 additionally vs C3's posterior (approximation error of the
  bounded design).
- **M2 NEES consistency:** ε_k = e_k' P_k⁻¹ e_k with e_k the 9-dim
  output error and P_k = `consistency.outputCovariance`. Report
  mean(ε)/9 and the fraction of ticks inside the two-sided 95% χ²₉
  band [2.700, 19.023]. Target: fraction ≥ 0.9, mean(ε)/9 ∈ [0.5, 2].
- **M3 NIS consistency:** per accepted fusion of channel c with dim m:
  ν' S⁻¹ ν from `consistency.*Innovation*`. Same band bookkeeping per
  channel with χ²_m. (This is why innovation + S are interface
  outputs, §3.2.)
- **M4 Delay/dropout/outlier recovery:** for each scripted event,
  recovery time t_rec = first t after event end such that e_p stays
  within 2× its pre-event RMSE for 1.0 s continuously; DIVERGED if
  never, or if e_p > 10 m, or quaternion norm error > 1e-3.
- **M5 Rejection behavior:** false-reject rate on clean segments
  (target < 1%: fusions rejected / offered), missed-accept rate on
  injected ≥ 20σ outliers (target 0 accepted), and **no-lockout**: max
  `health.consecutiveRejections` bounded and `health.reinitCount`
  recovers fusion within 5 s (the B-3 silent-forever-rejection class is
  an automatic gate failure).
- **M6 Timestamp robustness:** M1/M2 deltas between the
  clean-timestamp and substituted-timestamp (normalizer `substituted`)
  variants of the same trace.
- **M7 CPU:** median and p99.9 of per-tick `dostep` wall time over a
  full Tier-A trace, host (`bench_dostep` methodology, -O2) — plus the
  flight assessment's M7 extrapolation factor for the 480 MHz M7.
  Gate: extrapolated worst tick ≤ 20% of the 5 ms tick (matches the
  measured 10–25% envelope of the current filter; budget shared with
  guidance on the thread).
- **M8 Peak memory:** static: `sizeof` of the generated state struct
  (binary32); stack: `-fstack-usage` worst chain through `dostep` on
  arm-zephyr-eabi at -Os (the assessment's `compile_metrics.sh` +
  `su_summary.sh` scripts, promoted into the repo per §9 S7). Gate:
  state + worst stack ≤ 32 KiB thread budget (the refiled B-1 value).
- **M9 Generated-C size:** `.text` bytes from `arm-zephyr-eabi-size`
  at -Os and LOC of the `galec-production` ProductionCode C file.
  Floor context: cyecca `rdd2.py`-equivalent footprint reported
  alongside as the complexity floor; C3's footprint as the ceiling.
- **M10 Mission completion:** Tier-B closed-loop pass/fail of the
  existing corner-capture criteria in `run_waypoint_qualification.py`
  for both missions, plus max track error and the existing
  local-vs-global track agreement check.

### 6.2 Measurement pipeline per candidate (all through existing GALEC)

For each candidate block B (all four measured identically; C3 included
for bounds even though flight-disqualified):

1. `rumoca compile <root> --model EstimatorHarness.Candidates.B
   --source-root <models> --target galec-production` (and
   `embedded-c-galec` for the plain-C track) — the exact flight
   assessment invocation. `galec-production` emits the eFMU container
   (`__content.xml`, `AlgorithmCode/*.alg` + manifest,
   `ProductionCode/*.c/.h` + manifest, `.efmu` zip;
   `crates/rumoca-phase-codegen/src/templates/galec-production/target.toml`).
   GALEC itself emits **no size/CPU report** — measurement is external
   (steps 2–3), following the in-repo driver pattern of
   `crates/rumoca/tests/cli_target_galec_production.rs` (`DRIVER_MAIN`)
   and `galec_equivalence.rs`.
2. Host + ARM compile with the strict preflight flag set
   (`-std=c99 -pedantic -Wall -Wextra -Wconversion …`), `-Os`,
   `-fstack-usage`; artifacts: `.o`, `.su`, `size` output → M8/M9.
3. Path-2 replay runner links the host `.o` → Tier-A outputs + M7
   timing in one run.
4. Scorer (§6.3) consumes Tier-A outputs + Tier-B qualification JSON.

### 6.3 Scorer and verdict

`EstimatorHarness/tools/score_estimators.py` (new; imports the
corner/plot helpers rather than editing
`run_waypoint_qualification.py`): computes M1–M10, emits one JSON +
HTML report with **candidates identified by content hash first, label
second** — the report generator literally sorts and gates on
metrics before it prints names (mechanical enforcement of
"never select by filter label").

Verdict procedure: hard gates first (M4 no-divergence, M5 no-lockout,
M7/M8 budgets, M10 both missions) → surviving candidates ranked by a
declared weight vector over normalized M1–M3, M4 recovery times, M9
(weights proposed in the report, chosen by James, recorded in the
ledger — not auto-decided). A geometric-change proposal enters as a
new candidate hash and must strictly improve the gated-then-ranked
outcome to displace the incumbent.

## 7. Embedded constraints (binding on C1/C2 candidate design)

- **binary32 end-to-end** in GALEC: all thresholds/tolerances in
  candidates and harness blocks must be parameterized against machine
  epsilon (the B-3 lesson: no literal 1e-12-class tolerances; the
  precision-parameterized solveSPD from the health stream is the
  pattern). Tier-A Path-2 scoring runs the real binary32 artifact, so
  f32 cliffs are *scored*, not discovered in flight.
- **Fixed memory:** every array dimension is a `parameter Integer`
  frozen at translation (GAL-020); no `delay()` (GALEC-unsupported and
  sim-approximated to `pre()` anyway, §10 G6), no dynamic structures;
  buffer depths per stream at stream rate as in §4.2/§4.3 (GPS 6,
  mocap 6, flow 3 slots; IMU 26 frames ≈ 1 KB f32). Slot layouts and
  final budgets are an S2 deliverable with a hard ceiling: all harness
  buffers + C2 state ≤ 8 KiB f32 static.
- **200 Hz tick:** harness blocks (normalizer, buffers, predictor,
  injector) must together cost < 5% of the tick so they don't distort
  M7 across candidates; measured in S7 as a harness-only baseline
  model.
- **GALEC export admissibility** (verified constraints, SPEC_0034):
  exactly ONE `sample(0.0, period)` clock per exported block — GALEC
  rejects multi-rate (`ClockCountNotOne`/ET005, GAL-016) and non-zero
  clock phase; the C exposes only `Startup`/`Recalibrate`/`DoStep`
  with the host scheduling ticks (GAL-017). The composed export
  wrapper (candidate + normalizers + buffers + predictor) therefore
  shares the single 200 Hz clock, with sensor-rate behavior expressed
  by `fresh`-gating, exactly as the current flight models do. No
  `mod`/`rem`, no `while`, no record-returning functions, no
  subscripted function-output targets, all runtime array indices
  carry declared `min`/`max` bounds (§10).
- **No heap, no recursion** per candidate export — matches the current
  eFMU export properties.

## 8. Candidate set (honest names, fixed)

| ID | Block (planned) | Description | Role |
|----|-----------------|-------------|------|
| C1 | `Candidates.GeometricEskfCurrentTick` | today's geometric error-state EKF (SE_2(3) propagation/injection, Joseph update, reset Jacobian), current-tick fusion, wrapped per §3.4 | incumbent |
| C2 | `Candidates.DelayedEskfOutputPredictor` | clean-room EKF3-*style* delayed-horizon error-state EKF: fuses recalled observations at the horizon (§4.2/§4.3 buffers), current-time via §4.4 predictor; bounded cost | embedded challenger |
| C3 | `Candidates.FixedLagReplayReference` | deterministic rewind-and-replay exact fixed-lag smoother (§4.6) | exact reference, flight-disqualified |

Clean-room rule (restated as a work instruction for S4): C2 is derived
only from the official EKF3 documentation set (algorithm descriptions,
equations, tuning docs); no GPL source is opened; the S4 commit message
records the exact document list. C2 reuses the *harness* buffers — the
adopt-first latency architecture lives outside the filter and is
therefore shared with any future candidate, which is the point.

## 9. Implementation slices (disjoint file ownership)

Ordering constraint: the models repo working tree currently carries the
uncommitted `estimator-numerical-health` stream touching
`Avionics/package.mo` + `Estimation/MultiSensorInvariant/*` +
`MixedInvariantNavigationEstimator.mo`. Slices S1–S4, S6–S7 are
**disjoint from those paths by construction** (new `EstimatorHarness/`
directory + new tools files only). Only S5 (C1 wrapper exposure
functions) and S8 (optional promotion of records into `Avionics`)
touch their area, and both are sequenced after that branch lands.

| Slice | Content | Owns (exclusively) | Depends on |
|-------|---------|--------------------|------------|
| S1 | `EstimatorHarness` package skeleton: `FilterConsistency`, `EstimatorHealth`, `PartialInstrumentedNavigationEstimator`, package.order | `EstimatorHarness/package.mo`, `EstimatorHarness/Interfaces/` | — |
| S2 | Normalizer, MeasurementBuffer, ImuHistoryBuffer, FaultInjector, OutputPredictor + Modelica unit tests (Cubs2EstimatorTests style scripted asserts) + buffer budget table + GALEC canary model (select-chain C-size at depth 26, bounds-proof idioms) | `EstimatorHarness/Blocks/`, `EstimatorHarness/Tests/BlocksTests.mo` | S1; register rows G2–G5, G8, G9 |
| S3 | C3 ReplayReference | `EstimatorHarness/Candidates/FixedLagReplayReference.mo` | S1, S2 |
| S4 | C2 DelayedEskf (clean-room; doc list in commit) | `EstimatorHarness/Candidates/DelayedEskfOutputPredictor/` | S1, S2 |
| S5 | C1 wrapper + `outputCovariance9`/`lastInnovations` exposure functions | `EstimatorHarness/Candidates/GeometricEskfCurrentTick.mo`, new files under `Estimation/MultiSensorInvariant/` + its `package.order` line | S1; **after estimator-numerical-health merges** |
| S6 | Trace recorder scenarios, committed fault tables, `SideBySide` comparison model, trace hashes | `EstimatorHarness/Comparisons/`, `EstimatorHarness/Scenarios/` | S2; G1 decides Path 1 inclusion |
| S7 | Path-2 replay runner C + measurement scripts (promote `generate.sh`/`compile_metrics.sh`/`bench_dostep.c` patterns into the repo) + scorer | `EstimatorHarness/tools/` (runner.c, measure.sh, score_estimators.py) | S1 headers; candidates as they land |
| S8 | First full comparison report; optional promotion of §3.2 records into `Avionics/package.mo`; ledger update | report artifact; `Avionics/package.mo` (only here) | all; James's weight vector |

Each slice ends green-and-committed per the campaign protocol. Harness
tests live inside `EstimatorHarness/Tests/` — deliberately NOT in the
top-level `Tests/` package, so `Tests/package.order` ownership stays
with the general models corpus and no slice touches it.

## 10. MLS constructs used and rumoca feature gaps (explicit dependencies)

MLS 3.7 constructs the design uses: partial blocks + `extends`
(§4.5.2, §7.1); `replaceable`/`redeclare` with `constrainedby`
(§7.3) for the sim-only `SideBySide` candidate slots; records as
connector types (§9.1, existing pattern); `when sample(t0, p)`
discrete clauses with `pre()` (§8.3.5, §3.8.5, §8.5); pure functions
with multi-output array I/O (§12); parameter-sized array dimensions
folded as structural parameters; bounded `for` scans and `if`
wrap-around updates (§11); `assert` in scripted tests (§8.3.7).
Deliberately NOT used anywhere on a GALEC-export path: `mod`/`rem`,
`delay()`, `while`, record-returning or subscripted-target functions,
arrays of records as states, multi-rate clocks, non-zero clock phase,
clocked synchronous elements (§16) — each per the verified register
below. Modelica has no function pointers; candidate swapping is class
parameterization (redeclare) in sim and one thin export wrapper model
per candidate for GALEC.

Feature register — statuses **verified in the rumoca worktree
2026-08-12** (evidence paths relative to the rumoca repo; re-verify at
each slice start):

| # | Construct | Status in sim pipeline | Status in GALEC production-C | Consequence for this design |
|---|-----------|------------------------|------------------------------|------------------------------|
| G1 | Table/CSV-backed `TracePlayer` source | table runtime exists (`crates/rumoca-eval-solve/src/table_runtime.rs`) | not applicable (Path 1 never exported) | Modelica-source-level table binding for the player is the one UNVERIFIED item → Path 2 (§5.3) is authoritative for Tier-A scoring; Path 1 gated on an S6 spike. |
| G2 | Arrays of records as block state | PARTIAL — scalarized per element at instantiate (`rumoca-phase-instantiate/src/array_expansion.rs`); dynamic index into record arrays unsupported | **REJECTED** — record state compartments hard-fail `ensure_c_exportable` (`rumoca-galec-codegen/src/emit.rs:495`); dynamic subscript after scalarization fails (`lower/expr/references.rs:171`) | Struct-of-arrays buffers (§4.2) are mandatory, not stylistic. |
| G3 | `discrete Integer` states; runtime array subscripts | SUPPORTED | Integer states SUPPORTED; runtime subscript SUPPORTED **only with declared `min`/`max` bounds** — lowers to a static if-chain over the range, unbounded indices rejected (`references.rs:551-600`; `spec_0034_battery.rs:724,760`) | Every buffer index declares tight bounds; small per-stream depths keep select-chain C-size bounded; S2 canary measures it. |
| G4 | `mod()`/`rem()` | SUPPORTED | **REJECTED** — `builtin:mod`/`builtin:rem` unsupported (ET017; `lower/expr.rs:1113`; SPEC_0034 trap T8); `div` is supported | Ring advance uses `if pre(head) >= depth then 1 else pre(head)+1` (§4.2). |
| G5 | Functions: record I/O; subscripted output targets; `for`/`while` bodies | SUPPORTED in solve lowering | **REJECTED** — inliner accepts only Assignment/If with single unsubscripted identifier targets (`lower/expr/function_inline.rs:94-125,301`); multi-output array projections SUPPORTED (`spec_0034_battery.rs:1250`) | Harness helpers are multi-output array functions; no record returns, no `out[i] :=`, loops live in block algorithms not functions. |
| G6 | `delay()`; `pre()` on arrays/records | `delay` PARTIAL — lowered to `pre(expr)` one-tick placeholder (`rumoca-phase-solve/src/lower/scalar_ops.rs:254`), NOT true dead-time; `pre` on arrays SUPPORTED; `pre` on whole records unverified | `delay` **REJECTED** (`builtin:delay`); `pre` on arrays SUPPORTED (`'previous(x)'`, trap T2) | No `delay()` anywhere; `pre` only on scalars/arrays (struct-of-arrays makes this natural). |
| G7 | `replaceable`/`redeclare` | **SUPPORTED** — resolved at instantiate, transparent downstream (`rumoca-phase-instantiate/src/{type_overrides,type_lookup,mod_env}.rs`; `mod_propagation_test.rs:995`; INST-014/015/022/023/026/044/046); only Media-style constrained-package lookup gapped (SPEC_0022 §4.16.1) | transparent (resolved pre-DAE) | `SideBySide` uses three replaceable slots constrained by the instrumented partial (§5.3). |
| G8 | Parameter-sized array dims | SUPPORTED — structural parameters evaluated at instantiate to literal dims (`rumoca-phase-typecheck/src/tests.rs:542`; `rumoca-phase-instantiate/src/dims.rs`) | SUPPORTED for static allocation; dims frozen, parameter stays manifest-tunable but never resizes (GAL-020) | §4.2 sizing works; clamp + assert against tuning past the frozen bound. |
| G9 | Multi-rate `sample()`; non-zero phase | SUPPORTED (multi-rate schedules vector; proven in `examples/interactive/reusable_booster/`) | **REJECTED** — one static base period per exported block (`admissibility.rs:118-140`, GAL-016/GAL-017, ET005); phase ≠ 0 rejected (`unsupported-feature:clock-phase`) | One `sample(0.0, 0.005)` per export wrapper; sensor rates expressed by `fresh`-gating (§7); multi-rate stays sim-side only. |

Every REJECTED entry has its workaround inside this design; no slice
is blocked on a compiler feature request. The only compiler follow-up
worth filing (not a dependency): a GALEC-side `remainderDown`/`mod`
lowering and record-state compartments would simplify future buffer
code (both already have active spec surface — SPEC_0034 Appendix C,
and the in-flight record-array work in the compiler stream).

## 11. Open questions (for James / next mailbox round)

- **Q1 weights:** M-weight vector for the §6.3 ranking (gates are
  fixed; weights are policy).
- **Q2 delay defaults:** `maxSensorDelay_s` per stream for the flight
  vehicle (GPS 0.25–0.5 s? mocap ≤ 50 ms? flow ≤ 20 ms?) — sets real
  buffer budgets in S2.
- **Q3 flight-log truth:** for Tier-A flight-log traces, is OMC/mocap
  the accepted truth source (outdoor GPS flights have no truth →
  NEES unavailable, only NIS/M5/M7-M9)?
- **Q4 health-record reconciliation:** final `EstimatorHealth` field
  names once the estimator-numerical-health branch lands (§3.2 note).
- **Q5 C3 fidelity:** should the replay reference also re-fuse
  *subsequent already-fused* measurements on rewind (full smoother
  semantics, costlier) or IMU-only replay (cheaper, slightly
  sub-exact)? Design assumes full re-fuse in stored order (§4.6).
- **Q6 report location:** comparison reports as committed artifacts
  under `artifacts/estimator-harness/` (models repo) vs rumoca `dev/`.

---

Signed-off-by: James Goppert <james.goppert@gmail.com>
