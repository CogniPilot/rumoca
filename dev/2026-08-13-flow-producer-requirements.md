# Optical-Flow Producer / Transport Requirements

**Status: DRAFT contract for the firmware lane. Not implemented anywhere.**

This document states what the **estimator** requires of an optical-flow producer
and transport, derived from the actual model source — not from what a flow
sensor typically provides. Codex's lane implements; this is the contract the
implementation is measured against.

| Field | Value |
| --- | --- |
| Date drafted | 2026-08-13 |
| Estimator source of record | `~/git/modelica_models` @ `sparsity-limit-covariance` `a9e5037` |
| Interface record | `Avionics.OpticalFlowSample` (`Avionics/package.mo`) |
| Consumer | `Estimation.MultiSensorInvariant.step` → `correctOpticalFlow` |
| Firmware boundary | `cerebri_rdd2/src/processes/navigation_estimator.c` |
| Transport schema available | `synapse_fbs` v0.9.0 `fbs/optical_flow.fbs` (`OpticalFlow`, `OpticalFlowVelocity`) |
| Ledger | `~/git/rumoca/dev/2026-08-11-rdd2-agent-handoff.md` |

Items the model does not answer are marked **OPEN-Fn** with a proposed default.
Nothing here is invented silently.

---

## 0. Current state of the world (verified, not assumed)

| Piece | State |
| --- | --- |
| Estimator flow input | **Exists and is complete.** `Avionics.OpticalFlowSample` is a first-class input on `PartialNavigationEstimator`; `correctOpticalFlow` is a real correction in the priority chain |
| Firmware adapter | **Does not exist.** `navigation_estimator.c` hard-sets `process->efmu.opticalFlow_valid = false; process->efmu.opticalFlow_fresh = false;` on every estimator tick |
| zros topic | **Does not exist.** No flow topic in `src/interfaces/zros_topics.{c,h}` |
| Driver / producer | **Does not exist** in `cerebri_rdd2` |
| Transport schema | **Exists.** `synapse_fbs` v0.9.0 defines `OpticalFlowData` (raw/integrated) and `OpticalFlowVelocityData` (derived body velocity), both nanosecond-timestamped |

So the work is: driver → zros topic → adapter → the five estimator fields that
actually matter (§2.1). The schema layer is already designed and does not need
inventing.

---

## 1. The one fact that shapes everything

**Only two of the nine `OpticalFlowSample` fields reach the estimator math.**

`correctOpticalFlow` consumes exactly:

- `measurement.velocityBodyFlu_m_s` (2-vector), and
- `measurement.velocityCovarianceBody_m2_s2` (2×2).

`step.mo` additionally consumes `valid` and `fresh` as the gate. Every other
field — `timestamp_s`, `integratedLineOfSight_rad`, `integrationTime_s`,
`groundDistance_m`, `quality` — is **carried into `step` and never read by any
correction**. `groundDistance_m` in particular is populated by callers and
ignored by the math; this was independently confirmed and recorded in the ledger
(2026-08-11 15:48).

Consequences, and these are requirements, not advice:

1. **All range compensation is producer-side.** The record's own comment says
   `velocityBodyFlu_m_s` is the planar velocity *"after camera/range
   compensation"*. The estimator will not divide flow by height.
2. **All rotation/de-rotation is producer-side.** The estimator does not subtract
   body rotation from raw flow.
3. **All quality gating is producer-side.** The estimator cannot see `quality`.
   The only channels a producer has for "trust this less" are (a) `valid=false`,
   and (b) a larger `velocityCovarianceBody_m2_s2`.
4. **All staleness rejection is producer/adapter-side.** The estimator never
   reads `timestamp_s` on the flow path and performs no latency compensation.
5. **All lever-arm compensation is producer-side.** The measurement model is the
   velocity of the *body origin*, not the camera (§4.3).

---

## 2. Interface contract

### 2.1 Fields the producer must get right

| Field | Type | Unit | Requirement |
| --- | --- | --- | --- |
| `valid` | Boolean | — | True **only** when the sample is fully trustworthy after all producer-side gating (§6). The estimator does no further checking. |
| `fresh` | Boolean | — | True for **exactly one estimator tick** per new sample (§3). |
| `velocityBodyFlu_m_s[2]` | Real | m/s | Planar velocity of the **body origin** in body FLU: `[1]` = +forward, `[2]` = +left. Range- and rotation-compensated, lever-arm corrected, mount-rotation corrected. |
| `velocityCovarianceBody_m2_s2[2,2]` | Real | m²/s² | Symmetric positive-definite measurement noise in the **same body FLU axes**. Must factor (§5). |
| `timestamp_s` | Real | s | Sample time. **Ignored by the estimator**; required for adapter-side staleness and for logging/telemetry. Use the *center of the integration window*, not the publish time. |
| `integratedLineOfSight_rad[2]` | Real | rad | Ignored by the estimator; the record comment calls it *"optional raw flow integral retained for transport adapters"*. Populate it from `flow_rad` for post-flight forensics. |
| `integrationTime_s` | Real | s | Ignored by the estimator. Populate from `integration_timespan_ns`. |
| `groundDistance_m` | Real | m | **Ignored by the estimator.** Populate from the fused range for forensics and for the telemetry watch item; do **not** assume it provides any altitude aiding. |
| `quality` | Real | 1 | Normalized to [0,1] (`min=0, max=1` in the record). **Ignored by the estimator.** Populate for telemetry; the *decision* it drives must already be baked into `valid` and the covariance. |

### 2.2 Frame and sign conventions

- **World frame: ENU.** **Body frame: FLU** (x forward, y left, z up). This is the
  canonical convention stated in `Avionics/package.mo` and it is consistent with
  `synapse_fbs` `OpticalFlowData.flow_rad` ("body FLU (x forward, y left)").
- `velocityBodyFlu_m_s` is the **vehicle's velocity**, not image motion. Moving
  forward gives `velocityBodyFlu_m_s[1] > 0`. Image motion for a downward camera
  runs opposite to vehicle motion; the sign inversion is producer-owned.
- Attitude convention elsewhere in the interface: scalar-first Hamilton
  quaternion `{w,x,y,z}` mapping body FLU → world ENU; Euler is intrinsic 3-2-1
  ordered `{roll,pitch,yaw}`. The flow path does not use these directly but the
  producer's tilt compensation must agree with them.

> **OPEN-F1 — raw-flow-to-velocity sign derivation.** The model states the frame
> but not the algebraic relation from `flow_rad` / `delta_angle_flu_rad` / range
> to `velocityBodyFlu_m_s`, because it assumes that conversion already happened.
> The `synapse_fbs` comment fixes one half of it (`flow_rad` is oriented so it
> *subtracts directly from* `delta_angle_flu_rad`), leaving the translational-flow
> to velocity sign and the small-angle scaling producer-owned.
> **Proposed default:** the producer owns the derivation, and its correctness is
> established **empirically and mandatorily** by the hand-translation bench test
> in Block 7 of `2026-08-13-gps-flow-test-card.md` — the flow analogue of the
> disarmed ENU walk used to prove the GPS frame. No flow block flies on a sign
> convention that was only reasoned about.

### 2.3 Timestamp semantics

- `timestamp_s` is **seconds**, Real. The transport is **nanoseconds**
  (`synapse_fbs` v0.9.0 nanosecond ABI). The adapter converts.
- Use `OpticalFlowData.timestamp_sample_ns` — *"center of the integration
  window… use this for fusion and latency compensation"* — **not**
  `timestamp_ns` (publish time).
- `distance_timestamp_ns` is separate and lets the consumer detect a stale range;
  the adapter must check it (§7).
- The estimator has **no measurement-delay compensation on the flow path at all.**
  A flow sample is fused as if it were instantaneous at the current estimator
  tick. Latency therefore appears directly as a velocity error proportional to
  acceleration × latency.

---

## 3. Freshness semantics — the `fresh` one-tick rule

The estimator thread is **IMU-driven**: `zros_sub_wait(&process->imu_sub, K_FOREVER)`,
i.e. it ticks at the IMU rate (~1600 Hz on this airframe). Existing sources
follow the pattern `x_fresh = zros_sub_update(&sub) == 0`, which is true exactly
on the tick where a new message arrived.

Requirements:

1. `fresh` must be true on **exactly one** estimator tick per new flow sample.
2. A latched `fresh=true` is a **defect**, not a conservative choice: the same
   measurement would be re-fused every tick at ~1600 Hz, collapsing the velocity
   covariance and producing a falsely confident, biased state.
3. `valid` is a *sample property* and may persist across ticks; `fresh` is a
   *tick property*. They are not interchangeable.

### 3.1 Priority starvation — a fresh flow sample can be silently dropped

`step.mo` accepts **at most one aiding correction per tick**, in a strict
`elseif` chain:

```
mocap  →  GPS(pos+vel joint)  →  GPS position  →  GPS velocity  →  optical flow
```

If a flow sample and a GPS sample land on the same estimator tick, **the flow
sample is discarded entirely** — not queued, not deferred. Since `fresh` is a
one-tick flag, that sample is lost forever.

With GNSS at 10 Hz and a plausible flow rate of 10–50 Hz against a ~1600 Hz tick,
collisions are rare but nonzero, and they are systematic if the two producers
share a timing source.

> **OPEN-F2 — dropped-sample policy.**
> **Proposed default: accept the loss.** Presenting each sample once matches the
> model's semantics exactly and adds no state. The adapter *may* instead hold
> `fresh` until a tick where no higher-priority source is fresh, bounded by the
> staleness limit (OPEN-F5) — this recovers the dropped samples but makes the
> fused sample slightly older. Pick one and state it; do not implement both.
> Whichever is chosen, the **rate at which flow samples are dropped by priority
> must be counted and exposed** so the flight team can see it.

---

## 4. What the estimator actually does with the measurement

### 4.1 Measurement model

```
residual = velocityBodyFlu_m_s − (Rᵀ · v_world)[1:2]
H        = [ 0₂ₓ₃ | [1 0 0; 0 1 0] | wedge(Rᵀ v_world)[1:2, :] | 0₂ₓ₆ ]
```

over the 15-state tangent ordered
`[position(3), velocity(3), attitude(3), gyroBias(3), accelBias(3)]`.

### 4.2 Observability — state it plainly on any brief

- **No position row.** Flow never observes position. Position error is bounded
  only in its *growth rate*, by the velocity correction; it still drifts.
- **No vertical row.** Only body components 1 and 2 are observed. Vertical
  position and vertical velocity are **structurally unobservable from flow**.
- **Attitude is coupled**, through `wedge(Rᵀ v_world)[1:2,:]`, and only while the
  vehicle is actually moving — the coupling vanishes at hover.
- **Biases are not directly observed.**

Measured consequence, from the 45 s reference mission (ledger, 2026-08-11):
under flow aiding, horizontal navigation-vs-truth error stayed at mm scale
(East mean/max 0.00150/0.00629 m, North 0.00108/0.00500 m) while vertical error
was 0.2406/0.6146 m armed, free-drifting to ~1.035 m final after disarm. That is
the observability structure, not a defect. **Optical flow is not a GPS
substitute and must never be described as one.**

### 4.3 Lever arm

The measurement model uses `Rᵀ · v_world` — the velocity of the **body origin**
where the IMU/state lives. A camera mounted `r` from the body origin measures
`v_body + ω × r`. The estimator applies no lever-arm term.

> **OPEN-F3 — lever-arm compensation.** The model is silent because it assumes
> the producer delivers body-origin velocity.
> **Proposed default:** the producer subtracts `ω × r` using the mount offset `r`
> as a calibration constant and the best available body rates. If `|r|` is small
> enough that `|ω × r|` stays below the measurement sigma across the flight
> envelope, the compensation may be omitted **only with that arithmetic written
> into the calibration receipt** — not by assertion.

---

## 5. Covariance requirements

`correctLinear` forms `S = H·P·Hᵀ + R` and calls `LinearAlgebra.solveSPD`.

Requirements on `velocityCovarianceBody_m2_s2`:

1. **Symmetric.**
2. **Positive definite**, and well enough conditioned to factor in the target's
   floating-point format. A failed factorization returns `accepted=false`, the
   state is left unmodified, and the rejection counter advances (§6.2).
3. **Expressed in body FLU axes**, matching the measurement.
4. **Scaled to the actual error**, which for flow is dominated by range error and
   correlation quality. The producer should scale with range: a flow-derived
   velocity error grows roughly linearly with range error at fixed angular flow
   noise. Off-diagonal terms should be populated if the sensor's axes are
   correlated; a diagonal matrix is acceptable if that is honest.
5. **Never a fixed constant chosen to "make it accept".** The innovation gate
   (§6.1) is calibrated against a consistent filter; a deliberately loose
   covariance defeats the gate rather than satisfying it.

### 5.1 Degrading via covariance inflation — permitted, but not yet proven for flow

There is a precedent on the GPS path: rather than fabricate a measurement,
inflate the variance on an axis that carries no information. That was probed on
the v3 estimator bytes and the numbers are on record — `1e4 m²/s²` accepted and
behaving exactly like "invalid" on the inflated axis (0.199 vs 0.200 m/s, i.e.
zero drag), while `1e6 m²/s²` was **rejected**. It also quantified the harm of
the alternative: reporting a *tight* zero dragged true horizontal velocity ~100×
toward zero.

> **OPEN-F4 — inflation bound for the 2×2 flow covariance.** The `1e4` / `1e6`
> numbers were established for the **3×3 GPS velocity** covariance, not for the
> flow measurement, which has a different dimension, a different `H`, and a
> different gate threshold (12.0 vs 36.0). They must not be copied across.
> **Proposed default:** do not use per-axis inflation on the flow path in the
> first implementation — use `valid=false` for anything untrustworthy (§6). If
> single-axis degradation is later wanted, run the same host-harness probe
> against the v3 bytes on the 2-dof path and pin the verified bound in a named
> constant with a comment recording the value that failed, exactly as the GPS
> adapter rule specifies.

**A zero-velocity "measurement" with a tight covariance is prohibited.** It is
not a conservative default; it is a lie that actively drags the state.

---

## 6. Fault behavior — what the producer must send when things are bad

### 6.1 The estimator's gating assumptions

The estimator's only protections on the flow path are:

| Protection | Behavior |
| --- | --- |
| `valid and fresh` gate | The correction is not even attempted otherwise |
| SPD factorization | Failure ⇒ `accepted=false`, state unmodified |
| Chi-square innovation gate | `innovationGate = 6.0` per dof, dimension 2, so **NIS > 12.0 ⇒ rejected**, state unmodified, `innovationGateRejected` set |
| Persistent-rejection re-init | 50 consecutive rejected corrections ⇒ full re-initialization (§6.2) |

There is **no** finiteness check, no magnitude sanity check, no range check, no
staleness check and no quality check on the flow path. Everything else is the
producer's job.

### 6.2 The re-initialization hazard — the reason bad flow is dangerous

`rejectedCorrectionLimit = 50`. Fifty consecutive rejected corrections force the
estimator to re-run its declared initialization policy, flagged by
`covarianceReinitialized`. That policy seeds position from **mocap if valid, else
GPS position if valid, else the parameter `initialPositionWorldEnu_m`**.

**Optical flow never seeds position.** So a flow-only aiding configuration that
sustains 50 rejections re-initializes position to a *parameter constant* — the
state jumps. Note also that the counter advances only on ticks where a fresh
sample was actually attempted, so a producer that keeps publishing `valid=true`
garbage reaches the threshold faster than one that self-invalidates.

**Requirement: a producer that cannot vouch for its sample must set
`valid=false`, not publish a doubtful sample and rely on the innovation gate.**
The gate is a consistency check of last resort, not a filter for known-bad data.

### 6.3 Mandatory `valid=false` conditions

The producer must set `valid=false` (and must not publish a substitute value)
whenever any of the following holds:

| Condition | Source of the check |
| --- | --- |
| Correlation/flow quality below the calibrated threshold | `OpticalFlowData.quality`; flag bit0 `FlowValid` clear |
| Range invalid | flag bit2 `DistanceValid` clear |
| Range ambiguous / non-planar footprint | flag bit3 `DistanceAmbiguous` set; high `distance_spread_m`; low `distance_pixel_ok` |
| Range outside the sensor's compensated band | `distance_m` outside `[min_ground_distance_m, max_ground_distance_m]` |
| Range sample stale relative to the flow sample | `timestamp_sample_ns − distance_timestamp_ns` beyond the limit (OPEN-F6) |
| Body-rotation integral unavailable | flag bit1 `DeltaAngleValid` clear — de-rotation is impossible without it |
| Angular flow rate at or beyond the sensor limit | `max_flow_rate_rad_s` — saturated flow reads low, which is a *silent* underestimate of velocity |
| Sample stale at the adapter | staleness limit (OPEN-F5) |
| Any non-finite value in the computed velocity or covariance | producer-side check; the estimator has none |
| Covariance not SPD | producer-side check |
| Driver error count advancing | `error_count` |

Low-light modes (`FlowLightMode.LowLight`, `SuperLowLight`) are **not** an
automatic invalidation; they must be reflected in the covariance, and in `valid`
only if quality also fails.

### 6.4 Prohibited producer behavior

- Publishing zero velocity when correlation is bad (§5.1).
- Holding the last good velocity and republishing it as fresh.
- Extrapolating velocity across a dropout.
- Publishing non-finite values under any circumstance.
- Setting `valid=true` on the basis of "the estimator will gate it anyway".

---

## 7. Height-above-ground dependency

- Flow-to-velocity conversion **requires** a range to the observed surface. This
  is intrinsic — angular flow alone determines velocity only up to scale.
- The range comes from the **producer's** fused ranger
  (`OpticalFlowData.distance_m`, with `distance_quality`, `distance_spread_m`,
  `distance_pixel_ok`, `distance_timestamp_ns` as its trust metrics). The
  estimator supplies nothing and consumes nothing here — `groundDistance_m` is
  carried and unread (§1).
- The valid range band is a **sensor property**, published in the message itself
  as `min_ground_distance_m` / `max_ground_distance_m`. The producer must gate on
  its own limits rather than on a hard-coded number in the adapter.
- **The estimator's own altitude estimate must not be fed back as the range.**
  Under flow aiding the vertical channel is unobservable (§4.2) and drifting;
  closing that loop would scale the horizontal velocity by a drifting number and
  couple a diverging state into its own measurement.
- Range accuracy propagates directly into velocity: a 10% range error is a 10%
  velocity scale error, which the filter will partly absorb into accelerometer
  bias rather than reject. This must be reflected in the covariance (§5.4).

> **OPEN-F6 — range/flow timestamp skew limit.** The model says nothing; the
> schema provides `distance_timestamp_ns` precisely so a consumer can detect it.
> **Proposed default:** invalidate when
> `|timestamp_sample_ns − distance_timestamp_ns| > 100 ms`, and scale the
> velocity covariance by the vertical-rate-times-skew contribution below that.
> To be tightened from bench data.

---

## 8. Calibration requirements

The model carries **no** optical parameters — no focal length, no scale factor,
no mount rotation, no FOV. Every one of them lives in the producer, and the
model's only expression of them is the phrase *"after camera/range
compensation"*. Consequences:

- **A scale error cannot be estimated or corrected by the filter.** There is no
  flow-scale state. A systematic scale error appears as a velocity bias, which
  the accelerometer-bias state will partly absorb — quietly corrupting the bias
  estimate rather than announcing itself.
- Required calibration artifacts, all of which belong in the **calibration
  receipt** that Block 7 of the test card depends on:

| Item | Why |
| --- | --- |
| Focal length / angular scale per pixel (or the sensor's native rad output verified) | Sets the metric scale together with range |
| Field of view (`field_of_view_rad`) | Consistency check on the angular scale |
| Camera-to-body mount rotation | The measurement must land in body FLU, not camera axes; a yaw mount error rotates the velocity vector and will fight the GPS aiding |
| Mount lever arm `r` | §4.3 / OPEN-F3 |
| Range sensor bias and scale | Directly multiplies the velocity |
| Verified **sign** on both body axes | OPEN-F1; bench-proven, not reasoned |
| Rotation-compensation verification (rotate-in-place produces ~zero velocity) | Proves `delta_angle_flu_rad` is being subtracted with the right sign and scale |
| Noise characterization → the covariance model | §5 |

---

## 9. Rate expectations and latency budget

| Quantity | Value | Source |
| --- | --- | --- |
| Estimator tick | IMU-driven, ~1600 Hz | `navigation_estimator.c` (`zros_sub_wait` on `imu_sub`) |
| Model `samplePeriod` default | 1 ms | `PartialNavigationEstimator` |
| Aided-tick compute budget | **5 ms** (G7) | ledger; bench 285 µs |
| GNSS aiding | 10 Hz (`RDD2_GNSS_M10_TARGET_PERIOD_MS = 100`) | firmware |
| Trajectory-reference budget (control side) | **100 ms** | ledger, mission-ingress contract |
| Flow rate | **OPEN-F7** | — |
| Flow end-to-end latency | **OPEN-F5** | — |

> **OPEN-F7 — flow publication rate.**
> **Proposed default: 20–50 Hz**, matching the "aiding streams at 20–100 Hz"
> assumption the `rejectedCorrectionLimit = 50` comment is calibrated against
> (50 rejections spans ~0.5–2.5 s at those rates). Below ~10 Hz the rejection
> counter's time constant stretches past the point where the re-init comment's
> reasoning holds, and the correction becomes too sparse to bound velocity drift
> usefully. Above the IMU tick rate is meaningless.

> **OPEN-F5 — staleness limit and latency budget.** The estimator does **not**
> read `timestamp_s` on the flow path and does **no** delay compensation: a
> sample is fused as if instantaneous. The 100 ms figure in the ledger is the
> *trajectory-reference* budget for control and is **not** an estimator-side
> number; it must not be reused here by analogy.
> **Proposed default:** total sample-center-to-fusion latency ≤ **30 ms**, and
> the adapter sets `valid=false` when `imu_timestamp − flow_timestamp_sample`
> exceeds **50 ms**. Rationale: since the estimator applies no delay
> compensation, latency shows up directly as `acceleration × latency` of velocity
> error — at 30 ms an unmodelled 5 m/s² acceleration contributes 0.15 m/s, which
> is comparable to a plausible flow sigma, and at 50 ms it contributes 0.25 m/s,
> which is not. These are starting points to be replaced by measured bench
> latency and the rehearsal envelope; they are not derived limits.

### 9.1 Compute-budget note

Adding a live flow correction changes which branch of `step` executes but not
the worst case: the chain is `elseif`, so **at most one** correction runs per
tick, and the 2-dof flow correction is cheaper than the 6-dof joint GPS one that
already sets the budget. Enabling flow should not by itself threaten G7 — but
G7 must be re-measured on any image that adds the producer, because the driver,
transport and adapter themselves cost time.

---

## 10. Transport mapping

`synapse_fbs` v0.9.0 offers two layers. They are not equivalent.

| Layer | Contents | Fit to the estimator |
| --- | --- | --- |
| `OpticalFlowData` (raw) | Integrated angular flow `flow_rad`, `delta_angle_flu_rad`, fused range + trust metrics, sensor limits, quality, flags, `time_status` | Complete, self-contained, but requires the adapter to do the de-rotation and range projection. The schema's own comment says *"a downstream estimator should fuse this rather than the derived velocity below."* |
| `OpticalFlowVelocityData` (derived) | `velocity_flu_m_s` already tilt-compensated, plus `distance_m`, `roll_rad`, `pitch_rad`, `quality`, flags | Maps almost 1:1 onto `velocityBodyFlu_m_s`, but the tilt compensation uses **accelerometer-derived roll/pitch**, which is a second, weaker attitude solution than the estimator's own — and it carries **no covariance** |

> **OPEN-F8 — which transport layer feeds the estimator.**
> **Proposed default: publish `OpticalFlowData` (raw) as the flight path, and let
> the adapter derive `velocityBodyFlu_m_s` using the estimator's own
> gyro-bias-corrected body rates and attitude.** Reasons: (a) the schema
> explicitly recommends it; (b) using the estimator's attitude instead of an
> independent accelerometer-derived roll/pitch avoids injecting a second,
> inconsistent attitude solution into the filter's own measurement — which would
> correlate the measurement with the state and violate the EKF's independence
> assumption; (c) the raw layer carries the trust metrics needed for §6.3, which
> the derived layer partly discards; (d) neither layer carries a covariance, so
> the adapter must synthesize `velocityCovarianceBody_m2_s2` either way (§5) and
> it can only do so honestly from the raw metrics.
> `OpticalFlowVelocityData` should still be published for ground/telemetry
> cross-check, but must not be the estimator's input. Requires Codex's
> concurrence before implementation.

Additional transport requirements:

- Both layers are **nanosecond** (`synapse_fbs` v0.9.0). The adapter converts to
  the seconds-valued `timestamp_s` and must not lose precision in a way that
  breaks the staleness comparison.
- `time_status` must be checked: a `LocalFreerun` clock is monotonic boot time,
  not the shared gPTP domain, so cross-source timestamp comparisons (flow vs IMU
  vs GNSS) are only meaningful when the domains agree. **OPEN-F9 — proposed
  default: require the flow timestamps to be in the same domain as the IMU
  timestamps used by the estimator thread, and invalidate the sample otherwise.**
- `id` allows multiple flow instances. **OPEN-F10 — proposed default: exactly one
  flow instance is supported for this flight; the adapter binds a single
  configured `id` and ignores all others.** Multi-instance fusion has no place in
  the model — the estimator has exactly one `opticalFlow` input.
- The zros topic must be added to `src/interfaces/zros_topics.{c,h}` and consumed
  in `navigation_estimator.c` with `zros_sub_update` for one-tick freshness,
  matching the existing GNSS/mocap pattern, and replacing the two hard-coded
  `false` assignments.

---

## 11. Verification the producer must ship with

An implementation is not accepted on inspection. The following must exist and
pass before any flow block on the test card runs:

1. **Host tests** for each `valid=false` condition in §6.3, one discriminator each.
2. A **one-tick freshness** test proving the same sample is never fused twice.
3. A **priority test** proving that a flow sample colliding with a fresh GPS
   sample follows the chosen OPEN-F2 policy and that the drop is counted.
4. An **SPD/finiteness** test proving no non-finite or non-SPD value ever reaches
   the eFMU input.
5. A **staleness** test at the OPEN-F5 boundary (accept just inside, reject just
   outside), in the same style as the existing 100,000/100,001 µs radio boundary
   test.
6. A **calibration receipt** covering every row of §8, including the bench-proven
   sign verification (OPEN-F1).
7. **G7 re-measured** on the image containing the producer.
8. A fresh **independent adversarial review** of the frozen slice, matching the
   discipline applied to the M10 and interlock lanes.

---

## 12. OPEN items index

| ID | Question | Proposed default | Who decides |
| --- | --- | --- | --- |
| **OPEN-F1** | Raw-flow → velocity sign/scale derivation | Producer-owned; correctness established by a mandatory hand-translation bench test (test-card Block 7), never by reasoning alone | Codex + flight test |
| **OPEN-F2** | Flow samples dropped by aiding priority | Accept the loss (matches model semantics); count and expose the drop rate. Alternative hold-until-consumable policy is permitted but must be chosen exclusively | Codex |
| **OPEN-F3** | Lever-arm `ω × r` compensation | Producer compensates; omission allowed only with the magnitude arithmetic written into the calibration receipt | Codex |
| **OPEN-F4** | Covariance-inflation bound for the 2-dof flow measurement | Do not use inflation initially; use `valid=false`. If wanted later, re-probe on the v3 bytes for the 2-dof path — the GPS `1e4`/`1e6` numbers do **not** transfer | Claude (probe) + Codex |
| **OPEN-F5** | Flow staleness limit and latency budget | ≤ 30 ms sample-to-fuse; `valid=false` beyond 50 ms. A starting point, not a derived limit — replace with bench + rehearsal numbers | Codex + rehearsal |
| **OPEN-F6** | Range-vs-flow timestamp skew limit | Invalidate beyond 100 ms skew; scale covariance below that | Codex |
| **OPEN-F7** | Flow publication rate | 20–50 Hz, consistent with the `rejectedCorrectionLimit` calibration comment | Codex |
| **OPEN-F8** | Transport layer feeding the estimator | Raw `OpticalFlowData`; adapter derives velocity using the estimator's own attitude/rates. Derived layer is telemetry-only | Codex (concurrence requested) |
| **OPEN-F9** | Clock-domain requirement | Flow timestamps must share the IMU's time domain; invalidate otherwise | Codex |
| **OPEN-F10** | Multiple flow instances | Exactly one configured `id`; ignore others | Codex |

## 13. Status requests to the firmware lane

1. Does **any** calibrated flow producer exist, in any branch, or is this
   green-field? (Working-tree audit says green-field.)
2. Which hardware — the schema's comments reference a PAA3905-class sensor with a
   fused ranger. Confirm the part and the ranger.
3. OPEN-F8: concurrence on the raw-vs-derived transport layer.
4. OPEN-F2: which dropped-sample policy.
5. If flow is not going to exist for this test window, say so plainly and Blocks
   7–8 of the test card are struck rather than left conditional.
