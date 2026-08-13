# RDD2 GPS + Optical-Flow Flight Test Card

**Status: DRAFT — NOT RELEASED FOR FLIGHT.**
Two classes of cell are deliberately unfilled:

- `TBD-R#` — expected-value / dispersion numbers owned by the mission-rehearsal
  sim agent running in parallel. The card is structured so those numbers drop
  into the marked cells without editing prose.
- `PENDING-<name>` — a Codex-side (firmware) receipt or sign-off that does not
  exist yet. Every one is named in §1 with the exact evidence that closes it.

| Field | Value |
| --- | --- |
| Date drafted | 2026-08-13 |
| Vehicle | RDD2 quadrotor, `mr_vmu_tropic` (i.MX RT1064 M7), Zephyr |
| Firmware | `cerebri_rdd2` — image identity **PENDING-IMAGE-DECISION** (§1 P6) |
| Estimator | `Vehicles.Rdd2.NavigationEstimator` (eFMU) from immutable v3 bundle |
| Flight model source | `modelica_models` branch `sparsity-limit-covariance` @ `a9e5037` |
| Ledger of record | `~/git/rumoca/dev/2026-08-11-rdd2-agent-handoff.md` |
| Card owner | Claude (flight-readiness lane) |
| Firmware owner | Codex (`cerebri_rdd2` lane) |

---

## 0. Scope: what this test adds, and what it does not

### 0.1 Prior scope of record (unchanged baseline)

Ledger, 2026-08-13 03:51 EDT, "scope of record: no free-flight POSITION",
accepted in full by both agents:

1. props-off hardware gates and disarmed ENU walk;
2. restrained/tethered experimental POSITION behavior only after every gate is
   green, explicitly observing command continuity and motor shutdown;
3. manual ACRO/ATTITUDE free flight only if estimator RatesValid, timing,
   stacks, RC and actuator gates pass;
4. **no** untethered POSITION, **no** autonomous GPS box, **no** optical flight.

The disqualifier for untethered POSITION was not risk appetite: with no mission
or reference ingress, Guidance consumed a **zero-initialized** reference that
merely coincided with the freshly latched GPS origin. That is an implicit
fail-open default, not an accepted command.

### 0.2 What this card adds

| Addition | Gate that makes it possible | Default disposition |
| --- | --- | --- |
| **A. GPS-aided POSITION, tethered** | already in the prior scope (item 2); extended here with explicit GPS-aiding observation | RUN (Block 5) |
| **B. GPS-aided POSITION, untethered** | the mission-ingress lanes must be in the flown image AND carry the airborne-capability-loss contract | CONDITIONAL — **PENDING-INGRESS** (§1 P5) |
| **C. Optical-flow aiding** | a calibrated flow producer and transport must exist in the flown image | CONDITIONAL — **PENDING-FLOW-PRODUCER** (§1 P7); at time of writing **no producer exists**, so Blocks 7–8 are expected to be SKIPPED |

### 0.3 Explicitly still out of scope

- **Autonomous GPS box / waypoint mission.** Even with the mission-ingress lane
  in the image, the accepted shell contract loads a *bounded relative
  current-altitude box template while disarmed only*, and **it does not take
  off**. Any takeoff remains a manual ATTITUDE takeoff by the pilot. This card
  does not authorize an autonomous mission run; that needs its own card after
  the ingress lane has flight history.
- **Flow-only (GPS-denied) flight.** Optical flow provides no position and no
  vertical observation at all (see §4.4 and the requirements document
  `2026-08-13-flow-producer-requirements.md`). Flow blocks in this card are
  *aiding-in-addition-to-GPS* observation blocks only.
- **Anything using the v2 eFMI bundle.** v3 only.

---

## 1. Prerequisites and sign-off ledger

No block runs until every prerequisite listed in its entry criteria shows
**GREEN** here. "Green" means the named artifact exists and was checked on the
day, not that it was reported green in the ledger at some earlier hour.

| ID | Prerequisite | Evidence that closes it | Owner | Status as drafted |
| --- | --- | --- | --- | --- |
| **P1** | v3 eFMU bundle identity verified against the aircraft image | Manifest digest `f87f8e5acd5b1c51edd3d529a303043fb6abfc1137a759e4ca57034784a98c69` for bundle `/home/jgoppert/rdd2-flight-efmus-v3`; all 12 deployed C/H/ALG files byte-match v3; receipts under `/home/jgoppert/rdd2-flight-build-receipts-v3-final` | Codex | GREEN as of 03:36 EDT for the frozen images; **must be re-verified against whatever image is actually flashed** (see P6) |
| **P2** | Flashed ELF sha256 matches an approved build | Normal onboard-GPS `mr_vmu_tropic`: `98e622a3…` (flash 417,064 B / 9.94%, RAM 337,140 B / 64.30%). FastDyn/radio-GNSS: `6a432077…` (flash 218,064 B / 5.20%, RAM 260,820 B / 49.75%) | Codex | GREEN for the frozen pair; **invalid if a new image is built** (P6) |
| **P3** | **G7** — aided estimator tick on target under the 5 ms budget, plus stack watermark | On-target measurement receipt from the flown image. Bench margin is 17× at 285 µs; the ledger is explicit that it must be *measured, not assumed* | Codex | **RED / PENDING-G7.** Ledger records G7 as never satisfied — "G7 is not manufacturable without hardware". **Hard blocker for every powered block.** |
| **P4** | Flight-model source published and pinned | `modelica_models` branch `sparsity-limit-covariance` @ `a9e5037` pushed to `git@github.com:cognipilot/modelica_models` and pinned in `cerebri_rdd2/west.yml` | Codex, or James's terminal | **RED / PENDING-R2.** `west.yml` still pins `modelica_models d319854…`, which lacks four of five required sources. Claude cannot push (ssh publickey denied). This is a **provenance** blocker, not a flight-safety blocker — see §1.1 |
| **P5** | Mission-ingress lanes landed, frozen, and independently reviewed, and present in the flown image | Fresh adversarial APPROVE on the integrated tree covering: planner lifecycle, `mission_shell.c` loader, Guidance reference gating, SPEC_0002/SPEC_0003 updates; plus the **airborne-capability-loss contract** proven by wrapper discriminators | Codex | **PENDING-INGRESS.** Working-tree audit of `~/git/cerebri_rdd2` shows `src/interfaces/mission_shell.c`, `tests/mission_shell/`, `tests/waypoint_mission_ingress/` present but **untracked/uncommitted**; no integrated-tree APPROVE recorded in the ledger. **Gates Block 6 only.** |
| **P6** | Image identity decision: fly the frozen v3 pair, or a new build carrying the ingress lanes | Explicit Codex answer + a receipt for whichever image is chosen. A new image invalidates P1/P2/P3 receipts and needs its own | Codex | **PENDING-IMAGE-DECISION.** Question posed in the ledger; unanswered as drafted |
| **P7** | Optical-flow producer calibration receipt and transport | A calibrated flow producer in `cerebri_rdd2`, a zros topic carrying it, an adapter feeding `opticalFlow_*` into the eFMU, and a **calibration receipt** (scale/focal, mount rotation, sign verification) | Codex | **RED / PENDING-FLOW-PRODUCER.** Verified directly: `src/processes/navigation_estimator.c` hard-sets `opticalFlow_valid = false; opticalFlow_fresh = false;` on every tick; no flow topic exists in `src/interfaces/zros_topics.{c,h}`; no flow driver in the tree. The `synapse_fbs` v0.9.0 schema *does* define `OpticalFlow` / `OpticalFlowVelocity`, so transport is designable but unbuilt. **Blocks 7–8 SKIP by default.** |
| **P8** | GNSS observability on the downlink | `gnss status` shell output, `zros topic hz gnss_fix`, and telemetry exposing navigation odometry, origin latch and correction acceptance | Codex | **PENDING-VERIFY.** An earlier audit found VehicleHealth omitted GNSS and the radio downlink exposed neither odometry, origin latch, nor correction acceptance. The M10/diagnostics lane was approved afterwards; confirm on the day against the flown image before Block 1 |
| **P9** | CUBS2 CSyn v0.9 hard cutover | CUBS2 `west.yml` on `csyn c34dd35d…` / `synapse_fbs v0.9.0` plus validation receipt | Codex (ACKed 13:20) | **PENDING-CUBS2.** *Not an RDD2 flight blocker* — it is a cross-vehicle ABI **release** blocker. Recorded here because it was requested as a prerequisite row; it does not gate any block on this card |

### 1.1 Honest statement on P4

P4 is a reproducibility/provenance blocker. The bytes that would fly were
emitted from `a9e5037` and are independently receipted (P1); the pin gap means
the *advertised clean CI path cannot reproduce them from a public source*, not
that the flown bytes are unverified. Flying with P4 red is a documented
provenance debt, not an unverified-artifact hazard. The decision to accept that
debt is James's, and it must be recorded on the card before Block 1.

### 1.2 Hard blockers

**P3 (G7)** is the only prerequisite that blocks *every powered block* on this
card. If G7 has not been measured on the flown image, the whole card is NO-GO.
The remaining prerequisites gate specific blocks, as marked.

---

## 2. Phased test blocks

Blocks run strictly in order. A block that fails its success criteria ends the
test day at that block unless the abort ladder (§6) says otherwise.

Legend: **RUN** = unconditional; **COND** = conditional, with the prerequisite
that must be GREEN; **SKIP** = default disposition unless the prerequisite lands.

---

### Block 0 — Provenance and ground safety (RUN)

**Entry criteria**
- P1, P2, P6 GREEN (image identity decided and its receipts verified).
- P4 disposition recorded (accepted debt, or blocker).
- Props **OFF**. Battery secured. Aircraft on the bench, not on the pad.

**Procedure**
1. Read back the flashed ELF sha256 and compare against the P2 receipt for the
   chosen image.
2. Confirm the deployed generated sources match the v3 manifest digest (P1).
3. Confirm which build variant is flashed (normal onboard-GPS vs FastDyn/radio-GNSS);
   the normal onboard-GPS image is the flight image.
4. Record the `cerebri_rdd2` commit and working-tree state, and the
   `modelica_models` pin, verbatim on the card.

**Success criteria**
- sha256 match, exact, on the flashed image.
- Manifest digest match.
- Build variant is the intended one.

**Abort criteria**
- Any hash mismatch → **STOP**. Do not flash a different image to make it match;
  re-derive the receipt first.

---

### Block 1 — Props-off hardware gates (RUN)

**Entry criteria**: Block 0 green; **P3 (G7)** GREEN; P8 verified; props off.

**Procedure**
1. Boot; confirm G3 (image links and boots) on the actual aircraft.
2. Measure the aided estimator tick on target and the navigation-thread stack
   high-water mark (this *is* G7; if Codex measured it on a bench board,
   re-confirm on the airframe).
3. `gnss status` repeatedly; `zros topic hz gnss_fix 5000`; echo the full fix.
4. Verify accepted fix type is 3D-or-better and that `hacc`/`vacc` are inside
   the firmware thresholds.
5. Observe the estimator status word: `initialized`, `predictionAccepted`,
   `gpsPositionCorrectionAccepted`, `gpsVelocityCorrectionAccepted`,
   `consecutiveRejectedCorrections`, `covarianceReinitialized`,
   `innovationGateRejected`.
6. Confirm the RC link, failsafe behavior and actuator gates per standard
   preflight.

**Success criteria**

| Item | Threshold | Source of threshold |
| --- | --- | --- |
| Aided estimator tick | < 5 ms | G7 budget, ledger |
| Nav-thread stack headroom | TBD-R1 | rehearsal |
| GNSS sample period | 100 ms nominal, gaps within 75–125 ms | `RDD2_GNSS_M10_TARGET_PERIOD_MS/MIN_GAP_MS/MAX_GAP_MS` |
| GNSS recency | a sample within 300 ms | `RDD2_GNSS_M10_RECENT_MS` |
| Horizontal accuracy | ≤ 10,000 mm | `RDD2_GNSS_M10_MAX_HACC_MM` |
| Vertical accuracy | ≤ 15,000 mm | `RDD2_GNSS_M10_MAX_VACC_MM` |
| `consecutiveRejectedCorrections` | steady 0 with a good fix | model gating |
| `covarianceReinitialized` | never true after the first origin latch | model gating |

**Abort criteria**
- Tick ≥ 5 ms, or any stack watermark outside the rehearsal envelope → STOP.
- `covarianceReinitialized` toggling on the bench → STOP; that means 50
  consecutive rejected corrections and the filter re-ran its initialization.

---

### Block 2 — Disarmed ENU walk (RUN)

**Entry criteria**: Block 1 green. Props off. Disarmed. Open sky.

**Procedure**
1. With a latched origin, verify navigation odometry is finite and near local
   zero.
2. Walk the aircraft a measured distance East, return; then North, return; then
   raise/lower by a measured height.
3. Compare the reported ENU position against the tape/known geometry for **sign
   and scale** on all three axes.

**Success criteria**
- East displacement produces +x ENU; North produces +y; up produces +z.
- Scale error within TBD-R2 over a TBD-R3 metre walk.
- Return-to-start residual within TBD-R4.
- The 40 m local-pattern projection budget is the sanity reference: at a true
  40 m displacement the shipped projection yields +0.0997 m north /
  −0.0563 m east of systematic error.

**Abort criteria**
- Any axis sign inverted → **STOP**, full stop for the day. A sign error in the
  navigation frame is not a tuning issue.
- Scale error outside TBD-R2.

---

### Block 3 — POSITION interlock checkout, tethered/restrained, props off then props on (RUN)

Verifies the accepted GNSS-readiness interlock contract without asking the
aircraft to hold anything.

**Entry criteria**: Block 2 green. Aircraft restrained/tethered. Area clear.

**Procedure**
1. **Ground / pre-arm, GNSS unready** (mask the antenna or use the shell to
   force the unready predicate): request POSITION with the arm switch high.
   Observe: generated controller disarmed, rate/attitude publications withheld,
   control fault **latched**.
2. Attempt to clear the latch with *retained* low ManualControl data. It must
   **not** clear.
3. Clear the latch with a **fresh** `Valid|Active` update with ArmSwitch low.
   It must clear.
4. **Airborne-contract surrogate, restrained, armed**: with the vehicle armed
   and restrained, force ready→unready. Observe: effective mode degrades to
   ATTITUDE, generated controller stays **armed**, publication is **continuous
   with no gap**, and **no readiness latch** is created.
5. Verify ACRO and ATTITUDE are unaffected throughout.

**Success criteria**
- Ground case: withhold + latch + fresh-valid-low-only acknowledgement, exactly.
- Armed case: **zero publication gap**, no latch, effective ATTITUDE, motors
  never commanded to zero by the readiness path.
- Rate's 25 ms command-age gate observed to zero/disarm/latch only in the ground
  case, never in the armed case.

**Abort criteria**
- Any publication gap in the armed case → **STOP**. That is the crash case the
  contract was rewritten to remove.
- Latch created by an airborne-surrogate readiness loss → STOP.

---

### Block 4 — Manual ACRO / ATTITUDE free flight (RUN)

**Entry criteria**: Blocks 1–3 green. Estimator `RatesValid` true and stable.
Timing, stacks, RC and actuator gates all green. Pilot brief §5 delivered.

**Procedure**
1. Manual ATTITUDE takeoff to a low hover. Hold.
2. Gentle attitude steps in each axis.
3. Short ACRO segment at safe altitude, pilot's discretion.
4. Land in ATTITUDE.

**Success criteria**
- No estimator invalidity event at any point (see §5.1 — invalidity costs ACRO
  too).
- `estimate_valid` continuously true in flight.
- `consecutiveRejectedCorrections` stays below TBD-R5.
- Attitude tracking and control feel nominal to the pilot.

**Abort criteria**
- Any `estimate_valid` false transition → land immediately; end the day at this
  block pending analysis.
- `covarianceReinitialized` true in flight → land immediately.

---

### Block 5 — GPS-aided POSITION, tethered / restrained (RUN — this is the primary new objective)

**Entry criteria**: Block 4 green. Aircraft tethered or restrained such that a
runaway cannot travel. GNSS ready. Origin latched. Spotter posted.

**Procedure**
1. Arm in ATTITUDE, establish a stable low hover within the tether.
2. Transition to POSITION. Observe **command continuity** across the transition
   (no publication gap, no motor transient).
3. Hold for TBD-R6 seconds. Observe position and velocity residuals against the
   tether-constrained truth.
4. Introduce a deliberate GNSS degradation (antenna mask) and observe the
   degrade path: effective ATTITUDE, publication continuous, no latch.
5. Restore GNSS; observe reacquisition and correction-acceptance resumption.
6. Exit POSITION deliberately; observe **motor shutdown behavior** on disarm as
   the plan of record requires.

**Success criteria**
- Command continuity at every mode transition; no publication gap anywhere.
- GPS position/velocity corrections accepted at the expected rate
  (TBD-R7 acceptance fraction).
- Position residual inside the tether envelope, within TBD-R8.
- Degrade and restore behave exactly as Block 3 step 4 proved on the bench.
- Motor shutdown on disarm is clean and immediate.

**Abort criteria**
- Any publication gap or motor transient at a POSITION transition → STOP.
- Correction acceptance below TBD-R9, or `innovationGateRejected` persistently
  true → STOP; the filter is rejecting the aiding it is supposed to use.
- `consecutiveRejectedCorrections` approaching 50 (the re-initialization
  threshold) → land immediately.

---

### Block 6 — GPS-aided POSITION, untethered (COND — **PENDING-INGRESS**, P5)

> **This block does not run unless P5 is GREEN.** Specifically: the mission-ingress
> lanes must be committed, frozen, independently APPROVEd on the integrated
> tree, and present in the flown image — and the **airborne-capability-loss
> contract** (airborne loss degrades the effective mode to ATTITUDE, keeps the
> controller armed, publishes continuously, creates **no latch**) must be proven
> by wrapper discriminators in that image.
>
> Rationale, from the ledger: without ingress, Guidance consumes a
> zero-initialized reference that only coincidentally matches the freshly
> latched origin. A hold that works by coincidence of defaults is an implicit
> fail-open, not a command, and it does not fly. **If the flown image is the
> frozen v3 pair (`98e622a3…` / `6a432077…`), P5 is by definition RED for that
> image and this block is SKIPPED.**

**Entry criteria**
- P5 GREEN against the flown image, with the APPROVE receipt named on the card.
- Block 5 green with no findings.
- The reference actually accepted by Guidance is **observed on telemetry** to be
  a fresh, finite, LocalEnu, supported-mask reference — not a default.
- Untethered area clear; failsafe geofence/altitude limits briefed.

**Procedure**
1. Manual ATTITUDE takeoff to a low hover (the shell does **not** take off).
2. Transition to POSITION with the reference observed valid. Hold TBD-R10 s.
3. Small commanded reposition within a bounded box, pilot on the sticks.
4. Deliberate ready→unready GNSS degradation at safe altitude: confirm degrade
   to effective ATTITUDE with continuous publication, pilot retains control.
5. Exit POSITION deliberately; land in ATTITUDE.

**Success criteria**
- Reference freshness observed continuously; no interval where the accepted
  reference is stale beyond the **100 ms trajectory-reference budget**.
- Position hold within TBD-R11; drift rate within TBD-R12.
- Degrade path behaves as Block 3/5 proved, in the air, with the pilot reporting
  a flyable vehicle throughout.

**Abort criteria**
- Reference age exceeds 100 ms at any point → exit POSITION.
- Any latch created by an airborne readiness loss → land, end the day.
- Any drift beyond TBD-R13 → exit POSITION to ATTITUDE and land.

---

### Block 7 — Optical-flow producer ground checkout (COND — **PENDING-FLOW-PRODUCER**, P7; default **SKIP**)

> **Default disposition: SKIP.** As drafted, no flow producer, no flow transport,
> and no flow adapter exist in `cerebri_rdd2`; the estimator's flow inputs are
> hard-wired invalid. This block runs only if Codex signs off a calibrated
> producer **and** a calibration receipt, per
> `2026-08-13-flow-producer-requirements.md`.

**Entry criteria (all required)**
- P7 GREEN: producer + transport + adapter present in the flown image.
- Calibration receipt exists covering: metric scale (focal length / range
  projection), camera-to-body mount rotation, and **sign verification on all
  axes**.
- The producer satisfies the estimator-side contract in
  `2026-08-13-flow-producer-requirements.md`, including the fault behavior
  (bad correlation ⇒ `valid=false`, never zero-velocity).
- Block 2 (ENU walk) already green, so the navigation frame is trusted.

**Procedure**
1. Props off, disarmed, aircraft held over textured ground at a known height in
   the sensor's valid range.
2. Translate by hand at a measured rate, forward then left. Verify the reported
   `velocityBodyFlu_m_s` sign and magnitude against the hand motion (this is the
   flow equivalent of the ENU walk and it is **mandatory** — the raw-flow to
   velocity sign convention is producer-owned and unverified in the model).
3. Rotate in place without translating. Verify the de-rotated velocity stays
   near zero (rotation compensation working).
4. Cover the lens / present a featureless surface. Verify the producer sets
   `valid=false` and does **not** publish zero velocity.
5. Move outside the valid range band. Verify `valid=false`.
6. Observe `opticalFlowCorrectionAccepted` on the estimator status word while
   GPS is also fresh — confirm the **priority behavior** (mocap > GPS > flow;
   one correction per tick) is understood by the observers.

**Success criteria**
- Sign correct on both body axes; magnitude within TBD-R14.
- Rotation-only test produces velocity below TBD-R15.
- Degraded-scene and out-of-range cases produce `valid=false`, not zeros.
- `opticalFlowCorrectionAccepted` observed true on ticks where no higher-priority
  source is fresh, and `innovationGateRejected` not persistently set.

**Abort criteria**
- Any sign error → STOP; flow does not fly.
- Producer publishes zero velocity when correlation is bad → STOP; that is the
  quantified-harm case (honest-tight zeros drag true horizontal velocity ~100×
  toward zero, per the FINDING-10 probe on the v3 bytes).

---

### Block 8 — Optical-flow-aided tethered hover (COND — **PENDING-FLOW-PRODUCER**, P7; default **SKIP**)

> Runs only if Block 7 passed in full. This is an *aiding-in-addition-to-GPS*
> observation block. Flow provides **no position and no vertical observation**;
> it is not a GPS substitute and this block must not be described as one.

**Entry criteria**: Block 7 green; Block 5 green; aircraft tethered; textured
ground surface; height inside the sensor's valid range.

**Procedure**
1. Tethered hover in ATTITUDE at a height inside the flow valid range.
2. Observe `opticalFlowCorrectionAccepted` and the horizontal velocity estimate
   with GPS present.
3. Compare the estimator's horizontal velocity against the GPS-only baseline
   recorded in Block 5.
4. Mask GPS briefly while tethered. Observe flow taking over the aiding priority
   and the resulting horizontal velocity behavior — and observe **vertical**
   position beginning to free-drift, which is the expected structural behavior,
   not a fault.

**Success criteria**
- Flow corrections accepted with `innovationGateRejected` false.
- Horizontal velocity agreement with the GPS-only baseline within TBD-R16.
- On GPS mask, vertical drift rate matches the rehearsal prediction TBD-R17
  (drift is *expected*; the criterion is that it matches prediction).

**Abort criteria**
- `consecutiveRejectedCorrections` climbing under flow aiding → exit; flow
  covariance is mis-scaled.
- Any `covarianceReinitialized` event → land immediately. Under flow-only aiding
  a re-initialization seeds position from the **parameter**, not from a sensor,
  because flow never seeds position — the state will jump.

---

## 3. Prerequisite-to-block map

| Block | Requires GREEN | Default disposition |
| --- | --- | --- |
| 0 Provenance | P1, P2, P6 | RUN |
| 1 Props-off gates | P3 (G7), P8 | RUN |
| 2 ENU walk | Block 1 | RUN |
| 3 Interlock checkout | Block 2 | RUN |
| 4 Manual free flight | Block 3 | RUN |
| 5 Tethered POSITION | Block 4 | RUN |
| 6 Untethered POSITION | **P5** | COND — SKIP if the frozen v3 image is flown |
| 7 Flow ground checkout | **P7** | COND — **SKIP** as drafted |
| 8 Flow-aided hover | **P7** + Block 7 | COND — **SKIP** as drafted |

---

## 4. Structural facts the observers must know

These are model-derived and are not tunable on the day.

### 4.1 Aiding priority is strict and lossy

`Estimation/MultiSensorInvariant/step.mo` accepts **at most one** aiding
correction per estimator tick, in a strict `elseif` chain:
mocap → joint GPS(pos+vel) → GPS position → GPS velocity → optical flow.
A fresh flow sample arriving on the same tick as a fresh GPS sample is
**discarded, not queued**. The estimator thread is IMU-driven (~1600 Hz) and
GNSS is 10 Hz, so collisions are infrequent but real.

### 4.2 Innovation gating and re-initialization

- Chi-square gate: NIS > `innovationGate` × (measurement dimension), with
  `innovationGate = 6.0` per degree of freedom. For 2-dof flow the threshold is
  12.0; for 3-dof GPS position, 18.0; for the 6-dof joint GPS, 36.0.
- A rejected correction **never modifies the state**.
- `rejectedCorrectionLimit = 50` consecutive rejected corrections force a full
  re-initialization of the declared initialization policy, flagged by
  `covarianceReinitialized`. The counter advances only on ticks where a fresh
  aiding sample was actually attempted, so aiding dropout alone cannot trigger it.
- On re-initialization, position is seeded from mocap if valid, else GPS
  position if valid, else the **parameter** `initialPositionWorldEnu_m`.
  Optical flow **never** seeds position.

### 4.3 Covariance limits

Diagonal growth bounds: position 1e4 m², velocity 4e2 m²/s², attitude 10 rad²,
gyro bias 1e-2 rad²/s², accel bias 1.0 m²/s⁴. At those values the state is
already maximally uninformative; a state pinned at the limit means the filter
has stopped learning, not that it is healthy.

### 4.4 Optical flow observes velocity only

`correctOpticalFlow` builds a 2-row measurement with
`H = [0₂ₓ₃ | select(v_body,1:2) | wedge(v_body)[1:2,:] | 0₂ₓ₆]` over the
15-state tangent ordered [position, velocity, attitude, gyro bias, accel bias].
There is **no position row and no vertical row**. `groundDistance_m` is carried
into `step` but `correctOpticalFlow` never consumes it. Consequence, already
measured in simulation: horizontal error stays small (mm) under flow aiding
while vertical free-drifts (≈1 m of vertical error after disarm in the 45 s
reference mission). This is the observability structure, not a defect, and it
must not be "fixed" on the flight line.

---

## 5. Pilot brief

### 5.1 Estimator invalidity costs ACRO too — standing correction

**In any state where `estimate_valid` is false, ACRO is lost as well.** The rate
loop takes body rates from the **estimator**, not from the raw gyro. When the
estimate goes invalid, `RatesValid` clears, motors zero, and the fault latches.
This is fail-closed and consistent, but the earlier assumption that "the vehicle
remains flyable in manual under estimator invalidity" is **NOT TRUE**.
**ACRO is not an estimator-loss fallback.**

Mitigation of record: the no-GPS state is not reachable as a wedge — the
generated initialization falls back to the initial tunables and still
initializes, so an onboard build with no fix still flies.

### 5.2 POSITION refusal and degrade behavior

| Situation | Behavior the pilot will see |
| --- | --- |
| **On the ground / pre-arm**, POSITION requested with GNSS unready | Generated controller disarmed; rate/attitude publications **withheld**; the high-switch request **latches** a control fault. Rate's 25 ms command-age gate then zeros/disarms. The latch clears **only** on a *fresh* `Valid | Active` ManualControl update with ArmSwitch **low** — retained low data will not clear it. |
| **Already armed**, POSITION requested with GNSS unready, or ready→unready loss in flight | The request is **refused/degraded**: effective mode becomes ATTITUDE, the generated controller stays **armed**, commands keep publishing **continuously**, and **no readiness latch** is created. The pilot keeps a flyable vehicle and can retry or land in ATTITUDE/ACRO. |
| **ACRO / ATTITUDE** | Unaffected by GNSS readiness. Readiness gates the POSITION *capability*, never the airframe. |

### 5.3 Mission shell (only relevant if the ingress lane is in the image)

- The shell loads a bounded **relative**, current-altitude box template, **while
  disarmed only**, with fresh valid navigation and onboard readiness.
- **It does not take off.** Takeoff is always a manual ATTITUDE takeoff.
- The template stays pending through the manual takeoff; the planner rebases
  waypoint zero and all offsets to the current ENU position **only** on the
  armed POSITION transition, and issues once.
- RUNNING **aborts terminally** on mode exit, disarm, or RC / navigation /
  readiness loss, and **never resumes**. Recovery requires another valid
  disarmed load.

### 5.4 Timing contracts the pilot may hear called out

- Rate command age > 25 ms → zero/disarm/latch.
- Trajectory-reference budget: 100 ms.
- Estimator aided tick budget: 5 ms (G7).
- GNSS: 10 Hz nominal; a sample must arrive within 300 ms.

### 5.5 Flow blocks

Blocks 7 and 8 are **expected to be skipped**. If they are live, the brief must
add: flow gives horizontal velocity aiding only; the vertical channel will drift
and that is expected; flow is never a GPS substitute on this card.

---

## 6. Telemetry watch items

Envelope columns marked `TBD-R#` are owned by the rehearsal-sim agent. Threshold
columns with a citation are **contract numbers from source** and are not
rehearsal outputs — they do not change.

### 6.1 Estimator health

| # | Signal | Where | Nominal | Yellow (watch) | Red (act) | Action on red |
| --- | --- | --- | --- | --- | --- | --- |
| E1 | `estimate_valid` | odometry publish gate | true, continuous | any single-tick dropout | any false transition | Land immediately (§5.1 — ACRO is gone too) |
| E2 | `status_initialized` | estimator status | true | — | false in flight | Land immediately |
| E3 | `predictionAccepted` | estimator status | true every tick | TBD-R18 dropout rate | false while IMU valid | Land; IMU path suspect |
| E4 | `consecutiveRejectedCorrections` | estimator status | 0 | ≥ TBD-R19 | ≥ 25 (half of the 50 re-init limit) | Exit POSITION, land |
| E5 | `covarianceReinitialized` | estimator status | never true after origin latch | — | **any** true event | Land immediately; state may jump |
| E6 | `innovationGateRejected` | estimator status | transient only | > TBD-R20 fraction of aided ticks | persistently true | Exit aided modes, land |
| E7 | `gpsPositionCorrectionAccepted` | estimator status | true at aiding rate | acceptance < TBD-R21 | acceptance < TBD-R9 | Abort Block 5/6 |
| E8 | `gpsVelocityCorrectionAccepted` | estimator status | true when both GPS validities set | TBD-R22 | TBD-R22 | Note; GPS velocity is dropped when course is invalid (accepted design) |
| E9 | `opticalFlowCorrectionAccepted` | estimator status | *(flow blocks only)* true on non-colliding ticks | TBD-R23 | never true while flow claims valid | Abort Block 8 |
| E10 | `rumoca_galec_error_signal_status` | generated step | 0 | any nonzero | any nonzero | Land; generated-code error signal |
| E11 | Estimator outputs finite | firmware `efmu_estimate_is_finite` | true | — | false | Land immediately |

### 6.2 Navigation quality

| # | Signal | Nominal | Yellow | Red | Action |
| --- | --- | --- | --- | --- | --- |
| N1 | ENU position vs known reference | TBD-R24 | TBD-R25 | TBD-R26 | Exit POSITION |
| N2 | ENU velocity magnitude at hover | TBD-R27 | TBD-R28 | TBD-R29 | Exit POSITION |
| N3 | Position hold drift rate (Block 5/6) | TBD-R12 | TBD-R30 | TBD-R13 | Exit POSITION to ATTITUDE |
| N4 | Vertical estimate drift (flow blocks) | matches TBD-R17 prediction | TBD-R31 | TBD-R32 | Abort Block 8 (drift is expected; deviation from prediction is not) |
| N5 | Attitude vs visual | agrees | TBD-R33 | obvious disagreement | Land |
| N6 | `quality_pct` (published odometry) | TBD-R34 | TBD-R35 | TBD-R36 | Exit POSITION |
| N7 | `reset_counter` (published odometry) | static | any increment | repeated increments | Land |

### 6.3 GNSS

| # | Signal | Nominal | Yellow | Red | Action |
| --- | --- | --- | --- | --- | --- |
| G-1 | Fix type | 3D or better | — | below 3D | POSITION unavailable; stay ATTITUDE |
| G-2 | `hacc` | TBD-R37 | TBD-R38 | > 10,000 mm (`RDD2_GNSS_M10_MAX_HACC_MM`) | Readiness drops; expect POSITION refuse/degrade |
| G-3 | `vacc` | TBD-R39 | TBD-R40 | > 15,000 mm (`RDD2_GNSS_M10_MAX_VACC_MM`) | As above |
| G-4 | Sample gap | 100 ms (`TARGET_PERIOD_MS`) | outside 75–125 ms (`MIN/MAX_GAP_MS`) | no sample in 300 ms (`RECENT_MS`) | Readiness drops |
| G-5 | Origin latch state | latched once, stable | — | unlatch/relatch in flight | Land; the local frame moved |
| G-6 | Satellite count | TBD-R41 | TBD-R42 | TBD-R43 | Delay/abort GPS blocks |

### 6.4 Timing and resources

| # | Signal | Nominal | Yellow | Red | Action |
| --- | --- | --- | --- | --- | --- |
| T1 | Aided estimator tick | TBD-R44 (bench 285 µs) | TBD-R45 | ≥ 5 ms (G7 budget) | STOP / land |
| T2 | Nav-thread stack watermark | TBD-R1 | TBD-R46 | TBD-R47 | STOP |
| T3 | Rate command age | < 25 ms | TBD-R48 | ≥ 25 ms (zero/disarm/latch fires) | Fail-closed acts on its own; land |
| T4 | Guidance/trajectory reference age | < 100 ms (budget of record) | TBD-R49 | ≥ 100 ms | Exit POSITION |
| T5 | Guidance loop rate | 200 Hz | TBD-R50 | TBD-R51 | Land |
| T6 | Rate loop rate | 1600 Hz | TBD-R52 | TBD-R53 | Land |
| T7 | CPU load / missed deadlines | TBD-R54 | TBD-R55 | TBD-R56 | Land |

### 6.5 Optical flow (Blocks 7–8 only)

| # | Signal | Nominal | Yellow | Red | Action |
| --- | --- | --- | --- | --- | --- |
| F1 | Flow `valid` | true over textured ground in range | intermittent | false while in the flight envelope | Abort flow blocks |
| F2 | Producer quality metric | TBD-R57 | TBD-R58 | TBD-R59 | Abort flow blocks |
| F3 | Range / height above ground | inside the sensor's `min/max_ground_distance_m` | TBD-R60 | outside the band | Flow must self-invalidate; if it does not, abort |
| F4 | Flow sample age at fuse | TBD-R61 | TBD-R62 | > the staleness limit set in the requirements doc (OPEN-F5) | Abort |
| F5 | Flow-vs-GPS horizontal velocity agreement | TBD-R16 | TBD-R63 | TBD-R64 | Abort Block 8 |

### 6.6 Airframe / standard

| # | Signal | Red | Action |
| --- | --- | --- | --- |
| A1 | Battery voltage / remaining | TBD-R65 | Land |
| A2 | RC link quality / failsafe | any failsafe entry | Per standard failsafe procedure |
| A3 | Motor outputs | saturation or asymmetry beyond TBD-R66 | Land |
| A4 | Vibration / IMU clipping | TBD-R67 | Land; the estimator's prediction path is affected |

---

## 7. Rollback and abort ladder

Ascending severity. The pilot may skip levels upward at any time without asking.

| Level | Trigger | Action | Re-entry |
| --- | --- | --- | --- |
| **L0 — Observe** | A yellow cell in §6 | Call it out, log it, continue the current block | n/a |
| **L1 — Hold** | Two yellows, or one yellow trending toward red | Stop advancing; hold the current condition; do not enter the next block | Continue when the signal recovers and is stable for TBD-R68 |
| **L2 — Mode rollback** | Any red in §6.2 or §6.5, or reference age ≥ 100 ms | Exit POSITION → ATTITUDE. Pilot flies manually. Flow blocks: disable flow aiding | Only after ground review of the log |
| **L3 — Land** | Any red in §6.1 (except E1/E5/E11), §6.3 G-5, §6.4 T3–T7 | Land in ATTITUDE at the nearest safe point. Disarm on the ground | Day continues only with a named cause and a written disposition |
| **L4 — Land immediately** | E1 (`estimate_valid` false), E5 (`covarianceReinitialized`), E11 (non-finite outputs), T1 ≥ 5 ms | Immediate descent and landing. Expect that if the estimate is invalid the aircraft is **already** motors-zero and latched (§5.1) — the pilot's job is to protect people, not to recover the aircraft | **End of flying for the day.** |
| **L5 — Stop the day** | Any Block 0/1/2 abort criterion; any ENU sign error; any publication gap at a POSITION transition; any flow sign error | Power down. No further blocks. Written finding into the ledger before any re-attempt | New card |

### 7.1 Block-level rollback

- **Block 6 fails** → fall back to Block 5's tethered configuration; the day
  still counts as a GPS-aided POSITION result.
- **Block 7 fails** → Block 8 is cancelled; GPS results stand.
- **Block 8 fails** → flow is removed from scope; land, disable flow aiding,
  and the day's GPS results stand.
- **Any block fails on a firmware-contract violation** (publication gap, latch
  where none is allowed, refusal that is not honored) → the finding goes to
  Codex's lane and the card is not re-run until a fresh receipt exists.

### 7.2 Image rollback

The frozen v3 pair (`98e622a3…` normal, `6a432077…` FastDyn) is the fallback
image of record. Those build directories are read-only and must not be
rebuilt or overwritten. If a newer image with the ingress lanes fails any block,
reflash the frozen normal image, re-verify P1/P2, and continue at Block 5 scope
(tethered POSITION), dropping Block 6.

---

## 8. Conditional-dependency index

Every claim on this card that depends on Codex-side work, with the receipt that
closes it.

| Ref | Dependency | Blocks affected | Receipt that closes it |
| --- | --- | --- | --- |
| PENDING-G7 (P3) | On-target aided tick < 5 ms + stack watermark on the flown image | **all powered blocks** | Measurement receipt from the flown image |
| PENDING-R2 (P4) | `modelica_models` `sparsity-limit-covariance` @ `a9e5037` published and pinned in `cerebri_rdd2/west.yml` | provenance row of Block 0 | `git ls-remote` showing the branch at `a9e5037`, plus the west pin |
| PENDING-IMAGE-DECISION (P6) | Frozen v3 pair vs new build with ingress lanes | Blocks 0, 6 | Explicit Codex answer + receipts for the chosen image |
| PENDING-INGRESS (P5) | Planner lifecycle, `mission_shell.c`, Guidance reference gating, SPEC_0002/0003 — committed, frozen, APPROVEd, **and in the flown image** | Block 6 | Fresh independent adversarial APPROVE on the integrated tree + image inventory |
| PENDING-AIRBORNE-LOSS | Wrapper discriminators proving airborne capability loss degrades to effective ATTITUDE with **no publication gap and no latch** | Blocks 3, 5, 6 | Named passing discriminators in the flown image's test receipt |
| PENDING-FLOW-PRODUCER (P7) | Calibrated flow producer + transport + adapter + calibration receipt | Blocks 7, 8 | Producer implementation meeting `2026-08-13-flow-producer-requirements.md`, plus a calibration receipt covering scale, mount rotation and sign |
| PENDING-VERIFY-P8 | GNSS observability on shell + downlink in the flown image | Block 1 | Observed `gnss status`, `zros topic hz gnss_fix`, and downlink fields on the day |
| PENDING-CUBS2 (P9) | CUBS2 CSyn v0.9 hard cutover | **none** — release blocker only | CUBS2 `west.yml` + validation receipt |
| TBD-R1 … TBD-R68 | Rehearsal-sim expected-value envelope | §2 success criteria, §6 envelope columns | Rehearsal agent's envelope table |

---

## 9. Sign-off

This card is not valid for flight until the following are filled in on the day:

- [ ] All `PENDING-*` items either GREEN with a named receipt, or the dependent
      block explicitly marked SKIPPED.
- [ ] All `TBD-R#` cells populated from the rehearsal envelope.
- [ ] P4 provenance disposition recorded (accepted debt or blocker) — James.
- [ ] Flown image sha256 and `modelica_models` pin written on the card — Block 0.
- [ ] Pilot brief §5 delivered and acknowledged, including §5.1.
- [ ] Abort authority named. Any observer may call L4 or L5.
