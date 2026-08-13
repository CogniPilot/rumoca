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

## BLOCKERS — nothing arms on the frozen v3 images

**Four SEV-1 blockers were found on the receipted v3 bytes after this card's
first draft. The frozen v3 pair (`98e622a3…` / `6a432077…`) carries all four.
Every POSITION block on this card is RED until a fix set lands in a
re-receipted, re-reviewed bundle.**

| ID | Finding | Reachability | Effect | Owner |
| --- | --- | --- | --- | --- |
| **B1** | **Compiler wrong-code.** `EulerB321.from_Quat` assigns three euler elements per conditional branch; the generated GALEC kept only the **last write per branch**. Pitch and yaw are **hard-zero** in the flight bytes (witnessed: pitch 0.5 rad → euler (0,0,0)); **`euler[2]` (pitch) is assigned in no branch of any container**. Quaternion and DCM outputs are correct. Confirmed in **3 of 6** containers — `Controller`, `GuidanceController`, `NavigationEstimator` (§1.2) | **Every tick.** `GuidanceController` consumes it, **and `Controller` calls the defective function itself** to build its heading basis — yielding a **constant East-pointing heading basis** in normal flight (§1.2) | Breaks the attitude reference in any Guidance-active mode, and the heading basis wherever `Controller` runs | Compiler fix agent (`fix/branch-multi-write-erasure`) |
| **B2** | **NaN/Inf GPS is ACCEPTED.** A NaN NIS makes every gate comparison false, so `accepted=true`. State is permanently poisoned, `estimate.valid` **stays 1**, and auto-recovery is structurally impossible because acceptance resets the rejection counter. Only an external reset clears it | Any non-finite GPS field | Silent, unrecoverable navigation loss with a valid-looking estimate | Model-hardening agent |
| **B3** | **Auto re-init adopts the rejected fix.** 50 consecutive rejections re-seed position from the very outlier the gate rejected, reset attitude to identity and velocity to zero **mid-air**. At the 1 kHz wiring this triggers in **51 ms** — the model docstring assumed 20–100 Hz aiding, so the trigger is 10–50× faster than designed. `estimate.valid` stays 1 throughout | 50 consecutive rejections | Mid-air state teleport with no invalidity indication | Model-hardening agent |
| **B4** | **Flow-induced re-init lockout.** Ordinary preconditions (GPS outage > 1 s + degraded flow + speed > 6.3 m/s): the **shared** rejection counter makes a broken flow sensor invisible under healthy GPS; the outage drives 50 flow rejections → auto re-init → position teleports to the **parameter** origin, velocity to zero → post-re-init `P_vv = 1.0` gates out truthful returning GPS above 6.26 m/s → **permanent re-init loop at ~20 Hz** with `estimate_valid = 1` and `error_signal = 0x0` throughout. Witnessed end-to-end: Guidance consumes v=(0,0,0) at 8–12 m/s true; final position error **147–192 m** | GPS outage + degraded flow + moderate speed | Total loss of navigation, fully silent | Model-hardening agent (acceptance scenario) |

### What this changes on this card

| Block | Was | Now |
| --- | --- | --- |
| 0–2 (provenance, props-off gates, ENU walk) | RUN | RUN — unaffected, and B1–B4 make Block 1's status-word observation more important, not less |
| 3 steps 1–3 (ground interlock, disarmed) | RUN | RUN on v3 — these observe *interlock semantics*, not navigation quality |
| 3 step 4 (armed restrained surrogate) | RUN | **RED on v3** — arms with Guidance active (B1) |
| 4 (manual ACRO/ATTITUDE) | RUN | **RED on v3.** P11 resolved: the deployed `GuidanceController` evaluates `from_Quat` unconditionally *before* mode-select, and its branches are ACRO/ATTITUDE/POSITION — **all three modes** execute the corrupted path (§1.2) |
| 5 (tethered POSITION) | RUN | **RED on v3** |
| 6 (untethered POSITION) | COND | **RED on v3**, and still COND on ingress |
| 7–8 (flow) | COND/SKIP | **RED on v3** in addition to PENDING-FLOW-PRODUCER — B4 is a flow-triggered blocker and the flow-path producer requirements are now load-bearing safety requirements, not quality-of-implementation preferences |

### The honest read

The image decision (P6) is made by these findings, **unanimously between both
agents**: a new bundle from a fixed compiler plus a hardened model, dual-built,
re-receipted and re-reviewed, is the only path to **any** powered flight. That
also pulls the mission-ingress lanes into the image, which closes P5's image
question as a side effect. Timeline consequence accepted.

**Nothing arms on v3.** P11's resolution (§1.2) removed the last B1-independent
block: all three flight modes execute the corrupted path. The executable day on
the frozen v3 images is **Blocks 0, 1, 2 and Block 3 steps 1–3** — ground work
only. That is still worth doing: it validates the GNSS chain, the ENU frame and
the ground-side interlock semantics without depending on any of B1–B4, and none
of it has to be repeated after the re-cut.

**All old ELF/BIN receipts are superseded** (§1.5). No prior image evidence
counts toward anything on this card.

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
| **A. GPS-aided POSITION, tethered** | already in the prior scope (item 2); extended here with explicit GPS-aiding observation | **RED on v3** (B1) — RUN only on a fixed re-receipted bundle (Block 5) |
| **B. GPS-aided POSITION, untethered** | the mission-ingress lanes must be in the flown image AND carry the airborne-capability-loss contract | **RED on v3** (B1), and CONDITIONAL — **PENDING-INGRESS** (§1 P5) |
| **C. Optical-flow aiding** | a calibrated flow producer and transport must exist in the flown image | **RED on v3** (B4), and CONDITIONAL — **PENDING-FLOW-PRODUCER** (§1 P7); at time of writing **no producer exists**, so Blocks 7–8 are expected to be SKIPPED |

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
| **P1** | eFMU bundle identity verified against the aircraft image | Manifest digest of the **re-cut** bundle; all deployed C/H/ALG files byte-matching it. *(Historical: v3 digest `f87f8e5acd5b1c51edd3d529a303043fb6abfc1137a759e4ca57034784a98c69`, bundle `/home/jgoppert/rdd2-flight-efmus-v3`, receipts `/home/jgoppert/rdd2-flight-build-receipts-v3-final`)* | Codex | **RED — awaiting the re-cut bundle.** The earlier GREEN is **withdrawn**: v3 carries B1–B4 and its receipts are superseded (§1.5) |
| **P2** | Flashed ELF sha256 matches an approved build | sha256 of the **new** dual builds, with fit figures. *(Historical only, credits nothing: `98e622a3…` normal — flash 417,064 B / 9.94%, RAM 337,140 B / 64.30%; `6a432077…` FastDyn — flash 218,064 B / 5.20%, RAM 260,820 B / 49.75%)* | Codex | **RED — all old ELF/BIN receipts superseded** (§1.5) |
| **P3** | **G7** — aided estimator tick on target under the 5 ms budget, plus stack watermark | On-target measurement receipt from the flown image. Bench margin is 17× at 285 µs; the ledger is explicit that it must be *measured, not assumed* | Codex | **RED / PENDING-G7.** Ledger records G7 as never satisfied — "G7 is not manufacturable without hardware". **Hard blocker for every powered block.** |
| **P4** | Flight-model source published and pinned | `modelica_models` branch `sparsity-limit-covariance` @ `a9e5037` pushed to `git@github.com:cognipilot/modelica_models` and pinned in `cerebri_rdd2/west.yml` | Codex, or James's terminal | **RED / PENDING-R2.** `west.yml` still pins `modelica_models d319854…`, which lacks four of five required sources. Claude cannot push (ssh publickey denied). This is a **provenance** blocker, not a flight-safety blocker — see §1.1 |
| **P5** | Mission-ingress lanes implemented, reviewed, **and present in a creditable image** | Host-level review receipts **plus** image regeneration. See §1.3 for the full precondition chain | Codex | **PENDING-INGRESS — upgraded.** No longer "uncommitted working-tree source": it is **implemented and independently host-reviewed** (shell 12/12, planner 10/10, wrapper 23/23 at their reviewed snapshots). What remains is **image regeneration**, not implementation. **Gates Block 6 only.** |
| **P6** | Image identity decision: fly the frozen v3 pair, or a new build carrying the ingress lanes | Explicit Codex answer + a receipt for whichever image is chosen. A new image invalidates P1/P2/P3 receipts and needs its own | Codex | **PENDING-IMAGE-DECISION.** Question posed in the ledger; unanswered as drafted |
| **P7** | Optical-flow producer calibration receipt and transport | A calibrated flow producer in `cerebri_rdd2`, a zros topic carrying it, an adapter feeding `opticalFlow_*` into the eFMU, and a **calibration receipt** (scale/focal, mount rotation, sign verification) | Codex | **RED — CONFIRMED UNMET, NO WORK IN PROGRESS.** See §1.4. **Blocks 7–8 SKIP.** |
| **P8** | GNSS observability on the downlink | `gnss status` shell output, `zros topic hz gnss_fix`, and telemetry exposing navigation odometry, origin latch and correction acceptance | Codex | **PENDING-VERIFY.** An earlier audit found VehicleHealth omitted GNSS and the radio downlink exposed neither odometry, origin latch, nor correction acceptance. The M10/diagnostics lane was approved afterwards; confirm on the day against the flown image before Block 1 |
| **P9** | CUBS2 CSyn v0.9 hard cutover | CUBS2 `west.yml` on `csyn c34dd35d…` / `synapse_fbs v0.9.0` plus validation receipt | Codex (ACKed 13:20) | **PENDING-CUBS2.** *Not an RDD2 flight blocker* — it is a cross-vehicle ABI **release** blocker. Recorded here because it was requested as a prerequisite row; it does not gate any block on this card |
| **P10** | **B1–B4 fix set in a re-cut bundle** — **UNANIMOUS** (Codex concurrence on record) | Compiler fix for the conditional multi-write erasure (B1) + model hardening for the NaN acceptance predicate (B2), the re-init policy (B3) and the flow-induced lockout (B4); then **clean bundle freeze → fresh dual builds → exact manifest provenance → fit → adversarial artifact review** | Compiler fix agent + model-hardening agent, then Codex for the image | **RED / PENDING-FIXSET.** **Gates every block that arms the vehicle or commands motors** — i.e. everything except Blocks 0–2 and Block 3 steps 1–3. Both fixes are in adversarial review (§1.2.3) |
| **P11** | Scope of B1 — which flight modes execute the corrupted euler path | **ANSWERED with firmware evidence** (§1.2): `efmi.cmake` + link map prove the monolithic `Controller` is absent, but the deployed `GuidanceController` evaluates `from_Quat` **unconditionally, before** its mode-select equations, whose branches are 0=ACRO / 1=ATTITUDE / 2=POSITION | Codex | **RESOLVED — RED.** **All three flight modes** execute the B1-corrupted path on v3. **No mode is credited on v3.** Block 4 goes RED |

### 1.1 Honest statement on P4

P4 is a reproducibility/provenance blocker. The bytes that would fly were
emitted from `a9e5037` and are independently receipted (P1); the pin gap means
the *advertised clean CI path cannot reproduce them from a public source*, not
that the flown bytes are unverified. Flying with P4 red is a documented
provenance debt, not an unverified-artifact hazard. The decision to accept that
debt is James's, and it must be recorded on the card before Block 1.

### 1.2 P11 — RESOLVED RED: all three flight modes execute the B1-corrupted path

> **Ruling, adopted from Codex with firmware evidence (ledger row ~line 21357):
> no flight mode is credited on v3.** Flight images must be regenerated from the
> reviewed B1-fixed compiler/model bundle and dual-built/re-reviewed before any
> mode is credited.

**Firmware evidence — the monolithic `Controller` is NOT in the flight image.**
`cerebri_rdd2/src/efmi.cmake` adds only `WaypointTrajectoryPlanner`,
`GuidanceController`, `RateControlAllocator` and `NavigationEstimator`. There is
no `Vehicles_Rdd2_Controller` source or call anywhere in the firmware tree, and
the final normal M7 **link map** carries `GuidanceController` and
`RateControlAllocator` startup/recalibrate/dostep symbols with **no monolithic
Controller symbols**:

| Symbol | Address |
| --- | --- |
| `GuidanceController` startup | `0x700137cc` |
| `GuidanceController` dostep | `0x70013a68` |
| `GuidanceController` recalibrate | `0x7003f1f4` |
| `Controller` (any) | **no match** |

**This does not make B1 irrelevant — it makes it worse.** The deployed
`Vehicles.Rdd2.GuidanceController` evaluates

```
LieGroups.SO3.EulerB321.from_Quat(navigation.quaternionWorldBody)
```

**unconditionally, before** its mode-select equations. Those branches are
**mode 0 = ACRO, 1 = ATTITUDE, 2 = POSITION**. So **every one of the three flight
modes executes the affected quaternion/Euler path** in the current generated
artifact — including the manual modes that Block 4 was relying on as the
B1-independent fallback.

**Consequence on this card:**

- **Block 4 (manual ACRO/ATTITUDE free flight) is RED on v3.**
- The **executable day on the frozen v3 images collapses to ground work**:
  **Blocks 0, 1, 2 and Block 3 steps 1–3.** Nothing arms.
- **P10 (new bundle) is now the gating prerequisite for every block that arms
  the vehicle or commands motors**, and it is **UNANIMOUS** — Codex's concurrence
  is on record.

#### 1.2.1 Superseded analysis, retained because the mechanism is the same

An earlier revision of this card analyzed `Controller.alg`'s heading-basis path.
That analysis was correct and has since been **independently reproduced as the
B1 fix's execution witness** — but it describes a container that is **not linked
into the flight image**, so it is **not** the flight-path argument. It is kept
because it is the clearest available illustration of the defect's shape:

`Controller.alg` declares `navigation.eulerRpy_rad` and never reads it (verified:
one occurrence, line 13) — yet it calls the defective `from_Quat` itself at 4
sites:

```
902  (headingEuler) := 'LieGroups.SO3.EulerB321.from_Quat'(headingQuaternionWorldBody);
904  headingDirectionWorld[i] := (i==1 ? cos(headingEuler[1])
                                : i==2 ? sin(headingEuler[1]) : 0.0);
906  headingBasisWorld := headingDirectionWorld;
907+ bodyYWorld := thrustDirectionWorld × headingBasisWorld;
```

with the emitted `from_Quat` in its erased form:

```
for i in 1:3 loop 'euler'[i] := 0.0; end for;   (twice)
if (sinp*sinp) > 0.9999^2 then
    'euler'[1] := atan2(...);      <- gimbal-lock branch: ONLY yaw survives
else
    'euler'[3] := atan2(...);      <- NORMAL branch: ONLY roll survives
end if;
```

`euler[1]` is yaw and `euler[2]` is pitch (confirmed against `to_Quat`, which
reads `cy := cos('euler'[1]/2)`). **`euler[2]` is assigned in no branch of any
container.** In normal flight the `else` branch runs, yaw stays `0.0`, and
`headingDirectionWorld` becomes the constant `(1, 0, 0)` — a fixed East-pointing
heading basis regardless of commanded heading. The B1 fix branch's execution
witness records exactly this quantity recovering: *"Controller
headingDirectionWorld now tracks commanded yaw (was constant East)."*

**Measured blast radius across the v3 bundle** (`from_Quat` call sites / branch
assignments retained). Note that being unlinked is what spares the last three,
not being clean:

| Container | In flight image | `from_Quat` calls | roll | yaw | **pitch** |
| --- | --- | --- | --- | --- | --- |
| `Vehicles_Rdd2_GuidanceController` | **YES** | 2 | 1 | 1 | **0** |
| `Vehicles_Rdd2_NavigationEstimator` | **YES** | 1 | 1 | 1 | **0** |
| `Vehicles_Rdd2_Controller` | no | 4 | 1 | 1 | **0** |
| `Vehicles_Rdd2_RateControlAllocator` | **YES** | 0 | — | — | — |
| `Planning_Bezier_WaypointTrajectoryPlanner` | **YES** | 0 | — | — | — |
| `Vehicles_Cubs2_OuterLoop` | no | 0 | — | — | — |

Three of six containers carry the erasure, matching the ledger; **two of those
three are linked into the flight image.**

#### 1.2.2 Firmware euler consumption — clean, and immaterial

`cerebri_rdd2` contains **zero** references to any euler symbol in `src/`,
`subsys/` or `tests/`; the eFMU exposes `estimate_eulerRpy_rad` and nothing reads
it. The only repo-wide hits are host-side `xtask` tooling and a
`docs/ground_station_telemetry.md` note that attitude publishes as a quaternion.

This is genuinely clean — and it does not help. The corruption is consumed
**inside** `GuidanceController` before anything crosses the firmware boundary.
Auditing the boundary was the right question to ask and the wrong place to look.

#### 1.2.3 Fix status — both fixes in adversarial review

| Fix | Covers | Branch / status |
| --- | --- | --- |
| Compiler multi-write erasure | **B1** | `fix/branch-multi-write-erasure` @ `623a6845`, suite 329/12, execution witness recorded. **Adversarial review running.** Root cause: GALEC's `lower_indexed_function_update_expression` unwrapped one level of the DAE's nested `ArrayUpdate` chain and discarded the base; fix is a full chain peel plus a root guard. The DAE/sim path was always base-recursive, which is why only the GALEC bytes were wrong |
| Estimator hardening | **B2, B3, B4** | `estimator-nan-reinit-hardening` (modelica_models) — two-stage covariance ladder + affirmative acceptance predicate. **In review** |

**The bundle re-cut starts when both clear.** Until then there is no candidate
image for any powered block, and B1 is confirmed **LIVE at the compiler tip**
(`galec-c-integration` `44f67022`) — the ~360 commits since the earlier base did
not fix it.

---

#### 1.2.4 Original P11 evidence trail (model side)

**Partial answer received (model side, from the GPS-validation agent):**
`Controller.alg` declares `navigation.eulerRpy_rad` as an input but **never reads
it** — no occurrence beyond the declaration. **I verified this independently and
it is true:** `grep` over
`rdd2-flight-efmus-v3/Controller/…/Vehicles_Rdd2_Controller.alg` returns exactly
one occurrence, line 13, the declaration.

**But the conclusion "the model-side attitude/rate path is clean of B1" does NOT
follow, and is false.** B1 is a defect *inside* `EulerB321.from_Quat`, and
`Controller.alg` **calls that function itself** — 4 call sites. The corruption
does not need the input port.

**Witnessed chain in the v3 bytes** (`Vehicles_Rdd2_Controller.alg`):

```
line 902  (headingEuler) := 'LieGroups.SO3.EulerB321.from_Quat'(headingQuaternionWorldBody);
line 904  headingDirectionWorld[i] := (i==1 ? cos(headingEuler[1])
                                     : i==2 ? sin(headingEuler[1]) : 0.0);
line 906  headingBasisWorld := headingDirectionWorld;
line 907+ bodyYWorld := thrustDirectionWorld × headingBasisWorld;
```

and the emitted `from_Quat` body (lines 683–711) is the erased form:

```
for i in 1:3 loop 'euler'[i] := 0.0; end for;   (twice)
if (sinp*sinp) > 0.9999^2 then
    'euler'[1] := atan2(...);      <- gimbal-lock branch: ONLY yaw survives
else
    'euler'[3] := atan2(...);      <- NORMAL branch: ONLY roll survives
end if;
```

`euler[1]` is **yaw** and `euler[2]` is **pitch** (confirmed against `to_Quat`,
which reads `cy := cos('euler'[1]/2)`). **`euler[2]` is never assigned in any
branch, in any container.** In normal flight the `else` branch runs, so yaw stays
`0.0` from the initialization loop, and therefore:

> **`headingDirectionWorld` is the constant `(1, 0, 0)` — a fixed East-pointing
> heading basis — regardless of the commanded heading quaternion.** The
> controller's attitude setpoint is built on it via `bodyYWorld`.

**Measured blast radius across the v3 bundle** (`from_Quat` call sites / branch
assignments retained):

| Container | `from_Quat` calls | roll assign | yaw assign | **pitch assign** |
| --- | --- | --- | --- | --- |
| `Vehicles_Rdd2_Controller` | 4 | 1 | 1 | **0** |
| `Vehicles_Rdd2_GuidanceController` | 2 | 1 | 1 | **0** |
| `Vehicles_Rdd2_NavigationEstimator` | 1 | 1 | 1 | **0** |
| `Vehicles_Rdd2_RateControlAllocator` | 0 | — | — | — |
| `Planning_Bezier_WaypointTrajectoryPlanner` | 0 | — | — | — |
| `Vehicles_Cubs2_OuterLoop` | 0 | — | — | — |

Three of six containers, matching the ledger's "replicated in 3 of 6".

**Firmware side — verified clean.** The eFMU exposes
`estimate_eulerRpy_rad` / `eulerRpy_rad` in the generated headers, and
`cerebri_rdd2` contains **zero** references to any euler symbol in `src/`,
`subsys/` or `tests/`. The only hits repo-wide are host-side `xtask` simulation
tooling and a `docs/ground_station_telemetry.md` note stating that attitude is
published as a quaternion. So **no firmware consumer reads the eFMU's euler
output.**

**Net effect on P11 — it does not tilt Block 4 toward RUN.** The firmware
boundary is clean, which was the question asked; but the damage is *upstream of
that boundary*, inside the generated controllers themselves. The remaining
question is therefore **not** "does firmware read euler" (answered: no) but:

> **Which flight modes execute `Controller.alg`'s heading-basis path?** If manual
> ACRO/ATTITUDE routes through `Controller` at all, the attitude setpoint is
> built on a constant heading basis and Block 4 is **RED on v3**.

That is a firmware mode-router question and it is **Codex's to answer.** Until it
is answered, Block 4 stays CONDITIONAL and the model-side evidence does **not**
support relaxing it.

### 1.3 P5 — mission ingress is IMPLEMENTED and host-reviewed; only the image is missing

Codex's Q1 answer upgrades this row substantially. What exists **now**, in the
dirty integration tree, independently reviewed at host level:

- **`src/interfaces/mission_shell.c`** publishes a bounded **five-point square**
  to the single-publisher `waypoint_plan` topic via
  `mission box <side_m> <speed_m_s>`. It **rejects**: armed/failsafe state,
  arm/kill switch, stale or invalid health / manual / navigation, unready onboard
  GNSS, and out-of-range or non-finite arguments.
- **`src/processes/waypoint_trajectory_planner.c`** is authoritative for the
  **EMPTY / PENDING / RUNNING / ABORTED** lifecycle, current-altitude rebase,
  **no-resume abort**, and reference invalidation on abort.
- **Guidance independently requires** a current, finite, `LocalEnu`,
  zero-mask reference.

**Review receipts (at their reviewed snapshots):** shell **12/12**, planner
**10/10**, wrapper **23/23**.

This is a **bounded GPS square ingress** — *not* optical ingress, and *not*
automatic takeoff or landing. It does not by itself authorize an autonomous
mission run (§0.3 stands).

**Precondition chain before ingress is CREDITABLE for Block 6:**

```
P10 new bundle (B1 fix + B2/B3/B4 hardening)
  → fresh dual builds (normal mr_vmu_tropic + FastDyn)
    → adversarial artifact review
      → G7 on the new image
        → ingress creditable
```

Every arrow is a gate, and none of them is satisfied today. The host reviews are
real evidence and they do not shorten this chain.

### 1.4 P7 — optical flow is a confirmed HARD NO for this test cycle

Codex's Q2 answer, from source, matches my own independent audit exactly. In the
current `cerebri_rdd2` source the **only** production optical-flow assignments
are `navigation_estimator.c` hard-coding `opticalFlow_valid = false` and
`opticalFlow_fresh = false`. Repository search finds **no** optical-flow
producer, driver, transport, calibration, range/quality gate, or mounting/sign
conversion.

> **"Plots/model evidence cannot qualify optical for outdoor flight."**
> — Codex, adopted verbatim as this card's position.

That sentence is the one to carry forward. The estimator-side flow contract is
fully specified (`2026-08-13-flow-producer-requirements.md`), the sim evidence is
extensive, and the `synapse_fbs` v0.9.0 schema already defines
`OpticalFlow` / `OpticalFlowVelocity` — **none of that is a producer.** Nothing
in this card's flow analysis should be read as partial credit toward flying it.

**Disposition: the optical-flow half of this mission is DEFERRED beyond this
test cycle** unless a producer effort is started as its own lane. Blocks 7–8 are
SKIP — not "conditional pending a receipt", but skipped because the work has not
begun. If a producer lane starts, `2026-08-13-flow-producer-requirements.md` is
its contract and Block 7 is its first flight-line gate.

### 1.5 SUPERSEDING RULING — every old image receipt is void

**Codex's ruling, adopted: all old ELF/BIN receipts are superseded.** The
integration source is newer and dirty, and B1 requires new generated Guidance
bytes regardless. Consequences for this card:

- The ELF sha256s **`98e622a3…`** (normal) and **`6a432077…`** (FastDyn) **no
  longer constitute evidence of anything.** They are recorded below only as
  historical identifiers.
- P1's and P2's earlier "GREEN" status is **withdrawn.**
- The old final M7 map does contain the waypoint-planner object, but that fact
  **credits nothing** — the source it was built from has moved.
- The **only** path to a creditable image is: **clean bundle freeze → fresh
  normal `mr_vmu_tropic` and FastDyn builds → exact manifest provenance → fit
  → adversarial artifact review.**

There is consequently **no fallback image** on this card any more. §7.2's
rollback-to-frozen-v3 option is void for anything that arms.

### 1.6 Hard blockers

- **P10 (B1–B4 fix set)** blocks **every block that arms the vehicle or commands
  motors** — Blocks 3 step 4, 4, 5, 6, 7 and 8. **Unanimous between both agents.**
- **P3 (G7)** blocks every powered block, and must be re-measured on the **new**
  image — the old measurement, like every old receipt, is superseded (§1.5).
- ~~P11~~ is **resolved RED** (§1.2) and no longer a pending question; it is the
  reason Block 4 joined the RED list.

> **The executable day on the frozen v3 images is: Blocks 0, 1, 2 and Block 3
> steps 1–3. Nothing arms.**

The remaining prerequisites gate specific blocks, as marked.

**Still unanswered by the firmware lane, and tracked here:**

| Ref | Question | Gates |
| --- | --- | --- |
| **Q3** | CUBS2 CSyn v0.9 hard cutover status (P9) | Nothing on this card — release blocker only |
| **Q4** | Flight-model source publish route (P4/R2) — Codex's authenticated route, or James's terminal? | Block 0's provenance row, and Codex's FastDyn CI |
| **G7 scheduling** | When the on-target aided-tick and stack measurement happens on the **new** image | Every powered block |

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

### Block 3 — POSITION interlock checkout, tethered/restrained, props off then props on (steps 1–3 RUN; **step 4 RED on v3**)

Verifies the accepted GNSS-readiness interlock contract without asking the
aircraft to hold anything.

> **Step 4 requires P10.** Steps 1–3 are disarmed ground-side interlock
> semantics and are unaffected by B1–B4. Step 4 arms the vehicle with Guidance
> active, so on a v3 image its attitude reference carries the B1 yaw-zero
> defect. Run steps 1–3 on v3; hold step 4 for the fixed bundle.

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

### Block 4 — Manual ACRO / ATTITUDE free flight (**RED on v3 — requires P10**)

> **P11 is RESOLVED RED (§1.2).** The deployed `GuidanceController` evaluates
> `from_Quat` **unconditionally, before** its mode-select equations, and its
> branches are mode 0 = ACRO / 1 = ATTITUDE / 2 = POSITION. **All three flight
> modes execute the B1-corrupted path on v3.** Manual flight was the last
> candidate for a B1-independent block and it does not survive: **no mode is
> credited on v3.**

**Entry criteria**: **P10 GREEN** (B1-fixed + hardened bundle, dual-built and
re-reviewed). Blocks 1–3 green. Estimator `RatesValid` true and stable. Timing,
stacks, RC and actuator gates all green. Pilot brief §5 delivered.

**Procedure**
1. Manual ATTITUDE takeoff to a low hover. Hold.
2. Gentle attitude steps in each axis.
3. Short ACRO segment at safe altitude, pilot's discretion.
4. Land in ATTITUDE.

**Success criteria**
- No estimator invalidity event at any point (see §5.1 — invalidity costs ACRO
  too).
- `estimate_valid` continuously true in flight.
- `consecutiveRejectedCorrections` stays **≤ 5** (R5). Under healthy GPS the
  counter is *structurally* capped at 5 even with 100% corrupt flow — anything
  above 5 means the GPS aiding itself is being rejected.
- Attitude tracking and control feel nominal to the pilot.

**Abort criteria**
- Any `estimate_valid` false transition → land immediately; end the day at this
  block pending analysis.
- `covarianceReinitialized` true in flight → land immediately.

---

### Block 5 — GPS-aided POSITION, tethered / restrained (**RED on v3 — requires P10**; primary new objective on a fixed bundle)

> **Does not run on the frozen v3 images.** B1 gives Guidance an attitude
> reference with yaw hard-zeroed continuously; B2/B3 make a poisoned or
> teleported state indistinguishable from a healthy one on telemetry
> (`estimate.valid` stays 1 through both). Restraint bounds the consequence but
> does not make the observation meaningful.

**Entry criteria**: **P10 GREEN** (fixed, re-receipted, re-reviewed bundle).
Block 4 green. Aircraft tethered or restrained such that a runaway cannot
travel. GNSS ready. Origin latched. Spotter posted.

**Procedure**
1. Arm in ATTITUDE, establish a stable low hover within the tether.
2. Transition to POSITION. Observe **command continuity** across the transition
   (no publication gap, no motor transient).
3. Hold for TBD-R6 seconds *(hold duration — not derivable from model sim; owed
   by the conduct agent)*. Observe position and velocity residuals against the
   tether-constrained truth.
4. Introduce a deliberate GNSS degradation (antenna mask) and observe the
   degrade path: effective ATTITUDE, publication continuous, no latch.
5. Restore GNSS; observe reacquisition and correction-acceptance resumption.
6. Exit POSITION deliberately; observe **motor shutdown behavior** on disarm as
   the plan of record requires.

**Success criteria**
- Command continuity at every mode transition; no publication gap anywhere.
- GPS position/velocity corrections accepted at **≥ 0.98** of aided ticks (R7).
- Position residual inside the tether envelope, within **0.30 m** (R8).
- Degrade and restore behave exactly as Block 3 step 4 proved on the bench.
- Motor shutdown on disarm is clean and immediate.

**Abort criteria**
- Any publication gap or motor transient at a POSITION transition → STOP.
- Correction acceptance below **0.90** (R9), or `innovationGateRejected`
  persistently true → STOP; the filter is rejecting the aiding it is supposed
  to use.
- `consecutiveRejectedCorrections` **≥ 25** → land immediately (§6.7 RF-1).
  Do not wait for 50: under healthy GPS the counter is structurally capped at
  5, so 25 means the GPS aiding is being rejected and the vehicle is on the
  documented path to a B3/B4 re-initialization.

---

### Block 6 — GPS-aided POSITION, untethered (**RED on v3 — requires P10**; COND — **PENDING-INGRESS**, P5)

> **Requires P10 in addition to everything below.** B4 in particular is an
> untethered-flight killer: its preconditions (GPS outage > 1 s, degraded flow,
> speed > 6.3 m/s) are ordinary, and the failure is silent — `estimate_valid = 1`
> and `error_signal = 0x0` throughout a permanent re-init loop that took the
> witnessed case to 147–192 m of position error. No tether bounds that here.

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
- The full **§1.3 precondition chain** satisfied: P10 new bundle → fresh dual
  builds → adversarial artifact review → G7 on the new image → ingress
  creditable. The host-level review receipts (shell 12/12, planner 10/10,
  wrapper 23/23) are **necessary but not sufficient** and do not shorten it.
- Block 5 green with no findings.
- The reference actually accepted by Guidance is **observed on telemetry** to be
  a fresh, finite, LocalEnu, supported-mask reference — not a default.
- Untethered area clear; failsafe geofence/altitude limits briefed.

**Procedure**
1. Manual ATTITUDE takeoff to a low hover (the shell does **not** take off).
2. Transition to POSITION with the reference observed valid. Hold TBD-R10 s
   *(hold duration — not derivable from model sim; owed by the conduct agent)*.
3. Small commanded reposition within a bounded box, pilot on the sticks.
4. Deliberate ready→unready GNSS degradation at safe altitude: confirm degrade
   to effective ATTITUDE with continuous publication, pilot retains control.
5. Exit POSITION deliberately; land in ATTITUDE.

**Success criteria**
- Reference freshness observed continuously; no interval where the accepted
  reference is stale beyond the **100 ms trajectory-reference budget**.
- Position hold within **0.30 m** (R11); drift rate within **0.05 m/s** (R12).
- Degrade path behaves as Block 3/5 proved, in the air, with the pilot reporting
  a flyable vehicle throughout.

**Abort criteria**
- Reference age exceeds 100 ms at any point → exit POSITION.
- Any latch created by an airborne readiness loss → land, end the day.
- Any drift beyond **1.0 m** (R13) → exit POSITION to ATTITUDE and land.
- `consecutiveRejectedCorrections` ≥ 25 → **abort immediately** (§6.7 RF-1).

---

### Block 7 — Optical-flow producer ground checkout (COND — **PENDING-FLOW-PRODUCER**, P7; default **SKIP**)

> **Disposition: SKIP — DEFERRED BEYOND THIS TEST CYCLE (§1.4).** Confirmed from
> source by both agents independently: no producer, driver, transport,
> calibration, range/quality gate or mounting/sign conversion exists anywhere in
> `cerebri_rdd2`; the estimator's flow inputs are hard-wired invalid. This is not
> a pending receipt — **the work has not started.**
> **"Plots/model evidence cannot qualify optical for outdoor flight."**
>
> **Additionally RED on v3 (B4).** Byte-level validation confirmed that
> `quality`, `groundDistance_m`, `timestamp_s`, `integrationTime_s` and
> `integratedLineOfSight_rad` are **dead inputs**: a `quality = 0` sample and a
> NaN-timestamp sample both fuse at **full weight**, and a frozen/stuck flow
> sensor is **never** gated (its innovation is small by construction) — witnessed
> 3000/3000 accepted with the estimate at ~1/60 of true speed. The producer
> requirements are therefore **load-bearing safety requirements**, not
> implementation preferences.

**Entry criteria (all required)**
- **P10 GREEN** (B1–B4 fix set in a re-receipted bundle).
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
7. **Verify the publication phasing** (§4.5): confirm flow and GPS are not
   published from the same scheduler slot, and record the observed flow-applied
   fraction. Colliding publication silently starves flow.

**Success criteria**
- Sign correct on both body axes; magnitude within **0.10 m/s** (R14).
- Rotation-only test produces velocity below **0.04 m/s at 2 m AGL**
  (R15 — **ASSUMPTION-DERIVED, NOT MEASURED**: computed as 0.02 rad/s residual
  rate × AGL. Scale it with the actual test height, and treat it as an
  order-of-magnitude expectation until a bench measurement replaces it).
- Degraded-scene and out-of-range cases produce `valid=false`, not zeros.
- `opticalFlowCorrectionAccepted` observed true on ticks where no higher-priority
  source is fresh, and `innovationGateRejected` not persistently set.
- Flow-applied fraction **≥ 0.75** (R23 as a floor); a lower fraction indicates
  scheduler-slot collision with GPS (§4.5).

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

**Entry criteria**: **P10 GREEN**; Block 7 green; Block 5 green; aircraft
tethered; textured ground surface; height inside the sensor's valid range.

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
- Horizontal velocity agreement with the GPS-only baseline within **0.10 m/s**
  (R16).
- On GPS mask, vertical drift rate matches the prediction: **0.06 m/s down over
  an 8 s mask, 0.11 m/s sustained** (R17). Drift is *expected*; the criterion is
  that it matches prediction. Deviation means something other than the known
  observability structure is acting.

**Abort criteria**
- `consecutiveRejectedCorrections` **≥ 25** → **ABORT IMMEDIATELY** (§6.7 RF-1).
  This is the witnessed precursor to the B3/B4 re-initialization: in sim,
  corrupt flow with GPS masked drove the counter to 50, re-initialized twice,
  and took position error from 113 m to **3,924 m**.
- Any `covarianceReinitialized` event → **loss of navigation** (§6.7 RF-2).
  The filter has re-seeded position from a sensor that **cannot observe
  position**; under flow-only aiding the seed is the *parameter* origin. Land
  immediately and expect the state to have jumped.
- The GPS mask in step 4 must be **brief and tethered**. B4's witnessed
  precondition set is a GPS outage over 1 s with degraded flow above 6.3 m/s;
  do not combine a long mask with any speed.

---

## 3. Prerequisite-to-block map

| Block | Requires GREEN | On frozen v3 | On a fixed re-receipted bundle |
| --- | --- | --- | --- |
| 0 Provenance | P1, P2, P6 | RUN | RUN |
| 1 Props-off gates | P3 (G7), P8 | RUN | RUN |
| 2 ENU walk | Block 1 | RUN | RUN |
| 3 Interlock, steps 1–3 | Block 2 | RUN | RUN |
| 3 Interlock, step 4 (armed) | Block 2, **P10** | **RED** | RUN |
| 4 Manual free flight | Block 3, **P10** | **RED** (P11 resolved red — all modes) | RUN |
| 5 Tethered POSITION | Block 4, **P10** | **RED** | RUN |
| 6 Untethered POSITION | **P10** + **P5** | **RED** | COND on P5 |
| 7 Flow ground checkout | **P10** + **P7** | **RED** | COND — **SKIP** as drafted |
| 8 Flow-aided hover | **P10** + **P7** + Block 7 | **RED** | COND — **SKIP** as drafted |

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

### 4.5 Wiring note — flow and GPS must be phase-offset

**Flow and GPS published from the same scheduler slot silently starves flow.**
Because `fresh` is consumed per tick with no queue, a colliding sample is
**discarded, not deferred**. Measured both ways: in sim, a colliding publication
phase left only **80% of flow samples applied**; on the flight bytes, **100% of
flow samples on collision ticks are discarded**.

There is no telemetry that announces this — the flow producer reports healthy,
the estimator reports no rejection (the correction was never attempted), and the
only observable is a lower-than-expected `opticalFlowCorrectionAccepted` rate.

**Requirement on the producer/integrator: phase-offset flow publication from GPS
publication.** This is verified in Block 7 step 7 and watched as F6 in §6.5.

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

**Altitude, stated for the flight team without hedging: optical flow observes
NO vertical axis and NO position.** The `H` matrix has **zero position columns**
— verified from source *and* from simulation. Flow-navigated altitude
**free-runs** at roughly **+0.10 m/s**, reaching **2.75 m of vertical error over
45 s**. No flow calibration or tuning changes this; it is the rank of the
measurement.

Two qualification gates on the pinned lineage fail on exactly this behavior.
Those failures are **pending confirmation against the pinned qualification
rumoca revision `149c2ff3`** — until that confirmation lands, treat them as
*expected-and-explained*, not as new findings, and do not accept any "fix" that
merely suppresses the trace.

**Operational consequence:** any block that relies on flow while the vertical
channel matters needs an independent altitude source. **None is wired.** This is
why flow is an *aiding-in-addition-to-GPS* observation only (§0.3).

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

### 5.1a The failures that do NOT announce themselves — brief this explicitly

The §5.1 failure is loud: motors zero, fault latches, the crew knows. **B2, B3
and B4 are the opposite and the crew must be told so.** In every witnessed case
of a poisoned state (B2), a mid-air state teleport (B3) and the flow-induced
re-init lockout (B4), **`estimate_valid` stayed 1 and `error_signal` stayed
0x0** while the navigation solution was destroyed — in B4's case to 147–192 m of
position error while Guidance was consuming v = (0,0,0) at 8–12 m/s true.

**Practical brief:** a healthy-looking telemetry page is not evidence of a
healthy navigation solution. The only observables are
`consecutiveRejectedCorrections` (RF-1) and `covarianceReinitialized` (RF-2).
If the aircraft's behavior and the position display disagree, **believe the
aircraft.** Fly visually and land.

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

Threshold columns with a citation are **contract numbers from source** and are
not rehearsal outputs — they do not change.

**Rehearsal envelope provenance.** Filled numeric cells below come from the
rehearsal sim: **rumoca sim @ `44f67022`, corpus `a9e5037`, quote-aware parse,
deterministic-sinusoid noise.** They are **envelope indications, not
Gaussian-calibrated statistics**, and the **OMC oracle is pending**. Treat them
as "this is the shape of the expected behavior", not as certified limits. One
value (**R15**) is flagged in place as assumption-derived rather than simulated.

**Cells still marked `TBD-R#`** were **explicitly declined** by the rehearsal
agent as *not derivable from model simulation* — they are owed by the flight-C,
producer and test-conduct agents. These are: **R1–R4, R6, R10, R18, R33–R68**.
They are not oversights and must not be filled by inference.

### 6.1 Estimator health

| # | Signal | Where | Nominal | Yellow (watch) | Red (act) | Action on red |
| --- | --- | --- | --- | --- | --- | --- |
| E1 | `estimate_valid` | odometry publish gate | true, continuous | any single-tick dropout | any false transition | Land immediately (§5.1 — ACRO is gone too) |
| E2 | `status_initialized` | estimator status | true | — | false in flight | Land immediately |
| E3 | `predictionAccepted` | estimator status | true every tick | TBD-R18 dropout rate *(owed by flight-C agent)* | false while IMU valid | Land; IMU path suspect |
| E4 | `consecutiveRejectedCorrections` | estimator status | 0 (≤ 5 under healthy GPS — structurally capped, R5) | **≥ 6** (R19) — GPS aiding itself is now being rejected | **≥ 25** — see RF-1 in §6.7 | **Abort immediately**, land |
| E5 | `covarianceReinitialized` | estimator status | never true after origin latch | — | **any** true event | Land immediately; state has jumped. In a flow block see RF-2 |
| E6 | `innovationGateRejected` | estimator status | transient only | **> 0.02** of aided ticks (R20) | persistently true | Exit aided modes, land |
| E7 | `gpsPositionCorrectionAccepted` | estimator status | **≥ 0.98** of aided ticks (R7) | acceptance **< 0.98** (R21) | acceptance **< 0.90** (R9) | Abort Block 5/6 |
| E8 | `gpsVelocityCorrectionAccepted` | estimator status | **100% of GPS ticks** (R22) when both GPS validities are set | below 100% of GPS ticks | sustained below 100% | Note; GPS velocity is dropped when course is invalid (accepted design). A shortfall means the velocity path is degraded, not merely course-invalid |
| E9 | `opticalFlowCorrectionAccepted` | estimator status | *(flow blocks only)* true on non-colliding ticks | applied fraction **< 0.75** (R23) — suspect scheduler-slot collision (§4.5) | never true while flow claims valid | Abort Block 8 |
| E10 | `rumoca_galec_error_signal_status` | generated step | 0 | any nonzero | any nonzero | Land; generated-code error signal |
| E11 | Estimator outputs finite | firmware `efmu_estimate_is_finite` | true | — | false | Land immediately |

### 6.2 Navigation quality

| # | Signal | Nominal | Yellow | Red | Action |
| --- | --- | --- | --- | --- | --- |
| N1 | ENU position vs known reference | **0.05 m** (R24) | **0.30 m** (R25) | **1.0 m** (R26) | Exit POSITION |
| N2 | ENU velocity magnitude at hover | **0.04 m/s** (R27) | **0.15 m/s** (R28) | **0.50 m/s** (R29) | Exit POSITION |
| N3 | Position hold drift rate (Block 5/6) | **≤ 0.05 m/s** (R12) | **0.30 m** excursion (R30) | **1.0 m** (R13) | Exit POSITION to ATTITUDE |
| N4 | Vertical estimate drift (flow blocks) | matches prediction: **0.06 m/s down over an 8 s mask, 0.11 m/s sustained** (R17) | **0.15 m/s** (R31) | **0.30 m/s** (R32) | Abort Block 8 (drift is expected; deviation from prediction is not) |
| N5 | Attitude vs visual | agrees | TBD-R33 *(owed by conduct agent)* | obvious disagreement | Land. **On a v3 image, yaw is hard-zero in `eulerRpy_rad` (B1) — do not use euler telemetry to judge this** |
| N6 | `quality_pct` (published odometry) | TBD-R34 *(owed by flight-C agent)* | TBD-R35 | TBD-R36 | Exit POSITION |
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
| F5 | Flow-vs-GPS horizontal velocity agreement | **≤ 0.10 m/s** (R16) | TBD-R63 | TBD-R64 | Abort Block 8 |
| F6 | **Flow applied fraction** (accepted ÷ published) | ≥ 0.75 (R23) | < 0.75 | near 0 | Scheduler-slot collision with GPS (§4.5) — flow is being silently starved. Phase-offset the producer; do not tune the filter |
| F7 | Flow producer publish phase vs GPS publish phase | offset | — | same slot | Stop; §4.5 |

### 6.6 Airframe / standard

| # | Signal | Red | Action |
| --- | --- | --- | --- |
| A1 | Battery voltage / remaining | TBD-R65 | Land |
| A2 | RC link quality / failsafe | any failsafe entry | Per standard failsafe procedure |
| A3 | Motor outputs | saturation or asymmetry beyond TBD-R66 | Land |
| A4 | Vibration / IMU clipping | TBD-R67 | Land; the estimator's prediction path is affected |

### 6.7 RED FLAGS — immediate abort, no discussion

These two are called out separately because both are **silent**: in every
witnessed case `estimate_valid` stayed 1 and `error_signal` stayed 0x0 while the
navigation solution was being destroyed. Nothing else on the telemetry will tell
the crew. Any observer calls these; no one may talk them down.

| ID | Trigger | Why | Action |
| --- | --- | --- | --- |
| **RF-1** | `consecutiveRejectedCorrections` **≥ 25** while in any **GPS-denied or flow** block | Sim-witnessed **precursor to the covariance re-initialization that loses the vehicle**. Scenario S3b: corrupt flow with GPS masked drove the counter to 50 → re-init **×2** → position error **113 m → 3,924 m**. Under *healthy* GPS the counter is structurally capped at **5**, so a reading of 25 means the aiding the vehicle depends on is being rejected and the clock is running | **ABORT IMMEDIATELY.** Land. See **B3**, **B4** |
| **RF-2** | `covarianceReinitialized` true in **any flow block** | **Loss of navigation.** The filter has re-seeded position from a sensor that **cannot observe position** — flow has zero position columns in `H` (§4.4), so the seed falls through to the *parameter* origin, attitude resets to identity and velocity to zero, mid-air. `estimate.valid` stays 1 throughout | **ABORT IMMEDIATELY.** Treat the position solution as invalid from that instant. See **B3**, **B4** |

Both are direct manifestations of the **B1–B4** blocker set (see the BLOCKERS
section). On the frozen v3 images the underlying defects are present and
unfixed, which is why every flow and POSITION block is RED there. On a fixed
bundle these rows remain as regression watches — the re-init policy is being
redesigned, not deleted.

---

## 7. Rollback and abort ladder

Ascending severity. The pilot may skip levels upward at any time without asking.

| Level | Trigger | Action | Re-entry |
| --- | --- | --- | --- |
| **L0 — Observe** | A yellow cell in §6 | Call it out, log it, continue the current block | n/a |
| **L1 — Hold** | Two yellows, or one yellow trending toward red | Stop advancing; hold the current condition; do not enter the next block | Continue when the signal recovers and is stable for TBD-R68 |
| **L2 — Mode rollback** | Any red in §6.2 or §6.5, or reference age ≥ 100 ms | Exit POSITION → ATTITUDE. Pilot flies manually. Flow blocks: disable flow aiding | Only after ground review of the log |
| **L3 — Land** | Any red in §6.1 (except E1/E5/E11), §6.3 G-5, §6.4 T3–T7 | Land in ATTITUDE at the nearest safe point. Disarm on the ground | Day continues only with a named cause and a written disposition |
| **L4 — Land immediately** | E1 (`estimate_valid` false), E5 (`covarianceReinitialized`), E11 (non-finite outputs), T1 ≥ 5 ms, **RF-1**, **RF-2** | Immediate descent and landing. Expect that if the estimate is invalid the aircraft is **already** motors-zero and latched (§5.1) — the pilot's job is to protect people, not to recover the aircraft. **For RF-1/RF-2 the opposite trap applies: the aircraft will look healthy while its position solution is gone. Fly it visually. Do not trust the position display** | **End of flying for the day.** |
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

### 7.2 Image rollback — **VOID**

**There is no fallback image.** The frozen v3 pair (`98e622a3…` /
`6a432077…`) carries B1–B4 and **all old ELF/BIN receipts are superseded**
(§1.5). Rolling back to it is not a degraded-but-safe option; it is a rollback
to the defects this card exists to avoid, and on v3 **no flight mode is
credited** (§1.2).

If a re-cut image fails any block, the response is **stop and re-cut**, not
reflash-and-continue. The v3 build directories remain read-only and must not be
rebuilt or overwritten — they are historical artifacts, not a fallback.

---

## 8. Conditional-dependency index

Every claim on this card that depends on Codex-side work, with the receipt that
closes it.

| Ref | Dependency | Blocks affected | Receipt that closes it |
| --- | --- | --- | --- |
| PENDING-G7 (P3) | On-target aided tick < 5 ms + stack watermark on the flown image | **all powered blocks** | Measurement receipt from the flown image |
| PENDING-R2 (P4) | `modelica_models` `sparsity-limit-covariance` @ `a9e5037` published and pinned in `cerebri_rdd2/west.yml` | provenance row of Block 0 | `git ls-remote` showing the branch at `a9e5037`, plus the west pin |
| PENDING-IMAGE-DECISION (P6) | Frozen v3 pair vs new build with ingress lanes | Blocks 0, 6 | Explicit Codex answer + receipts for the chosen image |
| PENDING-INGRESS (P5) | **Implemented + host-reviewed** (shell 12/12, planner 10/10, wrapper 23/23); awaiting **image regeneration** (§1.3) | Block 6 | The full §1.3 chain: P10 → dual builds → adversarial artifact review → G7 |
| PENDING-AIRBORNE-LOSS | Wrapper discriminators proving airborne capability loss degrades to effective ATTITUDE with **no publication gap and no latch** | Blocks 3, 5, 6 | Named passing discriminators in the flown image's test receipt |
| PENDING-FLOW-PRODUCER (P7) | **CONFIRMED UNMET, no work in progress** — deferred beyond this test cycle (§1.4) | Blocks 7, 8 (**SKIP**) | A producer lane would have to start; then `2026-08-13-flow-producer-requirements.md` is its contract and Block 7 its first flight-line gate |
| PENDING-VERIFY-P8 | GNSS observability on shell + downlink in the flown image | Block 1 | Observed `gnss status`, `zros topic hz gnss_fix`, and downlink fields on the day |
| PENDING-CUBS2 (P9) | CUBS2 CSyn v0.9 hard cutover | **none** — release blocker only | CUBS2 `west.yml` + validation receipt |
| **PENDING-FIXSET (P10)** | **B1–B4** fix set: compiler conditional-multi-write fix, NaN acceptance predicate, re-init policy redesign, flow-lockout acceptance scenario — in a **new, re-receipted, re-reviewed bundle** | **Blocks 3 step 4, 5, 6, 7, 8** | New manifest digest + new ELF sha256s + fresh adversarial APPROVE |
| ~~PENDING-B1-SCOPE (P11)~~ | **RESOLVED — RED.** All three flight modes (ACRO/ATTITUDE/POSITION) execute the B1-corrupted path via `GuidanceController`'s unconditional pre-mode-select `from_Quat` (§1.2) | Block 4 → **RED on v3** | Closed by Codex's `efmi.cmake` + link-map evidence |
| **SUPERSEDED-RECEIPTS** | All old ELF/BIN receipts void (§1.5) | Blocks 0, and every fallback path | New dual builds with fresh provenance, fit and adversarial artifact review |
| PENDING-FLOW-PHASING | Flow producer phase-offset from GPS publication (§4.5) | Blocks 7, 8 | Observed publish phases + flow applied fraction ≥ 0.75 |
| PENDING-QUAL-149c2ff3 | Confirmation of the two failing altitude qualification gates against pinned qualification rumoca revision `149c2ff3` | §4.4 interpretation | Confirmation that both failures are the known flow-vertical-unobservability behavior |
| TBD-R1–R4, R6, R10, R18, R33–R68 | **Declined by the rehearsal agent** — not derivable from model sim | §2 success criteria, §6 envelope columns | Owed by the flight-C, producer and test-conduct agents |
| R5, R7–R9, R11–R17, R19–R32 | **FILLED** from rumoca sim @ `44f67022`, corpus `a9e5037` | §2, §6 | Envelope indications only; OMC oracle pending. R15 is assumption-derived, not simulated |

---

## 9. Sign-off

This card is not valid for flight until the following are filled in on the day:

- [ ] **P10 (B1–B4 fix set) GREEN with a re-cut, dual-built, re-reviewed
      bundle.** Unanimous. **Nothing arms without it** — on v3 the day is Blocks
      0–2 plus Block 3 steps 1–3 and stops there.
- [ ] **New image receipts recorded** — all old ELF/BIN receipts are superseded
      (§1.5) and credit nothing.
- [ ] **G7 re-measured on the new image**, not carried over.
- [ ] All `PENDING-*` items either GREEN with a named receipt, or the dependent
      block explicitly marked SKIPPED.
- [ ] Remaining `TBD-R#` cells (R1–R4, R6, R10, R18, R33–R68) populated by the
      flight-C, producer and conduct agents — the rehearsal agent declined them
      as not derivable from model simulation.
- [ ] Rehearsal-filled cells re-confirmed once the **OMC oracle** lands; they are
      currently envelope indications, not calibrated limits.
- [ ] **RF-1 and RF-2 briefed to every observer**, with abort authority confirmed.
- [ ] P4 provenance disposition recorded (accepted debt or blocker) — James.
- [ ] Flown image sha256 and `modelica_models` pin written on the card — Block 0.
- [ ] Pilot brief §5 delivered and acknowledged, including §5.1.
- [ ] Abort authority named. Any observer may call L4 or L5.
