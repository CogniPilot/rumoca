# Ranked implementation queue for the >=340 `sim_ok` goal

Mined from `dev/2026-07-28-spec-0036-cutover-checklist.md` (all 96 unchecked
entries, every section) against the tree at `38464fd8` plus the uncommitted
wave-4 slice. Read-only analysis; no source was changed.

Normative requirements stay in SPEC 0036/0043, SPEC 0033 §6a, and SPEC 0008.
This file answers only: *what is actually left, what does each item buy, is it
ready, and who can work on it without colliding.*

---

## 0. Method and measured ground truth

**Tree state.** `HEAD = 38464fd8` (`docs(spec): sim_ok is completion, never
parity`). Working tree is dirty: 52 modified files + 13 untracked files,
+3,551/-739 tracked lines plus ~3,200 untracked lines. That slice is called
**wave-4** throughout.

**Cohort evidence** is `target/msl/results-wave3/msl_results.json`, run at
`cdff8c3965f37e5ff6284f4685b4afb628b42916` (`HEAD~1`), i.e. it **already
includes** the parameter-coefficient affine derivative work (`b6de8e20`) and
the index-reduction wave (`03497a7d`, `26148370`). It does **not** include
wave-4.

**Certification evidence** is `target/msl/results-wave3-omcref/`
(`sim_trace_comparison.json`, 48 compared, 1 stochastic exclusion).

**Canary evidence** is `target/msl/results/` at `38464fd8` (6/20 `sim_ok`;
residual canary codes ED019 x8, ED008 x2, EL005 x2, ED010 x1, ED020 x1).

### 0.1 Reproducible LOC baseline (documented filter)

```
git grep -c '' <rev> -- 'crates/*/src/**/*.rs' 'crates/*/src/*.rs' \
 | awk -F: '{p=$2;n=$3; if(p~/\/tests\//||p~/tests\.rs$/||p~/_tests\.rs$/||p~/\/generated\//)next; s+=n} END{print s}'
```

| Revision | Repository production Rust lines |
|---|---|
| merge-base `1acf3641` | 438,804 |
| `HEAD` `38464fd8` | **390,427** (net **-48,377**) |

`rumoca-ir-dae` at `HEAD`, same filter, `wire` = paths containing `wire`:

| Split | Measured | SPEC 0036 budget | Over by |
|---|---|---|---|
| core | 12,281 | 11,000 | **+1,281** |
| wire | 3,613 | 3,250 | **+363** |
| total | 15,894 | 14,250 | **+1,644** |
| total, dirty worktree | 16,161 | 14,250 | **+1,911** |

Every LOC figure recorded in the ledger is stale (see §5, amendments A-2/A-3/A-4).

### 0.2 Cohort failure mass at `cdff8c39` (566 models)

107 compiled, 106/106 balanced, 99 attempted, **49 `sim_ok`**, 50
`sim_solver_fail`, 0 NaN / 0 timeout / 0 balance-fail, ic 56/59.

Typed buckets (`failure_phase`/`failure_bucket`/`failure_error_code`), with the
sub-classification that actually names the capability:

| Models | Bucket | Sub-family (measured) | Capability that unblocks it |
|---:|---|---|---|
| 112 | ToDae / DaeConstruction ED019 | `extent depends on the value of scalar` x52 (`m` 41, `nState` 8, `order` 3); `has unsupported type ExternalCombiTable1D/ExternalCombiTimeTable` x19; §16.5.2 clocked-move x12; record-field owner x6; `der()` operand x4; rank mismatch x5; guarded-return x3; no clock owner x3; other x8 | value-proven function specialization; ExternalObject lifecycle; clock ownership |
| 80 | ToDae / DaeConstruction ED008 | `unresolved Flat reference` — **subscripted array element x59** (`pulse2.twoPulse.fire_n[1]`, `*.enableLogic.internalEnable[1]`, `*.product.u[1]`…); type-name refs x12 (`SI.Frequency` 9, `String` 2); `firstTick` x3; qualified function x6 | **wave-4 `connection_member_expr`** (x59); Flat type-name reference resolution |
| 56 | Typecheck ET009 | `X has 0 dimension(s) but is subscripted with N` — `singlePhaseElectroMagneticConverter` 29, `plugToPin_p` 13, `sTM` 9, `R` 3, other 2. All are `Component c[m]` where `m` is a parameter | parameter-valued component-array dimension at typecheck |
| 51 | Compile / Resolve ER002 | unresolved component reference, all Fluid/Media (`PumpMonitoring.*`, `FlowModels.*`, `Media.CompressibleLiquids.*`) | replaceable-package / redeclare resolution |
| 39 | Solve / StructuralAnalysis EL005 | `structurally singular system: N matched out of N+1` — 39 of the 50 `sim_solver_fail` | index reduction beyond equality closures |
| 39 | ToDae / DaeConstruction ED013 | DiscreteReal initial-algorithm target x18 (`*.T_start` family); `isValidTable` checking call x18; unsubscripted-target x2; other x1 | **wave-4 `initial_discrete_values` + `checking_calls`** |
| 26 | ToDae / DaeConstruction ED018 | `semiLinear` x11 (**wave-4 `lower_semi_linear`**); `initial` x7; `Clock` conversion x2; `edge` x2; `vector`/`change`/`terminal`/`sample` x4 | per-operator checked owners |
| 25 | Flatten EF024 | `flat variable is missing structured identity` — `p`, `X`, `state`, `alpha20s`, `JL`, `Qnom` — all Media/Machines record members | Flat structured identity for record members |
| 25 | Flatten EF016 | `invalid call of function X: input Y has no argument and no default` — `Complex` x~9, Spice3 `mos1RenameParametersDev`/`diodeInitEquations` x~16 | record-constructor + function default materialization |
| 13 | Compile / Resolve ER066 | `flow variable m_flow in stream connector FluidPort must be a subtype of Real` — all Fluid | stream connector flow typing |
| 8 | ToDae ED010 | B.1c solved form: multi-owner discrete x3, unsubscripted resolved coordinate x3, branch target mismatch x2 | B.1c owner topology |
| 7 | Sim EX001 | BDF nonlinear-solver failures x6, step-size-too-small x1 | integrator robustness / event handling |
| 6 | Instantiate EI012 | `cannot instantiate partial class Medium.BaseProperties` | redeclare-driven instantiation |
| 4 | Typecheck ET004 · 3 ToDae ED020 · 3 Flatten EF025 · 4 Sim EX002 · 2 Instantiate EI007 · 2 Resolve ER130 · 1 each ET000/EI006/timeout/harness | long tail | — |

### 0.3 The >=340 arithmetic

From 49, reaching 340 needs **+291** honest models. There is no path there
through solver tuning: only 107 of 566 models compile. The arithmetic is:

| Source | Models addressable | Cumulative ceiling on compiled |
|---|---:|---:|
| baseline compiled | — | 107 |
| wave-4 (ED008-array 59, ED013 36, ED018 semiLinear 11, ED019 zero-output call) | ~106 | ~213 |
| parameter-valued shape family (ET009 56 + ED019 extent 52) | ~108 | ~321 |
| Fluid/Media frontend (ER002 51 + ER066 13 + EI012 6 + EF024 25 + EF025 3) | ~98 | ~419 |
| ExternalObject + record-ctor defaults (ED019 table 19 + EF016 25) | ~44 | ~463 |
| ED018 remainder 15 + ED010 8 + ED020 3 + long tail ~15 | ~41 | ~504 |

Compiled is the ceiling, not the result: today 49/107 = 46% of compiled models
reach `sim_ok`, and only 38/49 clear strict high agreement against OMC. To land
340 honest models the compile ceiling must reach ~450-500 **and** the
compiled→`sim_ok` conversion must rise from 46% to ~70%, which is what the
structural (EL005 x39) and event/clock waves buy. Waves 1-5 are therefore
mandatory and waves 6-8 are what makes the number quotable.

---

## 1. Wave queue (execution order)

Territory codes used for collision-free scheduling:

| Code | Crates |
|---|---|
| **T-CORE** | `rumoca-core` |
| **T-FRONT** | `rumoca-ir-ast`, `rumoca-phase-parse`, `-resolve`, `-instantiate`, `-typecheck` |
| **T-FLAT** | `rumoca-ir-flat`, `rumoca-phase-flatten`, `rumoca-eval-flat` |
| **T-DAE** | `rumoca-ir-dae`, `rumoca-phase-dae`, `rumoca-eval-dae` |
| **T-STRUCT** | `rumoca-phase-structural` |
| **T-SOLVE** | `rumoca-ir-solve`, `rumoca-phase-solve`, `rumoca-eval-solve` |
| **T-RUN** | `rumoca-solver*`, `rumoca-sim`, `rumoca-exec-*` |
| **T-GEN** | `rumoca-phase-codegen`, `rumoca-phase-galec`, `rumoca-ir-galec`, `rumoca-eval-galec`, `rumoca-phase-parse-galec` |
| **T-HARN** | `rumoca-test-msl`, `xtask`, `rumoca-worker`, `rumoca-compile`, `rumoca-contracts` |
| **T-IO** | `rumoca-input*`, `rumoca-codec*`, `rumoca-transport-*`, `rumoca-bind-*` |

`rumoca-core` is a shared root: **only one agent per wave may hold T-CORE**, and
`crates/*/src/lib.rs` module declarations are integrator-owned in every wave.

---

### Wave 1 — Land wave-4 and re-measure (serialize; 2 agents)

**Territory:** T-FLAT + T-DAE + T-STRUCT + T-SOLVE + T-RUN + T-HARN
(the whole dirty slice — it cannot be partitioned).

| Agent | Scope |
|---|---|
| W1-a | Spec-review, split into coherent commits, land: `connection_member_expr` (ED008-array x59), `lower_semi_linear` (ED018 x11), `initial_algorithms/checking_calls` + `initial_discrete_values` (ED013 x36), `function_shapes` zero-output call-statement contract (ED019 residue), `dae_transform/equalities` affine/exact layering, `phase-solve/lower/clocks` sample/hold acceptance contract, `phase-galec/admissibility` §8.6 rejection, `solver` homotopy + `initial_continuation`. |
| W1-b | Land the measurement half: `xtask verify_cmd/parity_comparator` (second independent "did anything get compared" boundary), `balance_pipeline_quality_gate/parity_measurement` + `reference_stage`, `sim_trace_compare` shape-label fix, nightly shard workflow. Then run Tier-1 canary and a Tier-2 566 sweep + OMC comparator and publish `results-wave4` / `results-wave4-omcref`. |

**Why first.** Every downstream leverage estimate in this document is computed
against `results-wave3`; wave-4 invalidates the four largest ToDae buckets at
once (~106 models). Scheduling anything on top of stale buckets wastes agents.
W1-b also closes the standing rule gap: without the comparator boundary a shard
can exit 0 with no comparison, so no wave-2+ number would be quotable.

**Exit evidence:** committed clean tree; `results-wave4` cohort JSON with the
recomputed bucket table; `results-wave4-omcref` bands. No `sim_ok` count quoted
without bands.

---

### Wave 2 — Parameter-valued shape (3 agents, disjoint)

**Leverage: ~108 cohort models** (ET009 x56 + ED019 `extent` x52), the single
largest remaining block. Both halves are the same MSL idiom: `Component c[m]`
and `function f(input Real x[m])` where `m` is a `parameter Integer`.

| Agent | Territory | Scope | Leverage |
|---|---|---|---|
| W2-a | T-FRONT (`rumoca-phase-typecheck`) | Component arrays whose dimension is a parameter expression must carry that dimension into the typecheck component table. Today `singlePhaseElectroMagneticConverter[m]` typechecks as rank 0 and every `c[k]` subscript is ET009. | ET009 x56 |
| W2-b | T-DAE (`construction/function_shapes.rs`) | Value-proven function specialization: prove the extent from an evaluated parameter binding at the exact call site instead of rejecting `extent depends on the value of scalar`. `m` x41, `order` x3. | ED019 x44 |
| W2-c | T-FLAT (`phase-flatten` constant materialization) | Materialize exact-`DefId` package constants into every function parameter `shape_expr` before Flat freezes (`Xorshift128plus.nState=4` for `AutomaticSeed`). Named in the ledger at line 1040. Test two packages with the same constant leaf name. | ED019 `nState` x8 |

**Readiness:** needs design for W2-a and W2-b (no written plan); W2-c has a
ledger-specified contract. **Collision risk:** W2-b and W2-c meet at the
`shape_expr` contract — pin that interface in a shared note before forking.

---

### Wave 3 — Fluid/Media frontend (3 agents, disjoint)

**Leverage: ~98 cohort models.** This is the only remaining *frontend* mass; all
of Fluid and most of Media never reach Flat.

| Agent | Territory | Scope | Leverage |
|---|---|---|---|
| W3-a | T-FRONT (`rumoca-phase-resolve`) | Replaceable-package / redeclare-driven member resolution: `Modelica.Fluid.Machines.BaseClasses.PumpMonitoring.*`, `FlowModels.*`, `Media.CompressibleLiquids.*` resolve as unresolved component references. Ledger item at line 544 ("Resolve IR represents only resolved references") is the owner. | ER002 x51 |
| W3-b | T-FRONT (`rumoca-phase-instantiate`) + stream typing | `cannot instantiate partial class Medium.BaseProperties` (EI012 x6) and `flow variable m_flow in stream connector FluidPort must be a subtype of Real` (ER066 x13) — both are redeclared-`Medium` type identity. | 19 |
| W3-c | T-FLAT (`rumoca-phase-flatten`) | `flat variable is missing structured identity` for Media/Machines record members (`p`, `X`, `state`, `alpha20s`, `JL`, `Qnom`) — the EF023/EF024 production rejection landed but the producer does not mint identity for these members. Also EF025 `loadResource` function-selection identity x3. | EF024 x25 + EF025 x3 |

**Readiness:** W3-a/W3-b need design. W3-c is the "Close the remaining direct
public-field assembly bypass in `rumoca-ir-flat`" item (line 462) reached from
the producer side.

---

### Wave 4 — ExternalObject lifecycle + call defaults (3 agents)

**Leverage: ~44 cohort models + retires a fail-early risk.**

| Agent | Territory | Scope | Leverage |
|---|---|---|---|
| W4-a | T-FRONT (`rumoca-phase-resolve`) | Ledger line 558: resolved ExternalObject as one typed owner fact — specialized `class` restriction, canonical base identity, owned non-replaceable constructor/destructor with checked signatures and exact provenance; strict reachable-closure planning consumes the same fact. | precondition |
| W4-b | T-FLAT + T-DAE | Ledger line 568: replace Flat's constructor-only executable projection with a typed lifecycle target carrying both identities; checked DAE owns exactly-once construction/destruction. Unblocks `ExternalCombiTable1D`/`ExternalCombiTimeTable`. | ED019 `type:` x19 |
| W4-c | T-FLAT (`phase-flatten/functions*`) | Materialize call arguments into declaration order and substitute dependent defaults by exact formal identity (ledger line 96-100). `Complex(re, im)` record constructor and Spice3 `mos1RenameParametersDev`/`diodeInitEquations` are the frontier. Must preserve `QuadratureLobatto3`'s legal partial application `fun7(A=A,w=ws)` — do not pad `u`. | EF016 x25 |

**Readiness:** W4-a/W4-b have a written contract in the ledger (lines 558-573);
W4-c has the acceptance contract in the ledger (lines 96-105) plus the
closure-conversion requirement at line 1059.

---

### Wave 5 — Structural: index reduction and B.1c solved form (2 agents)

**Leverage: 39 sim-side models + 8 ToDae + the compiled→`sim_ok` conversion
rate.** EL005 `structurally singular system: N matched out of N+1` is 39 of the
50 `sim_solver_fail`, i.e. **78% of every model that compiles and then fails**.

| Agent | Territory | Scope | Leverage |
|---|---|---|---|
| W5-a | T-STRUCT | Extend index reduction past equality closures: Pantelides + dummy derivatives on the residual singular families (`SolenoidActuator.SimpleSolenoid`, `OpAmps.OpAmpCircuits.Gain`, `Translational.EddyCurrentBrake`). Ledger line 183 names this exact list. | EL005 x39 |
| W5-b | T-DAE | B.1c discrete solved form: `has more than one semantic definition owner` x3, `must have one unsubscripted resolved coordinate` x3, `all branches must assign the same target` x2. Lands the ordered B.1c owner arena from ledger lines 320-368 in the analysis half. | ED010 x8 |

**Collision:** W5-a must not take T-DAE; the checked DAE-to-DAE reconstruction
API is frozen for this wave and any change goes to wave 7.

---

### Wave 6 — Events, clocks, and the certification findings (4 agents)

**Leverage: 15 ToDae models + retires the "sim_ok is not parity" risk on 11 of
the 49 currently-passing models.** The OMC comparator (48 compared) reports
high 38 / minor 6 / deviation 4, with named defects.

| Agent | Territory | Scope | Leverage |
|---|---|---|---|
| W6-a | T-DAE | Per-operator checked owners for the ED018 remainder: `initial()` x7, `edge` x2 (ledger line 663 — typed current/pre coordinates and checked condition ownership), `change`, `terminal`, `vector`, `sample` start evaluability, `Clock` Integer conversion x2. Also ED020 x3 (Boolean where numeric expected; UpSample1/SuperSampleInterpolated forms). | ED018 x15 + ED020 x3 |
| W6-b | T-DAE + T-SOLVE | MLS §16.5.2 clocked-move x12 (`X on a clocked value moves it to a derived clock; the canonical DAE owns one clock per discrete coordinate`) and clocked expression with no exact connected clock owner x3. | ED019 x15 |
| W6-c | T-SOLVE + T-RUN | Certification defects: Sample3 clocked feedback freezes after one update; the systematic one-tick sampling lead shared by five clocked models (needs a first-tick semantics decision recorded in SPEC, not a tolerance); `Modelica.Clocked.*.Sample1/Sample2/SampleClocked` sit at 0.09 / 0.03 bounded-normalized L1 purely from that lead. | 6 minor-band models → high |
| W6-d | T-RUN | Dead-trace family: `OpAmps.SignalGenerator` is `sim_ok` with 47/66 channels identically zero (worst model at 0.216 L1, one severe channel at 0.9998); `LCOscillator` shares the signature at 1e-6 scale; `CharacteristicIdealDiodes` carries a sustained -10000 A ideal-switch current; `ChuaCircuit` long-horizon divergence needs chaos-aware adjudication (or unscored status). | 4 deviation-band models |

**Readiness:** W6-a/W6-b need design. W6-c/W6-d are already root-caused enough
to start (existing task list items 14-19).

---

### Wave 7 — Correct-by-construction structural debt (5-6 agents, two stages)

No direct cohort gain. This wave retires the structural risks the ledger calls
blocking: the **Solve construction cutover**, **B.1c C1-C4**, the
**operation-shaped wire**, **A2**, and the **consuming DAE transform**. It also
is the only routed path to the LOC caps (§0.1: over by 1,644 in `rumoca-ir-dae`).

**Stage 7A — additive, parallel (4 agents):**

| Agent | Territory | Scope |
|---|---|---|
| W7-a | T-FLAT (`rumoca-ir-flat` only) | A2 Stage A per `dev/2026-07-31-a2-flat-reference-target-plan.md` §5: S1 `FlatReferenceTarget` + `InstanceValueId` + branded function/binder IDs; S2 `flat::Model::construct`; S3 root-owned views; **S3b delete the `ClockPartitions` orphan (1,421 of 4,309 production lines, zero consumers outside its own crate)**. Additive — old surface stays until 7B. |
| W7-b | T-DAE | B.1c C1 (checked-DAE core storage/API/errors/views) + C3 (wire-v12 operation replay) staged behind a feature-free private module; the ordered owner arena with packed target/branch/value columns, the consuming non-cloneable topology capability, and the erased plan/issued state (ledger lines 320-368). |
| W7-c | T-SOLVE | `SolveProblem`/`SolveModel` private and computable by construction; remove public defaults, raw index assembly, whole-root validation, and `filter_map` loss of dynamic events. Close the public `ComputeNode`/`ComputeBlock` bypass (`ir-solve/src/tensor.rs:262,366`). Requires the SPEC 0029/0007/0036 amendment named at ledger line 715 in the same change. |
| W7-d | T-DAE + T-STRUCT | Consuming, aggregate-bound closed DAE transforms: `prepare_for_solve(model: &dae::Dae)` (`phase-structural/src/dae_transform.rs:99`) is still borrowed whole-root. Transfer unchanged immutable arenas, reconstruct only changed owners. Pairs with the perf item at ledger line 295. |

**Stage 7B — integrate, mostly serial (2-3 agents):** A2 Stage B worktrees W1-W6
from the plan (phase-flatten producers / pipeline+postprocess / eval-flat /
phase-dae / codegen+CLI / Flat wire replay), then Stage C integration, spec
amendments, evidence sweep, squash. **Do not start 7B until waves 2-4 have
stopped editing `rumoca-phase-flatten`** — A2's W1/W2 own ~31,000 production
lines there and would collide with W2-c, W3-c, and W4-c.

---

### Wave 8 — Evidence, budgets, targets, certification (4 agents)

| Agent | Territory | Scope |
|---|---|---|
| W8-a | T-HARN | The whole "Required DAE cutover evidence" block (ledger 403-414): focused suites, wire-v12 round trips + malformed negatives, strict Clippy all-targets/all-features, full workspace matrix, representative-MSL coverage matrix, full MSL/OMC parity gate, clean git status, and the SPEC 0025 DCO trailer rewrite before PR. Plus the LOC ledger re-measurement (§0.1) and the release-build wall-time/peak-RSS proof. |
| W8-b | T-GEN | Codegen residue: the 684 remaining production `format!`/`write!`/`push_str`/`replace` sites under `phase-codegen/src` (audit baseline was 873); `render_expr.rs`, `render_stmt.rs`, `render_solve_ops.rs`, `render_solve*.rs` are the concentration. Remove the C/Rust linear-solve `0.0` singular fallback. Replace the `sanitize` filter (`codegen/mod.rs:666,742,766`) with a template-declared `allocate_symbols` policy. Split `phase-galec/src/lower.rs` (1,789 lines, over SPEC 0021's 1,000 threshold). |
| W8-c | T-RUN + T-IO | SPEC 0038 / FMI 3 ME boundary block (ledger 891-939): FMI-ME event indicator contract, typed batched setters replacing `set_input(name, f64)` (`solver-rk45/src/lib.rs:127`, `solver-diffsol/src/session.rs:117`), lockstep transport typed frames, provenance-bearing model-evaluation errors through Diffsol callbacks, exact typed periodic-clock tick ordinals (`solver/src/timeline.rs:209`), cross-form trace equivalence, latency/throughput budgets. |
| W8-d | T-HARN (docs) | Verification/certification design block (ledger 743-764): reactivate SPEC 0037, per-phase refinement relations, certification chain, generated-C assurance profile (ISO C dialect, MISRA C:2023, MISRA Compliance:2020, analyzer versions, deviations, template hashes), per-artifact translation validation, DO-178C/330/331/333 mapping. Design-only until 7B lands. |

**Also in wave 8 (any agent, T-DAE/T-SOLVE):** the SPEC 0039 sparsity/complex
tail — compressed AD execution when colors < columns, dense/sparse policy
estimates + structured-MSL benchmarks, precision-neutral Real/Complex element
kinds with proved lane expansion, and the malformed-affine/coloring/complex-lane
negative + property tests. None of these move the cohort; all of them gate
promoting SPEC 0039.

---

## 2. Top-10 highest-leverage items

Ranked by (cohort models unblocked) x (confidence) + (structural risk retired).

| # | Item | Ledger anchor | Leverage | Readiness | Territory |
|---|---|---|---|---|---|
| 1 | **Land wave-4** — connection array-element identity, semiLinear owner, initial-algorithm discrete owners + checking calls, zero-output call-statement shape contract, and the parity-comparator boundary | closes parts of 108/111/205/1078; comparator serves SPEC 0033 §6a | **~106 models** across ED008/ED013/ED018/ED019 + makes every later number quotable | **written and implemented**; needs review + split + re-measure | T-FLAT/T-DAE/T-SOLVE/T-RUN/T-HARN |
| 2 | **Parameter-valued component-array dimension at typecheck** | ET009 has no ledger entry — *needs a new one* | **56 models**, the largest single unowned bucket | **needs design** | T-FRONT |
| 3 | **Value-proven function specialization over parameter extents** | 93-105 ("Support function locals, defaults, arrays…"), 1040 | **52 models** (`m` 41, `nState` 8, `order` 3) | partial contract at 1040; W2-b **needs design** | T-DAE + T-FLAT |
| 4 | **Replaceable-package / redeclare resolution (Fluid, Media)** | 544 ("Resolve IR represents only resolved references") | **51 models** ER002 + 6 EI012 + 13 ER066 = 70 | **needs design** | T-FRONT |
| 5 | **Index reduction past equality closures** | 183 (alias/BLT/tearing/Pantelides/dummy derivatives) | **39 models** = 78% of all `sim_solver_fail`; raises compiled→`sim_ok` from 46% | design exists for the *reconstruction* half (`03497a7d`); Pantelides half **needs design** | T-STRUCT |
| 6 | **Flat structured identity for record members** | 462-465 ("Close the remaining direct public-field assembly bypass") | **25 models** EF024 + 3 EF025 | contract stated; producer side **needs design** | T-FLAT |
| 7 | **Call-argument declaration order + dependent default substitution** | 96-105 | **25 models** EF016 (`Complex`, Spice3) | **acceptance contract written** in the ledger, incl. the `QuadratureLobatto3` non-padding rule | T-FLAT |
| 8 | **ExternalObject lifecycle as one typed owner fact** | 558-573 | **19 models** ED019 `type:` + the residual `isValidTable` family; retires a fail-early hole (a used external object currently risks becoming a structural record) | **contract written** in the ledger | T-FRONT + T-FLAT + T-DAE |
| 9 | **Certification findings: dead circuits, frozen clocked feedback, one-tick lead, ideal-diode current** | 1096-1109 | 11 of 49 currently-`sim_ok` models are not strict-high; fixing them is the difference between "49 sim_ok" and a defensible parity number | root-caused; tasks 14-19 already filed | T-RUN + T-SOLVE |
| 10 | **Solve construction cutover + B.1c C1-C4 + consuming DAE transform + operation-shaped wire** | 57, 205, 295-399, 712-730 | 0 cohort models; retires **three of the four named structural risks** and is the only routed path to the LOC caps (over by 1,644) | B.1c has a **complete written design (C1-C4)**; consuming transform and Solve constructor **need design** | T-DAE + T-STRUCT + T-SOLVE |

**Deliberately not top-10:** A2 (`FlatReferenceTarget`). Its plan is the most
mature design in the repo, but it unblocks **zero** cohort models, has a ~50,000
line / 1,571 test blast radius, and its Stage B collides with waves 2-4. Its
Stage A (`rumoca-ir-flat` only, incl. the `ClockPartitions` deletion) is cheap
and non-colliding and belongs in wave 7A; Stage B/C should follow the parity
waves.

---

## 3. Per-item status catalog (all 96 unchecked entries)

Columns: **S** = status vs `HEAD` + wave-4 (`OPEN` / `PARTIAL` / `STALE` /
`DONE-verify` = believed satisfied, needs a verification gate rather than
implementation). **Lev** = leverage. **R** = readiness. **T** = territory.

### 3.1 Current priority: canonical checked DAE

| Ln | Item | S | Lev | R | T |
|---|---|---|---|---|---|
| 57 | Consuming, aggregate-bound closed DAE transforms | OPEN — `prepare_for_solve(model: &dae::Dae)` still borrows the whole root (`dae_transform.rs:99`) | structural risk: repeated whole-arena reconstruction; pairs with the perf item (295) | needs design | T-STRUCT+T-DAE |
| 60 | Replace ToDAE provenance `expect` / owner-span fallbacks | PARTIAL — all three sub-bullets checked; **70 `.expect(` remain** in `phase-dae/src` production | trust: a fabricated span is a wrong-but-plausible diagnostic | needs an inventory pass first | T-DAE |
| 91 | Owner-scoped typed function values + structured statement bodies | **STALE (mostly done)** — `FunctionBody`, `FunctionLoop`, `FunctionDefinitionId`, `Functions::define_external` all exist | residual only | see amendment A-9 | T-DAE |
| 93 | Function locals/defaults/arrays/control flow/multi-output/assert/return | PARTIAL — external kind landed (`8143a0c6`); `alg_005_while_boolean`, `alg_013_return_in_function`, `alg_012_break_in_for` still open; declaration-order + dependent defaults open | **EF016 x25** via the defaults half | acceptance contract written (96-105) | T-DAE+T-FLAT |
| 106 | Compact folds over nested loops / runtime-dependent finite domains | OPEN | quadrotor guards parked behind the compact-fold capability (`6da19a57`) | needs design | T-DAE |
| 108 | Conditional / indexed-slice assignment / comprehension function bodies | OPEN — frontier is `Polyphase.Functions.symmetricOrientation` | Polyphase family overlaps ED019 `m` | needs design | T-DAE |
| 111 | Owner-scoped indexed-assignment op replacing generic `array_update` | PARTIAL — template + typed-provenance halves signed (`b6b0b233`, `a60c7333`); the generic constructor op survives (31 `array_update` sites) | correct-by-construction | contract written | T-DAE |
| 121 | Compact executable Solve op for checked function folds | OPEN — Solve rejects a pending fold projection | blocks 106/181 | needs design | T-SOLVE |
| 124 | Decode every wire-v12 function value/statement through the same ops; then make v12 **operation-shaped** | PARTIAL — wire is at schema v12 (`wire.rs:114,577`); the operation-shaped reduction is not done | **primary safe path to the 11,000-line DAE cap** (§0.1: +1,281 core / +363 wire) | contract written (124-133) | T-DAE |
| 142 | Replace blanket `reserve_recursive` with call-graph/SCC construction | **STALE** — the symbol `reserve_recursive` no longer exists anywhere | — | see amendment A-7 | T-DAE |
| 145 | Closure-convert every legal enclosing constant before DAE function construction | OPEN | `QuadratureLobatto3` (canary member) | contract at 1059 | T-DAE |
| 148 | Pure externals as purity-bearing callables + one ordered effectful action | PARTIAL — the external-definition kind landed; the *ordered effectful action owner* is not built and no intrinsic-backed external executes | ED019 table family adjacency | contract written; **duplicated bullet, see A-1** | T-DAE |
| 181 | Compact nested folds with explicit lexical parent-domain ownership | OPEN | with 106, gates nested MSL algorithm loops | needs design | T-DAE |
| 183 | Migrate structural transformations to checked DAE-to-DAE construction | PARTIAL — demotion + index-one holonomic rebuild finalized DAEs and `gear_loop_regression` reaches physical trajectory; **alias elimination, BLT prep, tearing, Pantelides, dummy derivatives remain** | **EL005 x39** (wave 5) | reconstruction half designed; Pantelides half needs design | T-STRUCT |
| 205 | Checked Solve lowering for every DAE partition | PARTIAL — static `div`/`mod`/`rem` done; scalar single-output algorithms done; `alg_016_for_range` compact domain done. Open: dynamic quotient discontinuity owner, atomic vector-equation owner, multi-output sequential fold (`alg_for_loop_algorithm`) | ED013/ED018 adjacency; wave-4 adds the initial-discrete lowering | contract written (206-223) | T-SOLVE |
| 224 | Prove all consumers use canonical `rumoca_ir_dae::Dae` | DONE-verify — needs an architecture gate, not code | evidence | ready | T-HARN |
| 226 | Prove no production source constructs/decodes a legacy DAE | DONE-verify — `AssignDiscreteValue`, `unassigned_discrete_values`, `ResolveOptions` all absent | evidence | ready | T-HARN |
| 228 | Replace every removed legacy capability + equivalent tests (SPEC 0036 restoration table) | OPEN — inventory pass | evidence | ready | T-HARN |
| 230 | Compile-fail coverage: escaped/cross-DAE/cross-function IDs, missing provenance, bypassed owners | PARTIAL — several exist; no completeness proof | evidence | ready | T-DAE |
| 232 | Exact source/generated provenance coverage for every node kind | OPEN | trust | ready | T-DAE |
| 234 | Browser-worker parsed-source acceleration with a proof-carrying cache entry | OPEN — deliberately reparses today | perf only; no cohort effect | needs design | T-IO |
| 238 | Checked-DAE split budgets (11,000 / 3,250 / 14,250) | OPEN — **core 12,281, wire 3,613, total 15,894** at HEAD (16,161 dirty) | routed through 124 | **all recorded figures stale, see A-2** | T-DAE |
| 281 | Record repository production LOC before/after, prove net decrease | OPEN — HEAD 390,427 vs merge-base 438,804 = **-48,377** | evidence | **stale, see A-3** | T-HARN |
| 295 | Prove release-build compile/structural wall time and peak RSS do not regress | OPEN — the `Storage::finish_construction` global B.1c validator replacement is the named blocker | pairs with 57 and the B.1c topology capability | **complete written design (B.1c bullets 306-399)** | T-DAE |

### 3.2 Required DAE cutover evidence (403-414)

All eight are gates, not features; all OPEN. **T-HARN**, wave 8 (W8-a).
Line 411 ("final Git status is clean") is currently **false** — 52 modified +
13 untracked. Line 412 (SPEC 0025 DCO trailer rewrite) must be the last action
before PR publication and cannot be parallelized.

### 3.3 Subsequent correct-by-construction IR work

| Ln | Item | S | Lev | R | T |
|---|---|---|---|---|---|
| 418 | Enforce checkpoint `180f183f`: parsers only in `rumoca-phase-parse*` | **DONE-verify** — no `rumoca-ir-*` crate declares a parser dependency or `parse` feature; `rumoca-phase-parse-galec` exists; `ir-galec/src/lexical.rs` is lexical *validation* | needs an architecture gate | ready | T-HARN |
| 422 | Flat IR makes structurally invalid flattened classes unrepresentable (the whole A2 block, 422-543) | PARTIAL — `WhenChain` owns ordered branches and `WhenClause` is gone; `InstanceIdentitySpace`, `postprocess_def_id.rs`, `structured_refs.rs`, unique-suffix recovery, untagged wire shape all deleted. **Open: `FlatReferenceTarget`, `InstanceValueId`, branded Flat function/binder IDs, `flat::Model::construct`, Flat wire replay; `flat::Model` is still a ~30-field public struct with `Default` + derived `Deserialize`** | 0 cohort models; retires the largest identity risk | **complete written design** at `dev/2026-07-31-a2-flat-reference-target-plan.md` (§4 dependency graph, §5 slice list, §6 evidence, §7 LOC/perf plan) — **but §3 lists 11 checklist amendments still pending** | T-CORE+T-FLAT+T-DAE+T-GEN |
| 544 | Resolve IR represents only resolved references | PARTIAL — full-path lookup, owned `ResolvedTree`, strict reachable-closure pruning landed; the benchmark sub-bullet is open | **ER002 x51 + EI012 x6 + ER066 x13** | needs design for the Fluid/Media half | T-FRONT |
| 558 | Resolved ExternalObject lifecycle as one typed owner fact | OPEN | **precondition for 19 models** | contract written | T-FRONT |
| 568 | Typed external-object lifecycle target in Flat + exactly-once DAE/FMI semantics | OPEN | **ED019 `type:` x19** | contract written | T-FLAT+T-DAE |
| 574 | Solve IR represents only executable/computable programs | PARTIAL — `ScalarProgramBlock` private columns + constructor-proven flow landed; **public `ComputeNode`/`ComputeBlock` (`ir-solve/src/tensor.rs:262,366`) and Solve-root bypasses remain** | structural risk (Solve construction cutover) | needs design + a SPEC 0029/0007/0036 amendment | T-SOLVE |
| 582 | All three migrations retain fixtures/storage/brands/provenance/wire invariants | OPEN — closes only after 422, 544, 574 | evidence | blocked-on | T-HARN |

### 3.4 Proof-carrying sparsity and complex values

| Ln | Item | S | Lev | R | T |
|---|---|---|---|---|---|
| 599 | Compact checked affine representation (SPEC 0039) | PARTIAL — the *decomposition* landed (`b6de8e20`); no `StructuralPattern::Affine` owner exists, the zero-coefficient obligation is a compile-time probe of the declared value, derivative resolution is scoped to continuous algebraic + initialization rows, and the `Evaluate=true` guard is resolved at Solve rather than at DAE construction | EL005 affine families already extinct in the cohort; remaining leverage is trust (a tunable coefficient overridden to zero) | honest remainder written (612-631) | T-SOLVE+T-DAE |
| 632 | Execute compressed AD when colors < columns; prove dense/compressed equivalence | OPEN | perf | needs design | T-SOLVE+T-RUN |
| 634 | Deterministic dense/sparse execution-policy estimates + structured-MSL benchmarks | OPEN | perf | needs design | T-SOLVE |
| 636 | Precision-neutral checked Real/Complex element kinds + proved lane expansion | OPEN | **adjacent to EF016 `Complex` x~9** — the MSL `Complex` operator record is the same family | needs design | T-CORE+T-SOLVE |
| 639 | Malformed affine / coloring / complex-lane / wire negative + property tests | OPEN | gates promoting SPEC 0039 | ready once 599/636 land | T-SOLVE |

### 3.5 Fail-early trust audit

| Ln | Item | S | Lev | R | T |
|---|---|---|---|---|---|
| 644 | Delete the public allow-unbalanced ToDAE/session/worker lane | **DONE-verify** — no `allow_unbalanced` symbol survives; unbalanced is an ED001 failure carrying `BalanceDetail` (`worker/src/failure_row.rs:29-65`) | trust | needs an architecture gate, **see A-5** | T-HARN |
| 660 | Checked typed DAE action owner for event multi-output function calls | OPEN — Flat preserves, ToDAE fails explicitly | small direct; unblocks 674 | needs design | T-DAE |
| 663 | Restore `edge(...)` through typed current/pre coordinates | OPEN | **ED018 `edge` x2** + Digital family adjacency | needs design | T-DAE |
| 666 | Dynamic/concatenated assert/terminate messages as executable checked message programs | OPEN — Solve accepts literals only | trust | needs design | T-SOLVE |
| 670 | Full source-to-simulation event regressions (nested conditional actions, multi-index `for` actions, single-branch clocked `when`) | OPEN | evidence | ready | T-HARN |
| 674 | Source-pipeline negative regression for multi-output event calls | OPEN | evidence; cheap | ready | T-HARN |
| 676 | Checked assertion-level execution semantics in Solve | OPEN | trust | needs design | T-SOLVE |
| 679 | Clocked assert/terminate through the owning clock capability | OPEN | clocked family adjacency (wave 6) | needs design | T-SOLVE |
| 682 | Source `when`/`elsewhen` wire-v12 round trip → Solve → simulation, incl. later-rise-while-first-true | OPEN | evidence for the B.1c cutover | ready | T-HARN |
| 686 | Source-to-simulation coverage for conditional `reinit`, vector `when` conditions, clocked `previous` + malformed-wire rejection | OPEN | evidence | ready | T-HARN |
| 689 | Remove/replace the orphan `rumoca-ir-flat::ClockPartitions` | OPEN — **still 1,421 of `rumoca-ir-flat`'s 4,309 production lines, no consumer outside its own crate** | **-1,421 lines**, one clock authority | A2 plan §3 assigns it to Stage A S3b, **see A-6** | T-FLAT |
| 698 | Opaque spanned AST `EquationKind` cutover (A3) | OPEN | exact per-action spans for every event; trust | **complete written design** at 445-461 | T-CORE+T-FRONT+T-FLAT |
| 701 | Lockstep transport: typed validated frames, distinguish timeout/disconnect/backpressure/IO/malformed | OPEN | trust; no cohort effect | needs design | T-IO |
| 704 | Replace name-plus-`f64` model input writes with typed FMI value references + all-or-nothing batches | OPEN — `set_input(&mut self, name: &str, value: f64)` at `solver-rk45/src/lib.rs:127` and `solver-diffsol/src/session.rs:117` | trust | contract written; part of SPEC 0038 | T-RUN+T-IO |
| 706 | Strict/non-strict root zero ownership into the FMI-ME event indicator contract | OPEN | trust | needs design | T-RUN |
| 708 | Constructor-proven branded slot bindings for pre/previous/relation-memory/root copies | OPEN | trust — zip truncation is a silent-wrong-answer channel | needs design | T-RUN |
| 710 | Preserve the first provenance-bearing model-evaluation error through Diffsol callbacks | OPEN | **directly serves EX001 x7** — "Exceeded maximum number of nonlinear solver failures" today names no responsible owner | needs design | T-RUN |
| 712 | Make `SolveProblem`/`SolveModel` private and computable by construction | OPEN — both `pub struct` at `ir-solve/src/lib.rs:549,1751` | **Solve construction cutover** structural risk | contract written (715-730); needs the SPEC amendment first | T-SOLVE |
| 731 | Advance periodic clocks by exact typed tick ordinals | OPEN — `solver/src/timeline.rs:209` still floors a float | **the systematic one-tick clock lead in 5 comparator models is adjacent** | needs design | T-RUN |
| 734 | Remove unique-suffix/textual constant recovery from ToDAE; `EvalContext` exclusively typed identity | PARTIAL — unique-suffix recovery deleted (`1db32f1e`); **`EvalContext` is still name+scope keyed** (`eval-flat/src/constant/context.rs:131,151,170-182`) | blocked-on the Flat typed instance identity (A2) | **stale text, see A-11** | T-FLAT |

### 3.6 Verification and certification design (743-764)

All seven OPEN, all design-only, all **T-HARN/T-GEN**, all **blocked-on** IR
shape stabilization (i.e. wave 7B). Zero cohort leverage. Line 762 ("make
wrong-but-plausible simulation results impossible") is the one that should be
treated as an active invariant rather than deferred work — the comparator
findings in §0 (dead circuits, frozen feedback, sustained -10000 A) are exactly
its failure mode and are scheduled in wave 6.

### 3.7 Main runtime and code-generation targets (768-939)

| Ln | Item | S | Lev | R | T |
|---|---|---|---|---|---|
| 768 | Move every language-specific emitted fragment into its owning MiniJinja target | PARTIAL — **`rumoca-galec-codegen` crate deleted; `phase-codegen/src/galec/` gone; `checked_modelica.rs` and `render_c.rs` gone; `TargetBuildKind::Efmu`/`compile_efmu_target`/`write_efmu_zip`/`efmi_asset_source` gone; `rumoca-lsp-position` gone; `rumoca-eval-galec` exists; `.alg` parsing moved out of `rumoca-ir-galec`. Emitter sites 873 → 684.** Open: `render_expr.rs`, `render_stmt.rs`, `render_solve_ops.rs`, `render_solve*.rs`; the `sanitize` filter (`codegen/mod.rs:666,742,766`); the linear-solve `0.0` singular fallback; GALEC `c_lines` pass-through; Integer interval proofs; `phase-galec/src/lower.rs` at 1,789 lines | 0 cohort models; GAL-008 compliance claim | contract written; **large stale block, see A-10** | T-GEN |
| 891-939 | SPEC 0038 / FMI 3 block (13 entries) | OPEN | 0 cohort models; retires the parallel model/runtime API risk | SPEC 0038 is deferred/archived — **needs reactivation before design** | T-RUN+T-IO |

### 3.8 Cross-cutting repository gates

| Ln | Item | S | Lev | R | T |
|---|---|---|---|---|---|
| 943 | Re-run the production-LOC report at final cutover | OPEN — recorded 376,885; **HEAD measures 390,427** | evidence | **stale by +13,542, see A-4** | T-HARN |

### 3.9 Parity after the reliable core

| Ln | Item | S | Lev | R | T |
|---|---|---|---|---|---|
| 978 | Stage-owned target catalogs replacing the resolved-reference root/target split | PARTIAL — synthetic `DefId`, `symbol_ancestry`, `postprocess_def_id.rs`, unique-suffix recovery, untagged wire shape all deleted (`1db32f1e`). Remainder is exactly `FlatReferenceTarget` = A2. Also open: `Variable::is_enumeration` + ownerless DAE enumeration scalar; Flat wire replay; rendered-scope lookup removal | 0 cohort models | A2 plan | T-FLAT |
| 1011 | Fixed 20-model canary compiles and simulates honestly | PARTIAL — **6/20 `sim_ok` at `836cec9a`/`38464fd8`** (was 0/20, then 4/20). `interval()` + the recursive typed clock-plan constructor landed and `Sample1` + `ClockedWithDiscreteController` now pass, so those two sub-bullets are closed. `TestSensors` already replaced by `TransformerYD` and the ER070 conformance test exists (`phase-resolve/src/tests/semantic_rules.rs:153`). Open: `AutomaticSeed` `nState` shape_expr, `FullAdder` dimension, `QuadratureLobatto3` closure conversion | canary is the Tier-1 tripwire, never a cohort number | **stale sub-bullets, see A-13/A-15/A-16/A-19** | T-HARN |
| 1064 | Re-establish the MSL baseline using only checked end-to-end paths | PARTIAL — latest recorded is the `090d45ae` sweep (33 `sim_ok`); **`results-wave3` at `cdff8c39` supersedes it with 49** | Tier-2 is the only cohort parity source | **stale, see A-17** | T-HARN |
| 1078 | Reach >=340 trustworthy `sim_ok` models | OPEN — **49/566 `sim_ok`, 38/566 strict high agreement** | the goal | this document | all |
| 1134 | Continue toward trace parity without silent defaults/fallbacks/plausible traces | OPEN — standing invariant | the comparator findings in §0 are live violations | wave 6 | all |

---

## 4. Scheduling summary

| Wave | Agents | Territories held | Gate to next wave |
|---|---:|---|---|
| 1 Land wave-4 | 2 | T-FLAT, T-DAE, T-STRUCT, T-SOLVE, T-RUN, T-HARN | clean tree + `results-wave4` + `results-wave4-omcref` bands |
| 2 Parameter-valued shape | 3 | T-FRONT / T-DAE / T-FLAT | ET009 + ED019-`extent` extinct or reclassified |
| 3 Fluid/Media frontend | 3 | T-FRONT x2 / T-FLAT | ER002, ER066, EI012, EF024 reduced; new buckets named |
| 4 ExternalObject + defaults | 3 | T-FRONT / T-FLAT+T-DAE / T-FLAT | ED019-`type:` and EF016 extinct |
| 5 Structural | 2 | T-STRUCT / T-DAE | EL005 mass reduced; compiled→`sim_ok` measured |
| 6 Events, clocks, certification | 4 | T-DAE x2 / T-SOLVE+T-RUN / T-RUN | strict-high band ratio, not `sim_ok`, is the exit metric |
| 7A Additive structural debt | 4 | T-FLAT / T-DAE / T-SOLVE / T-DAE+T-STRUCT | new surfaces compile with own tests green |
| 7B A2 integrate | 2-3 | T-FLAT+T-CORE+T-DAE+T-GEN | one squashed commit, no coexistence in history |
| 8 Evidence / targets / certification | 4 | T-HARN / T-GEN / T-RUN+T-IO / docs | all "Required DAE cutover evidence" boxes; DCO rewrite last |

Waves 2, 3, and 4 are mutually **non-colliding at crate granularity except in
`rumoca-phase-flatten`** (W2-c, W3-c, W4-c). If more than one of those three runs
concurrently, partition by module: W2-c owns `pipeline/constant_*` +
`constant_extraction.rs`; W3-c owns `variables.rs` + `postprocess*`; W4-c owns
`functions/`. Nobody edits `phase-flatten/src/lib.rs` or `pipeline/mod.rs` —
integrator-owned, exactly as the A2 plan requires.

Peak concurrency is 4 agents/wave, so waves 2+3 can overlap at 6 agents if the
`phase-flatten` partition above is enforced, staying under the nine-agent box.

---

## 5. STALE checklist entries requiring ledger amendment

Each entry gives the anchor, why it is stale, and suggested replacement text.
These are proposals; the ledger is amended by whoever lands the corresponding
work, per the completion rule.

### A-1 · Lines 148-180 — duplicated external-function entries
**Stale:** the two `2026-07-30` bullets (151-160 and 161-180) record the same
commit `8143a0c6` twice, at ~10 and ~20 lines, with the same open remainder.
**Suggested:** delete the first bullet (151-160) and keep the second, appending
its open remainder verbatim:
> Open remainder: the one ordered effectful action owner is not built (an impure
> call is still an ordinary expression call in its legal context); no
> intrinsic-backed external executes (`LinearOp::ImpureRandom*` has no producer);
> and Flat does not preserve the declared simple name, so an omitted external
> entry point is rejected rather than defaulted.

### A-2 · Lines 238-280 — DAE split budgets, all figures stale
**Stale:** the newest figure is `a60c7333` = 14,492. At `HEAD` `38464fd8`, with
the documented physical-line filter, `rumoca-ir-dae` measures **core 12,281 /
wire 3,613 / total 15,894**, and the dirty worktree measures 16,161.
**Suggested (append):**
> - 2026-07-31 `38464fd8`, same physical-line filter (`wire` = paths containing
>   `wire`): core 12,281 (cap 11,000, +1,281), wire 3,613 (cap 3,250, +363),
>   total 15,894 (trigger 14,250, +1,644). The dirty wave-4 worktree measures
>   16,161 total. All three triggers fail. The routed reduction remains the
>   operation-shaped wire (line 124) plus the B.1c owner arena (lines 320-368);
>   do not code-golf or delete capability.

### A-3 · Lines 281-294 — repository LOC net-decrease evidence
**Stale:** the newest clean checkpoint records `a60c7333` merge-base 440,257 /
branch 369,722 / net -70,535. At `HEAD` the same one-liner gives merge-base
**438,804** and branch **390,427**, i.e. net **-48,377**. Both the merge-base
and the branch numbers moved.
**Suggested (append):**
> - 2026-07-31 `38464fd8`, same path exclusions and physical-line count:
>   merge-base `1acf3641` = 438,804; branch = 390,427; net = **-48,377**
>   repository production lines. The earlier -70,535 figure is superseded; the
>   merge-base itself measures differently under the current filter, so only
>   same-run pairs may be compared.

### A-4 · Lines 943-959 — cross-cutting production-LOC report
**Stale:** records `376,885` at the exact-identity commit and an explicitly
non-comparable 456k/459k pair from an undocumented filter. `HEAD` = 390,427
(+13,542 since that entry).
**Suggested (append and mark):**
> - 2026-07-31 `38464fd8`: 390,427 repository production lines under the
>   documented convention (+13,542 since the exact-identity commit). The
>   456,357/459,575 figures earlier in this entry used an undocumented filter,
>   are not comparable, and should be read as historical only. Re-run on the
>   final clean tree for acceptance evidence.

### A-5 · Line 644 — allow-unbalanced lane
**Stale:** no `allow_unbalanced` / `ResolveOptions` / strictness-copy surface
exists anywhere in the workspace. Unbalanced models fail as ED001 and carry
`BalanceDetail` for diagnostics only (`worker/src/failure_row.rs:29-65`,
`compile/src/session.rs:1794`).
**Suggested:**
> - [ ] Prove the allow-unbalanced ToDAE/session/worker lane cannot return. The
>   symbol and every strictness copy are deleted; what remains is to add an
>   architecture gate asserting that no production path constructs a DAE from a
>   model whose balance check failed, and that `BalanceDetail` reaches only
>   diagnostics. Balance diagnostics may retain Flat plus `BalanceDetail`.

### A-6 · Lines 689-697 — `ClockPartitions` orphan
**Stale in scheduling, not in substance.** Still present at 1,421 lines with no
consumer outside `rumoca-ir-flat`.
**Suggested (append):**
> - This is the A2.2 atomic cutover, not a separate task:
>   `rumoca-ir-flat/src/clocks.rs` is 1,421 of that crate's 4,309 production
>   lines. Delete it in A2 Stage A (slice S3b of
>   `dev/2026-07-31-a2-flat-reference-target-plan.md` §5) and count the -1,421
>   in the A2 LOC ledger. Until then ToDAE remains the single production
>   authority for Appendix-B clock ownership.

### A-7 · Line 142 — `reserve_recursive`
**Stale:** the symbol does not exist in the workspace. The item as written
cannot be checked or worked.
**Suggested:**
> - [ ] Prove function construction runs in call-graph/SCC dependency order:
>   acyclic functions construct directly, and only genuinely self- or
>   mutually-recursive SCC members receive linear forward authority. The blanket
>   `reserve_recursive` entry point is already deleted; what remains is a
>   construction-order property test over a mutually recursive pair and an
>   architecture gate forbidding a reintroduced blanket reservation.

### A-8 · Lines 306-399 — the B.1c prerequisite and deletion list
**Stale:** "Mandatory prerequisite A" is **done** — `rumoca_ir_flat::WhenChain`
exists (`ir-flat/src/when_equations.rs:6`) and `WhenClause` has zero occurrences
workspace-wide. The deletion list at 355-360 is also partly done:
`EventActionKind::AssignDiscreteValue`, the legacy discrete-assignment arena,
and `unassigned_discrete_values` have zero occurrences.
**Suggested:**
> - Mandatory prerequisite A is **satisfied for the `WhenChain` half**:
>   `flatten_when_blocks` returns one owned nonempty `WhenChain`
>   (`ir-flat/src/when_equations.rs:6`) and `WhenClause` is deleted. The
>   surviving half of prerequisite A is the Flat-owned branded
>   `InstanceValueId`, which is A2 Stage A slice S1.
> - The deletion list is partly discharged: `EventActionKind::AssignDiscreteValue`,
>   the legacy discrete assignment arena and ID, and `unassigned_discrete_values`
>   no longer occur in the workspace. What remains to delete in the same cutover
>   is the duplicate scan, the event-group reconstruction scan, and every final
>   B.1c dependency validator in `Storage::finish_construction`.

### A-9 · Line 91 — result-expression-only functions
**Stale:** `FunctionBody`, `FunctionLoop`, `FunctionDefinitionId`,
`FunctionValueId`, and `Functions::define_external` all exist and are exported;
the three sub-items at 136-141 are already checked.
**Suggested:**
> - [ ] Finish owner-scoped typed function values and structured statement
>   bodies. The owner types exist (`FunctionBody`, `FunctionLoop`,
>   `FunctionDefinitionId`) and result-expression-only construction is gone; the
>   remaining gap is executable structured control flow (`while`, early
>   `return`, `break` in runtime-dependent finite `for`) and the compact fold
>   Solve operation, tracked at lines 102-105 and 121.

### A-10 · Lines 768-890 — codegen/GALEC/eFMI block
**Stale in eight named places.** Verified at `HEAD`: the `rumoca-galec-codegen`
crate is deleted; `rumoca-phase-codegen/src/galec/` does not exist;
`TargetBuildKind::Efmu`, `compile_efmu_target`, `write_efmu_zip`, and
`efmi_asset_source` have zero occurrences; `checked_modelica.rs` and
`render_c.rs` (with `render_c/discrete_statespace.rs`) are gone;
`rumoca-lsp-position` is not in the workspace; `rumoca-ir-galec` declares no
parser dependency or `parse` feature and `rumoca-phase-parse-galec` exists;
`rumoca-eval-galec` exists with `interpreter.rs` at 690 lines. The emitter
audit baseline of 873 is now **684**.
**Suggested (replace the audit-baseline bullet and mark the closed sub-bullets):**
> - Audit re-measured 2026-07-31 at `38464fd8`: **684** production Rust
>   `format!`/`write!`/`writeln!`/`concat!`/`push_str`/`replace`/`replacen` sites
>   under `rumoca-phase-codegen/src` (baseline was 873). The remaining
>   concentration is `render_expr.rs`, `render_stmt.rs`, `render_solve_ops.rs`,
>   and the syntax portions of `render_solve.rs`,
>   `render_solve/dense_solve_render.rs`, `render_solve/template_partition.rs`,
>   and `render_solve/mlir_family.rs`. `checked_modelica.rs` and `render_c.rs`
>   are deleted.
> - Closed at `HEAD` and retained here only as history: the
>   `rumoca-galec-codegen` crate deletion, the `phase-codegen/src/galec/`
>   removal, the eFMI Rust manifest/schema/context deletion
>   (`TargetBuildKind::Efmu`, `compile_efmu_target`, `write_efmu_zip`,
>   `efmi_asset_source` all absent), the `.alg` parser move to
>   `rumoca-phase-parse-galec`, the `rumoca-eval-galec` crate, and the
>   `rumoca-lsp-position` deletion.
> - Still open in this block: the `sanitize` filter
>   (`phase-codegen/src/codegen/mod.rs:666,742,766`) must become a
>   template-declared `allocate_symbols` policy; the C/Rust linear-solve
>   singular `0.0` fallback must be removed; the GALEC `c_lines` / whole-string
>   pass-through must become a typed language-neutral view; Production Code
>   Integer overflow proofs; and the SPEC 0021 split of
>   `phase-galec/src/lower.rs` (**1,789 lines**). `eval-galec/interpreter.rs` is
>   690 lines and no longer exceeds the threshold.

### A-11 · Line 734 — unique-suffix/textual constant recovery
**Stale:** unique-suffix recovery was deleted by `1db32f1e`. The surviving half
is `EvalContext`'s name+scope keying.
**Suggested:**
> - [ ] Make post-Flat constant evaluation exclusively typed-identity keyed.
>   Unique-suffix and textual constant recovery are deleted (`1db32f1e`); what
>   survives is `EvalContext`'s name+scope lookup surface
>   (`rumoca-eval-flat/src/constant/context.rs:131`, `:151`, `:170`-`:182`) and
>   the core reference helper at
>   `ir_primitives/component_refs_and_functions.rs:369`. This is blocked on
>   Flat's `InstanceValueId` (A2 Stage A S1); do not add a resolved/text dual
>   mode in the meantime.

### A-12 · Lines 1032-1039 — TestSensors canary member
**Stale:** `TestSensors` is no longer in `dev/msl-canary-20.json` (the Polyphase
member is `Modelica.Electrical.Polyphase.Examples.TransformerYD`), and the ER070
conformance test exists at
`crates/rumoca-phase-resolve/src/tests/semantic_rules.rs:153`.
**Suggested:** mark the bullet closed, keeping the normative sentence:
> - `Modelica.Electrical.Polyphase.Examples.TestSensors` reaches an upstream MSL
>   4.1.0 declaration that violates MLS §18.6. **Closed 2026-07-31:** the canary
>   member is now `Polyphase.Examples.TransformerYD` and the rejection is pinned
>   by `phase-resolve/src/tests/semantic_rules.rs:153`. ER070 must never be
>   weakened and the full 566-model report must retain the upstream-invalid
>   classification.

### A-13 · Lines 1053-1058 — `interval()` and the clock-plan constructor
**Stale:** the entry says canary evidence "is still pending a post-cutover run
and both bullets stay open until it exists." That evidence now exists: the
`836cec9a` canary and the `38464fd8` canary both report 6/20 `sim_ok`
**including `Sample1` and `ClockedWithDiscreteController`**.
**Suggested:** check both bullets and append:
> - 2026-07-31: canary evidence exists. `target/msl/canary-20-wave3`
>   (`836cec9a`) and `target/msl/results` (`38464fd8`) both report 6/20
>   `sim_ok` with `Sample1` and `ClockedWithDiscreteController` among them.
>   Note the OMC comparator still shows a systematic one-tick sampling lead on
>   five clocked models, so these are `sim_ok` in the minor-agreement band, not
>   proven parity; the first-tick semantics decision at lines 1103-1105 stays
>   open.

### A-14 · Lines 1118-1133 — "affine-derivative frontier (uncommitted)"
**Stale:** that work is commit `b6de8e20` (`feat(solve): lower
parameter-coefficient affine derivatives`), an ancestor of `cdff8c39`, so the
`results-wave3` sweep already contains it.
**Suggested:** replace the parenthetical:
> - 2026-07-31 affine-derivative frontier (**commit `b6de8e20`, included in the
>   `results-wave3` sweep at `cdff8c39`**): both EL005 families the index-reduction
>   wave exposed are extinct across the 566 cohort … [rest unchanged]. The
>   residual EL005 mass in the cohort is 39 models, all
>   `structurally singular system: N matched out of N+1`, i.e. an index-reduction
>   frontier rather than an affine one.

### A-15 · Lines 993-1000 — canary snapshot at `911c9199`
**Stale:** superseded twice (4/20 at `090d45ae`, 6/20 at `836cec9a`/`38464fd8`).
**Suggested:** prefix the bullet with `Superseded — retained for the delta
baseline only:` and add a pointer to the wave-3 canary. Do not delete it; A2's
before/after delta needs a fixed baseline.

### A-16 · Lines 1024-1031 — the 2026-07-30 canary snapshot
**Stale:** reports 0 `sim_ok`, 2 Flatten failures, 13 ToDae. The `38464fd8`
canary reports 8 compiled, 6 `sim_ok`, 12 ToDae with codes ED019 x8, ED008 x2,
EL005 x2, ED010 x1, ED020 x1, and **zero** Flatten failures.
**Suggested (append):**
> - 2026-07-31 canary at `38464fd8` (`target/msl/results`): 8 compiled, **6/20
>   `sim_ok`**, 0 Resolve/Instantiate/Typecheck/Flatten failures, 12 ToDae
>   (ED019 x8, ED008 x2, ED010 x1, ED020 x1) and 2 EL005. The frontier is now
>   function-shape specialization (ED019) and structural singularity (EL005);
>   the Flatten failures named in the 2026-07-30 entry are closed.

### A-17 · Lines 1064-1077 — MSL baseline entry
**Stale:** newest recorded sweep is `090d45ae` (33 `sim_ok`). `results-wave3` at
`cdff8c39` supersedes it and is already summarized at lines 1087-1095, but the
Tier-2 item itself still points at the older run.
**Suggested (append under the Tier-2 bullet):**
> - 2026-07-31 `verify full` sweep (commit `cdff8c39`, `target/msl/results-wave3`):
>   107/566 compile, 106/106 balanced, 99 attempted, **49 `sim_ok`**, 50
>   `sim_solver_fail`, 0 NaN / 0 timeout / 0 balance-fail, ic 56/59. Cohort
>   failure mass: ToDae 269 (ED019 112, ED008 80, ED013 39, ED018 26, ED010 8,
>   ED020 3), Resolve 66, Typecheck 62, Flatten 53, Instantiate 9, plus 39
>   sim-side EL005. The `090d45ae` numbers above are superseded.

### A-18 · Line 418 — checkpoint `180f183f`
**Stale as work, live as a gate.** No `rumoca-ir-*` crate declares a parser
dependency or `parse` feature; `rumoca-phase-parse-galec` owns `.alg` parsing.
**Suggested:**
> - [ ] Pin checkpoint `180f183f` with an architecture gate rather than a
>   migration: no `rumoca-ir-*` crate may declare a parser dependency, a `parse`
>   feature, or a recoverable-CST/syntax-diagnostic type. The migration itself is
>   complete at `HEAD` (`rumoca-phase-parse-galec` owns `.alg`;
>   `rumoca-ir-galec/src/lexical.rs` is lexical validation, not parsing); what is
>   missing is the gate that keeps it true.

### A-19 · Lines 1011-1063 — canary item framing
**Stale:** the item reads as "0/20, frontend-blocked." It is now 6/20 and
DAE/Solve-blocked. Two of its five sub-bullets (interval/clock-plan, TestSensors)
are closed by A-12/A-13; the `AutomaticSeed` sub-bullet is now precisely
measurable as ED019 `extent:nState` x8 in the cohort.
**Suggested (append to the `AutomaticSeed` bullet):**
> - Cohort scope of this bullet at `cdff8c39`: 8 models fail ED019 with
>   `extent depends on the value of scalar nState`. The larger sibling is
>   `extent … scalar m` at 41 models plus `order` at 3, and the same parameter
>   value also drives 56 ET009 typecheck failures on `Component c[m]`
>   declarations — 108 models on one capability. Track them together.

### A-20 · New entry needed — ET009 has no owner
**Missing, not stale.** The 56-model `X has 0 dimension(s) but is subscripted
with N subscript(s) (MLS §10.5.1)` bucket is the second-largest failure family
in the cohort and no checklist entry claims it.
**Suggested (add under "Parity after the reliable core"):**
> - [ ] Carry parameter-valued component-array dimensions into typecheck.
>   `Component c[m]` where `m` is a `parameter Integer` currently typechecks as
>   rank 0, so every `c[k]` subscript is rejected as ET009 (MLS §10.5.1). 56 of
>   566 cohort models fail here at `cdff8c39`:
>   `singlePhaseElectroMagneticConverter` x29, `plugToPin_p` x13, `sTM` x9, and
>   4 more, across Magnetic.FundamentalWave, Magnetic.QuasiStatic, and
>   Electrical.QuasiStatic.Polyphase. This is the same parameter-value capability
>   as ED019 `extent depends on the value of scalar m`; do not solve it by
>   weakening the subscript-rank rule.

### A-21 · Line 60 — ToDAE `expect` inventory
**Under-specified rather than stale.** All three sub-bullets are checked but the
parent has no measurement, so it cannot be closed or sized.
**Suggested (append):**
> - Inventory at `38464fd8`: 70 `.expect(` sites remain in
>   `crates/rumoca-phase-dae/src` production code. Each must be classified as
>   (a) proven by the single recursive analysis pass — keep and cite the proof,
>   or (b) a broad owner-span fallback — replace with a typed
>   missing-provenance failure. Record the classification per site; do not add
>   redundant whole-tree validation.

---

## 6. Standing-rule notes for the orchestrator

- `results-wave3` is a **partial** picture of Tier-1 vs Tier-2: the canary
  snapshot at `target/msl/results` is Tier-1 (`partial`) and may not be quoted
  as a cohort number under SPEC 0033 §6a.
- The only defensible parity figure today is **38/566 strict high agreement**
  (`results-wave3-omcref`, 48 compared, 1 stochastic exclusion). `sim_ok 49` is
  a completion count, not parity — the `OpAmps.SignalGenerator` dead circuit
  (47/66 channels identically zero, one severe channel at 0.9998) is the proof.
- Wave-1's `parity_comparator` boundary must land before any wave-2+ number is
  reported; without it a shard can exit 0 with zero comparisons.
- No wave in this queue closes a checklist box by deleting code or tests. Where
  an amendment reports a symbol as absent (A-5, A-7, A-8, A-10, A-18), the
  remaining work is an architecture gate proving it stays absent, not a claim
  that the item is finished.
