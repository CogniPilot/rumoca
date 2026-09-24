# Fourbar1 performance investigation

The user selected `Modelica.Mechanics.MultiBody.Examples.Loops.Fourbar1` for
detailed performance repair. Production baseline is `7e67b9954`; the initial
profile checkout was documentation-only descendant `48daaad36`. Preserve the
full baseline's 192 high identities, 211 raw completions, and all historical
recovery obligations. Fluid expansion remains paused.

## Latest recovery-loop milestone

Production `0aa7a5380` (worker `ca309a2f`), validated source `513621d3`,
interchanges the affine recovery loops so each issued causal row is traversed
once across its tear columns. Each cell retains the same dependency order,
+0.0 start, self-target skip and final division; guards, pivots, factor
invalidation, reduced LU and certificates are unchanged. The old-red work
control observes eight metadata visits instead of the required four; the new
loop observes four. Finite/signed-zero differential checks and nonfinite
classification controls pass. Independent frozen review found no blocker.
All 586 solver tests, strict solver clippy, fmt, 244 architecture tests and
four MSL simulation regression tests pass. The latter include the separately
[corrected precision request and analytic RLC oracle](switched-rlc-ci-precision.md).

Fixed focus/canary preserve all 14 raw traces byte-for-byte, all model/channel
bands and initial metrics. Focus has five completions, three compared/high,
two exclusions and zero missing; canary has nine compared/high and no
exclusions/missing. Complete 566-model `tier2-recovery-loop-513621d3` retains
exactly 211 completions, 192 compared/high, 19 exclusions and zero missing.
All 211 Rumoca and OMC traces match `f7819dc1e` byte-for-byte; channel scores,
initial metrics and failure classifications are unchanged. The official xtask
transition reports zero gains/losses, band changes or dropped coverage. See
[full audit](recovery-loop-full-513621d3-audit.md) for timeout-string and
worst-variable presentation differences; these are not numerical changes.

Fourbar is 6.394 s full/6.246 s focused, versus 6.555/6.436 at `f7819dc1e`.
Runtime preparation is 10.001 s full/9.988 s focused. These single observations
suggest a modest improvement; they are not a controlled speedup estimate.
Center-tap Thyristor completes at 11.815 s full, still near the 12-second limit.
Full quality gates retain the same four local historical-high timeouts and
accounting/runtime floors; no baseline was promoted. The broader dual-speed
OMC goal is unachieved. Raw work-control/check logs remain in
`/tmp/rumoca-fluid-speed-bdf/target/fourbar-current-me-census-af458f68/`.

## Current work-count diagnostic

One temporary-probe capture on `af458f68` (production `f7819dc1e`) completes
Fourbar with a byte-identical trace. The active BDF instance records 1,265
attempts, 1,124 accepted steps, 141 error-test failures and zero nonlinear
failures. Sim-scoped ME calls remain 2,678 RHS, 40 JVP and 3,001 observable-error
pairs, containing 6,002 projections. Pair time is 4.096 s of the instrumented
6.600 s simulation; nested projection time must not be added. The unchanged
counts and large projection cost support investigating cheaper execution of
existing certified causal/affine work. They do not prove a JIT-dispatch defect
or justify changing tolerances. No production change follows from this census.

[Census and hashes](fourbar-current-me-census-af458f68.md) and
[independent scope review](fourbar-current-me-census-af458f68-review.md) retain
source bindings, timer scopes and OMC counter limitations. Probes are removed.
The full goal remains faster Fourbar simulation **and** preparation than OMC.
Recorded Rumoca frontend + runtime preparation + IC is 11.136 s; retained
OMC total minus simulation is 19.332 s. These differing timer boundaries are
only a provisional preparation comparison, not a verified win. Both methods'
library-load/cache, initialization and output boundaries need a matched study.

## Latest complete comparison

Source `f7819dc1e`, run `tier2-seed-layout-f7819dc1e`, reuses construction-issued
torn factors for seed sensitivity and retains one lazy dense LU per immutable
point when fallback is needed. Dense J, its row scales, refinement product,
diagnostics and original selected-JVP certificate are unchanged. Exact-zero RHS
still succeeds for a singular matrix; consistent and inconsistent nonzero
singular RHS retain typed refusal and seed rollback. Busy-cache fallback reuses
the same dense factor. No sparse-rank or SVD admission was added. The follow-up
captures only matching issued layouts after J assembly, avoiding copies of
unrelated execution metadata.

The full 566-model sweep has the same 211 raw-completion identities and 192
compared/high models, 19 exclusions and zero missing traces. The official xtask
transition report records zero gains, losses, band changes or dropped coverage.
Independent focus/canary audit confirms all 14 completed traces and their OMC
references are byte-identical to `30e82e22d`, with all scores and initial bands
unchanged. Independent full audit confirms exact model identities and channel
bands, but not byte-exact numerical preservation: 201/211 Rumoca traces match
`30e82e22d`; ten differ. Six model score rows change, including three slightly
worse bounded-L1 scores that remain high. All 211 OMC references and all initial
metric records match. The 35,961 channel bands remain 35,919 high/42 minor/zero
deviation. Failure classifications are unchanged; `MoistAir1` has a substantive
diagnostic-message change within the same ToDae failure. Exact details and the
publication disposition are in `seed-layout-full-f7819dc1e-audit.md`.

Fourbar is 6.555 s in the full sweep versus 7.090 at the reader checkpoint and
7.987 before compact storage; focused Fourbar is 6.436 s versus 7.250 and 7.570.
These are single-run observations, not a controlled repeated-run estimate.
Thyristor completes at 11.708 s full/11.622 s focused and remains near its
12-second limit. Intermediate `739c5a513` timed out on Thyristor; that failed
focus remains recorded, and its cause is not established. It was not retried
unchanged, published, or followed by broader gates.

All 584 worker solver tests, strict clippy/fmt, frozen reviews and the final
244 architecture checks pass. The full gate retains the historical failures
and accounting/runtime floors. One source-bound perf capture finds seed-LU
stacks reduced from 97 to zero samples, and dynamic LU from 100/1,123 to
3/1,041 CPU samples. This corroborates the intended hotspot reduction, not zero
factorizations or a precise speedup. See `seed-factor-perf-f7819dc1e.md`.

## Candidate-reader checkpoint

Source `30e82e22d`, run `tier2-torn-readers-30e82e22d`, retains separate prepared
read maps for the compiler-issued guarded and primary candidates. One numeric
factor cache and all fresh guards/pivot checks remain. The regression is
old-red/new-green: repeated guarded decline then primary success prepared four
maps instead of two; both readers now survive later projections and coefficient
changes, while a new application owner rebuilds them. Independent review,
23 affine and six sparse-cache controls, strict clippy/fmt, 580 solver tests and
244 architecture tests pass.

Across the complete 566-model sweep, all 211 raw-completion and 192 high-model
identities remain unchanged, with 192 compared, 19 exclusions and zero missing.
Independent audit confirms all 211 Rumoca traces and OMC references are
byte-identical to `ac559e1f3` (and transitively to `3b7773263`). All 35,961
channel/initial bands remain unchanged. Failed-model diagnostic drift is retained
in `torn-readers-full-30e82-audit.md`; it is not hidden as exact diagnostic parity.
The official xtask transition report has zero gains, losses or band changes.

Fourbar simulation is 7.090 s versus 7.785 in the preceding full sweep; the
fixed focus is 7.250 s versus 7.668. Both observations improve, but they are not
a controlled repeated-run speedup estimate. Thyristor completes at 11.570 s and
remains close to its 12-second limit. Focus/canary trace hashes and bands are
unchanged. Full accounting/runtime floors and four older historical-high timeouts
still fail; no baseline is promoted.

A subsequent bounded perf capture confirms the repeated read-map hotspot is
absent at sampling resolution: `JacobianValueLayout::locate` appears in zero of
1,123 CPU stacks, versus 34 of 1,338 before the fix. The next source-bound cost
is dense LU in seed linearization: 97 of 100 sampled LU stacks pass through
that owner. See `torn-readers-perf-6b5e.md`. This is distinct from the main affine
projection, which already uses compact storage and a reduced solve. No cost is
assigned to `memcmp` without caller evidence.

## Compact-storage checkpoint

Source `63ffc1727`, documentation checkpoint `ac559e1f3`, run
`tier2-compact-ac559e1f3`, preserves all 211 raw-completion identities from
`3b7773263`. Across all 566 targets, 192 models are compared and all 192 have
high agreement; 19 existing exclusions and zero missing traces remain.
Fourbar simulation is 7.785 seconds versus 7.987 in the previous full sweep,
but 7.668 versus 7.570 in the fixed focused run: these observations do not
establish a meaningful Fourbar speedup. Thyristor still completes, at 11.714
seconds versus 10.058 previously. Independent audit confirms all 211 completed
Rumoca traces are byte-identical to `3b7773263`; all 211 OMC references are
identical too. Exact high identities and all 35,961 channel/initial bands match
both `3b7773263` and `7e67b9954`: 35,919 high/42 minor trajectory channels,
zero deviation, and all initial channels high. Failed-model diagnostic changes
remain recorded in `compact-jacobian-full-preservation-audit.md`.

Compact prepared Jacobians now retain only issued full-pattern slots through
native publication, affine scaling and torn/sparse solves. Dense materialization
remains explicit for dense fallback and retained sensitivity snapshots. Foreign
owner checks precede kernel selection, so fallback cannot bypass ownership.
Focused/native/source tests and independent hash-bound review passed; the
combined architecture suite passes all 244 tests. The five-model focus and fixed
canary preserve byte-identical traces (independently audited); canary remains
nine compared/high models with 175 high channels. New perf attribution is in
progress; storage savings alone are not a performance claim.

The full gate still fails classified/accounting floors, runtime speedup floors
and the same four older historical-high timeouts. No baseline was promoted.

## Previous complete comparison

Source `3b7773263`, run `tier2-distinct-primal-3b7773263`, preserves the exact
baseline 211 raw completions and 192 high-agreement identities across 566
targets. Common channel bands are unchanged, with 19 exclusions, zero missing
traces, and zero deviation channels. Thyristor's intermediate raw timeout is
closed in this sweep: simulation takes 10.058 seconds. Fourbar simulation takes
7.987 seconds and Solve lowering 7.049 seconds; the performance gap to OMC
remains. These are single-run observations, not a controlled speedup estimate.

The overall gate still fails on the four historical timeouts and existing
coverage/accounting/runtime floors. No baseline is promoted. The fixes below
reuse checked construction and shared execution owners, with independent
adversarial review and retained old-red/new-green controls. The latest official
transition report is `target/fluid-campaign/fourbar1-study/tier2-3b7773263-diff.md`.

## Baseline and OMC reference

Full run `target/fluid-campaign/tier2-bulk-clear-7e67b9954/` records Rumoca
simulation 8.608951222 seconds and preparation 11.056757209 seconds. Preparation
includes Solve lowering 7.971347929 seconds and native backend construction
3.017986980 seconds. Frontend-through-DAE compilation is separately 1.032571478
seconds. All 854 compared channels are high.

Both tools integrate two runtime states over 0–5 seconds with 0.01-second output
spacing. Rumoca records 3110 channels at 501 timestamps; OMC records 1012 at 502
timestamps, including a duplicate final timestamp. DAE's 35 states are not the
runtime state count. OMC uses `j1.phi/j1.w`; Rumoca publishes two internal
`$state_coordinates` whose 501 sampled values are bit-identical to those series.
This observed equality is not a checked source-slot/alias mapping. The retained
artifacts do not establish a state-selection defect or a different coordinate
choice (`fourbar-state-coordinate-direction-check-30e82.md`).
Rumoca settings imply rtol 1e-6 and atol 1e-10; OMC's retained XML requests DASSL
and tolerance 1e-6. These settings and output loads are not identical error/work
contracts. The saved 0.335460266-second OMC `timeSimulation` is not OS system CPU
time, nor does the elapsed ratio establish a numerical-kernel ratio.

One subsequent core-0 OMC `-lv=LOG_STATS` run used the retained executable and
unchanged XML after the Rumoca profile completed. Exit status was zero. CSV
SHA-256 `ce9cae8c42ac2ccb8bc46d654d7837a7a34288ce71a9edee57f9e031d6a96f16`
is byte-identical to the original reference, including every timestamp/channel.
OMC reports 510 steps, 695 ODE calls, 31 Jacobian evaluations, 16 error-test
failures, zero convergence-test failures and zero events. Its timers report
0.155846 seconds simulation, 0.232187 seconds creating output, and 0.391932
seconds total. Exact commands, executable/XML hashes and timer scopes are retained
under `/tmp/rumoca-zero-rhs/target/fourbar1-omc-log-stats-7e67/`.

## Measured Rumoca cost and review

One diagnostic unwind-enabled perf capture completed simulation in 8.316764842
seconds, using the actual full-run request with only its output directory
changed. The 12-second budget, core-0 affinity, Rayon/jobs1 and canonical
mimalloc overrides remain. The 6.509-second interior window accounts for 6.15
seconds user CPU, 0.30 seconds system CPU and 135777 minor/zero major faults.
This isolated diagnostic is not a new canonical acceptance run.

Raw bounded RBP chains and frozen ELF disassembly validate native Jacobian
publication copying at 125/1228 CPU samples (10.18%), sparse numeric LU at
132 samples (10.75%), and 47 dense-LU samples attributable to retained seed
linearization construction. Affine projection covers 53.09% inclusively and
torn projection 28.50%; these overlap and must not be added. The capture does
not establish dominant issued-block identity, callback counts, or torn-decline
frequency. Existing factor reuse already compares fresh coefficient bits.

Main holds the proposed owned-buffer publication redesign: removing a measured
10% copy cost alone does not explain the much larger gap. Noether agrees the
next discriminant is counts plus cost per callback. One bounded temporary
instrumentation diagnostic is planned: internal BDF accepted/rejected work,
host-accepted proposals, shared callback counts and elapsed buckets, issued
projection-block sizes/counts, fallback frequency, and factorization counts.
No numerical policy change is authorized. Probes must be removed before a
production fix. Output timestamps are not integrator steps; existing hotpath
`solver_steps` counts host-accepted proposals, not BDF internal attempts.

Full profile artifacts are at
`/tmp/rumoca-fluid-speed-bdf/target/fourbar1-perf-canonical-48daaad36/`.
Compact reviewed evidence is copied to `target/fluid-campaign/fourbar1-study/`.
Work follows SPEC_0033's first-owner/counterhypothesis requirements and
SPEC_0007/SPEC_0029's shared compiler/runtime ownership. No source fix or new
performance claim is established by this checkpoint.

## Counter diagnostic and first rejected pivot

One temporary aggregate-counter run retained a partial trajectory through
host-accepted time 3.811385537 before its unchanged 12-second budget expired.
Instrumentation increases cost; its 12.4999-second result is not a production
performance measurement. It records 901 BDF accepted steps, 110 LTE rejections,
zero nonlinear failures, 2142 RHS calls, 30 JVP calls, and 2411 observable-error
pairs (4822 projections). Internal failures are not a complete attempted-step
count. Outermost callback timers separate RHS, JVP and observable work; block
timers remain inclusive and cannot be added to them.

The issued-index1531 bucket records a 616-variable affine block with 14 tears,
7356 projections, 14712 attempted reduced solves, 14712 declines, 14712 sparse
linear solves and 7082 sparse numeric factorizations. It accounts for 6.292
inclusive seconds of the instrumented session. Counter keys lack runtime-owner
identity, so these aggregates alone cannot prove the source of every decline.
Noether reviewed those limitations. The retained bundle is
`/tmp/rumoca-fluid-speed-bdf/target/fourbar1-counter-48daaad36/`.

A separate first-decline diagnostic binds one runtime owner, local index0,
issued index1531, exact row/Y maps, matrix, scales, layout and programs. It exits
intentionally with status86 during initial projection before simulation. It
is not a completion or timing result. Full/reduced kernel admission passes
SparseCandidate/SmallDense, zero guards pass, and elimination enters. At causal
position519, local row322/column402 (owned residual2385/Y2115) has pivot exactly
zero. Its scaled row magnitude is1, so the unchanged pivot check correctly
rejects it before reduced LU. Available nonzero columns313/321 were already
assigned at positions504/505; substituting one locally would violate ordering.

The typed residual selection binds program710/output2 to this row. Its source
span belongs to the Prismatic position equation
`frame_b.r_0 = frame_a.r_0 + Frames.resolve1(frame_a.R, e*s)`.
The differentiated target coefficient is
`-(Y781*P91 + Y784*P92 + Y787*P93)`. Captured identity rotation and axis[1,0,0]
give exact zero, independently checked by Noether against the saved Y/P and
source bytes/hash. This proves an initial unusable pivot, not that the
coefficient is always zero. Construction-time exclusion needs definition and
parameter-immutability evidence; the runtime guard must remain unchanged.

The first selection owner is structural `tear_scc`; Solve lowering preserves
its causal pairs and affine-layout construction checks structural membership.
The remaining bounded lookup is the definitions of Y779–787 and axis P91–93,
before selecting a general producer fix. No production patch has been made.
Both diagnostic probes have been removed from the source checkout; their frozen
binaries are diagnostic-only. The witness bundle is
`/tmp/rumoca-fluid-speed-bdf/target/fourbar1-first-decline-48daaad36/`.

## Construction-owned candidate and first acceptance failure

Definition lookup established that the rotation coefficients are constrained
algebraics, and the axis is a final parameter derived from configurable input.
The captured zero therefore cannot justify deleting the dependency. Candidate
`576a140cc` (worker `5d687c717`) instead restricts structural causal selection to
complete-residual unit-coefficient evidence. Private evidence is issued by the
incidence builder using the existing scalar/domain-point views and shared
dependency projector. Full incidence remains unchanged. Whole-plan admission
checks eligibility, dependency order, and unique complete row/coordinate coverage;
graph-only incidence cannot issue a coefficient-certified plan. Touched owners
recompute evidence, while unchanged owners retain the existing reuse contract.

The actual lowered Fourbar block retains all 616 rows and solver coordinates.
Its tear count changes from 14 to 16, within the unchanged small-dense limit.
The former zero-pivot row and coordinate become a residual and tear variable.
The 279/362-row blocks keep six tears each; the 15-row block changes from four
to six. Runtime guards, solver policy, tolerances, and budgets are unchanged.

Independent review approved integration validation. All 234 structural and 144
Solve tests, affected-crate clippy, and formatting pass. The regression fixture
fails with the old unrestricted selector and passes with the candidate. Negative
controls include cancellation, nonlinear self-dependencies, configurable
coefficients, scalar identity, and changed coefficients with identical incidence.
Frozen evidence and retained unsuccessful fixture attempts are under
`/tmp/rumoca-fluid-speed-bdf/target/structural-causal-candidates-48daaad36/`.

The first canonical `cargo xtask verify msl-parity` run at `576a140cc` **fails**:
`target/fluid-campaign/fourbar1-proven-pivots-576a140cc/`. Fourbar exceeds its
10-second Solve preparation budget, with the watchdog reporting 12.597 seconds.
Frontend compilation takes 2.368727731 seconds. Simulation never starts; OMC
selection is empty and the comparator produces no bands. This is neither a
runtime measurement nor parity evidence. No retry or cohort promotion is credited.
Other host workloads were active, but this does not establish the timeout's cause.

Static review identifies repeated subtree dependency projections in the new
coefficient analysis: the projector cache does not cache arbitrary subtree
dependencies, and each call creates traversal storage. This is a profiling target,
not a proven explanation. One bounded diagnostic profile is authorized before any
further source change. The candidate remains unaccepted for performance and
preservation; the authoritative cohort remains production `7e67b9954`.

One standalone diagnostic subsequently completed with the unchanged candidate
worker and simulation limit. Solve took 9.842209940 seconds and simulation
7.318499528 seconds. Its 9.612-second sampled Solve interval used 9.53 seconds
of process CPU. Of 1878 user CPU samples, SVD accounts for 174 named self samples
and tensor-affine assignment derivation for 59. New coefficient-analysis symbols
account for 21 named self samples; this is not an inclusive bound because shared
allocation and collection work is unattributed. No dominant new-analysis cost or
cause for the prior timeout has been established. Worker-thread affinity was not
sampled. Frozen evidence is at
`/tmp/rumoca-fluid-speed-bdf/target/fourbar1-compile-perf-576a140cc/evidence.md`.

Official `cargo xtask repo msl -- plot-compare --reuse-traces` compares the
diagnostic against the retained OMC reference over 0–5 seconds: 854 channels,
501 aligned points, mean bounded normalized L1 score 3.042e-5 and worst 1.454e-4.
The plot and log are under `target/fluid-campaign/fourbar1-study/diagnostic-576a140cc-*`.
These diagnostic results do not replace the failed canonical acceptance attempt.

Independent source review found that assignment construction can recompute all
affine assignment shapes for the same output/prefix once per target. A bounded
follow-up will share that existing analysis within its immutable construction
owner. SVD samples do not establish repeated identical factorizations: trial
correction can change its matrix each iteration.

## Shared analysis validation and remaining regression

`02afe866d` (worker `3543d32516bc`) introduces one construction-local owner for
each exact output/store prefix. Both assignment queries use it; affine shapes,
including empty results, are derived lazily once. Ordering, dependency refusal,
certificate lengths, and duplicate-register rejection are unchanged. The proposed
early dependency rejection was deferred. Actual derivation-count controls show
72 calls becoming nine, and two empty analyses becoming one. Independent review
approved the candidate; 677 affected-crate tests, strict clippy, formatting, and
12 main shared-runtime regression tests pass.

Canonical focused Fourbar passes with Solve 7.701211058 seconds, simulation
7.249605812 seconds, and 854/854 channels high. Channel metadata, state count, and
timestamps match the prior run; the OMC reference is byte-identical. The fixed
canary preserves all nine compared/high identities. The original eight-model
recovery set preserves its four completions/high identities and four timeouts.
Official focused transition reports record zero regressions.

The full `tier2-proven-pivots-02afe866d` sweep covers all 566 models and retains
all 192 compared/high identities with identical channel counts and agreement-band
distributions. Fourbar simulation is 7.330636937 seconds, versus 8.608951222 in
the prior full run. Raw completions, however, fall **211 to 210**. The sole loss is
`Modelica.Electrical.Analog.Examples.DemoPowerSupplyWithBuffer`, now failing EX002
event-condition iteration with fixed pre at time 0.17310768214591532. Its previous
run completed in 2.470945298 seconds. This raw loss blocks acceptance of the
candidate despite preserved high-parity totals.

The model's existing parity exclusion concerns only an ambiguous ideal-diode
Boolean at the complementarity knee; it does not waive successful execution.
No exclusion, tolerance, or policy change is proposed. A bounded event-point
witness is the next diagnostic. Full transition and channel audits are retained
under `target/fluid-campaign/fourbar1-study/tier2-02afe866d-*`.

### Event regression: proven mechanism and remaining design boundary

The failure is on `DCPowerSupply.cv`, not the diode knee. Issued root0 is
`-Y2-P11`, with Y2 the supply current and P11 its initialized current limit.
The fixed-pre loop alternates roots about +7.8e-12 and -7.1e-15 through all 32
iterations. A frozen-point replay confirms identical assignment results across
202 program owners/239 shapes before and after sharing. Changing only structural
selection changes block7 from two to three tears and moves the first root by
6.75e-14; both plans still contradict both selected branches at these captured
points. Tiny coupled residuals therefore do not establish branch consistency.

One earlier-entry capture records the refined root bracket
[0.17310768214591357, 0.17310768214591532], with values +1.1013e-13 to -7.1054e-15.
Host and component agree on Positive to NonPositive and issue override `(0,1)`.
After cv=true projection, root0 is +7.8053e-12. The existing opposite-sign rule
then releases the override; the next outer iteration cycles without it. This
proves the mechanism, not that retaining the override would be correct.

Independent high-precision calculation finds cancellation in the source current
limit formula: stored iLim is 10.32 ulps above the exact CV/CP intersection for
the captured inputs. The resulting branch-boundary gap is about 1.08915e-13 V,
and both witnessed voltages lie inside it. Even nearest-float iLim retains a
smaller gap. This does not justify a source rewrite or arbitrary event deadband.
The reproducible calculation is documented in
`/tmp/rumoca-zero-rhs/infra/verification/demo-power-rounded-limit-proof.md`.

The sole old causal pair excluded by the unit proof is residual15 to `diode.s`;
its coefficient depends on `diode.off` and tunable `Goff`, whose legal minimum
is zero. Restoring this pair as an unconditional nonzero pivot is unsound.
Event-domain handling remains the open repair direction. All diagnostic probes
have been removed. The point and entry bundles are retained under
`/tmp/rumoca-fluid-speed-bdf/target/demo-power-event-witness-02afe866d/`.

### Guarded candidate pair: focused recovery, broader rejection

`c1d787c2e` preserves the conditional graph candidate alongside the unit-admitted
candidate, with existing untorn fallback last. Both candidates use the same
partition/source checks and numerical guards. Failed attempts restore their entry
state; native execution distinguishes ordered numerical decline from execution
errors. Wire schema 71 rejects the old representation. Independent review found
no blocking defect before integration; all 1717 affected library tests, strict
clippy, and formatting passed on the frozen worker commit `1b70172109e9`.

The canonical focused pair passes: DemoPowerSupplyWithBuffer simulates in
2.452621883 seconds, and Fourbar simulates in 8.021636162 seconds with 854/854
channels high against OMC. Fourbar Solve preparation takes 8.069038140 seconds.
This restores execution at this scope without claiming to repair the general
floating-point event-boundary gap. The canary preserves all nine high identities;
the official transition diff records zero simulation or parity regressions.

The original eight-model recovery gate rejects this candidate: Engine1b now fails
EL005 during checked Solve construction ("tearing step reads an unrecovered block
coordinate"), and PlanarFourbar reaches the 10-second Solve watchdog, reported at
12.392 seconds, before simulation starts. Previously both completed. ArmatureStroke
and Fourbar still complete with high parity. The three machine models also change
from simulation timeouts to construction failures; UniversalConstraint still fails.
No full sweep is justified until these regressions are repaired. The original
211-raw/192-high cohort remains authoritative. Artifacts are retained under
`target/fluid-campaign/{guarded-pair-focused,canary-guarded-pair,recovery-guarded-pair}-c1d787c2e`.

One bounded, unchanged-worker `perf` diagnostic reveals that PlanarFourbar also
reaches the unrecovered-coordinate EL005 after 8.625 seconds of preparation;
the canonical watchdog hid this refusal. No simulation starts. The sampled
failed-Solve interval uses 8.25 CPU seconds over 8.314 wall seconds. Among 1649
user CPU samples, SVD has 206 self samples and tensor-affine derivation has 44;
shared allocation and collection work is not uniquely attributed. These data
do not justify an additional performance change. Exact request, worker hash,
raw profile, failure result, and interval limits are retained in
`target/fluid-campaign/planar-solve-perf-c1d787c2e/evidence.md`.

The worker's compile-only SMPM witness identifies an over-reported tensor
dependency: row647 isolates Y116 from `P73*Y116 + Y127`, yet the shape dependency
record includes Y115, Y117, Y125, Y127, and Y129 from the surrounding vector.
Only Y117 and Y127 are known at that point. The selected coefficient/value do
not require the unrelated lanes. This establishes the first divergent owner
for this witnessed failure; it is not permission to weaken ordering checks.

Existing source integration checks also expose this regression: one of two
`prepared_projection_execution` checks and three of seven
`coupled_refresh_schedule` checks fail with the same EL005. All three
`initial_fixed_boundary` checks pass. These failures were retained before repair.
Independent review requires the shared dependency repair to preserve execution
error prerequisites: an unused pure-call result can still perform a bounds-checked
array access before returning the selected lane. Selected-value independence
alone does not authorize moving that call ahead of its required inputs.

### Exact tensor dependencies and bounded construction sharing

`c546f1784` repairs the witnessed dependency owner. The selected tensor output's
existing structural dependencies bound its isolated coefficient and value;
unrelated lanes are pruned only for supported arithmetic/fixed-shape prefixes.
Calls, dynamic indexing, and unknown operations retain conservative dependencies.
Both IR admission and evaluator scheduling use the same query. The typed-call
scheduling counterexample remains rejected. Independent review approved the
frozen change; 538 library tests, all 12 source integration checks, five native
controls, the retained SMPM construction witness, clippy, and formatting pass.

Focused Demo and Fourbar still complete; Fourbar retains 854 high channels.
The recovery gate restores construction for the three machine examples, which
reach their existing 12-second simulation limit. Engine and Planar now hit the
10-second Solve watchdog, so acceptance remains blocked. Fourbar takes 10.389
seconds of simulation in that recovery run. A standalone Planar diagnostic
completes with Solve 10.807 seconds and simulation 7.236 seconds. Its 1943 sampled
Solve CPU events contain 248 SVD and 73 tensor-affine derivation self samples;
shared allocation remains unattributed. These timings do not establish which
change dominates cost. Artifacts: `recovery-guarded-pair-c546f1784` and
`planar-solve-perf-c546f1784` under `target/fluid-campaign`.

Source inspection also finds identical row/target analysis repeated across
paired tearing candidates. A private owner now binds an immutable scalar-source
inventory to construction-local dependency results keyed by exact row and target.
Existing shape, effect, and dependency derivation is unchanged; every candidate
still checks its own partition and causal order. There is no numerical cache or
reuse across source changes. The overlapping-plan control reduces three visits
to two derivations; a reversed plan still fails using cached evidence. Independent
review finds no blocker; all 338 IR tests, strict clippy, and formatting pass.
Performance and model preservation for this sharing change remain unmeasured.

The `2204bf919` focused and canary gates pass. Recovery restores all four
baseline high identities and all 3227 compared channels, with the same four
older timeouts. Fourbar preparation is 6.793 seconds and simulation 8.219 seconds
in recovery. An independent issued-plan comparison finds 2480 causal visits but
1247 distinct row/target queries in Fourbar's candidate pairs. Between c1d and
c546, canonical blocks, row/seed ordering, stages, and causal certificates are
unchanged; dependency metadata changes for 44 assignment programs. This does not
establish the cause of observed runtime variation.

The complete `tier2-shared-source-2204bf919` sweep rejects the candidate: 208 raw
completions and 190 high models versus the authoritative 211/192. Official xtask
transition reports and independent audit identify exactly three raw losses:
`Modelica.Mechanics.MultiBody.Examples.Elementary.SpringWithMass` and
`Modelica.Mechanics.MultiBody.Examples.Elementary.RollingWheel` fail EL005 causal
ordering; `ThyristorCenterTap2Pulse_RLV_Characteristic` reaches the 12-second runtime
limit. The first two are the only high-parity losses. All 190 common models retain
their channel counts and agreement-band distributions. No baseline is promoted.

One unchanged-worker thyristor `perf` capture confirms a CPU-bound timeout:
11.59 process CPU seconds over an 11.718-second interior simulation window,
14080 minor faults, no major faults. Among 2342 user CPU samples, dense LU has
194 self samples, native single-output entry has 124, row scaling 119, native
external-table context entry 116, and scaled Jacobian assembly 96. These are
self samples, not inclusive attribution. Exact evidence is retained under
`target/fluid-campaign/thyristor-runtime-perf-2204bf919`.

The paired affine-attempt loop repeats the same origin residual before candidate
first solves. It now evaluates that residual once within a projection invocation,
after Jacobian and scale assembly. All refinement residuals and certificates stay
fresh, and candidate resets, guards, errors, and publication are unchanged.
The counterexample fails old code with two origin evaluations instead of one;
controls cover primary and untorn fallback, fresh later invocations, and immediate
origin/refinement errors without publication. All 565 shared-runtime tests,
strict clippy, and formatting pass. This is no persistent numerical cache and
does not yet establish thyristor recovery or a measured speedup.

The `b9c6f8fab` focused check completes all three targets: Demo 2.453 seconds,
Fourbar 7.877 seconds with 854 high channels, and thyristor 11.976 seconds. This
leaves little runtime margin and does not close full preservation.

`650fbba16` fixes the two later construction failures at the producer. Exact
isolation alone did not establish dependency order: Spring's guarded row786/Y841
requires conservative Y840 from its call-containing prefix, and RollingWheel's
row782/Y42 matrix product reads future Y43/Y44. Normalization now promotes an
unready row and target together into residuals and tears. The independent checker
and conservative call/index prerequisites are unchanged. `bf7f2452d` shares the
existing dependency analysis by exact program/evaluation-prefix identity within
one immutable prepared-source owner; candidate readiness is always rechecked.
Both changes have independent frozen-source approval and old-red/new-green
controls. Parent checks cover 338 IR, 203 evaluator, 12 source tests and both
typed MSL witnesses; the follow-up passes all 204 evaluator tests and strict
lint/formatting. Probes were removed.

Canonical `paired-preservation-focused-bf7f2452d` restores SpringWithMass
(0.103 seconds, 218/218 high channels) and RollingWheel (0.923 seconds, 184/184).
Fourbar remains 854/854 high at 7.961 seconds; Demo completes in 2.439 seconds.
Thyristor again reaches its unchanged 12-second limit. The command correctly
fails, and subsequent canary/recovery/full gates are held. A bounded runtime
profile with validated call attribution is the next diagnostic; no retry or
baseline promotion is credited.

The single `thyristor-perf-census-bf7f2452d` diagnostic binds runtime counts to
the issued block's exact rows, coordinates, and layouts. Its affine 13-by-13
owner enters projection 728093 times. Both the guarded 13-to-1 and primary
13-to-4 candidates decline solely at the full-matrix `SmallDense` admission
gate, before numerical guards. Untorn execution performs 1444889 dense LU
factorizations; raw frame-pointer returns and disassembly confirm the scaled
Newton caller. No numerical failure of either reduced candidate is established.
The optimized instrumented run still times out at 12 seconds. Probe overhead is
substantial (612 of 2091 CPU sample stacks include probes); counts establish the
execution route, not a production speedup. The source tree is clean after probe
removal. The frozen evidence bundle is at
`/tmp/rumoca-fluid-speed-bdf/target/thyristor-perf-census-bf7f2452d/evidence.md`.

The next bounded proposal is to admit existing checked elimination layouts
independently of the full-matrix dense/sparse kernel choice. Direction review
must preserve dimension/layout checks, fresh numerical guards, exact factor
invalidation, residual/correction certificates, and untorn fallback. No new
symbolic analysis, cache, model exception, or solver-specific path is proposed.

`6fb18b777` implements the independently reviewed admission change: four
production lines remove the full-matrix `SparseCandidate` prerequisite. Existing
checked layouts, reduced-kernel policy, numerical guards, cache invalidation,
certificates, execution errors, and fallback remain unchanged. The existing test
fixture now supports multiple dimensions; controls cover all full-kernel classes,
malformed inputs, changed coefficients/scales, singular reductions, and failed
certification without publication. A positive small-full test fails on old code;
all 570 solver tests, 18 affine controls, strict clippy, and formatting pass.
Frozen worker commit `88b08bd70` and patch digest
`41c28c96fb602a0a5b12fe42c874c1ace70a61ec613720656c89970abe053608`
received independent approval before integration.

The canonical focused five-model check completes all five: Thyristor 11.246
seconds, Demo 2.448, Fourbar 7.945, Spring 0.104, RollingWheel 0.924. The three
compared models retain 1256 high channels; the two existing exclusions remain.
The fixed canary preserves nine compared/high identities and all 175 channels.
Recovery preserves four compared/high identities and the same four historical
timeouts. No raw identity or common channel-band changes occur in either check.

Complete Tier 2 `tier2-affine-admission-6fb18b777` covers all 566 targets and
preserves the exact 211 raw completions and 192 high-agreement identities from
`tier2-bulk-clear-7e67b9954`. All 35961 compared channels retain their agreement
band distributions, with zero deviation channels. There are 19 policy exclusions
and zero missing traces. The two ordering regressions and Thyristor timeout from
the intermediate full run are closed in this complete sweep. Thyristor runs in
11.136 seconds; Fourbar Solve takes 6.754 seconds and simulation 8.228 seconds.
These are single-run observations, not a controlled speedup estimate.

The overall quality gate still fails: IMC_YD, SMPM_VoltageSource, SMR_DOL, and
UniversalConstraint retain their historical 12-second timeouts, and historical
coverage/accounting/runtime floors remain unmet. Preservation of the 7e67 cohort
does not close those earlier losses or the Fourbar performance gap. No baseline
is promoted. The official xtask transition diff is retained at
`target/fluid-campaign/fourbar1-study/tier2-6fb18b777-diff.{json,md}`.

The same report records one fewer successful Flatten result (525 to 524).
`Modelica.Media.Examples.SolveOneNonlinearEquation.Inverse_sh_TX` changes from
ToDae ED019 (parameter binding evaluates an enumeration as an array) to Flatten
EF015 (missing exact record-constructor metadata). It reaches neither DAE nor
simulation in either run. This earlier-stage diagnostic change remains visible;
no causal attribution to the runtime admission change is established.

Independent full audit confirms all initial-condition channels remain high and
records the common trajectory distribution as 35919 high and 42 minor channels.
It also retains `ThyristorBridge2Pulse_DC_Drive` changing from a timeout to EX002:
accepted-interval sampler state 3 disagrees with the checked endpoint. Neither
run completes that model; this diagnostic change is not counted as a recovery,
and its cause remains unproven. The full audit is retained at
`/tmp/rumoca-zero-rhs/infra/verification/affine-admission-full-6fb-audit.md`.

One optimized Fourbar `perf`/census capture at `6fb18b777` completes through
5 seconds under the unchanged 12-second budget. The instrumented session takes
8.224 seconds: RHS callbacks 2.187, JVP callbacks 0.767, observable-error pairs
4.784, other session work 0.486. These disjoint buckets include instrumentation;
1124 host acceptance notifications are not BDF internal step counts. Retained
OMC statistics use different counters and output/error contracts, so no direct
step or kernel-speed ratio follows.

Construction-bound owner manifests establish that the 616-coordinate affine
block rejects its 14-tear guarded candidate at pivot preflight on all 9191
projections, then certifies its 16-tear primary candidate every time. Primary
execution has 18382 reduced solves, 8827 factorizations, and 9555 unchanged-factor
checks. No full 616-coordinate fallback occurs. The 279- and 362-coordinate
nonlinear blocks each settle all 9191 calls through six-tear guarded candidates.
Seed sensitivity separately factors each full large block 20 times.

Among 1333 interior user-CPU samples, memory copying has 211 self samples (139
bound by raw return addresses to the native prepared-Jacobian boundary), torn
solve 158, its separate pivot helper 50, dense LU 100, and clearing 92. Of the
LU samples, 97 bind to seed linearization; the numerous tiny nonlinear factors
do not dominate that cost. Exact buffers behind the native copy/clear boundary
remain unidentified, so this does not justify removing either operation. Only
14 sample stacks include probes, which is not an overhead bound. Raw ELF/RBP
caller proofs, source hashes, manifests, and limitations are retained in
`/tmp/rumoca-fluid-speed-bdf/target/fourbar-perf-census-6fb18b777/evidence.md`.
Probes and the ordinary-path instrumented worker were removed.

The next bounded direction review concerns repeated scaled causal-pivot checks
inside one invocation: share a checked result between preflight and refactor
only while its exact layout, matrix, and scales remain fixed. All guards must
still precede cache mutation; certificates and fallback must remain unchanged.
No implementation or performance benefit is established yet.

`21de683c1` implements the reviewed pivot-sharing change. Private `CheckedPivots`
borrows the exact immutable inputs and complete freshly checked sequence;
binding it to a matching cache constructs `CheckedUpdate`, which alone can
refactor. Reusable buffer capacity carries no authority between calls. Guards
still precede usable-cache mutation, and the existing pivot predicate, arithmetic,
factor revocation, certificates, and fallback remain unchanged. The regression
test changes six checks for three causal rows to three checks on every call.
All 573 solver tests, 21 affine controls, strict lint and formatting pass;
independent review approves worker `c85b21f34` and exact patch digest
`7ad16fd8ddda70088c35a283264343aa24c7769609c0561c44fc75b6aac4d3b6`.

Focused five-model, canary, and recovery checks preserve exact prior execution
identities and channel bands. Complete `tier2-pivot-sharing-21de683c1` preserves
all 211 raw and 192 high-agreement identities from 7e67, with unchanged channel
bands, 19 exclusions, and zero missing traces. The overall gate retains its
historical four timeouts and coverage/accounting/runtime failures. No baseline
is promoted. Official transition artifacts are
`target/fluid-campaign/fourbar1-study/tier2-21de683c1-diff.{json,md}`.

Fourbar focused simulation is 7.947 seconds versus 7.945 before pivot sharing;
full-cohort simulation is 8.160 versus 8.228. These single runs do not establish
a Fourbar speedup. Thyristor completes the new full run in 10.993 seconds.
The proven duplicate-work removal and ownership improvement do not close the
remaining performance gap.

A bounded read-only follow-up identifies one native-copy witness precisely:
captured stack return `0x7e7800` follows ELF `memcpy` call `0x7e77fa`, whose
arguments and source mapping identify `out.copy_from_slice(&scratch.matrix)` in
the compiled projection Jacobian. This proves publication-copy ownership for
that witness, not the complete sample bucket. The successful concrete provider
overwrites every output entry, but the generic optional fill hook lacks an
explicit full-overwrite contract. Removing the initial destination clear requires
reviewing that boundary; the post-decline clear remains necessary. Evidence:
`/tmp/rumoca-fluid-speed-bdf/target/fourbar-perf-census-6fb18b777/copy-owner-followup.md`.

`653330549` removes only the redundant pre-provider Jacobian clear after carrying
the existing complete-application contract explicitly through the optional hook.
Success requires fresh full overwrite including structural zeros; decline still
clears before fallback, and errors propagate without replay or publication.
Native scratch-to-output publication stays unchanged. This is an audited/tested
backend obligation, not a claim that metadata proves arbitrary machine writes.
All 573 solver tests, 11 native projection tests, strict lint and formatting
pass. Dirty-buffer observation supplies old-red/new-green evidence. Independent
review approves worker `9d181f6e054c` and patch digest
`56e879a26a53213f159a4626ec2578331e746f9e40a798219f6221e93ea5d915`.

Focused five-model, canary, and recovery checks preserve all previous raw/high
identities and channel bands. Fourbar focused simulation is 7.607 seconds;
Thyristor finishes in 10.879. Independent audit finds Fourbar and MultiBody
RollingWheel saved traces byte-identical to 21de, with 501 points each. Timing
changes alone are not attributed to numerical trajectory changes.

The complete `tier2-prepared-clear-653330549` run **fails preservation**: 210 raw
completions versus 211. Its sole raw loss is
`ThyristorCenterTap2Pulse_RLV_Characteristic`, which reaches 12.008 seconds and
times out. All 192 high-agreement identities and common channel bands remain,
with 18 exclusions and zero missing traces. Fourbar simulation takes 7.642
seconds, but this does not offset losing a baseline completion. The last fully
preserving source remains 21de; 653 is not accepted or promoted. The unchanged
candidate is not retried for a pass. Official transition artifacts are
`target/fluid-campaign/fourbar1-study/tier2-653330549-diff.{json,md}`.

Retained Thyristor traces/partial evidence are being compared before causal
claims. A separate bounded source review checks why the earlier bound 13-by-13
Thyristor owner used scalar residual adapters rather than an existing prepared
batch. No new optimization or capture follows without that owner evidence.

The independent retained-trace audit finds focused 653/21de/6fb and full 21de
Thyristor traces byte-identical: 160 channels, 55456 points, 0–10. Failed full
653 retains no numerical partial trace, last accepted time, step counts, or CPU
telemetry; its partial result is a frontend checkpoint. The timeout therefore
cannot be classified as numerically identical or divergent from saved evidence.
Raw preservation remains blocked.

A bounded compile-only witness at 653 identifies the first missing owner.
Thyristor canonical block 23's 13 residual rows have unique, repeatable,
single-output bindings, including row 77 to program 73. The existing checked
selection builder succeeds, but `ProgramOutputCatalog::shared_selection`
discards it solely because each program has one placement. The final residual
selection is absent. Missing/ambiguous ownership and repeatability refusal are
excluded by the actual checked constructor, rather than duplicated analysis.
One lowering execution produced the witness; probes were removed and no
simulation/profile ran. Exact bindings and provenance are retained at
`/tmp/rumoca-fluid-speed-bdf/target/thyristor-residual-witness-653330549/evidence.md`.

Independent direction review approves retaining the existing checked primal
residual selection for multiple requested placements. Single-placement blocks
and Jacobian-color/manifold consumers remain unchanged. The retained Thyristor
plan has 113 blocks: 112 singleton blocks plus the 13-row block, so this adds
one selection with 13 program records/placements. Existing native batching
already executes distinct programs in issued order with complete-program
evaluation, first-error propagation, and success-only publication. No new
evaluator, grouping algorithm, cache, threshold, or tolerance is proposed.
Eligibility is proven; performance and raw recovery remain unmeasured.

`3b7773263` implements the reviewed primal-only admission through the existing
checked selection builder. The seven-line production diff retains valid
multi-placement residual selections, leaving singleton, Jacobian-color, and
manifold consumers unchanged. Actual Thyristor before/after construction proves
exactly one added 13-program/13-placement selection; all 112 singleton blocks
and 115 color selections remain unchanged. All 340 IR, 204 evaluator, 110
native, 573 solver, and two source integration tests pass, together with strict
lint and formatting. Negative/effect and ordered error/nonfinite controls remain
strict. Source integration compares numerical vectors; native controls additionally
compare bits. Independent review binds worker `fbfe47af711d` to patch digest
`9810e77e4c7372d88a148625a097d6789bb97abd3ce7a549be74d4fa92d95258`.

Canonical focused five-model, fixed canary, and recovery checks preserve exact
prior execution identities and channel/initial bands. The focused Thyristor
trace is byte-identical to 653: 160 channels, 55456 points, 0–10. Focused
simulation takes 9.927 seconds; Fourbar takes 7.570 seconds. The complete
`tier2-distinct-primal-3b7773263` comparison restores the lost Thyristor completion
and preserves all 211 raw/192 high baseline identities, 19 exclusions and zero
missing traces. Common channel bands remain unchanged. Full Thyristor/Fourbar
simulation is 10.058/7.987 seconds. The official transition report has no
execution or compared-model losses. Historical four-model recovery and full-gate
failures remain; no baseline promotion or claim of reaching OMC speed follows.

## Paused for handoff — 2026-09-23

The user requested wrap-up and a new-agent handoff. See
[fourbar-resume.md](fourbar-resume.md) for validated state, exact workspace paths,
compact-Jacobian evidence, preserved partial implementation, remaining defects,
and resumption commands. Both workers are stopped. The compact-storage WIP is
uncommitted and fails its first cargo check; it is not an accepted optimization.
Production remains 3b7773263 with the full preservation result above.
