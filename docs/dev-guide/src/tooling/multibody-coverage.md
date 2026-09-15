# MultiBody coverage work

This is the working evidence ledger for `multibody-library-coverage`, based on
main commit `97eb3ab74b3e11264ab2000437eb47df1a57214d`. Work is in progress;
complete MultiBody support has not been established.

## Complete comparison after source-equation exclusion repair

The complete `multibody-lift-owners-full-11` run at
`6da1462ae77aa9d281bd2e384822d94a69644f15` passes with 165/566 strict-high models
(29.15%), 165 compared, 20 reviewed exclusions, and zero missing,
nonidentifiable, near, or deviating comparisons. All 22,976 initialization
channels are high. Raw execution remains 185; MultiBody remains 23/42 high with
one reviewed BevelGear1D exclusion. Every previous comparison band and completed
execution is retained. One already-failing Media model changes its reported
failure phase from Flatten to ToDae; this earlier-phase diagnostic change earns
no execution or coverage credit and is not attributed to the structural repair.

The run uses 11 workers, fresh Rumoca executions, and cached unchanged OMC
references. Tracked source is clean at the measured commit; the dirty flag comes
from the foreign untracked `comm_fastdyn.md`. The full previous/baseline receipts
are `rolling-wheel/lift-owners-full-{previous,baseline}-delta-1.json`. No baseline
is promoted. Combined `verify quick` and `verify full` remain outstanding.

A subsequent saved-source probe narrows the next LineForceWithTwoMasses gap.
`der(jointUPS.rAxis_0)` has a unique explicit definition,
`body2.v_0-body1.v_0`, whose value proof succeeds with those two state anchors.
The value proof nevertheless rejects the derivative coordinate itself. That
blocks the independent `der_rAxis_a_L` and angular-velocity definitions, leaving
the circular auxiliary reconstruction that the committed repair now excludes.
OMC's corresponding equations use the translational velocity directly.
`rolling-wheel/line-force-lift-owners-rate-facts-1.json` records the exact source
identities. The temporary diagnostic test passed and was removed. The next
repair must replay independent explicit rate definitions while retaining their
source owners and rejecting use of a dynamics equation to replace itself.

## Algebraic lifts exclude their source equation from auxiliary reconstruction

The exact pre-lift capture for LineForceWithTwoMasses identifies a source-owner
mix-up in the structural proof walk. Owner 434 has residual expression 6022 and
right-hand side 6021. Its auxiliary inverse for `jointUPS.R_ia_a.w` records
6022, but algebraic-lift preflight excluded 6021. It could therefore reconstruct
through the equation it was replacing, losing the independent angular-velocity
relation. The differentiated expression and excluded equation now have separate
roles: preflight excludes the source residual, while the derivative proof stays
bound to the right-hand side. Ordinary holonomic constraints retain their
existing exclusion. This implements SPEC_0007's structural-lowering ownership
contract and STRUCT-T03 under MLS Appendix B and section 8.3.1.

A small valid source model reproduces the defect with `u=der(x)`,
`v=shift(u,zeros(n))`, `q=forward(v)`, `der(q)=a`, and `a=-q`, where the pure
functions add the supplied offset and forward their argument. Its rejected lift
would replace the relation between `der(x)` and `q` with a circular alias.
The regression proves that its auxiliary block owns the source equation and
must not supply that equation's lift. An independent `u=x`, `der(x)=ones(n)`
control still lifts and retains the relation between `v` and `x`. Both tests
pass for sizes 1, 3, and 4,096; the positive reconstruction has constant IR size.
All 192 structural tests and all-target/all-feature structural Clippy pass.
The initial Clippy pass required extracting a nested test traversal into a
helper. OMC accepts both small models and preserves their expected differential
equations; its backend XML is retained under `rolling-wheel/lift-owners-omc-1`.

The rebuilt original model has byte-identical source DAE and retains owner
434's undifferentiated `Frames.absoluteRotation` relation. It still fails Solve
with EL005: the deepest snapshot matches 2,201/2,204 rows. The remaining rows
2171–2173 now map to owner 688, `rod1.frame_b.R.w=jointUPS.frame_a.R.w`, with
unmatched unknowns `jointUPS.f_bd_a[1:3]`. This is diagnostic evidence, not a
simulation or coverage gain. Next, trace the independent `w_rel_ia1` definition
through `der_rAxis_a_L` and the explicit derivative of `rAxis_0`, comparing each
substitution against OMC's translational-velocity equations.

Tier 1 `multibody-lift-owners-{frontier,origin,canary}` retains all prior bands
and phase/execution statuses from the demotion-owner controls. The fixed target
sets compare four, six, and nine high traces respectively, with one, two, and
zero reviewed exclusions. There are no missing, nonidentifiable, near, or
deviating comparisons. The ordinary LineForceWithTwoMasses attempt still exceeds
the unchanged Solve budget. `rolling-wheel/lift-owners-evidence-1.json` binds the
source captures, repair, tests, OMC equations, and gate deltas. Combined quick/full
verification and the full MultiBody goal remain outstanding.

## Complete cohort retains all prior results after equation-activity checks

The full `multibody-demotion-owners-full-11` comparison at
`dd3143d1ad006b9ae020a0dbcc3067e8b2d71f13` passes: 165/566 strict-high (29.15%),
165 compared, 20 reviewed exclusions, zero missing/nonidentifiable traces, and
zero near/deviating comparisons. All 22,976 initialization channels are high.
Raw execution remains 185 models; the exclusions receive no strict-high credit.
MultiBody remains 23/42 high, with one reviewed BevelGear1D exclusion. Every
model retains its preceding band and phase/execution status, including the
previously repaired counterexamples. The ordinary LineForceWithTwoMasses attempt
still exceeds the unchanged 10-second Solve budget.

The run uses 11 workers and fresh Rumoca execution; unchanged OMC references are
reused by the comparator. Tracked source is clean, and `comm_fastdyn.md` remains
foreign and untracked. Receipts
`rolling-wheel/demotion-owners-full-{previous,baseline}-delta-1.json` bind the
complete identical rosters and comparator artifacts. The aggregate gate exits
zero. No baseline is promoted. Combined `verify quick` and `verify full` remain
outstanding, and complete MultiBody support is not established.

## Structural candidate acceptance rejects reflexive coordinate equations

The next capture isolates the remaining circular substitution at holonomic
round 20, residue 24. Demoting `body1.w_a` to `jointUPS.frame_ia.R.w` differentiates
the latter through its existing definition `body1.z_a`. Both states already
have that same derivative definition, so the original `z_a=der(w_a)` equation
becomes reflexive. The source derivative-cycle walk need not re-enter the
demoted coordinate: its algebraic chain reaches body2's derivative instead.

A small source model, `x=y; der(x)=a; der(y)=a`, reproduces the false structural
success. OMC rejects it as structurally singular at `a[i]=a[i]`; a companion
model with independent derivative coordinates and `a=-y` compiles to the
expected first-order motion equation. Rumoca's direct and holonomic candidates
now share an acceptance check that refuses newly reflexive equalities of Real
coordinates. It compares branded coordinate identity, including distinct source
expression occurrences, and retains source equations and tensor domains. This
does not rewrite numerical residuals, eliminate function calls, or sample rank.

Both regression models pass for scalar, singleton, three-element, and
4,096-element payloads. All 190 structural tests and all-target/all-feature
structural Clippy pass. The first Clippy run identified a function two lines
over the size limit; the direct reconstruction checks were factored into a
helper, then tests and Clippy reran successfully. Temporary capture tests were
removed and archived outside production source.

LineForceWithTwoMasses now refuses Solve construction with EL005 instead of
entering initialization with the known tautology. Its deepest saved reduction
matches 2,201/2,204 equations; the three unmatched rows are owner 545's original
angular-velocity relation. The unmatched unknowns are `jointUPS.f_c_a[3]` and
`jointUPS.f_bd_a[2:3]`. The returned error still describes the initial 2,164/2,204
matching; the diagnostic snapshot preserves the later obstruction. The next
comparison follows JointUPS's explicit `w_rel_ia1` construction from its
translational kinematics to identify the missing independent reconstruction.
No simulation or parity gain is claimed.

Tier 1 `multibody-demotion-owners-{frontier,origin,canary}` retains every band
and execution status from the corresponding function-zero controls. The six
frontier targets compare four high traces with one reviewed exclusion; the eight
origin targets compare six with two reviewed exclusions; the fixed 20-model
canary compares nine with none. Every comparison is high, with zero missing,
nonidentifiable, or deviating traces. These partial runs are regression evidence.
`rolling-wheel/demotion-owners-evidence-1.json` binds the captures, OMC probe,
source repair, tests, inspector, and gate deltas. The complete cohort below
remains authoritative; combined quick/full verification is still outstanding.

## Exact identity and zero derivatives expose the next LineForce failure

Two structural producer repairs now preserve exact identities through tensor
reconstruction. A construction-proved identity coefficient in `A*q=b` emits
the RHS directly at each admitted derivative order. Other coefficients retain
the checked aggregate solve, including tunable parameters whose default is one.
Supplied function derivatives now respect the existing checked zero-expression
proof in both admission and construction. A zero-default parameter remains a
parameter; its numeric default does not establish an identically zero function.

The source-backed regressions fail before the respective repairs and pass after
them for vector extents 1, 3, and 4096, with constant IR size. All 188 structural
tests, all-target/all-feature structural Clippy, formatting, and whitespace
checks pass. The ordinary workers rebuild successfully. The OMC ZeroRotation
probe, using MSL `Frames.resolve2(R, zeros(3))`, retains `q[i]=y[i]` and
`der(y[i])=-y[i]`, with no spurious rotation derivative in those equations.

The actual LineForceWithTwoMasses source DAE remains byte-identical to the
original capture. The identity-only repair removes its synthetic identity solve
but leaves the circular angular-velocity derivative. With the exact-zero repair,
prepared owner 434 becomes `der(jointUPS.frame_ia.R.w)-body1.z_a`, matching the
previously captured OMC relation. Owner 545 retains the original angular-velocity
value equation and demotes `body1.w_a`; it is no longer replaced with the circular
acceleration residual. The diagnostic has 31 state declarations and 27 manifold
rows. These counts are not OMC's scalar-state count.

The model still fails initialization with EX002 and produces no trace. The next
capture identifies local block 60, logical row 1635, and `body1.z_a[1]` (Y 1399).
All 166 failure events share one numerical point and a zero 1-by-1 matrix.
Source owner 546 is `body1.z_a-der(body1.w_a)`; its prepared residual is exactly
`body1.z_a-body1.z_a`. Solve program 522 loads the vector once and subtracts the
same registers from themselves, producing logical rows 1635–1637. This is a
remaining circular substitution in state demotion, already present before Solve;
it is not evidence of an AD-only defect. The next proof must trace the derivative
anchor that permits this self-substitution. Solve still takes 12.623 seconds in
the artifact-enabled diagnostic, exceeding the ordinary 10-second budget.

Tier 1 runs `multibody-function-zero-{frontier,origin,canary}` retain every band
and execution status from the corresponding demotion-bounds controls. The six
frontier targets compare four high traces with one reviewed exclusion; the eight
origin targets compare six high traces with two reviewed exclusions. The fixed
20-model canary compares nine high traces, with no exclusions. All three have
zero missing/nonidentifiable/near/deviating comparisons. These partial runs are
regression checks, not new cohort coverage. Receipts
`rolling-wheel/function-zero-{evidence,next-failure}-1.json` bind the source,
generated equations, OMC probe, tests, worker captures, and comparator deltas.
The full cohort below remains authoritative: 23/42 MultiBody high. Combined
`verify quick` and `verify full` remain outstanding, and no PR is opened.

## LineForce sensitivity failure is a circular structural equation

The failure-only debug event identifies the exact singular block in
LineForceWithTwoMasses. All 172 failure events share one Y/P/time point and one
zero 3-by-3 matrix. The selected logical rows are 1634, 1632, 1633 and the
unknowns are `body1.z_a[2]`, `body1.z_a[1]`, `body1.z_a[3]`. Replaying the saved
Solve model identifies global block 886, with a full structural pattern.
The local refresh block number is 186; those two inventories differ.

The rows belong to the differentiated `w_a = Frames.angularVelocity2(frame_a.R)`
equation, not the torque equation. The initial zero-inertia torque hypothesis is
rejected by this exact row mapping. Source DAE owner 545 contains
`body1.w_a - angularVelocity2(body1.frame_a.R)`. The same owner in the structural
DAE contains:

```text
body1.z_a - (
  Internal.resolve2_der(jointUPS.R_ia_a.T, jointUPS.R_ia_a.w,
                       jointUPS.frame_a.R.w, zeros(3))
  + linear_solve(identity(3), body1.z_a))
```

The acceleration cancels through the generated identity-matrix solve. At the
captured point, independent unit-coordinate perturbations of the primal residual
and the projection, full, and directly regenerated JVPs all return zero on these
rows. This rules out missing sparse entries or an AD-only error as the cause of
the observed zero matrix. The circular form already exists before Solve lowering;
the next proof must isolate which source-constraint/auxiliary reconstruction
introduces it and reproduce that transformation in a focused regression.

The saved OMC backend instead retains
`$DER.jointUPS.R_ia_a.w[i] = body1.z_a[i]` (equations 310–312) and the equivalent
body2 derivative relation (320–322). Its four selected scalar states are the
two revolute angles and their angular velocities. Different state counts alone
do not establish the defect; the circular structural equation is the concrete
frontier for the next comparison.

`rolling-wheel/line-force-circular-sensitivity-evidence-1.json` binds the
source/structural/Solve artifacts, OMC equations, numerical capture, and public
replay. The source DAE is byte-identical to the preceding diagnostic. The new
runtime event reports the existing failed matrix without changing evaluation,
factorization, seeds, tolerances, or failure classification. All 24 seed-focused
tests, all-target/all-feature solver Clippy, formatting, and whitespace checks
pass. The worker build and diagnostic helpers complete; the first helper compile
needed an external-table slice type correction. These are diagnostic results,
not an execution or parity gain. The complete cohort below remains authoritative;
combined `verify quick` and `verify full` are still outstanding.

## Full comparison restores GyroscopicEffects and retains every high model

The complete `multibody-demotion-bounds-full-11` run at
`b008b1a794d951e17e1d9b2e9fc7ccc9a335e387` compares 165/566 strict-high
(29.15%): 165 compared, 20 reviewed exclusions, zero missing/nonidentifiable
traces, and zero near/deviating comparisons. All 22,976 initialization channels
are high. Raw execution is 185 models; exclusions are not high-parity credit.
The run uses 11 workers and the unchanged phase/solver budgets. Its worktree
digest is `48672dfdec9c3416aac00ca9d56725958329a60236f921974bbf966a09020c35`;
tracked source is clean and the foreign `comm_fastdyn.md` remains untracked.

MultiBody returns to 23/42 high, with one reviewed BevelGear1D exclusion and no
missing comparisons. GyroscopicEffects is restored from the preceding full
run's Solve timeout to high parity. Every previously high model and reviewed
excluded execution is retained. The four repaired counterexamples stay high;
both Cauer examples retain their gains against the preceding committed baseline.
RevoluteConstraint advances from a timeout to an explicit structural singularity,
which is still a failed model. LineForce still times out in Solve.

Receipts `rolling-wheel/demotion-bounds-full-{previous,baseline}-delta-1.json`
compare identical 566-model rosters against the failed independent-lift attempt
and the preceding `target/msl/results` baseline. The aggregate gate exits zero.
No baseline is promoted. This closes the lost-model regression and permits
returning to the LineForce frontier; 100% MultiBody and combined quick/full
verification remain outstanding.

The current LineForce diagnostic confirms two separate failures. Its exact saved
source prepares in 10.348 seconds with 32 state declarations, 28 manifold rows,
and 954 candidate reconstruction attempts. The artifact-disabled actual worker
spends 12.870 seconds in Solve, then fails initialization with `EX002`:
`algebraic projection sensitivity matrix is singular`. It produces no trace.
The diagnostic's larger outer budget grants no coverage credit; the ordinary
full run still fails the unchanged 10-second Solve budget.

The producer artifacts, worker hash, and perf capture are bound by
`rolling-wheel/line-force-demotion-bounds-evidence-1.json`. Perf attributes cost
to repeated incidence projection, variable reservation/equality, allocation, and
hashing; it does not establish that optimizing name lookup alone would meet the
budget. The next correctness investigation must identify the singular projection
block and its exact initialization point, then compare those equations with the
saved OMC backend equations. OMC explicitly retains the line-length first/second
derivative chain and damper force closure (`line-force-omc-damper-closure-1.json`).
The current evidence does not yet prove which Rumoca equation or coordinate
diverges from that closure. No further compiler/runtime change is made on this
hypothesis alone.

## GyroscopicEffects matching bound passes focused verification

The first divergent search decision precedes the independent additive-lift fix:
the necessary independent demotion-anchor repair expands first-round admission
from 27 to 57. Earlier affine/value-identity inspections emit 364 records versus
528 after that repair. The same-source prepared systems before/after the final
additive-lift change remain identical. Artifact-disabled saved/current workers
measure Solve at 5.165/6.952 seconds in one diagnostic pair; the saved worker is
the affine-state-anchor build, not the profiling script's current Git HEAD.
Hashes, selected records, and timings are retained in
`rolling-wheel/gyro-demotion-search-localization-1.json`.

Direct search already retains the first checked strictly reduced candidate.
Later candidates can supersede it only by completing the matching. The candidate
optimization bounds how many continuous scalar rows a demotion can change,
counting whole compact owners and conservatively adding any restored lifted
owner. Changing at most m rows can increase maximum matching cardinality by at
most m: removing those rows from any new matching leaves a source matching.
Direct demotion preserves equation/unknown counts, so residue cannot decrease
by more than 2*m. Candidates with a larger source residue need no trial once a
checked reduction exists. No candidates are removed from admission, no state
priorities change, and every attempted/selected result retains its checks.

The source-backed five-independent-constraint regression fails on the original
search with 15 reconstructions and passes with the bound for scalar, singleton,
and three-element tensor payloads. A separate mixed-width fixture proves that
a later Sorted tensor candidate still supersedes a Reduced scalar candidate,
producing the exact same prepared DAE as its exhaustive trial. All 184 structural
tests and all-target/all-feature Clippy pass (`demotion-bounds-green-5.log`,
`demotion-bounds-clippy-3.log`). Earlier fixture naming and nesting failures remain
recorded. No debug probe, model branch, timeout change, or tolerance change is used.

GyroscopicEffects retains byte-identical prepared DAE and manifold artifacts and
all 24 selected reductions. Actual reconstruction attempts fall from 452 to 281;
total observation records fall from 528 to 357. Its public structural diagnostic
falls from 5.287 to 3.097 seconds. A sequential artifact-disabled actual-worker
pair measures Solve at 6.865/5.043 seconds and integration at 0.407/0.405 seconds.
Both execute and initialize successfully. These are single diagnostic pairs,
not statistical benchmark claims. Perf capture completes with 5,062 candidate
samples; its deliberate SIGINT exit is recorded separately from the worker's
successful exit. Receipts are `gyro-demotion-bounds-{artifact-delta,ordinary-pair}-1.json`.

At `a671ecb8`, digest `4bdbfb623dc4082fb3a43e9c4ef67810c2733a35b40b48ae97fa7014a578a2a1`,
the ordinary `multibody-demotion-bounds-frontier` run has four high comparisons,
one reviewed BevelGear1D exclusion, and zero missing/deviating results. It restores
GyroscopicEffects execution relative to the latest full attempt and preserves
every phase/band against the same six-model `multibody-round-facts-origin` roster.
LineForce still times out in Solve. The eight-model origin run preserves all six
high comparisons and both reviewed exclusions. The fixed canary preserves all
twenty phase/band rows, nine high comparisons, zero exclusions/missing/deviation.
Their deltas are `demotion-bounds-{frontier,origin,canary}-delta-1.json`.

A fresh complete cohort is still required to restore coverage credit; the latest
authoritative MultiBody count below remains 22/42. Combined quick/full verification
also remains outstanding; this checkpoint is not release evidence.

## Full comparison closes false traces but loses GyroscopicEffects execution

The full `multibody-independent-lift-full-11` run at
`a671ecb8ac35bfc10a3a9db02e0f81feb40f651c` compares 164/566 strict-high
(28.98%): 164 compared, 20 reviewed exclusions, zero missing/nonidentifiable
traces, and zero near/deviating comparisons. All 22,009 initialization channels
are high. PreLoad and DCPM_Drive retain excluded execution; the four repaired
counterexamples remain high. Both Cauer models gain high comparison against
the preceding committed cohort. The run's worktree digest is
`48672dfdec9c3416aac00ca9d56725958329a60236f921974bbf966a09020c35`;
the only untracked workspace file is the preserved `comm_fastdyn.md`.

GyroscopicEffects loses its high result to a Solve timeout: the worker exceeds
the unchanged 10-second budget after 11.559 seconds. MultiBody consequently
falls to 22/42 high: 22 compared, one reviewed BevelGear1D exclusion, and no
missing comparisons. The aggregate gate exits successfully, but the loss of
a previously high model blocks campaign acceptance and baseline promotion.
The complete phase and band delta is
`rolling-wheel/independent-lift-full-delta-1.json`. LineForce remains a Solve
timeout and earns no new credit.

The first GyroscopicEffects diagnostic rejects an enlarged structural system
as the explanation for the independent-lift change. Public inspections of the
identical source DAE produce byte-identical prepared DAEs and manifold ordinals
before and after that change, with all 528 reduction records identical. Their
diagnostic reduction durations are 5.324 and 5.287 seconds; this single pair is
not a speed claim. The prepared DAE SHA-256 is
`38126fc10c1eb1c89c6b0cad70feed5efb3c1d4ef33396dad93d8216ac3a6dad`.
The receipt is `rolling-wheel/gyro-independent-lift-artifact-delta-1.json`.

An artifact-enabled actual-worker run completes simulation and records
7.545 seconds for Solve lowering. Its perf interval also includes artifact
serialization, so that profile cannot be treated as ordinary cohort timing.
The profile exposes existing variable-name equality and reservation costs,
incidence projection, allocation, hashing, and output writes; it does not
establish additive-proof search as the bottleneck. Artifact-disabled control
and candidate worker profiles are the next comparison before choosing a
performance change. The failed full attempt remains recorded; a diagnostic
completion does not restore its coverage credit.

## Independent source equations restore PreLoad execution

The new additive lift proof excludes the equation being replaced and the
coordinate being lifted. It derives the value from other source equations,
refuses cycles, and reconstructs the value and derivative from that same
postordered derivation. Rows are collected once per immutable source round;
payloads remain whole tensors. Non-additive lift profiles are unchanged.

The source-backed regression demonstrates the previous semantic loss directly:
`r+p=0; s=L-q; p*q=-1; r=q` admitted a lifted manifold at
`p=2, q=-0.5, r=-2, s=1.5, L=1`, although the original `r=q` residual is -1.5.
The repaired manifold rejects that point. Scalar, one-element, and three-element
payloads, independent signed aliases, circular aliases without an independent
definition, and restoration after demotion are covered. All 182 structural
tests and all-target/all-feature Clippy pass. Earlier fixture, shape, and lint
failures remain in the numbered logs; final evidence is
`independent-lift-green-5.log` and `independent-lift-clippy-3.log`.

The ordinary `multibody-independent-lift-origin` run at `78b2018f`, worktree
digest `80994095174927ccb78e0af45be059e0e8645b32f083db56336ba46e1216e4d0`,
restores PreLoad from EL005 to `sim_ok`/`ic_ok` under its existing reviewed
comparison exclusion. DCPM_Drive retains its reviewed exclusion. All six
compared traces remain high: Damper, ElastoGap, Oscillator, ArmatureStroke,
CauerLowPassAnalog, and CauerLowPassOPV. There are zero missing,
nonidentifiable, near, or deviating comparisons. This is focused evidence;
the repaired counterexamples remain required in the next complete sweep.

The exact saved PreLoad source now prepares with eight states and two manifold
rows. The transformed velocity equation is `friction.v_rel - spool.v`, matching
OMC's generated `spool.v = friction.v_rel`. No initial-value pins are added.
The prepared DAE has SHA-256
`dcf1430b5b2b91ab2650f4c8f7b0708922b4d6af85e1e003cb2cf0828d52ebfb`.
The public inspector is bound to the ordinary worker's exact library artifacts;
its timing is diagnostic only. Receipts are
`preload-independent-lift-equations-1.json` and
`independent-lift-inspector-receipt-1.json` under `rolling-wheel/`.

The fixed `multibody-independent-lift-canary` preserves every phase and band
against `multibody-lift-restoration-canary`: nine compared high, zero
exclusions/missing/nonidentifiable results, and all 175 initialization channels
high. The other eleven models remain unsuccessful. Origin and canary deltas
are in `independent-lift-{origin,canary}-delta-1.json`. This closes the focused
execution regression; full-cohort and combined quick/full verification remain
outstanding, and no new MultiBody coverage or RollingWheel speed claim is made.

## Focused counterexamples recover; PreLoad still blocks the candidate

The latest `multibody-lift-restoration-origin` run at `78b2018f`, with worktree
digest `dc6a6448d181197d9da861754ded24f6fa8e429ab85e4717dde6a4a96911cccb`,
compares all six available traces high: Damper, ElastoGap, Oscillator,
ArmatureStroke, CauerLowPassAnalog, and CauerLowPassOPV. DCPM_Drive executes
under its existing reviewed comparison exclusion. There are zero missing,
nonidentifiable, near, or deviating comparisons. PreLoad still fails structural
reduction with EL005, reporting 111/116 matches. Its prior excluded execution
must be restored before this candidate is complete. These focused results do
not establish a new full-cohort result or close the required full-sweep check.

The repairs separate exact value identities from displaced derivative
identities and preserve the proof used by reconstruction. Direct demotion
selects an independent derivative anchor. Retained first derivatives and lifted
definitions replay their captured exact anchor policy, including in cache keys.
Armature's two canceled velocity conditions now retain actuator/load velocity
differences, matching the relationship in OMC's generated equations. Finally,
undoing a lift restores the original source residual instead of a substituted
manifold expression. PreLoad's source connection at owner 108 is now restored
as `rod3.flange_b.s - rod4.flange_a.s`; it no longer duplicates owner 109,
`rod4.flange_a.s - spool.flange_a.s`.

All 180 structural tests and all-target/all-feature Clippy pass. The fixed
`multibody-lift-restoration-canary` preserves every phase and band against
`multibody-affine-state-anchor-canary`: nine compared high, zero exclusions,
missing, or nonidentifiable results, and all 175 initial-condition channels
high. Eleven other canary models remain unsuccessful; this is preservation,
not complete canary model support. Receipts under `rolling-wheel/` are
`lift-restoration-{origin,canary}-delta-1.json`,
`lift-restoration-green-1.log`, `lift-restoration-clippy-build-1.log`,
`lift-anchor-manifold-delta-1.json`, and
`preload-lift-restoration-equation-delta-1.json`.

The remaining PreLoad hypothesis is an earlier circular dependency: a lifted
definition's exact value anchor may rely on the equation being replaced.
Its derivative then duplicates the neighboring connection, and the next
holonomic replacement correctly refuses a vacuous residual. Restoring the
original equations fixes the subsequent ownership loss but does not establish
an independent lifting proof. The next reproduction must distinguish a value
definition supported by other equations from one supported by its own owner.
No tolerance, model-specific branch, or runtime dispatch change was made.
Changes remain uncommitted; a new full sweep and combined quick/full verification
are outstanding while this execution regression is unresolved.

## Uncommitted state-balance work exposes circular manifold reconstruction

The state/invariant balance regression now passes: a fixed support contributes
no derivative, while only a proved zero support may disappear from an exact
value equality. LineForce's identical source DAE now reaches a prepared system
instead of stopping at 2,203/2,204 matches. Its ordinary worker still exceeds
the 10-second Solve budget. An unrestricted diagnostic worker on the first
candidate reaches initialization and reports a singular algebraic sensitivity
matrix; it produces no trace and earns no coverage credit.

The first full candidate, `multibody-state-balance-full-11` at `78b2018f` with
worktree digest `50b444de5b5b0062fbc72ac2279a70e0a521db2422ebf62c1978a0bae5e06f1f`,
compares 165/566 high, with 19 reviewed exclusions and zero missing or deviating
traces. CauerLowPassAnalog and CauerLowPassOPV gain high comparison, but PreLoad
loses its previously executable, excluded result. This execution loss remains
a regression despite the gate passing.

Changing derivative lookup to the full affine class restores PreLoad's
structural replay, but the next full gate **fails**. At the same HEAD with
digest `842e1538ad40459344defcf99cf448f59f21d3851ccdb0d5912cac56e33594d9`,
`multibody-affine-state-anchor-full-11` compares 165 traces: 161 high, one near,
three deviating, 19 reviewed exclusions, and zero missing. Damper, ElastoGap,
Oscillator, and ArmatureStroke are actionable counterexamples. DCPM_Drive also
loses its previously executable, excluded result. Both focused six-model runs
and the fixed canary had unchanged bands; all 175 structural tests and affected
Clippy passed. The full evidence overrides those focused results.

The exact Damper replay localizes the first loss: retained manifold expressions
390, 400, and 410 become `massN.flange_b.s - massN.flange_b.s`. The other three
manifold rows retain the body half-length offsets. At initialization the actual
worker reports `mass1.flange_b.s=3.5` and `damper1.flange_a.s=4.5`, violating
their source connection by one metre. A source equality used to materialize
its own replacement has erased the independent position constraint. This
requires a structural proof repair; numerical tolerances cannot repair it.
The candidate remains uncommitted and further coverage work is stopped.

Receipts under `rolling-wheel/` are `state-balance-full-delta.json`,
`affine-state-anchor-full-delta.json`, `preload-{before,after}-inspection-1.log`,
and `affine-state-anchor-inspector-receipt-1.json`. Damper's exact artifacts and
prepared manifold are in `damper-affine-state-anchor-{artifacts,inspection}-1/`.
No solver dispatch change was made. Affine value recovery already uses guarded
tearing after full AD assembly; `SeedBlockLinearization` still factors the full
matrix for sensitivities. A compiler-composed reduced value/JVP kernel is pending.

## Retain stalled equations for the LineForce comparison

Structural inspection now retains one already-owned stalled DAE together with
its manifold and exact failure at the point it would otherwise be discarded.
The ordinary observer drops the same values without allocating, cloning IR,
or repeating analysis. This diagnostic snapshot is never a prepared model;
successful preparation returns no stalled snapshot.

The LineForceWithTwoMasses capture reaches 2,203 matched equations out of
2,204 equations and unknowns, with 26 retained manifold expressions. Its
seven-equation overdetermined witness ends at `f_x[1475]`, the connection
`jointUPS.axis.s = damper1.flange_b.s`; `jointUPS.f_bd_a[2]` remains unmatched.
All 1,176 recorded reduction events are byte-identical to the prior public
inspection, showing that observation preserved the actual decisions.
The snapshot has SHA-256
`f59b86731aa723b8db8523cc02f86012ebbf131c4a4ceccb754c9e5cc331be29`.
Receipts are `rolling-wheel/line-force-stalled-capture-1.json` and
`rolling-wheel/stalled-diagnostic-helper-receipt-1.json`; the captured DAE and
manifold are under `rolling-wheel/line-force-stalled-inspection-1/`.

All 174 structural tests and all-target/all-feature Clippy pass. The new
regression compares observed and ordinary failures, preserves the source DAE,
and checks the retained failure against a fresh analysis of that exact DAE.
The first build found an existing recorder test needing the changed private
signature; Clippy then required extracting the discard operation to keep the
entry point within its function budget. Final evidence is
`stalled-diagnostic-clippy-2.log` and `stalled-diagnostic-libraries-2.jsonl`.
This is diagnostic progress, not a new model pass or a worker timing result.

## Quick comparison preserves the cohort; spec budget repaired

The full MSL step of `verify quick`, written to `target/msl/results` at
`648ec46e0806afcdbdbcc5ef7c7b81e2485d5f70`, preserves every comparison band:
163/566 strict-high, 163 compared, 20 reviewed exclusions, zero missing or
nonidentifiable traces, and zero near/deviating comparisons. All 22,718
initialization channels are high. The only phase/status change from the
preceding named full sweep is Fourbar_analytic moving from timeout to explicit
structural EL005; it gains no parity credit. The receipt is
`rolling-wheel/round-facts-quick-msl-delta.json`.

Workspace lint, all 28 corpus-pin cases, and all 243 architecture tests pass.
The adjacent spec gate fails because SPEC_0036 and SPEC_0038 exceed the
2,500-word cap. Redundant prose is condensed with requirements and normative
catalog links preserved, bringing them to 2,498 and 2,495 words. The existing
gate executable's exact `test_specs_respect_size_budgets` test now passes
(`rolling-wheel/spec-budget-repair-focused-1.log`). This documentation repair
was applied after the architecture step while the unchanged compiler was
building workspace tests. The workspace step subsequently passed all 7,747
tests with zero skipped, plus its documentation tests. A complete rerun of
the repaired architecture command passed all 243 architecture tests and 17
suite gates (`spec-budget-architecture-recheck-1.log`). The original combined
quick run records its one failed step; combined full remains outstanding.

## Full sweep restores Driving without losing a high model

The complete `multibody-round-facts-full-11` sweep at
`720680e9ed3055308b2516271bbbb5d96a200cc1` passes its quality gate:
163/566 strict-high models (28.80%), 163 compared, 20 reviewed exclusions,
zero missing or nonidentifiable traces, and zero near/deviating comparisons.
All 22,718 compared initialization channels are high.

MultiBody reaches 23/42 high: 23 compared, one reviewed BevelGear1D exclusion,
and zero missing comparisons. RollingWheelSetDriving returns to high with
Solve completing in 6.1343 seconds under the unchanged 10-second budget.
No previously high model loses its band. The only other phase/status change
against `multibody-alternative-definitions-full-11` is
`DC_CompareCharacteristics` moving from timeout to an explicit structural
refusal; it gains no compilation or parity credit. LineForceWithTwoMasses
still times out in Solve. The complete delta and artifact digests are in
`rolling-wheel/round-facts-full-delta.json`.

The Driving cohort regression is closed by this complete comparison. The next
compiler frontier remains LineForceWithTwoMasses' unresolved displacement and
derivative closure against the retained OMC equations. Combined verify quick/full
remain outstanding; this successful MSL sweep does not establish those gates,
complete MultiBody support, or PR readiness.

## Share differentiation facts within each immutable reduction round

RollingWheelSetDriving's saved pre-change and current workers produce identical
Flat, DAE, structural DAE, Solve IR, and complete trace bytes. The timeout did
not arise from a larger generated system. A public reduction inspection spends
6.546 seconds selecting 24 transformations from 418 attempts. Actual-worker
`perf` samples locate repeated source analysis inside preparation. These records
are in `rolling-wheel/wheel-set-round-facts-diagnosis-1.json`.

Each reconstruction previously collected the same source differentiation facts
again. A private `ReductionSource` now binds one immutable DAE to its facts;
candidate discovery, manifold preflight, and all attempted reconstructions in
that round consume that pair. The next transformed source constructs fresh
facts. Candidate order, matching, retained equations, initialization checks,
and numerical execution are unchanged. SPEC_0007 records the ownership rule;
its wording was also condensed to satisfy the 2,500-word spec budget without
removing the governing STRUCT-T03 catalog requirements.

The two-independent-constraints regression records three collections instead
of one before the fix, then one per round and a fresh collection after the
source changes. All 173 structural tests and all-target/all-feature Clippy pass
(`round-facts-red-1.log`, `round-facts-green-2.log`, and
`round-facts-validation-build-2.log`). Earlier signature and lint failures
remain in the numbered logs. The ordinary worker build includes `msl_tests`.

One alternating ordinary-worker pair with artifact output disabled measures
Solve program preparation at 8.4275 seconds for the saved parent worker and
5.7507 seconds for the candidate. This observed 31.76% reduction is one
unpinned pair, not a repeated benchmark claim. Simulation remains about
0.333 seconds for both. The artifact-enabled candidate also retains all five
complete artifacts byte-for-byte, including the trace with SHA-256
`268a5d61f8adf56e17db4a2661b93593249b4cb3a112a25c0990af7d6826955d`.
Receipts are `round-facts-driving-timing-1.json` and
`round-facts-driving-artifact-delta-1.json`.

The timing field named `ir_solve_structural_dae_seconds` currently receives
`runtime_value_seconds`; it does not measure structural index reduction.
`ir_solve_lower_seconds` includes `prepare_for_solve` plus program/artifact
lowering. The approximately 47 ms field in earlier diagnostics must not be
interpreted as the cost of index reduction.

The six-model `multibody-round-facts-origin` gate compares RollingWheelSetDriving,
RollingWheel, GyroscopicEffects, and OvervoltageProtection high: four compared,
one existing BevelGear1D exclusion, zero missing/nonidentifiable/deviating
comparisons, and all 2,088 initialization channels high. Driving completes
Solve in 6.0957 seconds under the unchanged 10-second budget.
LineForceWithTwoMasses still times out in Solve and receives no credit.
`round-facts-origin-evidence-1.json` binds these results.

The fixed `multibody-round-facts-canary` has no phase or band changes against
`multibody-alternative-definitions-canary`: nine compared high, zero skips,
missing, exclusions or nonidentifiable results, and all 175 initialization
channels high (`round-facts-canary-delta.json`). This is focused recovery;
the next complete sweep must confirm the cohort regression is closed. Combined
verify quick/full and complete MultiBody support remain outstanding. This patch
does not implement the reduced AD kernel or establish an OMC runtime speed win.

## Alternative definitions gain two high models and expose a timeout

The complete `multibody-alternative-definitions-full-11` sweep at
`f0989e3f1e655485820d5aa267c8d1e76564dcd8` passes its MSL quality gate:
162/566 strict-high models (28.62%), 162 compared, 20 reviewed exclusions,
zero missing or nonidentifiable traces, and zero near or deviating comparisons.
All 21,826 compared initialization channels are high.

GearConstraint and `Modelica.Mechanics.Rotational.Examples.First` newly compare
high. GearConstraint completes Solve in 3.266 seconds; First completes it in
8.28 ms after previously refusing a 52/54 structural match. However,
RollingWheelSetDriving loses its high band through a Solve timeout after
11.103 seconds against the unchanged 10-second budget. Its previous full-run
Solve time was 8.675 seconds. MultiBody therefore remains 22/42 high: 22
compared, one reviewed exclusion, and zero missing comparisons. Fourbar_analytic's
structural refusal also becomes a timeout, which establishes no resolution of
its structural defect. The complete delta and artifact digests are retained in
`rolling-wheel/alternative-definitions-full-delta.json`.

RollingWheelSetDriving is the next regression to address. A subsequent
diagnostic profile completes with 8.1105 seconds of Solve lowering and a
0.3282-second simulation, but that output-enabled, unpinned diagnostic does not
replace the bounded full-run failure or establish a performance change. Its
artifacts and perf samples are in `rolling-wheel/wheel-set-definitions-candidate-1`.
No reduced-Jacobian kernel or runtime tearing change was made in this patch.
Combined verify quick/full remain pending, and complete MultiBody support and
PR readiness remain unestablished.

## Retain alternative defining equations during structural substitution

LineForceWithTwoMasses' timeout occurs in structural index reduction, before
Jacobian construction. The actual-worker Solve profile ends after 15.3 seconds
with 2,164/2,204 equations matched. Inspection records 107 rounds and reaches
an intermediate residue of one equation/unknown pair before returning the
original singularity. OMC's retained backend XML instead treats
`jointUPS.axisLength` as a dummy state and supplies its first and second
derivatives. Its source has both a geometric length definition and a
displacement-plus-offset definition. Evidence is
`rolling-wheel/line-force-structural-diagnosis-1.json` and the retained
`line-force/omc-stages` captures.

That investigation exposed a general distinction: executable causal elimination
requires a unique definition, but structural substitution may use an exact
state/invariant-anchored equality while retaining the other equations as
constraints. Structural facts now complete missing definitions through a
deterministic acyclic closure over checked whole-coordinate equations. Every
source equation remains; executable causal-definition policy is unchanged.
SPEC_0007's STRUCT-T03 profile records this source-preserving substitution.

The reduced scalar/vector regression fails before the fix and passes afterward.
A cycle without value anchors remains unmaterializable. Three older tests that
assumed displaced definitions were unsupported now check the admitted source
constraints; the constant and parameter-offset cases also evaluate both retained
position and velocity residuals at multiple parameter values. All 172 structural
tests and affected all-target/all-feature Clippy pass
(`alternative-definitions-validation-build-7.log`). Earlier fixture, oracle,
and lint failures remain in the numbered logs.

The five-model `multibody-alternative-definitions-origin` retains high comparisons
for RollingWheel, GyroscopicEffects, and OvervoltageProtection: three compared,
one reviewed BevelGear1D exclusion, and zero missing or deviating comparisons.
LineForceWithTwoMasses still exceeds the unchanged Solve budget. Replaying the
same source DAE with the new reducer records 102 rounds and still ends one
equation/unknown pair short; the alternative-definition gap is not the complete
explanation. No compilation or parity credit is claimed for that model.
Receipts are `alternative-definitions-origin-evidence-1.json` and
`alternative-definitions-line-force-delta.json` under `rolling-wheel`.

The fixed `multibody-alternative-definitions-canary` preserves every phase and
band from `multibody-invariance-sharing-canary`: nine compared high, zero
skipped/missing/excluded/nonidentifiable traces, and all 175 initialization
channels high (`alternative-definitions-canary-delta.json`). The next complete
cohort and combined verify quick/full remain pending.

## Full sweep confirms shared-proof construction

The complete `multibody-invariance-sharing-full-11` sweep at
`f821fcce94d964671e7dcc12e23c2da87cddc4f8` passes its MSL quality gate.
It measures 161/566 strict-high models (28.45%): 161 compared, 20 reviewed
exclusions, zero missing or nonidentifiable traces, and zero near or deviating
comparisons. All 21,865 compared initialization channels are high. The run
reports a dirty worktree because the foreign untracked `comm_fastdyn.md` is
present; authored source changes were committed before the run.

GyroscopicEffects returns to high with all 967 channels high; Solve takes
8.771 seconds, including 8.712 seconds of lowering, within the unchanged
10-second phase budget. MultiBody is 22/42 high, with 22 models compared,
one reviewed BevelGear1D exclusion, and zero missing comparisons. Its raw
simulation completion count is 23/42. IMS_Start remains a reviewed reference
boundary with no high-parity credit. No previously high model loses its band.

GearConstraint remains unsuccessful: the prior structural failure becomes a
Solve timeout after 11.623 seconds against the 10-second budget. That change
does not establish that its structural failure is fixed. The retained
`invariance-sharing-full-delta.json` records both phase changes and the
comparison-artifact digests against `multibody-guarded-affine-full-11`.
The earlier failed sweep remains recorded below.

This result establishes neither complete MultiBody support nor a RollingWheel
speed win. The affine path uses the checked tearing layout for its reduced
linear solve but still evaluates the full block Jacobian first. Its coordinate
recovery and original-residual refinement are necessary for the electrical
regressions; full-block differentiation is an implementation limitation, not
a mathematical requirement of those checks. Combined verify quick/full remain
pending; only the full MSL parity gate was run in this sweep.

## Share Jacobian invariance proofs across projection blocks

GyroscopicEffects exposed repeated construction work: every algebraic block
re-derived operation-invariance facts for the entire canonical Jacobian source.
A 32-block regression with 128 source operations recorded 4,096 proof visits
instead of 128 (`projection-invariance-sharing-red-2.log`). The first attempted
fixture used an incorrect enum spelling and failed to compile; only the second
run is behavioral red evidence.

Structural-artifact construction now derives one immutable proof for the exact
source and shares it across block applications. Replacement and specialization
still derive fresh facts. The regression checks both linear work and different
facts for a changed source, while retaining the original owner's facts. This
changes compiler preparation, with no change to Modelica equations, AD rules,
runtime value reuse, tolerances, or numerical acceptance. SPEC_0036 and
SPEC_0043 §6a record the construction scope.

All 323 Solve IR, 100 Cranelift, and 503 solver tests pass, as does affected
all-target/all-feature Clippy (`projection-invariance-sharing-build-1.log`).
The four-model `multibody-invariance-sharing-origin` compares GyroscopicEffects,
RollingWheel, and OvervoltageProtection high: three compared, one existing
BevelGear1D reviewed exclusion, zero missing/deviating comparisons, and all
1,196 initialization channels high. The fixed `multibody-invariance-sharing-canary`
retains every phase and band from `multibody-reference-lineage-canary`: nine
compared high, no skips/missing/exclusions/nonidentifiability, and 175 high
initialization channels. Receipts are `invariance-sharing-origin-evidence-1.json`
and `invariance-sharing-canary-delta.json`.

Three alternating pairs using the exact ordinary MSL worker build measure:

| Pair | Shared-proof lowering (s) | Saved-control lowering (s) |
|---|---:|---:|
| 1 | 7.3694 | 8.4929 |
| 2 | 7.3781 | 8.4287 |
| 3 | 7.2600 | 8.2475 |

Median lowering time drops from 8.4287 to 7.3694 seconds, a 12.57% reduction.
All six complete traces are byte-identical. These are three unpinned pairs on
a shared host, measuring lowering only. They establish no integration speedup
or OMC performance win. `invariance-sharing-ordinary-timing-series-1.json`
binds exact workers and traces. An earlier pilot omitted the harness's test
target from the build, changing dependency feature unification; its timing
series is retained but superseded by these ordinary-worker measurements.
The emitted Flat, DAE, structural DAE, and canonical Solve artifacts also match
byte-for-byte (`invariance-sharing-artifact-equality-1.json`).

The initial diagnostic-output hypothesis was rejected: JSON-enabled profiling
included roughly four seconds of requested artifact preparation/output after
lowering, but the output-disabled control spent only 53 ms there. No worker
shortcut or budget change was retained. The actual compiler fix addresses the
repeated proof derivation. GyroscopicEffects passes the focused bounded attempt;
its previous full-sweep timeout remains recorded until the next complete cohort
establishes the result under full-run conditions. Combined verify quick/full
remain pending.

## IMS_Start has a reviewed reference-accuracy boundary

The convergence evidence below now supports a tracked comparator exclusion for
IMS_Start. The model remains visible, receives zero strict-high or certification
credit, and remains in the next complete cohort. No compiler equation, solver
or comparator tolerance, official OMC trace, or baseline floor changes.

Quality-gate schema 5 records the policy migration against
`multibody-guarded-affine-full-11`, with convergence evidence committed at
`3d76411c1a41a1b27e6a0ecbf1cf3e204f0b47a4`: strict-high remains 160, reviewed
boundaries increase from 19 to 20, and the newly excluded model was non-high.
These are classification counts on the retained failed sweep, not a new run
or baseline promotion. The exclusion artifact's SHA-256 is
`30f7c38e58307d6af4f69b844512679ec860f367f88670481edad5318d7d29f8`.
The checked baseline retains all its previous measurements and certified model
identities; only its gate version and new reference-boundary metadata change. Historical
comparator, partial-classification, and promoted-baseline lineage evidence remain
unchanged. Both baseline readers reject missing or forged boundary evidence;
the remote-baseline resolver preserves all unrelated ratchets across version 4
to 5. The exclusion digest is checked against the exact tracked artifact.

Validation passes: 97 quality-gate tests, 169 harness library tests, 21
baseline-resolver tests, and affected all-target/all-feature Clippy. The final
`multibody-reference-lineage-canary` retains every phase and band from
`multibody-trace-json-canary`: nine compared high, zero skipped/missing/excluded
or nonidentifiable traces, and all 175 initial-condition channels high. Evidence
is `ims-start-boundary-{migration-1,canary-delta}.json` and
`ims-start-boundary-lineage-tests-1.log`. Combined verify quick/full and the next
complete cohort remain pending; this change does not establish release readiness.

The decoder regression remains the focused test for the actual transport bug.
The recorded default/reference-refinement comparisons establish the numerical
oracle boundary; they do not certify untested conditions or turn the model
into a high-parity result. GyroscopicEffects' compile-budget failure still
requires a compiler fix, and the failed full sweep remains failed.

## Full guarded-tearing sweep exposes trace transport and remaining failures

The complete `multibody-guarded-affine-full-11` sweep at `b0217592` fails its
quality gate. It compares 161 models: 160 high, one near, 19 reviewed exclusions,
and zero missing or nonidentifiable traces. IMC_DOL and IMC_Steinmetz newly
compare high. IMS_Start newly simulates but has 690 high, 14 near, and six
deviating channels out of 710. GyroscopicEffects loses its previous high result
because Solve lowering exceeds its 10-second phase budget; its previous
lowering time was 8.805 seconds. MultiBody therefore measures 21/42 high in
this attempt, down from 22/42. The original failure remains recorded in
`guarded-affine-full-delta.json`; no retry or partial snapshot replaces it.

IMS_Start revealed a transport defect before any equations needed changing.
The solver's JSON contains distinct left-limit and settled times
`0.09999999999999999` and `0.1`. The default JSON decoder rounds both to `0.1`,
although the voltage metadata correctly declares a continuous channel. The
comparator then loses a side of the discontinuity and creates an interpolation
ramp across the preceding output interval. The two reduced red tests prove
both the changed timestamp bits and an artificial error integral of 3.50000000385
instead of approximately `3.85e-9` (`trace-json-roundtrip-red-2.log`). This is a
decoder defect, not a reason to merge event coordinates or loosen comparison.

The workspace JSON dependency now enables `float_roundtrip`. The same shared
transport preserves finite IR coefficients and literals as well as trace
samples. SPEC_0033/0050 and SPEC_0036 record exact finite-value transport. Three
regressions cover adjacent event times, the manufactured ramp, signed zero,
subnormal values, and extreme finite values. All 187 DAE IR, 322 Solve IR, and
131 simulation facade tests pass, with affected Clippy clean
(`trace-json-roundtrip-green-1.log`, `trace-json-roundtrip-libraries-build-1.log`).

Recomparing the unchanged IMS_Start files through the production loader and
comparator removes four voltage-channel deviations, but does not close the
counterexample: 690 channels remain high, 18 near, and two deviate. The remaining
worst channels are aliases of a nearly zero summed current; OMC reaches about
`9.24e-6 A` while Rumoca stays near `1e-13 A`. Their numerical and equation-level
origin remains under investigation. No simulation tolerance, comparator
threshold, or exclusion changed. The helper asserts the decoded adjacent
timestamps remain distinct; its exact libraries and result are retained in
`trace-json-helper-libraries-1.json` and `ims-start-roundtrip-comparison-1.json`.

The fixed `multibody-trace-json-canary` retains every phase and band from
`multibody-sparse-tearing-sensitivity-canary`: nine compared high, zero skipped,
missing, excluded, nonidentifiable, or deviating comparisons, and all 175
initial-condition channels high (`trace-json-canary-delta.json`). This completes
focused validation of the shared decoder fix. IMS_Start's remaining numerical
differences, the GyroscopicEffects timeout, and combined verify quick/full
remain open; no cohort baseline is promoted.

The same xtask-generated IMS_Start OMC executable was then evaluated at two
higher numerical accuracies, changing only the copied initialization XML's
experiment tolerance. Executable, XML, sparsity inputs, CSV, and trace digests
are recorded in `ims-start-omc-refinement-{2,3}/receipt.json`. The initial
diagnostic bundle omitted OMC's sparsity files and failed before initialization;
the corrected bundles include those exact inputs. Both numerical runs finish
successfully. Using the production comparator with the repaired decoder:

| Comparison | Shared channels | High | Near | Deviation |
|---|---:|---:|---:|---:|
| Original Rumoca vs OMC at `1e-6` | 710 | 690 | 18 | 2 |
| Same Rumoca trace vs OMC at `1e-10` | 710 | 710 | 0 | 0 |
| Same Rumoca trace vs OMC at `1e-12` | 710 | 710 | 0 | 0 |
| OMC `1e-10` vs OMC `1e-12` | 735 | 735 | 0 | 0 |

Rumoca's worst bounded channel error against the two refined references is
approximately 0.00256. The reference-to-reference comparison's worst channel
is `der(aims.i_0_r)` at 0.03731; all 735 channels meet the existing high threshold.
MSL's SpacePhasor component defines `-m*zero.i = sum(i)`, and generated OMC
equations 381/449 evolve the two zero-sequence currents from their inductor
voltages. Together with the original summed-current traces, the convergence
results identify an accuracy limitation of the default OMC reference. They
provide no reason to make Rumoca reproduce its small spurious currents.
This diagnostic does not replace the official reference or promote a cohort
count. Reference-quality handling remains open, alongside the GyroscopicEffects
compile-budget regression. Results are `ims-start-refined-comparison-{2,3}.json`
and `ims-start-omc-self-comparison-3.json`.

## Guard affine tearing and scale sensitivity checks by their direction

Affine projection now consumes a checked elimination layout derived from the
exact block's tearing and AD sparsity. RollingWheel's 24-coordinate block
reduces to five tear coordinates. The runtime retains sparse coefficients,
the recovery relation, and the reduced factorization; changed conditioned
coefficients revoke the cached factor. It solves the complete original
right-hand side and recovers every coordinate, including corrections during
mandatory original-equation refinement. Weak pivots or failed reduction retain
the full implicit solve from the same arithmetic origin and tolerance.
The original full AD Jacobian is still evaluated; this is not yet a
compiler-composed reduced derivative kernel.

The AD pattern conservatively contains three future causal dependencies that
structural tearing eliminated through invariant values. A replay at all 501
recorded RollingWheel coordinates found their coefficients exactly zero
(`inspect-affine-guards-result-1.json`). This sample is not an all-state proof:
the checked layout issues explicit guards, and the runtime checks fresh,
unconditioned coefficients before every solve. Nonzero and nonfinite guards
decline reduction, including a nonzero coefficient that conditioning would
underflow to zero. No model names or frozen parameter assumptions select this
path. SPEC_0036, SPEC_0043, and SPEC_0038 record these obligations.

The initial focused run exposed a BevelGear1D execution regression; it was
retained as a failure despite that model's existing comparison exclusion.
Capturing its first failed sensitivity block showed a finite direction as
large as `3.25e11`, certified against scales from much smaller primal
coordinates. Row 1191's original JVP residual was `-1.172149e-6`: its ratio to
the unchanged `1e-10` tolerance was 8.07 with the primal scale, but 0.0535 with
the direction scale (`bevel-sensitivity-scale-diagnosis-1.json`). This identifies
a sensitivity acceptance defect, not an invalid tearing layout.

A two-equation regression reproduces the defect independently of MSL:
`17*a + 19*b = 23*s`, `31*a + 7*b = 37*s`, with analytic sensitivities
`da/ds = 271/235`, `db/ds = 42/235`. Scaling the seed to `1e12` falsely failed
the old check (`sensitivity-scaling-red-2.log`). The cached linearization now
retains its original Jacobian and computes acceptance scales from each solved
direction. The final original JVP and tolerance are unchanged. Positive and
negative scaled directions pass; overflow and deliberately inconsistent JVPs
still fail and restore incoming algebraic seeds. All temporary capture probes
were removed before validation.

The final implementation passes 322 Solve IR and 503 solver tests, plus
all-target/all-feature Clippy (`sparse-tearing-sensitivity-build-1.log` and
`sparse-tearing-sensitivity-origin-1.log`). The three-model origin run restores
BevelGear1D to `sim_ok` with its existing reviewed exclusion, while RollingWheel
and OvervoltageProtection compare high: two compared, one excluded, zero
missing or deviating comparisons, and all 229 initial-condition channels high.
The fixed 20-model canary has no phase or band changes: nine compared high,
zero skipped, missing, excluded, or nonidentifiable comparisons, and all 175
initial-condition channels high. Receipts are
`sparse-tearing-sensitivity-origin-delta.json` and
`sparse-tearing-sensitivity-canary-delta.json`, against the corresponding
`multibody-register-constants` runs. These focused checks do not update the
last complete cohort count or establish completion of combined verify quick/full.

Three alternating ordinary-worker timing pairs retain every candidate trace
byte-for-byte equal to the compared origin trace, with identical canonical IR
between candidate and control. Sim times in milliseconds were:

| Pair | Guarded tearing | Saved control |
|---|---:|---:|
| 1 | 73.600 | 74.706 |
| 2 | 72.170 | 75.226 |
| 3 | 75.007 | 74.726 |

The medians are 73.600 and 74.726 ms, an observed 1.51% reduction with overlapping
ranges. Three unpinned pairs on a shared host do not establish a reliable speed
improvement. Preparation medians are 10.650 and 10.397 seconds, respectively;
these are separate from Sim. The retained receipt is
`guarded-tearing-timing-series-1.json`. The analysis helper initially expected
IR files in the focused harness directory, which does not emit them; its
bookkeeping repair reused the completed first sample without rerunning it.

Exact-worker perf samples confirm the guarded solve is active in all three
candidate runs. The control profiles retain faer's full sparse numeric
factorization; the candidate profiles sample `TornNewtonCache::solve_scaled`
and the existing full `rumoca_projection_871_application`. The sparse reduced
factorization therefore fixes the blanket dispatch bypass, while removing the
full derivative assembly remains a separate compiler task. Logs and symbol
maps are retained in `guarded-tearing-final-*` and
`guarded-tearing-perf-leaves-1.log`. RollingWheel is still slower than the pinned
OMC measurements; this change makes no OMC speed or end-to-end performance claim.

## Why affine projection previously bypassed tearing

Commit `82630a4d` placed affine projection before tearing to repair stale
switching coordinates: a residual below tolerance could still leave a voltage
on the wrong side of zero. The affine path computes `A*x = -F(0)` and refines
against original residuals. Later commits `c4789fce` and `3a7c0b12` preserve
small coordinates beside large offsets. These guarantees do not inherently
require the full block, but the existing reduced Newton path uses finite
differences and different acceptance rules; reordering dispatch alone does
not establish equivalent behavior.

The current RollingWheel block has 24 unknowns, five tears, and 19 exact
back-substitution steps. A private checked-IR replay restored all 916 solver
coordinates through the trace's exact `LoadY` output projections at all 501
recorded times. Every substitution sweep completed, both from recorded tears
and from zero tears. At recorded tears, the maximum absolute recovered-value
change was `4.55e-13` and the maximum reduced residual was `2.16e-13`.
This rejects an unusable isolator at those sampled points as the reason for
the bypass; it does not prove usable pivots at every integrator stage or
validate a reduced solver. The evidence is
`torn-sweep-trajectory-result-1.json`, with its helper and input mapping retained
under `.git/multibody-campaign/rolling-wheel/`.

OMC's generated linear system 731 has six unknowns and an analytical Jacobian.
The implementation target identified by this inspection was an AD-derived reduced affine system from
the issued substitution schedule, preserving original-equation refinement and
recovered-coordinate accuracy. An unusable reduced pivot must retain the
original implicit solve. The inspection itself made no dispatch change or new
timing claim; the latest complete MSL result remains the run below.

## Invalidate native constant facts on register writes

A checked Solve program exposed a native indexing counterexample: it sets
register 0 to index 1, overwrites that register from parameter input 2, then
indexes `{10,20}`. Cranelift's constant-index shortcut retained the old fact
and selected 10; the reference evaluator correctly returned 20. The actual
red test records that exact native/reference mismatch in
`register-constants-red-1.log`. The first divergence is native constant
tracking, downstream of a valid register-flow certificate.

Constant facts now belong to register versions. The emitter computes a new
fact from an operation's input versions, emits the complete operation, removes
old facts throughout its destination range, and installs the new fact if
proven. Tensor invalidation visits existing facts without expanding tensor
extents. The same rule covers dynamic writes and in-place arithmetic.
SPEC_0036 and SPEC_0043 §6a record the lifetime; MLS §10.5 supplies the indexed
array semantics. Tests cover scalar overwrite, both destinations of a tensor
write, and in-place addition. All 100 Cranelift tests and affected Clippy pass.

RollingWheel's complete IR and trace are byte-identical to the preceding
projection-reuse build. Its one diagnostic run took 73.695 ms; that single
sample is not a new performance claim (`register-constants-profile-evidence-1.json`).
The origin check retains two high comparisons, the existing BevelGear1D
reviewed exclusion, no missing traces, and 229 high initialization channels.
The fixed canary retains nine comparisons, all high, no skipped or missing
traces, and 175 high initialization channels. Every phase and band is unchanged
(`register-constants-origin-delta.json`, `register-constants-canary-delta.json`).

The complete sweep at `0c22de12` passes in
`target/msl/multibody-register-constants-full-11`: 159/566 strict-high (28.09%),
159 compared, 19 reviewed exclusions, no missing or non-identifiable traces,
no deviation channels, and all 20,964 initialization channels high. All phase
and band results match the previous projection-reuse sweep, leaving MultiBody
at 22/42 high (`register-constants-full-delta.json`).

A subsequent allocation experiment reused the solved Newton vector while
unscaling it. All 75 projection tests and Clippy passed, but three alternating
ordinary worker pairs measured 73.900 ms versus 74.261 ms median with overlapping
samples and identical complete traces. The 0.49% difference did not establish
a clear speed improvement, so the experiment was removed and the ordinary
worker rebuilt (`owned-delta-timing-summary-1.json`).

The exact worker dependency chain enables faer's Rayon feature, but replay of
the issued 24-by-24, 96-entry pattern selects simplicial LU. That implementation's
numeric factorization and triangular solves run serially; forcing sequential
configuration cannot remove threaded work from this block
(`faer-issued-parallelism-evidence-1.json`). The larger unresolved issue remains
the full coupled system and its repeated evaluations, compared with OMC's
six-unknown generated linear system.

## Reuse primal work across projection-Jacobian colors

The complete projection application now issues seed-invariance facts using
the existing register-source checker and whole-program repeatability proof.
Every write replaces its destination version's evidence. Cranelift retains
certified results as native SSA values after their first ordered execution
and reuses them across subsequent colors in that call. Tensor operations stay
complete; mixed seed loads remain live. Source replacement rederives the facts,
each call uses fresh coordinates, and native failure prevents matrix publication.
SPEC_0036 and SPEC_0043 §6a define this boundary.

The work-count regression first failed because a seed-independent sine ran
twice for two colors. It now runs once per call and recomputes for changed
Y/P/time. Controls cover overwritten registers, mixed tensor tangent lanes,
owner identity, output placements, table validation, and a singular typed
tensor solve followed by successful calls at fresh parameters. All 318 Solve
IR tests and 96 initial Cranelift tests pass, as does the added native-failure
test (415 distinct tests total). Affected all-target/all-feature Clippy passes.

For RollingWheel, the issued proof certifies 378 of the hottest program's
536 operations, including 26 matrix products, 12 cross products, and four pure
calls. This is stronger evidence than the earlier output-dependency census;
unproven operations still execute in every selected color. Three alternating
ordinary `msl-fast` worker pairs measure 75.426 ms median simulation time versus
81.620 ms for the preserved control, a 7.59% reduction. All Flat, DAE, structural
DAE, Solve, and complete trace artifacts are byte-identical across the six runs.
Preparation increases from 9.899 s to 10.945 s median. These unpinned samples
show a simulation improvement, not an end-to-end speedup or a win over OMC.
Receipts are `projection-reuse-alternating-evidence-1.json`,
`projection-reuse-timing-summary-1.json`, and `projection-reuse-issued-counts-1.json`;
the exact workers and native perf maps are retained with the profiles.

`multibody-projection-reuse-origin` retains two high comparisons, one existing
BevelGear1D reviewed exclusion, no missing traces, and 229 high initialization
channels. The fixed canary `multibody-projection-reuse-canary` retains nine
comparisons, all high, no skips or missing traces, and 175 high initialization
channels. Every phase and band is unchanged from the corresponding derivative
tensor runs (`projection-reuse-origin-delta.json` and
`projection-reuse-canary-delta.json`).

The full sweep at `5d1a8f06fd7658892f5aa4e4573c103b73885337` passes in
`target/msl/multibody-projection-reuse-full-11`: 159/566 strict-high (28.09%),
all 159 compared models high, 19 reviewed exclusions, no missing or
non-identifiable traces, no deviations, and all 20,964 initialization channels
high. MultiBody remains 22/42 high. Every phase and band is unchanged from
`multibody-derivative-tensor-full-11` (`projection-reuse-full-delta.json`).
Combined `verify quick`/`verify full` and the older coverage ratchet repair
remain outstanding; this full MSL result alone does not establish PR readiness.

## Share derivative tensor definitions across component projections

The reduced case `der(x)=A*x; z=sum(der(x))` exposed duplicate tensor
computation in Solve lowering. Both derivative components used the same matched
definition, but their scalar-view domain points created different expression
contexts. The regression initially emitted two complete matrix products. The
definition's construction-issued binder metadata now distinguishes a real
binding dependency from an output projection, reducing that case to one product.
A binder-dependent control retains separate products and correct values for
both tested parameter/state tuples. SPEC_0032 §2 records the boundary.

All 141 Solve tests and 605 core tests pass, together with affected Clippy and
formatting checks. OMC agrees exactly with all four initial-equation values
used by the reduced controls (`derivative-tensor-sharing-omc-1/receipt.json`);
this is an equation check, not a trajectory comparison. The focused originating
run `multibody-derivative-tensor-origin` retains two high comparisons, one
existing reviewed exclusion, no missing traces, and 229 high initialization
channels. The fixed 20-model canary `multibody-derivative-tensor-canary` retains
all previous phase and band results: nine compared models, all high, no skips
or missing traces, and 175 high initialization channels. Receipts are
`derivative-tensor-origin-delta.json` and `derivative-tensor-canary-delta.json`.

The complete 566-model sweep at `e880636bf2feef099de096d4d998b6190cf30632`
passes in `target/msl/multibody-derivative-tensor-full-11`: 159 strict-high
models (28.09%), 159 compared, 19 reviewed exclusions, no missing traces or
deviations, and 20,964 high initialization channels. MultiBody remains 22/42
strict-high. Every phase and comparison band matches the previous passing
`multibody-invariant-binding-full-11` sweep (`derivative-tensor-full-delta.json`).

This fix does not change RollingWheel's generated kernel. Its Flat, DAE,
structural DAE, Solve, and complete trace remain byte-identical to the control
(`derivative-tensor-sharing-profile-evidence-1.json`). The fresh single run is
86.469 ms versus 79.726 ms for the control; unchanged executable IR and single
samples do not establish a speed effect. Checked replay identifies the hot
Jacobian program as Body's `a_0=der(v_0)`: its primal has 392 operations and its
directional program 536, executed in six of the block's nine colors. The
remaining expansion must be traced through structural differentiation.

A diagnostic using the canonical seed/effect dependency authority finds 435
of that program's 535 result-producing operations independent of seeds and
free of recorded opaque effects, including 31 of 38 matrix products and 16 of
20 cross products. The program executes for six colors at one numerical
coordinate (`derivative-tensor-projection-seed-independence-1.json`). Reusing
first-color calculations is therefore a concrete next investigation. These
output-dependency counts are not an execution-reuse certificate: any retained
implementation must also prove operation/version ownership, failure behavior,
scratch lifetime, and unchanged coordinates before omitting evaluations.

Three temporary cache experiments produced no retained optimization: bypassing
all pure-call result caches, bypassing small arithmetic bodies, and inlining
small cache copies. Their receipts explicitly retain the single-sample and
build-configuration limitations; they do not establish an optimal cache policy.
All probes were removed and the ordinary worker rebuilt before validation.

## Structural functions retained in symbolic bindings

The full sweep at `617e315a2f3986dceddb1d3096b4ddc4738e9d29` exposed a
regression missed by the fixed canary: six previously high electrical models
and five previously excluded simulation completions fail Flatten. The run
reports 153/566 strict-high, all 153 compared models high, 14 reviewed
exclusions, and zero missing, nonidentifiable, or deviating comparisons; it
fails the quality gate (`zero-parent-full-delta.json`). Thirty phase outcomes
change, mostly from the same missing structural function. GearConstraint also
changes from structural refusal to a Solve timeout; it remains unsupported.

The electrical failures share `MultiDelta`/`MultiStar`'s declaration
`mSystems=numberOfSymmetricBaseSystems(m)`. Function precollection scans only
the evaluated instance binding, now a literal, while Flatten correctly retains
the symbolic source call. The integer evaluator therefore lacks the function
needed to resolve dimensions and connection-loop bounds. This is a missing
dependency in Flatten preparation, not permission to freeze mutable Real
parameter bindings again (SPEC_0007/0040; MLS §4.4.4, §8.3.3, §12.4).

Precollection now consumes the same retained binding source as variable
flattening, preserving the existing transitive callee discovery. A reduced
imported-function regression with two differently modified instances fails
before the change and passes afterward, retaining symbolic calls while
producing three and five array coordinates and eight equations
(`structural-binding-functions-{red,focused}-1.log`). Pinned OMC produces the
same eight exact observable values in all six output rows
(`structural-binding-functions-omc-1/receipt.json`). All 631 flattening tests,
602 compiler-core tests, and affected Clippy pass
(`structural-binding-functions-broad-build-1.log`).

The eleven-model originating run still fails after that preparation repair:
all eleven pass Flatten, but five now reject nonliteral array extents during
DAE construction and six reject recursive functions in the executable
pure-call graph. It compares zero traces and has parity unmeasured
(`structural-binding-functions-origin-1.log`). Translation-time evaluations
must remain available to structural consumers without freezing changeable
parent bindings or requiring compile-time recursion to execute at runtime.
Neither this originating run nor the earlier failed full sweep is passing
evidence.

A second reduced case reproduces the next boundary exactly: a recursive pure
Integer function computes two instances' fixed array widths, three and five;
`fill(1.0, width)` then fails DAE construction with a nonliteral extent
(`invariant-binding-recursion-red-1.log`). Under SPEC_0007's FLAT-C01 contract,
Flat finalization now specializes scalar bindings only after following exact
instance/declaration references through every fixed, non-changeable parent.
The shared constant interpreter owns function evaluation, including recursion,
purity, assertions, and evaluation limits. It receives no unproven global
parameter values. Failed or unsettled evaluations retain their bindings;
array-valued bindings remain symbolic. Existing function pruning removes the
now-unreachable compile-time helper.

The recursive fixture now compiles and simulates both arrays and their
integrals. Four focused tests pass, including a transitive final-alias chain
whose legal parent override changes its output, and refusal to fold impure or
`fixed=false` dependencies (`invariant-binding-controls-1.log`). Pinned OMC
produces the same literal widths and all ten analytic observable values in six
rows, with maximum absolute error 1.12e-16
(`invariant-binding-recursion-omc-1/receipt.json`). All 631 flattening tests,
605 compiler-core tests, and affected Clippy pass
(`invariant-binding-broad-build-1.log`).

The eleven-model originating run now passes: all eleven simulate; six compare
high, including all 2,205 initialization channels, and five retain their
previous reviewed exclusions. There are no missing or deviating comparisons.
All eleven bands match their entries in the last passing full sweep at
`59cee76f` (`invariant-binding-origin-delta.json`). The prior failed originating
run had no measured bands; the receipt distinguishes its phase outcomes from
that complete baseline's band evidence. The fixed 20-model canary also passes:
all phases and bands remain unchanged, nine models compare high with 175 high
initialization channels, and none are skipped, missing, or deviating
(`invariant-binding-canary-delta.json`).

The complete `multibody-invariant-binding-full-11` sweep at
`cb9920bcd84b57287f545a0110364d2af242ba0f` passes: 159/566 strict-high (28.09%),
all 159 compared models high, 19 reviewed exclusions, zero missing,
nonidentifiable, or deviating comparisons, and all 20,964 initialization
channels high. MultiBody remains 22/42 high. Every simulation band matches
the last passing full sweep at `59cee76f`; the only phase-field difference is
an error-code change from ED020 to ED019 for the already unsupported
PolyphaseRectifier (`invariant-binding-full-delta.json`). The comparison to the
failed `617e315a` sweep separately records restoration of its regressions
(`invariant-binding-full-regression-repair-delta.json`). This restores cohort
coverage; it establishes no RollingWheel speed improvement.

## RollingWheel kernel measurements after coverage recovery

The ordinary worker at `cb9920bc` runs RollingWheel in 79.894 ms under the
development benchmark profile. Its Flat, DAE, structural DAE, Solve, and full
trace are byte-identical to the previous control
(`invariant-binding-profile-evidence-1.json`). The 74-sample `perf` capture
still includes sparse factorization, triangular solves, and Jacobian execution;
it is a hotspot diagnostic, not a precise cost breakdown.

A controlled kernel comparison does not support replacing the current sparse
solve with dense LU. The initial experiment used the 74-entry numerical union
of three captured matrices. Checked replay of the saved Solve model reconstructs
the actual 96-entry projection pattern; repeating with that exact pattern gives
median costs of 3.195 microseconds for the current sparse path and 3.420 for
dense LU, about 7.1% slower. Each median covers seven alternating rounds of
3,000 changing-matrix solves after a warmup round
(`dense-crossover-issued-measurement-1.json`,
`dense-crossover-pattern-audit-1.json`). This measures those kernels, not a
whole-simulation speed ratio. The selection policy is unchanged and the
temporary measurement code has been removed.

The previous elimination experiment did handle its zero pivot by retaining it
in an eight-variable simultaneous system; it did not merely fall back to the
full solve. Mapping the same captured matrices to the current five-tear plan
finds no zero causal pivots or nonzero future causal dependencies at any of the
three captured coordinates (`affine-elimination-pivot-recheck-1.json`). This
does not prove nonzero pivots throughout a trajectory or establish a faster
implementation. The production affine executor still solves all 24 coordinates.

## Invariant-zero tensor incidence and declaration binding repair

The RollingWheel investigation reached a second producer defect: scalar
dependency projection reports off-diagonal inertia dependencies even when the
selected coefficient is a fixed, non-tunable literal zero. Under SPEC_0032 §2
and MLS §10.6.4, the projection now proves such zeros through the original
literal, parameter, array, concatenation, and signed-expression owners. It
omits only the multiplied direct Real coordinate's incidence; the canonical
tensor expression and numerical evaluation remain intact. Tunable parents,
initialization unknowns, and arbitrary expressions with domain obligations do
not qualify. The reduced matrix dependency test fails before the change while
these negative controls already pass (`zero-coefficient-red-1.log`). All 46
evaluation, 139 Solve, and 170 structural tests pass, with affected Clippy.

The first ordinary RollingWheel diagnostic changes the normalized plan from
seven tears to five and replaces the zero causal pivot with diagonal inertia
coefficients. Flat, DAE, structural DAE, and the complete trace are byte-identical
to the final-modifier control. Its 78.924 ms runtime is not a speedup: the
affine executor still solves the full 24-coordinate system. The receipt is
`zero-coefficient-profile-evidence-1.json`.

A runtime parameter-override control then exposed an existing correctness
counterexample. In `ParentCoefficientProbe`, `final parameter Real
offDiagonal=parent` was frozen to zero during instantiation because `parent`
defaults to zero. With `I=[2,offDiagonal;offDiagonal,2]` and `I*x={1,2}`, the
legal override `parent=0.5` must produce `x={1/3.75,3.5/3.75}`. Rumoca instead
kept `{0.5,1}`. The archived ordinary worker also emits the wrong literal Flat
binding, ruling out the new incidence proof or the numerical solver as the
first divergent owner (`zero-coefficient-baseline-flat-1/`).

Instantiation now retains the original declaration binding when evaluating a
different value for structural queries. Flattening consumes that retained
symbolic source using the existing declaration scope. This preserves the
binding equation and changeable-parent dependency under MLS §4.4.4 and §7.2.6.
The direct instantiation regression fails before this repair
(`zero-parent-phase-red-2.log`); all 224 instantiation, 631 flattening, and 601
compiler-core tests now pass, including the actual runtime override and Flat
binding regressions. Affected-package Clippy passes
(`zero-parent-focused-1.log`, `zero-parent-broad-build-1.log`). Pinned OMC agrees
with all four reduced default/override cases to within 5.6e-17
(`zero-coefficient-omc-1/receipt.json`).

The combined ordinary RollingWheel diagnostic retains the same complete trace
and five-tear plan at 78.591 ms. Its sole Flat change preserves the declaration
binding `world.groundLength_v=world.groundLength_u` instead of literal 4
(`zero-parent-flat-delta-1.json`, `zero-parent-profile-evidence-1.json`). Tier 1
originating comparison retains RollingWheel and OvervoltageProtection high:
two compared models, 229 high initialization channels, one reviewed BevelGear
exclusion, and no missing or deviating comparisons. The fixed twenty-model
canary retains every phase and band: nine compared models, all nine high,
175 high initialization channels, and zero skipped, missing, nonidentifiable,
or deviating comparisons. Receipts are `zero-parent-{origin,canary}-delta.json`.

## Affine elimination experiment and final-modifier investigation

The complete `multibody-tearing-choice-full-11` sweep at
`2892128a7e9337989b455fe2911e7c6f52bffe51` passes with every phase and band
unchanged: 159/566 strict-high, 159 compared models, 19 reviewed exclusions,
zero missing/nonidentifiable/deviating comparisons, and 20,964 high initialization
channels. MultiBody remains 22/42 high. The receipt is
`rolling-wheel/tearing-choice-full-delta.json`.

Three captured RollingWheel matrices at t=0, 0.5005823461, and 2.0053747285
expose one zero causal pivot in the seven-tear plan: residual 782, solver
coordinate 43 (`body.z_a[2]`). Its canonical tensor program is the first
component of Body.mo line 261,
`frame_a.t = I*z_a + cross(w_a, I*w_a) + cross(r_CM, frame_a.f)`. The coefficient
is `I_21`, set to zero with a final modifier in RollingWheel.mo. The other causal
diagonals are plus or minus one, with no upper causal coefficients. The capture
preserves every IR and complete trace byte; all probes were removed
(`affine-matrices-1.json`, `affine-matrix-capture-receipt-1.json`).

A checked Schur-elimination experiment retained unsuitable pivots in the
simultaneous system, recovering all original coordinates under unchanged
conditioning, residual checks, and mandatory refinement. Two 24-coordinate
cycle regressions failed before it; the all-unsuitable-pivots control already
passed. All three subsequently passed, along with four construction controls
and all 1,006 affected library tests. Clippy passed after removing two redundant
test-only copies. Logs are `affine-elimination-red-1.log`,
`affine-elimination-libraries-1.log`, and `affine-elimination-clippy-build-1.log`.

Its ordinary run took 82.948 ms against an immediate archived control at
77.444 ms, 7.11% slower (`affine-elimination-profile-delta-1.json`). Perf identified
factor construction as a major cost. Skipping zero-coefficient products and
retaining the existing small-matrix policy passed all 78 projection tests and
Clippy, but still took 80.323 ms. The originating comparison retained both
RollingWheel and OvervoltageProtection high: two compared models, 229 high
initialization channels, one reviewed BevelGear exclusion, and no missing or
deviating comparisons (`affine-elimination-origin-delta.json`). Compiler IR bytes
were unchanged; trace bits changed. Neither candidate demonstrated a speedup.
Both implementations, their tests, and proposed contract extensions were removed
and preserved in `affine-elimination-first-candidate-1.patch`,
`affine-elimination-rejected-2.patch`, and the associated new-file archives.
No canary or cohort claim is made for this rejected runtime experiment.

The earlier divergent compiler layer is now under investigation. OMC retains
`body.I_21` as a non-changeable calculated parameter, but Rumoca's Flat flags
contain only the declaration-final `body.I`, omitting modification-final `I_21`,
`I_31`, `I_32`, and `r_CM`. Instantiation records declaration finality while
ignoring the accepted modifier's final prefix. Three reduced tests fail for a
literal modifier, an each-final component array, and a symbolic parent binding;
the declaration-final and ordinary-sibling controls behave as expected
(`final-modifier-red-1.log`). Final modifiers must retain their source bindings,
including dependencies on changeable parent parameters, under
[MLS §7.2.6](https://specification.modelica.org/maint/3.6/inheritance-modification-and-redeclaration.html#final-element-modification-prevention).
The candidate now carries effective finality through instance construction and
nested scopes. All 223 instantiation and 598 compiler-core tests pass, as does affected-package
Clippy. Pinned OMC agrees on the exact reduced model and on a tensor regression
with source modifications and a legal runtime parent override to gain=4; all
three trajectories match their analytic solutions within 2.3e-16 in OMC.
The artifacts are `final-modifier-omc-1/` and `final-tensor-omc-1/receipt.json`;
logs are `final-modifier-focused-1.log`, `final-modifier-core-2.log`, and
`final-modifier-core-build-1.log`.

The ordinary RollingWheel diagnostic now records the effective final flags in
Flat and non-tunable attributes in DAE, but Solve and the complete trace remain
byte-identical. Its 78.849 ms runtime is not a speedup
(`final-modifier-profile-evidence-1.json`). Tier 1 originating comparison retains
two high models and 229 high initialization channels, with one reviewed
BevelGear exclusion and zero missing/nonidentifiable/deviating comparisons.
The fixed twenty-model canary retains every phase and band: nine high compared
models, 175 high initialization channels, and no skipped, missing,
nonidentifiable, or deviating comparisons. Receipts are
`final-modifier-{origin,canary}-delta.json`. The full `multibody-final-modifier-full-11` sweep at
`59cee76f19ec6fc0f97121e45b506d7fa6086d8b` then retained all phases and bands:
159/566 strict-high, all 159 compared models high, 19 reviewed exclusions,
zero missing/nonidentifiable/deviating comparisons, and 20,964 high
initialization channels. MultiBody remains 22/42 high. Its receipt is
`final-modifier-full-delta.json`; tracked source was clean at launch.

## Rejected native colored-application experiment

A native batch consumed each issued projection color and complete source program
in order, sharing immutable Y/P loads and scattering the checked output
placements in one native call. Eleven focused numerical/ownership/failure tests
and Clippy passed. The ordinary RollingWheel run took 78.378 ms versus an
immediate archived control at 78.045 ms. Every compiler artifact and complete
trace remained byte-identical (`projection-batch-profile-delta-1.json`). The
hot batch occupied 126,064 native bytes and remained a leading perf leaf;
removing Rust dispatch did not demonstrate a speedup. The implementation was
removed and archived in `projection-batch-rejected-1.patch` and
`projection-batch-rejected-source-1.rs`. No MSL gate or parity credit is claimed
for this experiment.

Its first standalone package test exposed an existing fixture wiring defect:
Cranelift tests use `StructuralPattern::from_row_dependencies`, but only another
package's development dependency enabled `pattern-fixtures`. The adapter now
explicitly requests that feature in its own development dependencies. Production
dependencies and the checked pattern authority remain unchanged. The original
failure is `projection-batch-focused-1.log`. All 94 standalone Cranelift
library tests and Clippy pass after the fixture fix, and the ordinary worker
has been rebuilt without the rejected batch (`cranelift-fixture-final-1.log`).

## Current compiler work: compare complete tearing candidates

RollingWheel's 24-coordinate force/torque/acceleration block exposes a structural
cost defect: prioritizing immediate causal unlocks chooses twelve tears, then
exact-assignment normalization promotes one more. A degree-first candidate
needs seven. The source incidence graph is retained in the regression test and
`rolling-wheel/tearing-graph-heuristic-diagnostic-1.json`. The probes preserve
all compiler artifacts and trace bytes. Normalization is not the main cause;
transitive-unlock lookahead also produces twelve tears. OMC's generated linear
system 731 has six unknowns, though its coordinates differ from Rumoca's.

Under SPEC_0007/0040 STRUCT-T06 and MLS §8.3.1, the structural producer now
compares two complete deterministic candidates and selects fewer tears,
retaining the original on ties. Both preserve the exact causal restrictions
and complete equation/unknown partitions. No source equation, tensor owner,
runtime tolerance, or model-specific rule changes. The graph regression fails
before the change (`tearing-choice-red-1.log`); partition, restricted-candidate,
and input-order controls pass with the structural suite and Clippy
(`tearing-choice-focused-1.log`, `tearing-choice-focused-build-2.log`).

The actual normalized Solve block changes from thirteen tears to seven while
Flat, DAE, structural DAE, and the complete trace remain byte-identical
(`tearing-choice-profile-evidence-1.json`). The ordinary msl-fast diagnostic
takes 78.080 ms. This is a compiler plan improvement, not a demonstrated runtime
speedup: the affine executor still solves the full 24-coordinate matrix.

Tier 1 `multibody-tearing-choice-origin` retains RollingWheel and
OvervoltageProtection high: two compared models, 229 high initialization
channels, one reviewed BevelGear exclusion, and zero missing or deviating
comparisons. `multibody-tearing-choice-canary` retains every phase and band of
the fixed twenty-model list: nine compared models, 175 high initialization
channels, and zero skipped, missing, nonidentifiable, or deviating comparisons.
The receipts are `tearing-choice-{origin,canary}-delta.json`.

A third candidate prioritizing branded derivative coordinates passed 171 tests,
Clippy, the originating list, and the canary, but did not reduce this actual
normalized block further: derivative aliases are algebraic coordinates at this
stage. Its 77.516 ms diagnostic still has seven tears and an identical complete
trace. The speculative extra candidate was removed; its patch, tests, and
measurements remain in `tearing-derivative-preference-experiment-1.patch`,
`tearing-derivative-choice-focused-build-2.log`, and
`tearing-derivative-choice-profile-evidence-1.json`. No five-coordinate runtime
claim follows from its isolated graph result. The final two-candidate source
is restored exactly; its completed cohort evidence is recorded above.

## Current cohort and RollingWheel measurement

The complete `multibody-bdf-step-budget-full-11` sweep at commit
`6e3b5c2d3a415db5d1191c7208e9c2968697d2ad` passes: 159/566 strict-high
models (28.09%), including 22/42 MultiBody. All 159 compared models and 20,964
initialization channels are high. There are 19 reviewed exclusions and zero
missing, typed nonidentifiable, near, or deviating comparisons. Every previously
high model is retained; IMC_YD additionally becomes high after the step recovery
budget correction. BevelGear1D remains a reviewed exclusion, never a high result.
The tracked source was clean; the foreign untracked communication file accounts
for the harness dirty flag. The receipt is `bdf-step-budget-full-delta.json`.

RollingWheel remains slower than OMC. At that same commit, the ordinary release
worker takes 71.411 ms versus an immediate msl-fast control at 77.773 ms. This
8.18% difference is a paired measurement, not repeated medians; release already
uses ThinLTO and no build profile was changed. All Flat, DAE, structural DAE,
Solve, and complete trace bytes are identical (`release-lto-profile-delta-1.json`).

OMC's reported 16.582 ms simulation median excludes its solver timer. Inspection
of `solver_main.c` at pinned OMC revision
`a96aa1a682c463b0fd2d285b486c09a8b7fe496d` establishes the subtraction used by
`finishSimulation`. Per-run sums over the archived 40 runs give 17.722 ms for
simulation plus solver, 18.924 ms for total minus output, and 18.236 ms after
also subtracting initialization and preinitialization. Total including CSV
output is 50.655 ms. Those scopes differ from Rumoca's prepared run with
in-memory output; none establishes a Rumoca speed advantage. The receipt is
`omc-timer-accounting-1.json`.

A removed 40-run timer probe has an 80.721 ms median, including instrumentation
overhead. Numerical advance takes 52.475 ms median. Inclusive accumulated
timings identify the 24-coordinate affine block at 38.306 ms per run, including
14.474 ms for Jacobian evaluation and 10.422 ms for linear solves. Timings include
small build/initialization contributions and nested categories must not be
summed. All repeated traces and compiler artifacts match the ordinary release
byte-for-byte. Exact executable/JIT maps, perf leaf samples, source restoration
hashes, and the measurement limits are retained in
`release-cost-repeat-profile-1` and `release-cost-repeat-summary-1.json`.

An experiment specializing AD further to each color passed 1,043 focused tests
and affected-package Clippy. Its complete compiler artifacts and trace were
byte-identical, but an immediate ordinary-worker pair took 82.120 ms versus
77.854 ms for the archived baseline (5.48% slower). Preparation took 9.677 s
versus 9.743 s. No performance improvement was demonstrated, so the experiment
and its proposed contract extension were removed. The exact patch, both worker
executables, tests, and receipts remain in `color-seed-domain-experiment-1.patch`,
`color-seed-domain-focused-{1,2,3}.log`, and
`color-seed-domain-profile-delta-1.json`; the second focused attempt only exposed
a test constructor unavailable under that feature set. This is rejected
performance evidence, with no additional MSL coverage claim.

## Latest focused work: distinguish step recovery from cumulative statistics

The full `multibody-bdf-convergence-history-full-11` comparison at clean commit
`6f203e1cc5e809229ac25a554c742beb1766e7e7` failed its quality gate. Of 566 targets,
158 were compared: 157 strict-high, one near (`BevelGear1D`, including one
deviating channel), 18 reviewed exclusions, and zero missing or typed
nonidentifiable traces. All 20,900 initialization channels were high.
`OvervoltageProtection` regressed from strict-high to nonlinear-solver
exhaustion; `BevelGear1D` previously refused a projection sensitivity and now
finishes with an unacceptable trajectory. Neither is a performance success.
GyroscopicEffects completes high in this full run; its earlier focused timeout
remains recorded. `Inverse_sh_TX` changes from Flatten EF015 to ToDae ED019
without a compiler source change. The complete delta is retained in
`rolling-wheel/bdf-convergence-history-full-delta.json`.

The electrical failure is at the numerical dependency, not a changed equation:
the BDF failure counter accumulates 51 failures across 50 distinct accepted-step
indices, with at most two failures in any one step. It aborts after 804 accepted
steps at t=0.3748128288461084. The step limit is incorrectly checked against
lifetime statistics. This remains present in upstream Diffsol commit
`7036380f908dbd93baa4253d2e0a34aa115cbbb5`; the frozen source and diagnostic
receipts are `diffsol-upstream-bdf.rs` and `bdf-failure-count-probe-summary.json`.

The direct constant-derivative regression with injected recoverable failures
fails before the correction, while its persistent-failure negative control
already passes (`bdf-failure-budget-red-2.log`; `red-1` only exposed a missing
test type annotation). BDF now checks the unchanged configured recovery limit
against failures in its current step, preserving lifetime statistics and
rejection of an unresolved step. All ten dependency tests and all-target,
all-feature Clippy pass (`bdf-failure-budget-focused-1.log`). The ordinary
`multibody-bdf-step-budget-origin` run restores OvervoltageProtection: all 45
trajectory and initialization channels are high. RollingWheel retains all 184
high channels and a byte-identical trace. These are two comparisons, one
reviewed exclusion, and zero missing or deviating comparisons. The fixed
`multibody-bdf-step-budget-canary` preserves every preceding phase and band:
nine comparisons, 175 high initialization channels, and zero skipped, missing,
nonidentifiable, or deviating comparisons (`bdf-step-budget-canary-delta.json`).
The tracked-exclusion validation test passes; the subsequent complete cohort
result is recorded above.

BevelGear's reference was examined independently of Rumoca: the unchanged,
xtask-generated OMC executable produces `revolute1.phi(1)` of 72.3016880708 rad
at its default `1e-6` tolerance, 48.8580301756 at `1e-9`, and 48.1459606592 at
`1e-12`. Each diagnostic uses a separate copy of its initialization XML with
only `DefaultExperiment.tolerance` changed, supplied with `-f`; the tracked
reference and Rumoca tolerances remain unchanged. At `1e-12`, perturbing only
the fixed initial `revolute1.phi` by `1e-8` rad changes the endpoint to
41.0327222753 rad. The first diagnostic incorrectly used
`-override=tolerance=...`; OMC warned that this variable did not exist, so those
runs provide **no tolerance-refinement evidence**. Both attempts are retained
as `rolling-wheel/bevel-omc-convergence-{1,2}/receipt.json` with the exact
commands and executable, XML, and trace digests.
The OMC executable SHA-256 is
`444907bed49f3347e26913253c351bbd73c4e086e72a3c481fbcfdb4247325cf`;
the original XML is
`03427ae6499f2835cd528f80fa6ad0ddf8a92708f90aa2118fb74fc6daf7f139`.
The `1e-12` unperturbed trace used by the derivative comparison is
`a273bce69959d7dada8f34d653a91f26cd1a9c4cefc03e00c943c06247ff78ac`.

To reject a different-equations explanation, an isolated, removed adapter
probe calls the ordinary component derivative handle at all 502 output points
from the `1e-12` OMC trace. State order comes from the actual Solve layout;
the extra gear-output angle derivative is the connected `inertia2.w`.
All 5,020 derivatives agree: maximum absolute error is `6.8453e-8`, and maximum
`abs(error)/max(1,abs(reference))` is `1.4462e-11`. Every Flat, DAE, structural
DAE, and Solve artifact is byte-identical to the preceding BevelGear worker.
`bevel-point-probe-summary-1.json` retains the worst values and their times;
`bevel-omc-points-1.json` binds the exact reference CSV. The adapter is restored
to its archived source hash before ordinary validation.

The reviewed exclusion records this demonstrated default-reference limitation.
It does not claim a proved positive infinite-time Lyapunov exponent, invariant
refinement, or high trajectory parity. The strong initial-condition sensitivity
and OMC self-divergence make a single default OMC trajectory non-identifying;
the model remains outside the strict-high count, with invariant and sensitivity
obligations outstanding. No compiler, runtime, tolerance, or model-name branch
is added to obtain agreement.

## Prior focused work: consume BDF correction history at the corrected point

Diffsol's backtracking Newton path never populated the correction-norm history
used by its convergence-rate estimate. Three direct dependency regressions fail
against that path (`bdf-convergence-history-red-2.log`): it repeats residual
evaluation for affine solves, oversolves a rapidly contracting nonlinear root,
and fails to reject a rate that cannot meet the iteration budget. The earlier
`red-1` attempt only exposed test-import errors.

A history-only change is incorrect: a successful line-search trial has computed
the next Newton correction but has not applied it. The negative control in
`bdf-convergence-naive-history-red.log` returns 0.0098039 for an exact root of
zero when the requested nonlinear bound is 0.0002. The corrected iteration
consumes that pending correction and its norm together, applying the correction
before success. Armijo backtracking, tolerances, iteration budgets, and the
existing Jacobian/timestep estimate-reset policy remain unchanged. Frozen
upstream commit `7036380f908dbd93baa4253d2e0a34aa115cbbb5` retains the original
behavior; the dependency patch and provenance are recorded in
`vendor/diffsol/RUMOCA_PATCH.md`.

All eight direct dependency tests, 162 simulation tests, both sampled-integral
event-entry regressions, and affected-package all-target/all-feature Clippy
pass (`bdf-convergence-history-focused-{1,2}.log`). The ordinary worker
`1e90fe17cb1338e608a6de49b4a12fa635107ecf0818b4132b7017baa9ca6a6a` takes
79.153 ms against the immediate archived `4b4dc9c5…` recheck at 98.345 ms:
19.51% improvement for that pair. Compiler artifacts are identical; trajectory
bits change with the nonlinear iteration (`bdf-convergence-history-profile-delta-1.json`).
The separate 40-run profile has a 79.444 ms median (78.917–83.315 ms), with every
repeated trace bit-identical. RHS calls fall from 2,248 to 1,560; accepted steps
change from 1,054 to 1,051. The new run has 58 error-test failures, two nonlinear
failures, 109 linear setups, and 17 complete matrix evaluations. These remain
numerical statistics, not parity evidence. Diagnostic probes are removed and
their two source files restored exactly before ordinary validation.

The canary retains all phase/band rows: nine high comparisons and 175 high
initialization channels. All 15 electrical regression comparisons and 3,486
initialization channels remain high. Both scopes have zero skipped, missing,
nonidentifiable, or deviating comparisons. The five-model origin attempt has
three high comparisons, including RollingWheel, plus the existing GenerationOfFMUs
EL005 refusal and a GyroscopicEffects Solve-phase timeout at 12.561 seconds.
That timeout remains a failed normal-budget attempt. An isolated control with
the phase watchdog absent takes 7.246 seconds in Solve lowering versus 7.423
seconds for the archived worker and produces identical compiler artifacts
(`bdf-gyro-solve-control-delta.json`); it earns no normal-budget parity credit.
Receipts are `bdf-convergence-history-{canary,electrical,origin}-delta.json`.
The complete cohort comparison subsequently failed as recorded above.
RollingWheel is still slower than OMC; no victory or release-ready claim is made.

## Latest focused work: block-specific numerical AD seed domains

RollingWheel's hottest projection JVP reads 47 seed coordinates, but only six
belong to that block's unknowns. The other 41 coordinates are fixed during the
block solve. The compiler now issues a separate numerical AD kernel for the
block's exact seed domain, preserving ordered primal operations, whole-program
outputs, canonical primal/JVP owners, and the unrestricted state/parameter
derivatives. Partially active tensor loads split at domain boundaries; compact
register-run packing preserves the exact ordered register values.

All 1,041 focused IR, AD, native-execution, and solver tests and affected-package
all-target/all-feature Clippy pass (`block-seed-domain-focused-3.log`). Earlier
attempts retain a missing re-export and a nesting lint. Regressions check fixed
orientation matrix products, partial tensors of 4,096 elements, exact register
bits, fresh Y/P/time, and stale canonical primal rejection.

The ordinary worker `4b4dc9c53ac993e1bc3a3d73d6d18d468bc26672305960213b5ae2cb06562dc8`
takes 96.962 ms against the immediate archived `21e0c64e…` recheck at 112.555 ms:
13.85% improvement for that pair. Flat, DAE, structural DAE, serialized Solve,
and complete trace bytes are identical (`block-seed-domain-profile-delta-1.json`).
The separate 40-run diagnostic has a 98.874 ms median (98.592–104.560 ms), with
all complete traces bit-identical. The hottest selected program shrinks from
1,012 to 536 operations; its leaf samples fall from 561 in the earlier profile
to 212 in the new profile. These are sampled profiles, not instruction timings.
RollingWheel remains slower than OMC.

The first diagnostic attempt hit the 6 GiB worker limit because the temporary
artifact probe duplicated shared general programs. Its exit 70 is retained and
earns no validation credit. The bounded second probe dumps only specialized
sources. All probe code is removed before ordinary verification; the restored
worker source has SHA-256 `0138785a9c71c923eb46793cbb95c5b069323b40af0f4df3fa0861244a24a5ee`.

The fixed 20-model canary retains every phase and band: nine high comparisons,
175 high initialization channels, and zero skipped, missing, nonidentifiable,
or deviating comparisons. The five-model origin check retains four high
comparisons, 2,960 high initialization channels, and the existing
GenerationOfFMUs EL005 refusal. Receipts are
`block-seed-domain-{canary,origin}-delta.json`. The 15-model electrical regression
set also retains every phase and band: all 15 comparisons and 3,486 initialization
channels are high, with zero skipped, missing, nonidentifiable, or deviating
comparisons (`block-seed-domain-electrical-delta.json`). These are focused checks,
not a new cohort claim.

The subsequent complete `multibody-block-seed-domain-full-11` sweep at clean
commit `755c2169d66197da9d4853ff723a2017b9464658` passes with 158/566 strict-high
models, including 22/42 MultiBody. All 158 comparisons and 20,379 initialization
channels are high; the same 18 reviewed exclusions remain, with zero missing,
nonidentifiable, or deviating comparisons. Every comparison band is unchanged.
GearConstraint returns from the preceding standalone attempt's timeout to its
existing StructuralAnalysis EL005 refusal; it still produces no trace.
Receipt: `block-seed-domain-full-delta.json`.

## Latest focused work: prepare complete colored Jacobian applications

The candidate binds each projection's colors and output placements to its
immutable scalar-program owner. Scalar-program clones share their complete
checked storage; canonical wire data and checked replay are unchanged. Native
preparation retains code ownership and exclusive seed, output, and matrix
scratch. Each call uses fresh coordinates, enters the external-table context
once, preserves complete program order/status checks, and publishes only a
successful matrix. Admission preserves mixed reverse/forward evaluation and
the unrestricted initialization and parameter-sensitivity paths.

`rolling-wheel/prepared-projection-profile-delta-1.json` records 111.058 ms for
the candidate worker `f5c9fe7555cad3db316d2dca4061955091f9908401710e3ece3df4bfc0cc1ae7`
and 126.144 ms for the immediately rechecked archived worker `0669bf79…`.
That pair improves 11.96%; it is not a repeated-median estimate. Flat, DAE,
structural DAE, Solve, and complete trace bytes match. RollingWheel remains
slower than OMC; no speedup over OMC is claimed.

The 897 focused tests and all-target/all-feature Clippy pass in
`prepared-projection-focused-3.log`. Tests cover fresh coordinates, exact JVP
placement, distinct source owners, retained native-code lifetime, external-table
failure/recovery, mixed reverse admission, full parameter seed space, and atomic
state preservation on failure. Earlier check logs retain a visibility error,
a test dependency error, and a nesting lint; none counts as a passing run.

`multibody-prepared-projection-canary` preserves every phase and band: nine
high comparisons and 175 high initialization channels, with no skipped,
missing, nonidentifiable, or deviating comparison. The five-model
`multibody-prepared-projection-origin` likewise preserves four high comparisons
and 2,960 high initialization channels; the existing ThermalGenerationOfFMUs
EL005 refusal remains. Receipts are `prepared-projection-{canary,origin}-delta.json`.
The precursor passes the complete `verify quick` suite in
`verify-quick-prepared-projection-1.log`: lint, 243 architecture tests, 17 file-size
checks, 7,674 workspace tests, doctests, and all 28 pinned corpus models. Its
complete MSL sweep preserves 158/566 strict-high models (22/42 MultiBody), all
20,379 initialization channels high, 18 tracked exclusions, zero missing or
nonidentifiable traces, and zero deviations. Every phase and band matches the
retained-workspace cohort (`prepared-projection-quick-full-delta.json`).

Review then found a stale-source counterexample: replacing the canonical JVP
while retaining projection metadata silently used the old expressions. The
reduced negative control fails in `prepared-source-owner-red-1.log`. Scalar
views now retain the exact canonical owner when borrowing one scalar node;
runtime rejects stale metadata and declines optional preparation for materialized
views without that identity. The existing output-extent overflow check remains
mandatory. All 1,090 focused tests and affected-package all-target/all-feature
Clippy pass in `prepared-source-owner-focused-2.log`.

The final ordinary worker `21e0c64e50737cea054027ce6aee748fb02272e374f8cc13ab5087f603944885`
runs in 113.944 ms against an immediate 134.107 ms archived-baseline recheck
(15.03% for that pair). All five IR/trace artifacts remain byte-identical;
`prepared-source-owner-profile-delta-1.json` records the comparison. The final
`multibody-prepared-source-owner-canary` retains the same nine high comparisons,
175 high initialization channels, and all twenty phase/band rows, with zero
skipped, missing, nonidentifiable, or deviating comparisons.

The final `multibody-prepared-source-owner-full-11` sweep passes with 158/566
strict-high models (22/42 MultiBody), 158 comparisons, 18 tracked exclusions,
zero missing/nonidentifiable traces, and zero deviations. All 20,379 initialization
channels remain high. Every band matches the retained-workspace cohort; the
existing GearConstraint failure changes from StructuralAnalysis EL005 to an
attempt timeout, without producing a trace (`prepared-source-owner-full-delta.json`).
The combined `verify full` attempt preserves every MSL phase and band, including
GearConstraint's existing EL005 refusal
(`prepared-source-owner-combined-full-msl-delta.json`). Lint, architecture,
7,677 workspace tests, doctests, 28 pinned corpus models, examples, and binary
builds pass. The template report test exposed a stale version-5 expectation
against the canonical template schema version 6; the literal is corrected and
all six report regressions pass (`template-schema-version-6-2.log`).
Coverage increases from 77.27% to 78.34%, but the unchanged per-package
uncovered-function ratchet fails. The failed gate report is retained beside
`verify-full-prepared-source-owner-1.log`; no baseline is promoted. The attempt
was stopped with exit 130 during documentation work after those failures to
resume focused performance work. Remaining documentation/editor gates and a
complete green release sweep are outstanding.

## Verification checkpoint: remove asserted projection preconditions

`rolling-wheel/verify-quick-projection-milestone-1.log` preserves the failed
combined quick run: MSL and the 28-model corpus passed, while test-helper lint,
two added production assertions, and missing `cargo-nextest` blocked completion.
The fixed tensor-update branch now consumes static-only subscripts. Logical
output iteration borrows its checked source owner. Test helper refactoring
retains every case and tolerance; the worker artifact test uses a normal
success assertion. Neither architecture ceiling nor validation policy changes.

The focused checks pass: 316 Solve IR tests, all 243 architecture checks,
22 contact regressions, and all-target/all-feature Clippy for Solve IR, the
worker, and the compiler facade. Logs are `quick-blocker-focused-{1,2}.log`.
The first command stopped after architecture because its contact command named
a nonexistent feature; the corrected command ran those tests successfully.
The repository's `full` Nix shell supplies nextest for subsequent combined
verification. Combined quick/full are still pending, and this checkpoint makes
no new performance or cohort claim.

## Latest complete cohort: projection optimizations preserve all outcomes

`target/msl/multibody-retained-sparse-workspace-full-11`, at `e4817572` with a
clean tracked worktree, retains 158/566 strict-high models (27.92%), including
22/42 MultiBody models. All 158 compared models and 20,379 initialization
channels remain high. The 18 reviewed exclusions remain; there are zero missing,
nonidentifiable, or deviating comparisons. Every phase, simulation status, and
comparison band matches `multibody-native-inactive-tangents-full`. This complete
sweep covers grouped residual evaluation, streamed row scaling, and retained
sparse factorization storage. Receipt:
`rolling-wheel/retained-sparse-workspace-full-delta.json`.

The preceding `multibody-retained-sparse-workspace-full` attempt was interrupted
with exit 130 while the initial memory budget restricted it to one worker.
After the external memory-intensive job exited, the named `-full-11` run used
the requested eleven workers. The earlier incomplete log remains preserved and
earns no credit; no model was retried within the completed sweep.

Removed timing probes identify the remaining cost. `sim-step-probe-1` measures
1,054 accepted steps: 93.276 ms in numerical advances and 33.202 ms in the
following host work. The latter includes 0.668 ms of sampler validation,
2.216 ms of event scanning, and 29.959 ms of endpoint/output work. Lease and
initialization take 0.944 ms and plugin setup 0.465 ms. Its complete trace is
byte-identical to the retained-workspace reference. `sim-phase-probe-1` likewise
shows initialization/setup around 1.5 ms, with identical trace bytes. These
are attribution diagnostics, not additional coverage or comparative-speed
measurements. Source archives and summaries are
`rolling-wheel/sim-{phase,step}-probe-{original,source}.json` and
`rolling-wheel/sim-{phase,step}-probe-1-summary.json`; all probes are removed.

RollingWheel remains slower than OMC. Combined quick/full verification and
complete MultiBody coverage remain open.

## Latest focused work: retain sparse numeric factorization storage

On top of `c9e60096`, the sparse projection owner retains faer's numeric LU
storage, exact conditioned coefficient bits, and a scratch buffer sufficient
for both factorization and solving. The high-level API previously allocated
a new numeric factor and scratch on each refactorization, then separate RHS,
solution, and scratch buffers on each solve. The new owner uses the same faer
symbolic/numeric kernels and pivot policy, with one owned result vector.
Changed matrix patterns reconstruct the owner. Refactorization consumes the
usable-factor state before modifying numeric storage; rejected factors cannot
solve a RHS, and an identical rejected matrix is not refactored. Cloning owns
independent factor and scratch storage. SPEC_0043 §6a states the ownership rule.

`retained-sparse-workspace-focused-3.log` records all 486 solver library tests
and all-target/all-feature Clippy passing. Differential tests compare retained
and freshly allocated faer solves bit for bit under coefficient, scaling, RHS,
pivot, pattern, and dimension changes, plus singular failure/recovery and clone
independence. Scratch addresses stay unchanged across repeated refactorizations.
The first command had a test fixture integer-type error. The second caught an
incorrect test assumption that every infinite coefficient produces a nonfinite
solution; the final test compares faer's existing behavior and checks recovery,
without changing production acceptance or fallback policy.

The fixed `target/msl/multibody-retained-sparse-workspace-canary` retains nine
high models and 175 high initialization channels. The five-model
`target/msl/multibody-retained-sparse-workspace-origin` retains four high models
and 2,960 high initialization channels. Both have unchanged phase/status/band
outcomes, zero skipped/missing/nonidentifiable comparisons, and zero deviations.
Receipts: `rolling-wheel/retained-sparse-workspace-{canary,origin}-delta.json`.

Controlled RollingWheel Sim is 0.125927 seconds versus the immediate archived
baseline's 0.136581 seconds, a single-pair 7.80% reduction. Flat, DAE, structural
DAE, canonical Solve, and complete trace bytes are identical. Candidate worker
SHA-256: `0669bf7946f1d278887c3940424f223b7ec3a81b6a6bf01da5c2c89f803c0916`.
Receipt: `rolling-wheel/retained-sparse-workspace-profile-delta-1.json`.
The complete-cohort result above now covers this change. Combined quick/full
verification remains open.

## Previous focused work: stream structural row scales

On top of `fce39f4e`, sparse Jacobian row scaling consumes the pattern's ordered
borrowed row visitor instead of allocating every nonzero coordinate on each
projection. Arithmetic, finite-contribution selection, and fallback scales are
unchanged. The regression covers finite maxima and zero/nonfinite fallback rows
against the dense implementation. Formatting, all 70 projection tests, and
solver all-target/all-feature Clippy pass in `streamed-row-scales-focused-1.log`.

The fixed `target/msl/multibody-streamed-row-scales-canary` retains nine high
models and all 175 high initialization channels, with unchanged phase/status/
band outcomes and no skipped, missing, nonidentifiable, or deviating comparisons.
Receipt: `rolling-wheel/streamed-row-scales-canary-delta.json`.
Controlled RollingWheel Sim is 0.133409 seconds versus the immediate archived
baseline's 0.136935 seconds, a single-pair 2.58% reduction. All four compiler IR
artifacts and the complete trace are byte-identical. Candidate worker SHA-256:
`754b3278ea9b2f246385c01e3143ec9665d0d0f81c09c34e74780b2a6f535978`.
Receipt: `rolling-wheel/streamed-row-scales-profile-delta-1.json`.

The attempted `multibody-streamed-row-scales-full` sweep was interrupted with
exit 130 before completion. Its initial memory budget capped compilation and
simulation at one worker despite the requested eleven. Its retained log is
`rolling-wheel/streamed-row-scales-full-1.log`; it earns no cohort credit.
The latest complete cohort below still predates both recent projection changes.
RollingWheel remains slower than OMC; the next investigation is retained numeric
factorization storage and scratch allocation.

The post-commit `target/msl/electrical-grouped-residual-origin` run at `fce39f4e`
also retains all 15 high models and 3,486 high initialization channels, with
unchanged phase/status/band outcomes and no skipped, missing, nonidentifiable,
or deviating comparisons. Receipt: `rolling-wheel/grouped-residual-electrical-delta.json`.

## Previous focused work: evaluate shared residual outputs once

On top of `02706ddf`, construction binds each affine block's selected residuals
to unique, complete, repeatable canonical programs. The same output-selection
utility now serves residuals and JVPs. Native and interpreted execution evaluate
each selected program once and scatter its outputs in block order. Distinct
owners stay distinct; assertions and failures remain part of complete program
execution. Numerical projection, refinement, and acceptance are unchanged.

The RollingWheel dynamics block's 24 residual outputs come from eight programs,
three outputs apiece. The previous per-output API invokes each tensor program
three times. `grouped-residual-source-census.json` records the source mapping.
The longer `repeated-sim-profile-1` diagnostic collects 6,062 leaf samples over
40 identical traces; 1,581 are in mapped JIT code. The main JVP program remains
the largest kernel, and complete-input pure-call cache comparisons account for
much of the sampled `memcmp` work. Repeated-run instrumentation is archived in
`repeated-sim-probe-source.json` and removed from production source.

`grouped-residual-red-2.log` reproduces the absent grouped evaluation and failure
propagation. The first RED command stopped at an unused-trait-method compiler
error and is not a failing-test witness. Focused native tests prove one call
per complete program, changing inputs, source output placement, prevalidation,
explicit decline, and admitted table errors. Runtime tests cover changing
coefficients and failure without replay or partial state commit. Ownership
checks reject missing, ambiguous, and impure output selections. All 884 relevant
library tests pass in `grouped-residual-libraries-1.log`; Clippy's diagnostic
nesting issues are corrected, with final affected-package Clippy green in
`grouped-residual-libraries-3.log` and six grouped projection tests green in
`grouped-residual-libraries-2.log`.

`target/msl/multibody-grouped-residual-origin` retains the same four high models
and 2,960 high initialization channels. The fixed
`target/msl/multibody-grouped-residual-canary` retains nine high models and 175
high initialization channels. Both have unchanged phase/status/band outcomes
and zero skipped, missing, nonidentifiable, or deviating comparisons. Receipts:
`grouped-residual-origin-delta.json` and `grouped-residual-canary-delta.json`.
These are focused results; the complete cohort measurement below predates this
change.

Controlled Sim is 0.144187 seconds versus an immediate archived baseline of
0.148633 seconds, a single-pair 2.99% reduction. Flat, DAE, structural DAE,
canonical Solve, and the complete trace are byte-identical. Candidate worker
SHA-256 is `4ca1831140c8a3ef38919a2e525c9469eb1f4988abac07864f95a96deddefc66`.
Receipt: `grouped-residual-profile-delta-1.json`. RollingWheel remains slower
than OMC; no broader speed or complete-cohort claim is made.

## Previous complete cohort: inactive AD tangents preserve the parity floor

`target/msl/multibody-native-inactive-tangents-full`, at `c300c58d` plus tracked
worktree digest `0a5cc5ca811daf460bfc208afbce15ec01a29ae837671709aee525d4be01082b`,
retains 158/566 strict-high models (27.92%), including 22/42 MultiBody models.
All 158 compared models and 20,379 initialization channels remain high.
There are 18 reviewed exclusions, zero missing traces, zero nonidentifiable
comparisons, and zero deviations. Every comparison band matches
`multibody-affine-conditioning-full`. The already failing
ChopperBuckBoost_DutyCycle reaches the checked interval-endpoint disagreement
EX002 before its previous timeout; it remains a refusal and earns no credit.
Concurrent RollingWheel Sim is 0.159451 seconds. Receipt:
`rolling-wheel/native-inactive-tangents-full-delta.json`. Combined quick/full
verification and complete MultiBody coverage remain open.

AD propagates constructor-proved zero tangents while retaining primal
operations and their call owners, assertions, and domains. Solver-Y parameter
loads may have zero tangents; full Y|P initialization and sensitivities retain
parameter seeds. Inactive bilinear factors use the remaining product-rule term,
with compact tensor packing and exact immutable-run transpose reuse. A
numerically zero primal does not establish an inactive tangent. This follows
SPEC_0039's declared seed-domain rule and does not change Modelica equations.

`native-inactive-tangents-focused-2.log` records formatting, all 134 Solve
library tests, and affected-package Clippy. Tests include finite-difference
bilinear checks, parameter sensitivities, changing primal values, assertions,
and retained invalid primal arithmetic. The first focused attempt failed
formatting because restored sibling modules were absent; no tests ran until
the complete candidate was restored. The fixed canary retains nine high
models and 175 high initialization channels, with no status or band changes,
skips, missing traces, or deviations. Receipt:
`native-inactive-tangents-canary-delta.json`.

With bounded native SSA storage already landed, the general AD candidate
measures 0.150045 seconds against the immediate archived baseline recheck of
0.152998 seconds. Both workers exit successfully and produce identical trace
bytes (`a81708c82927bf5721cf527c75b80f83c9c7fb97aedab05cda49da22d5e3c619`).
This single-pair 1.93% improvement is small; it is not a broad performance
claim. Profiles are `native-inactive-tangents-profile-1` and
`native-inactive-tangents-baseline-recheck-1`; candidate worker SHA-256 is
`68f4d57b6d43d1b86bd170a19f8c2dfc10f20fd8a31be8e33ed9f75ef4f149aa`.
The older pre-SSA trial remains rejected; its evidence is not reused as a pass.

A separate temporary diagnostic restricts each program's Y seeds to the union
of all projection unknowns selecting its outputs. It measures 0.137266 seconds
with identical RollingWheel trace bytes (`native-projection-seed-probe-1`).
The diagnostic changes a general derivative artifact and therefore is not a
production implementation or coverage evidence. Its sources are archived in
`native-projection-seed-probe-trial.json` and completely removed. A retained
optimization needs a separate source-bound domain and must preserve unrestricted
initialization and sensitivity derivatives. OMC's generated system 731 solves
six variables and reconstructs other forces/accelerations; Rumoca's current
certified affine dynamics block retains 24 variables. RollingWheel remains
slower than OMC.

## Previous complete cohort: fixed affine conditioning preserves all traces

`target/msl/multibody-affine-conditioning-full`, at `b6a78cda` plus tracked
worktree digest `27b72247987f290dce47b2ab24944aeada03cbb440d69135beaf7451206a2b05`,
retains 158/566 strict-high models (27.92%), including 22/42 MultiBody models.
All 158 compared models and 20,379 initialization channels remain high.
Eighteen reviewed exclusions remain; there are zero missing, nonidentifiable,
or deviating comparisons. Every phase, simulation status, and band matches
`multibody-inline-dae-view-full`. Concurrent RollingWheel Sim is 0.163662
seconds, Driving 0.524226, and GyroscopicEffects 0.840462. Receipt:
`rolling-wheel/affine-conditioning-full-delta.json`. Combined quick/full gates
remain pending, and RollingWheel remains slower than OMC.

The temporary `runtime-cost-probe-1` counters preserve the baseline trace
byte for byte. Across that worker's execution, the 24-unknown affine block
has 2,794 Jacobian evaluations, 8,355 residual evaluations, and 5,561 linear
solves. Its sparse cache performs 5,538 numeric factorizations: changing
candidate-derived conditioning defeats reuse within the same fixed-matrix
refinement. The counters include initialization checks, not only Sim. The
active BDF session reports 1,054 accepted steps, 50 error-test failures,
2,245 nonlinear iterations, and zero nonlinear failures; its equation counters
report 2,248 RHS calls and 160 Jacobian products, including setup probes.
All temporary probes are archived in `runtime-cost-probe.patch` and removed.

Under the constructor-issued affine relation (SPEC_0043 §6), one invocation
already retains its exact block Jacobian. Its conditioning now belongs to
that same immutable matrix. Corrections solve with the original row/variable
scales so the existing bitwise matrix cache can reuse LU. Acceptance still
reevaluates the original residual, computes fresh candidate-derived scales,
and performs the required small-coordinate correction. Nothing is reused
across changed matrices by an approximate comparison. This follows the
fixed-factor correction pattern in
[LAPACK DGERFS](https://netlib.org/lapack/explore-html/df/d32/dgerfs_8f_source.html),
while retaining Rumoca's existing convergence policy and source residual.

The reduced offset-port electrical regression first fails the new conditioning
reuse assertion (`affine-conditioning-red-1.log`). All 69 projection tests,
including tiny junction voltages, opposing large currents, and fresh solution
scales, then pass with affected-package Clippy in
`affine-conditioning-focused-2.log`. The first focused command stopped at
formatting; it did not run tests. The five-target origin retains four high
MultiBody models and 2,960 initialization channels. The fixed canary retains
nine high models and 175 initialization channels with unchanged statuses and
bands. All fifteen models in the electrical affine-regression list compare
high, with 3,486 initialization channels high and no skipped, missing, or
deviating comparisons. Their bands match the corresponding members of the
preceding full cohort; the older fifteen-model focused snapshot predates four
already-landed recoveries, so its four apparent gains earn no new credit.
Receipts: `affine-conditioning-origin-delta.json`,
`affine-conditioning-canary-delta.json`, and
`affine-conditioning-electrical-delta.json`.

The controlled candidate measures 0.153893 seconds against an immediate
archived baseline recheck of 0.169451 seconds, a 9.18% reduction. Both exit
successfully. Flat, DAE, structural DAE, and canonical Solve files are identical;
floating-point traces differ and are validated by the OMC comparisons above.
The candidate worker hash is
`5b7273e9cc0b2e2464f810e6298d3cc4b71ebd6b8591fa1e38f9d1296f4cfa87`.
Receipt: `affine-conditioning-profile-delta-1.json`. The candidate's 148 leaf
samples include seventeen in JVP program 212 and five in sparse numeric LU;
these self samples are distinct from invocation counts. Jacobian evaluation
remains the next measured runtime target.

## Prepared reverse-mode capability

`PreparedScalarProgramBlock` now derives its immutable per-program reverse-AD
capability once, alongside existing row requirements, and preserves that
metadata on clone. Previously every Jacobian call rescanned all operations,
even to decline a multi-output tensor program. The capability predicate and
input validation are unchanged; its cache is bound to the private immutable
program block and contains no numeric results.

All 190 evaluator library tests, formatting, and Clippy pass in
`prepared-reverse-focused-1.log`. The fixed twenty-member canary retains every
phase, status, and band: nine compared models and 175 initialization channels
high, with no skipped, missing, or deviating comparisons. Receipt:
`prepared-reverse-canary-delta.json`. An isolated candidate measures 0.151358
seconds against an immediate 0.153747-second control. This single-pair 1.55%
difference is small; it does not establish a broad runtime speedup. All Flat,
DAE, structural DAE, Solve, and trace bytes match. Receipt:
`prepared-reverse-profile-delta-1.json`. The complete-cohort claim remains
bound to the preceding full run above.

## Previous complete cohort: expression-view inlining restores the floor

`target/msl/multibody-inline-dae-view-full`, at `fdef84cc` plus tracked
worktree digest `3f8fdd535362d6a4caf8c3279f4248ce82cf2dfb2f9bc65cad93bc70cbee50bd`,
restores 158/566 strict-high models (27.92%), including 22/42 MultiBody models.
All 158 compared trajectories and 20,379 initialization channels are high;
eighteen reviewed exclusions remain, with zero missing, nonidentifiable, or
deviating comparisons. The complete band table matches
`multibody-identity-affinity-full`. Driving completes Solve in 8.729543 seconds
and GyroscopicEffects in 8.478000 seconds under the unchanged ten-second
budget. The existing Media Inverse_sh_TX refusal changes ED019 to EF015;
this earns no coverage. Receipt: `rolling-wheel/inline-dae-view-full-delta.json`.

Driving's isolated Solve profile attributes 7.88% of self samples to
`ExpressionView::operation` and 4.80% to `DaeView::expression`. These borrowed
view accessors construct checked wrappers that consumers immediately inspect.
Adding ordinary cross-crate inline hints exposes their bodies to consumer
optimization without changing checks, data ownership, or evaluation. Isolated
Solve falls from 8.574600 to 7.925615 seconds. DAE, structural DAE, canonical
Solve, and trace files are byte-identical between
`direct-row-storage-driving-solve-profile-1` and
`inline-dae-view-driving-solve-profile-1`.

Formatting, 43 evaluation-DAE tests, 187 DAE-IR tests, 125 Solve-phase tests,
and affected-package Clippy pass in `inline-dae-view-focused-1.log`. The
five-target origin retains four high models and all 2,960 initialization
channels; the fixed twenty-target canary retains nine high models and all
175 initialization channels. Neither focused target set changes phase,
simulation status, or band, and neither has missing or deviating comparisons.
Receipts: `inline-dae-view-origin-delta.json` and
`inline-dae-view-canary-delta.json`.

RollingWheel remains high with concurrent Sim 0.175470 seconds. It is still
slower than OMC; no baseline promotion, complete MultiBody claim, or combined
quick/full verification result is implied. Runtime profiling continues.

## Previous complete cohort: native speedup exposed a Solve timeout

`target/msl/multibody-direct-row-storage-full`, at `6fd321b2` plus worktree
digest `81a504337fd25db9d9e228bca672e474abd1a25f9b375805247b00dec8593c63`,
compares 157/566 models high (27.74%), including 21/42 MultiBody examples.
All 19,487 compared initialization channels are high. Eighteen reviewed
exclusions remain; no comparison is missing, nonidentifiable, or deviating.
The prior 158-model floor is not preserved: RollingWheelSetDriving times out
in Solve after 11.159 seconds against the unchanged ten-second budget. It
completed Solve in 8.831501 seconds in the focused run. No timeout retry or
budget change is used to recover coverage.

Fourbar_analytic and GearConstraint also reach their Solve watchdog instead
of their previous structural refusals, and Media Inverse_sh_TX changes its
existing frontend refusal back to EF015. None earns coverage. RollingWheel
remains high with Sim 0.195300 seconds; the same run's stored OMC simulation
system timer is 0.133511 seconds. These concurrent timings are distinct from
the isolated measurements below. The configured command exits zero, but the
lost certified model prevents floor promotion. Receipt:
`rolling-wheel/direct-row-storage-full-delta.json`. Combined quick/full
verification and complete MultiBody coverage remain open.

## Native performance repair: bounded tensor rows retain direct values

The seed-dependency census `seed-frontier-1.log` uses the production dependency
deriver on every producer output of the checked RollingWheel JVP. Program 212
contains 1,106 producer operations: 571 wholly seed-independent, 128 mixed,
and 407 wholly seed-dependent. All 31 matrix products and eight directional
calls are mixed. Most independent operations are copies. Skipping only whole
seed-independent operations would leave the expensive tensor work intact.

The native emitter exposes a separate concrete cost: any row-level tensor
operation creates a register tape for every intermediate, including tiny
three-vector operations. Existing direct lowering was already available for
these operations. Under SPEC_0032 §§4–5 and SOLVE-C53, final native storage
selection now retains direct SSA values when every tensor operation fits the
existing 64-unit static-work bound. Matrix work includes contraction extent
and AD lanes; other tensor ranges include their storage lanes. Larger tensors,
overflowing work estimates, and tensor-update slices retain the prior tape
selection. Canonical tensor owners and floating-point accumulation order do
not change. The rule is independent of model identity or numerical values.

The reduced regression fails on the previous allocation policy in
`direct-row-storage-red-1.log`. It then checks direct and tape-backed dual
matrix products, two transposes, cancellation-sensitive accumulation, and
changing seeds against the interpreter. A 4,096-element fill retains bounded
loop storage and reloads changing inputs, including negative zero. All 88
native tests, formatting, and affected-package Clippy pass in
`direct-row-storage-focused-2.log`; the first focused command stopped at
formatting before running tests. All 99 compiler library tests, 595 core tests,
seventeen architecture/spec gates, and 111 projection tests pass in
`direct-row-storage-core-1.log`.

The controlled actual-worker trial measures Sim 0.168387 seconds. An immediate
archived baseline recheck measures 0.287090 seconds: a 41.35% reduction.
Both workers exit successfully. Flat, DAE, structural DAE, canonical Solve,
and trace files are byte-identical; trace SHA-256 remains
`c9be85f47452aba0d450ab626c98a4cfdc838127d62df3eceea9edfd23f034e0`.
The hot residual kernel shrinks from 24,792 to 10,928 bytes and its JVP from
59,968 to 35,280 bytes. The trial worker is
`599f608b7be9f51b262bf39fe452bb00ef58d4641deeff10650f512b80a6f7eb`;
its exact source patch and profile are retained in
`direct-row-storage-trial-1.patch` and `direct-row-storage-profile-1`.
The retained implementation moves the same selector to a dedicated module.
Receipt: `direct-row-storage-profile-delta-1.json`.

The new profile has 168 leaf samples, 40 mapped to nonoverlapping JIT ranges.
Sparse numeric LU contributes 21 samples, JVP program 212 contributes nineteen,
and residual program 212 contributes four. These are self samples, not
inclusive costs. Repeated algebraic factorization and JVP evaluation remain
targets; this change does not establish that RollingWheel is faster than OMC.

The normal five-target origin comparison retains four high MultiBody models,
all 2,960 initialization channels high, zero skipped/missing/excluded,
nonidentifiable, or deviating comparisons, and the thermal GenerationOfFMUs
EL005 refusal. RollingWheel Sim is 0.170735 seconds, GyroscopicEffects 1.018274,
RollingWheelSetDriving 0.582798, and SphericalConstraint 0.177060. The fixed
twenty canary members retain every phase, status, and band: nine models and
all 175 initialization channels high, with eleven unchanged refusals and no
unmeasured or deviating comparison. Receipts:
`direct-row-storage-origin-delta.json` and `direct-row-storage-canary-delta.json`.
The complete-cohort timeout above remains unresolved despite these focused
checks.

## Previous complete cohort: identity affinity preserves the restored floor

The complete `target/msl/multibody-identity-affinity-full` comparison at
`cc51f08ac23451e75ed9705085183900cc2e05ab` retains 158/566 strict-high models
(27.92%), including 22/42 MultiBody examples. All 20,379 compared
initialization channels are high. Eighteen reviewed exclusions remain;
there are zero missing, nonidentifiable, or deviating comparisons. The entire
566-row band table is unchanged from `multibody-shared-materialization-full`.
Media Inverse_sh_TX changes its existing frontend refusal from EF015 to ED019;
this earns no coverage credit and no model changes simulation status or band.

RollingWheel retains all 184 trajectory and initialization channels high.
Concurrent Sim falls from 0.782661 to 0.327667 seconds; the controlled
measurement below is 0.288191 seconds. It remains slower than OMC. Solve costs
are SphericalConstraint 4.522983 seconds, RollingWheelSetDriving 9.211795,
and GyroscopicEffects 9.539006. The latter two still need more margin under
the unchanged ten-second budget. The run uses eleven simulation workers,
the named commit's tracked source, and the existing clean tracked-tree digest;
the foreign untracked communication file remains untouched. Receipt:
`rolling-wheel/identity-affinity-full-delta.json`.

All focused, fixed-canary, core, and architecture/spec checks recorded below
pass. Combined quick/full verification and the remaining runtime performance
gap remain open. This is not a baseline promotion, 100% MultiBody result, or
release approval.

## Runtime investigation: repeated coefficient evaluation

`identity-affinity-symbols-profile-1` resolves the previous native-code blind
spot without compiler instrumentation. Cranelift writes its JIT map when
`PERF_BUILDID_DIR` is present at worker startup. The archived worker's hash is
`46722de1fb006b880281c8c9e7b213f7815cc9c4a0b65ce0c64fdc87570988af`.
Of 287 leaf samples, 148 map into the captured nonoverlapping JIT ranges.
Program 212 accounts for 36 Jacobian samples and 35 residual samples, about
25% combined. These are sampled self costs, not inclusive call-chain costs.
The three outputs still originate from `Body.mo`'s `a_0 = der(v_0)`.

A private seed-domain probe binds each source program to the union of the
unknowns of projection blocks selecting its outputs. Replacing other seeds
with literal zeros alone changes Sim from 0.290190 to 0.293735 seconds and
barely changes the hot Jacobian's 59,968-byte code size. A subsequent prototype
omitting structurally zero tangent calculations reduces the restricted kernel
to 42,800 bytes and Sim to 0.274063 seconds. This is diagnostic evidence only:
a restricted kernel cannot replace the general sensitivity artifact.

The attempted unrestricted production change also removes inactive bilinear
terms and preserves compact storage through tensor fills, transposes, and
concatenations. Its 134 Solve tests, 111 projection tests, 99 library tests,
595 core tests, seventeen architecture/spec gates, and Clippy pass. The
five-target `multibody-inactive-tangents-origin` comparison retains four high
models, all 2,960 initialization channels, zero skipped/missing/excluded or
deviating comparisons, and the prior thermal `EL005` refusal. However, the
isolated candidate measures 0.304896 seconds, while an immediate archived
baseline recheck measures 0.286091 seconds. Their trace files are identical.
The extra packing outweighs the arithmetic savings, so the candidate and its
temporary instrumentation are removed. Source, tests, and spec proposal are
preserved privately in `rolling-wheel/inactive-tangents-rejected-source.json`;
the normal worker was rebuilt and matches the archived baseline byte for byte.
No new compiler capability or cohort improvement is claimed from this trial.

The next boundary is source coefficient evaluation across the entire solve.
The read-only `coefficient-frontier-2.log` probe uses the production dependency
deriver on every producer output of the current 24-unknown block. In program
212, 313 of 391 producer operations are independent of that block's unknowns,
including four of eight pure calls. Program 214 has 98 independent operations
out of 112. Both programs occur in six of the block's nine issued forward
color selections; these are construction selections, not measured invocation
counts. Merely reducing tangent arithmetic leaves this primal coefficient work
inside each selected evaluation. OMC's corresponding system 731 is a
six-unknown linear solve with a symbolic Jacobian. The next experiment should
bind a reusable coefficient evaluation to the exact immutable block coordinate
and source owners, preserving assertions and fresh residual certification,
then measure matrix assembly and residual evaluation together. General
state/parameter sensitivities retain their complete seed domain. The latest
complete cohort remains the 158/566, 22/42 checkpoint above; combined
quick/full verification and the performance gap remain open.

## Previous complete cohort: previous high-parity floor restored

The complete `target/msl/multibody-shared-materialization-full` comparison at
`f494d96207769c257168356e29fc5285b022fedb` restores 158/566 high models
(27.92%), including 22/42 MultiBody examples. All 20,379 compared
initialization channels are high. The eighteen reviewed exclusions remain;
there are zero missing, nonidentifiable, or deviating comparisons. Against
the preceding `multibody-fixed-anchor-full`, GyroscopicEffects and
RollingWheelSetDriving both recover their high bands. Against the earlier
158-model `multibody-component-definition-full` checkpoint, the complete
566-row band table is unchanged: no earlier high model has been exchanged for
a different success. The run uses the unchanged phase budgets and eleven
simulation workers. This restores the prior floor but does not establish
100% MultiBody coverage, a 28.00% floor, or release readiness.

GyroscopicEffects Solve measures 8.399072 seconds; RollingWheelSetDriving
9.541474; SphericalConstraint 4.684997. Driving remains close to the ten-second
budget and still needs margin. Thermal HeatTransfer GenerationOfFMUs returns
its explicit `EL005` fixed-initial-value refusal without the earlier worker
crash. Fourbar_analytic and GearConstraint reach structural refusals instead
of timing out; Media Inverse_sh_TX changes its frontend refusal phase. These
diagnostic changes earn no simulation coverage. RollingWheel retains all 184
trajectory and initialization channels high, with Sim 0.782661 seconds. It
remains slower than OMC; the next runtime investigation is the repeated
algebraic projection block and its causal assignment/tear selection.

Receipts are `rolling-wheel/shared-materialization-full-delta.json` and
`rolling-wheel/shared-materialization-full-restoration-delta.json`. All
tracked files match the named commit; the comparator's dirty flag reflects
the retained foreign untracked `comm_fastdyn.md`. Its tracked worktree digest
is `48672dfdec9c3416aac00ca9d56725958329a60236f921974bbf966a09020c35`.
The focused and core validation below passes. Combined quick/full validation
remains open; this cohort result is not a baseline promotion or release gate.

## Focused runtime repair: identity tensors retain their constant degree

The current RollingWheel dynamics block has 24 unknowns and thirteen tear
variables. Replaying its exact checked output dependencies with the existing
candidate-aware tearing algorithm still produces thirteen tears, so changing
the greedy candidate selection alone does not address the cost. Its issued
affine certificate is false. OMC's corresponding generated C uses a
six-variable linear system (`RollingWheel_03lsy.c`, system 731, symbolic
Jacobian method), rather than an analogous nonlinear solve.

The temporary affinity probe reads the captured checked Solve programs and
locates the first divergence: 21 block rows prove affine, but rows 770–772,
the three outputs of program 212, have no degree proof. Their source is
`Body.mo`'s `a_0 = der(v_0)`. Program 212's second operation constructs an
identity tensor; the degree checker lacks that constant operation and stops
before reaching the remaining arithmetic. It has not proved a nonlinear
equation. `shared-materialization-tearing-inspect-2.log` records the unchanged
tearing counts, and `shared-materialization-affinity-probe-1.log` records the
three missing proofs. Both diagnostics are read-only model replays.

The generic degree checker now recognizes a primal `TensorIdentity` as
independent of all solver unknowns. The reduced regression fails in
`identity-affinity-red-1.log` and passes after the change. It checks constant
identity outputs and their affine product with unknown vectors at extents 3
and 4096, retaining compact ranges rather than enumerating matrix entries.
The actual captured block then proves all 24 residuals affine
(`identity-affinity-probe-green-1.log`). The temporary source probe is removed;
all other unsupported operations, source evaluation, rank checks, and fresh
affine-coordinate recovery remain unchanged under the existing
SPEC_0036/0043 §6a degree-proof profile.

`identity-affinity-focused-1.log` passes all 315 Solve IR tests, 111 projection
tests, formatting, and Solve IR all-target/all-feature Clippy. The normal
`target/msl/multibody-identity-affinity-origin` five-target gate retains all
four high MultiBody models, all 2,960 initialization channels, and the thermal
GenerationOfFMUs `EL005` refusal. No comparison is skipped, missing, excluded,
nonidentifiable, or deviating. RollingWheel Sim falls from 0.782056 to
0.286439 seconds, retaining all 184 trajectory and initialization channels
high. Receipt: `rolling-wheel/identity-affinity-origin-delta.json`, at
`3723c597` plus worktree digest
`17513b4fed4c226a5f93f65bed222c2b6f23a788242c188d88a7e0f8c2806f81`.

The controlled actual-worker `identity-affinity-rolling-profile-1` measures
Sim 0.288191 seconds and 0.29 seconds user CPU, with zero major faults.
The exact worker is archived for symbolization; 285 samples have zero reported
loss. However, 49.47% of samples have unresolved shared-object identity and no
JIT map was captured, so this profile does not yet attribute the remaining
generated-code cost. It earns no separate coverage credit. The fixed twenty
canary members retain their previous phases and bands: nine compared models
and all 175 initialization channels high, eleven unchanged refusals, and no
skipped, missing, excluded, nonidentifiable, or deviating comparison. Receipt:
`rolling-wheel/identity-affinity-canary-delta.json`. RollingWheel remains
slower than OMC; no closed-form trajectory or completed runtime optimization
is claimed from this algebraic linearity proof. All 99 Rumoca library tests,
595 core tests, and seventeen architecture/spec gates pass in
`rolling-wheel/identity-affinity-core-suite-1.log`. The complete cohort and
combined quick/full verification have not yet been rerun after this change.

## Previous complete cohort: fixed-anchor repair and remaining regressions

The complete `target/msl/multibody-fixed-anchor-full` run at
`cf0c2c61f01b914cfa76b1813c1ffaa4f293cc33` compares 156/566 models high
(27.56%), including 20/42 MultiBody examples. The comparator measures 156
models, retains eighteen reviewed exclusions, and reports zero missing,
nonidentifiable, or deviating traces. All 18,520 initialization channels are
high. The configured CLI gate exits zero; the unresolved regressions still
prevent promotion or release.

SphericalConstraint regains its high band, with Solve 5.225251 seconds and
Sim 0.261808 seconds. GyroscopicEffects again exceeds the unchanged ten-second
Solve budget, despite passing the preceding focused comparison in 9.499843
seconds. RollingWheelSetDriving loses its prior high band to the same Solve
budget; its preceding full-run Solve time was already 9.430834 seconds. These
failures remain measured failures, without retries or changed limits. The
elementary RollingWheel retains all 184 trajectory and initialization channels
high; Sim is 0.830653 seconds, still slower than OMC.

The full run also changes Thermal HeatTransfer `GenerationOfFMUs` from its
previous typed fixed-initial-value refusal to a worker stack overflow
(`SIGABRT`). This is an actionable compiler regression to reduce and repair,
even though the model had no prior high band. The original log records
`rumoca-worker-main` stack exhaustion. PrismaticConstraint now reaches a
structural refusal instead of timing out, and Media `Inverse_sh_TX` changes
its frontend refusal phase; neither earns coverage credit. The complete
per-model receipt is `rolling-wheel/fixed-anchor-full-delta.json`. The next
work is the stack-overflow root cause and remaining Solve costs; no full-cohort
restoration is claimed from the focused three-model success. The subsequent
focused repair below addresses the crash without revising this cohort result.

## Focused repair: cancellation retains a shaped primal zero

The actual-worker `rolling-wheel/fixed-anchor-thermal-profile-1` reproduces
the thermal `GenerationOfFMUs` stack overflow. Its core dump repeatedly enters
`tensor_coefficient` while attempting to recover the shape of a zero primal.
The temporary source-DAE probe in `cancelled-offset-source-probe-1.log` locates
the exact recipe: two references to the same shared zero offset are subtracted
and negated. The source operand is the heat capacitor's `der_T` coordinate.
This is not a cycle in the source expression DAG. A separate source-level
`theta-theta` control already passes; the failure is in reconstruction of the
derived shared coefficient recipe.

The prior derivative repair reused exact-sum differentiation for order-zero
values. Its `Derivative::Zero` result lost the primal shape, and shape recovery
requested the same cancelled primal recursively. Primal cancellation now
constructs a zero with the identical checked operands' shape before returning
or caching its value. Positive derivative orders retain the structural-zero
tag. Reconstruction uses the existing shared `shaped_zero` constructor, with
no duplicated zero builder, tensor scalarization, raised stack limit, or
model-specific condition. The original equations and initialization checks
remain authoritative under SPEC_0007/0040 STRUCT-T03 and MLS §8.6.

The reduced checked-DAE regression reproduces the captured recipe and checks
scalar, vector, matrix, and empty-tensor values through derivative order two
with the independent DAE numeric evaluator. `cancelled-offset-shape-red-1.log`
records the failed shape-recovery boundary; `cancelled-offset-focused-2.log`
passes all 166 structural tests, 22 contact tests, formatting, and structural
all-target/all-feature Clippy. Temporary diagnostics have been removed.

The normal `target/msl/multibody-cancelled-offset-origin` gate compares all
four selected MultiBody models high, with all 2,960 initialization channels
high and zero skipped, missing, excluded, nonidentifiable, or deviating traces.
The fifth target, thermal GenerationOfFMUs, again returns its prior explicit
`EL005` fixed-initial-value refusal for `inverseCapacity.mass.T`; the crash is
repaired, but this is not thermal-model simulation support. SphericalConstraint
Solve is 4.906882 seconds; GyroscopicEffects is 9.893319 seconds; and
RollingWheelSetDriving is 9.252459 seconds. These near-budget Solve costs still
require profiling. RollingWheel Sim is 0.784325 seconds and remains slower than
OMC. Receipt: `rolling-wheel/cancelled-offset-origin-receipt.json`, at
`9cd80190` plus worktree digest
`2cff66a4996d90fafb05aec84234a5594c0a7d5dad05c5d85c8c9a18a22bbebe`.

The same-source `target/msl/multibody-cancelled-offset-canary` retains all
twenty phase, status, and band rows from `multibody-fixed-anchor-canary`:
nine compared models and 175 initialization channels high, eleven unchanged
refusals, and zero skipped, missing, excluded, nonidentifiable, or deviating
comparisons. Receipt: `rolling-wheel/cancelled-offset-canary-delta.json`.
All 99 Rumoca library tests, 595 core tests, and seventeen architecture/spec
gates also pass in `rolling-wheel/cancelled-offset-core-suite-1.log`. The
complete cohort and combined quick/full suites have not been rerun after this
repair; the remaining near-budget MultiBody Solve costs are still open.

## Focused performance repair: reuse complete source dependency witnesses

The actual GyroscopicEffects worker spends 8.534372 seconds in Solve at
`79347dca`, with 8.48 seconds of user CPU. The recorded Solve profile includes
repeated source materialization walks, expression decoding, and allocations.
Source inspection shows that each affine candidate starts the same root
dependency proofs again, allocating a whole-DAE visited array per query.
An initial singleton-projection prefilter avoids constructing a numeric
evaluator for ineligible aggregate shapes and literal integer indices, but
alone only changes the controlled Solve measurement to 8.424262 seconds.

Affine discovery now owns a cache bound to one immutable DAE view and facts
pass. Its key retains the exact source expression and function call path;
its value retains the complete state-anchor witness or the original refusal.
Every candidate still checks its own state against that witness. No cached
boolean independence result or visited marker can erase an anchor, and no
cache survives a facts-update boundary. This preserves the STRUCT-T03
independence contract and source tensor ownership. New checked-DAE tests
exercise alternating candidate states, nonlinear self-dependence, shared
function bodies with distinct arguments/refusals, and extents 3 and 4096.

`shared-materialization-focused-1.log` passes 168 structural and 22 contact
tests; its test-only nesting lint was corrected, and
`shared-materialization-focused-2.log` passes the two new tests, formatting,
and all-target/all-feature structural Clippy. The normal five-target
`target/msl/multibody-shared-materialization-origin` comparison has unchanged
phases and bands against `multibody-singleton-prefilter-origin`: four models
and all 2,960 initialization channels high, with zero skipped, missing,
excluded, nonidentifiable, or deviating comparisons. Thermal GenerationOfFMUs
retains its prior `EL005` fixed-initial-value refusal without a worker crash.
GyroscopicEffects Solve measures 8.967672 seconds; SphericalConstraint
4.596436; RollingWheelSetDriving 9.132433. RollingWheel retains its 184 high
channels and Sim measures 0.782056 seconds, still slower than OMC. Receipt:
`rolling-wheel/shared-materialization-origin-delta.json`, at `79347dca` plus
worktree digest
`bfd38449f5f27a9b11501de6973429b0e48a98f1d56404a28f9a4977cda696d9`.

The controlled actual-worker `shared-materialization-gyro-solve-profile-1`
measures Solve 7.759928 seconds and 7.72 seconds user CPU, versus the 8.534372
baseline. The profile has zero lost samples and archives its exact worker
binary for subsequent symbolization. This single-worker diagnostic bypasses
the parent Solve watchdog and earns no coverage credit; the normal gate above
supplies the parity evidence. The fixed twenty-model
`target/msl/multibody-shared-materialization-canary` has unchanged phases and
bands against `multibody-cancelled-offset-canary`: nine compared models and
175 initialization channels high, eleven unchanged refusals, and zero skipped,
missing, excluded, nonidentifiable, or deviating comparisons. Receipt:
`rolling-wheel/shared-materialization-canary-delta.json`. All 99 Rumoca
library tests, 595 core tests, and seventeen architecture/spec gates pass in
`rolling-wheel/shared-materialization-core-suite-1.log`. Full-cohort
restoration is not established by these focused measurements; combined
quick/full verification remains open.

## Previous complete cohort: two state-reduction regressions recorded

The complete `target/msl/multibody-no-slip-full` run at
`5be301f43b2738cde950733e59de1e5e57aa9faa` compares 156/566 models high
(27.56%), including 20/42 MultiBody examples. Eighteen reviewed exclusions
remain; there are zero missing, nonidentifiable, or deviating comparisons.
All 18,495 compared initialization channels are high. Although the configured
CLI gate exits zero, this is a regression against the preceding 158-model
checkpoint and is not acceptable for promotion or release.

`SphericalConstraint` and `GyroscopicEffects` both lose their prior high bands
because Solve exceeds its unchanged ten-second parent budget. The preceding
Solve measurements were 5.220201 and 8.457811 seconds, respectively. Their
failures remain visible in `rolling-wheel/no-slip-full-delta.json`; no timeout
or comparator policy was relaxed. RollingWheel retains all 184 trajectory and
initialization channels high with eight states, and Sim measures 0.835341
seconds in this cohort run. This still does not meet the OMC speed objective.

The separate `no-slip-spherical-solve-profile-1` diagnostic reveals that
SphericalConstraint's expensive reduction attempts end in a structural refusal:
2194 matched of 2219 equations and unknowns. This diagnostic invokes the worker
directly and does not apply the cohort parent's ten-second Solve watchdog;
it does not replace the timed-out cohort result or earn coverage credit.
Profiling was enabled at Solve entry and retained through the refusal. Leading
self costs include DAE operation/view access, projected incidence traversal,
and materialization proofs. The controlled reproduction below locates the
responsible state-reconstruction decision.

The source-DAE inspection in `no-slip-spherical-reduction-2.log` selects a new
auxiliary definition for `freeMotionScalarInit.initAngle.angle` after three
direct state demotions, and later selects one for `constraint.frame_b.r_0`.
The direct-first lane stalls after reducing the unmatched residue from fifty
to six; the pristine holonomic-first attempt also fails. The initial hypothesis
that the angle identity solve hides explicit component kinematics is rejected
by the controlled comparison below.

## Fixed-anchor regression: retaining exact derivative zeros

The source-DAE controls in `rolling-wheel/no-slip-spherical-aux-control-2.log`
isolate `constraint.frame_b.r_0`: disabling only its new auxiliary definition
restores structural reduction in 3.885 seconds. Disabling only the angle
definition still fails after 21.757 seconds. These temporary diagnostic
controls have been removed from the compiler and retained only in the private
campaign evidence. The production fix makes no decisions from model names or
variable ordinals.

The reduced `TensorFixedAnchor.mo` fixture contains eighteen scalar equations
and unknowns. Its constant matrix constrains a position vector to a fixed
anchor, while source derivative equations define velocity and acceleration.
Before the fix, structural reduction matches only twelve of eighteen unknowns.
OMC eliminates all dynamic states and agrees with the independent analytical
solution on all eighteen channels over twelve rows, with zero observed error
(`rolling-wheel/omc/fixed-anchor-1/analytical-comparison.json`).

The first divergent layer is auxiliary tensor differentiation. For
`A*q=b`, it materialized a zero derivative matrix and retained `A'*q` even
when `A'=0`, creating a false primal-coordinate dependency. Coefficient
reconstruction and its shared cache now preserve the existing
`Derivative::Zero` distinction. Only nonzero coefficient derivatives read the
primal coordinate or its first derivative. The original checked aggregate
solve with `A` remains authoritative, including its nonsingularity requirement.
This follows MLS array equality and continuous-equation semantics, SPEC_0022
§3.7, and SPEC_0007/0040 STRUCT-T03. It introduces no scalarized tensor owner.

The reduced fixture now has no dynamic states and checks all eighteen
analytical channels with both BDF and RK. A singular constant-matrix variant
is still rejected by both integrators. All 165 structural library tests,
22 contact tests, formatting, and structural all-target/all-feature Clippy pass
in `rolling-wheel/fixed-anchor-focused-2.log`.

The normal `target/msl/multibody-fixed-anchor-origin` comparison measures all
three selected models high, with zero skipped, missing, excluded,
nonidentifiable, or deviating traces. All 2,068 initialization channels are
high. SphericalConstraint completes Solve in 4.898042 seconds and Sim in
0.247828 seconds with thirteen states; GyroscopicEffects completes Solve in
9.499843 seconds and Sim in 1.494477 seconds with eighteen states. The original
ten-second Solve budget is unchanged, leaving GyroscopicEffects little margin.
RollingWheel retains eight states and all 184 channels high, with Solve
1.269984 seconds and Sim 0.782680 seconds. It remains slower than OMC.
The receipt is `rolling-wheel/fixed-anchor-origin-receipt.json`, at
`7602af65` plus worktree digest
`786ae41e9185c7215630f8c3892cef53bf52aa7f8fd07eb20773e946c93ef3e2`.

The same-source fixed twenty-model canary
`target/msl/multibody-fixed-anchor-canary` has no phase, status, or band changes
against `multibody-no-slip-canary-2`: nine compared models and 175
initialization channels remain high, with eleven unchanged refusals and zero
skipped, missing, excluded, nonidentifiable, or deviating comparisons. Receipt:
`rolling-wheel/fixed-anchor-canary-delta.json`. Complete-cohort validation
remains pending; these focused checks do not revise the measured cohort above.
The broader focused checkpoint passes all 99 Rumoca library tests, 595 core
tests, and seventeen architecture/spec gates in
`rolling-wheel/fixed-anchor-core-suite-1.log`. The combined `verify quick`
and `verify full` suites have not been rerun for this checkpoint.

## Previous complete cohort: tensor-state reconstruction preserves parity

The complete `target/msl/multibody-component-definition-full` gate passes at
`27dd45f57af983cae6faf5f6ec9c2b7936129f4c`, using eleven Rumoca workers,
the unchanged 566-model roster, and unchanged tolerances and budgets. All
158 previously strict-high models retain their bands: 158/566 (27.92%),
including 22/42 MultiBody examples. The comparator measures 158 models,
retains eighteen reviewed exclusions, and reports zero missing traces,
nonidentifiable traces, or deviating channels. All 20,379 initialization
channels are high.

RollingWheel completes the normal pipeline with eleven integrated coordinates
and all 184 trajectory and initialization channels high. Sim measures
1.071142 seconds. This establishes cohort safety for the tensor-state change;
it does not establish a meaningful speed improvement or erase the preceding
single-model startup failure. The only phase/status delta is the already
excluded `ThyristorBridge2Pulse_RLV_Characteristic`, which completes instead
of timing out and returns the exclusion count from seventeen to eighteen.
No comparison band changes and no coverage credit comes from this timing delta.

Receipt: `rolling-wheel/component-definition-full-delta.json`; worktree digest:
`48672dfdec9c3416aac00ca9d56725958329a60236f921974bbf966a09020c35`.
OMC version is `a96aa1a-cmake`. RollingWheel remains slower than OMC; its
velocity reconstruction and the combined quick/full gates remain open.

## Latest focused work: reconstructing dependent velocity tensors

`TensorNoSlipContact` reproduces the remaining RollingWheel velocity-state
problem. Its contact geometry and two no-slip equations determine the velocity
vector from independent position and angular coordinates. OMC selects three
states (`theta`, `x`, `y`); Rumoca initially retains the three additional
velocity coordinates. Independent analytical motion is `theta=0.2+t`,
`x=0.2`, `y=0.3+sin(0.2)-sin(theta)`, and `z=cos(theta)`, with velocity and
force given by their first and second derivatives. All 21 OMC channels agree
with that solution to 1.079e-8 over twelve rows in
`rolling-wheel/omc/no-slip-reconstruction-1/analytical-comparison.json`.

The STRUCT-T03 auxiliary-state profile now combines independent source scalar
constraints with independently materializable entries of authored literal-array
equalities. Coefficients remain aggregate identity, projection, transpose,
product, and outer-product expressions; there is no tensor-basis enumeration.
The original equations, initialization obligations, and assertions remain
owners. Self-dependent coefficients, incomplete systems, and causal projections
that merely repeat their own definitions are refused. A direct source state
definition takes precedence over a redundant auxiliary solve for that state.

Tracing the real wheel immediately after its four direct state demotions found
a further discrepancy: the equality proof knew that the joint velocity equaled
the body's velocity state, but affine reconstruction required a unique causal
assignment. The joint has multiple source defining equations, so that lookup
returned none. The affine proof now consumes the existing signed, offset-free
state-alias proof. It recognizes both no-slip rows and the independent vertical
velocity row before holonomic differentiation. Position-array discovery and
projection follow the same signed value aliases. Positive and negated velocity
aliases have reduced analytical regressions; signed literal arrays retain their
signs when contributing an independent row.

The normal `target/msl/multibody-no-slip-alias-origin` gate succeeds with eight
integrated coordinates (`x`, `y`, three angles, three angular rates), matching
OMC's state set, and no retained manifold rows in the structural diagnostic.
All 184 trajectory and initialization channels are high: one model compared,
zero skipped, missing, excluded, or nonidentifiable traces. Sim is 0.801544
seconds versus 1.068513 seconds in the preceding eleven-state focused run;
Solve is 1.284566 seconds and backend build 1.165971 seconds. This remains much
slower than OMC and establishes no new cohort coverage. The focused receipt is
`rolling-wheel/no-slip-alias-origin-delta.json`, with worktree digest
`f8e606b2f16621b50aaa0e2cb8107524c2b3f02d6e02447a2583e32e645199d0`.

All 165 structural library tests, nineteen contact tests, formatting, and
structural all-target/all-feature Clippy pass in `no-slip-focused-suite-6.log`.
The additional negated-position case also passes within the complete 593-test
core suite; all 99 compiler library tests and seventeen gates pass in
`no-slip-core-suite-1.log`. Temporary source-DAE probes have been removed; their
results remain in `no-slip-intermediate-{4,alias-5}.log` and
`no-slip-real-model-alias-6.log`.

The fixed `multibody-no-slip-canary-2` passes with all twenty phase, status,
and band rows unchanged from `multibody-component-definition-canary`: nine
models compared high, 175 initialization channels high, eleven unchanged
refusals, and zero skipped, missing, excluded, nonidentifiable, or deviating
comparisons. Receipt: `rolling-wheel/no-slip-canary-2-delta.json`; worktree
digest: `3c24b570f8d3970f81bc080d6a5b74bc0e09134f2788ab626ab2cef6e90dfe35`.
The first `multibody-no-slip-canary` was externally interrupted during library
parsing and has no parity measurement; its log remains preserved. No solver
failure was retried, and no budget or tolerance changed.

The controlled actual-worker `no-slip-profile-1` measures Sim at 0.795771
seconds (0.78 seconds user CPU, zero major faults during Sim). Every native
sample address maps to a generated function. Projection Jacobians account for
13.23% of sampled self time, assignment schedules 10.51%, and residual programs
7.54%; there are no manifold Jacobian programs. Source-bound programs 212 and
214 originate at `Parts/Body.mo`'s `a_0=der(v_0)` and `z_a=der(w_a)` equations.
Their projection Jacobians alone account for 8.55% and 3.38%, respectively.
Runtime interpretation, assignment certification, and allocation remain
visible costs. The worker SHA-256 is
`4ef449ffd30e161cf8030f8eaf130f568a3ebb1515fb7cda6ccdfa2fc5e7939f`.
This is a performance diagnostic, not separate parity or cohort evidence.
The next step is a complete cohort at this implementation, followed by tracing
those acceleration kernels and prepared projection execution against OMC.

## Previous focused work: inverse coordinates of tensor states

`TensorStateContact` reproduces RollingWheel's source pattern: a position
state array equals `{x,y,z}`, its derivative is a velocity array, and independent
orientation constraints determine the contact vector. Structural discovery
could not derive `z=-delta[3]` through those array equalities. The RED test keeps
five state declarations (nine scalar coordinates), while OMC selects only
`theta`, `x`, and `y`. Its generated equations and eighteen analytical channels
agree to 4.44e-16 over twelve output rows in
`rolling-wheel/omc/component-definition-1`.

The STRUCT-T03 component profile now follows unique literal-array definitions
of non-`Always` states while discovering their inverse scalar coordinates.
It captures one component expression for proof and reconstruction, rejects
cyclic/self-dependent definitions, and retains the source tensor owners,
assertions, and initialization obligations. Both ordinary residuals and the
existing row-major structured bodies participate. The reduced model reaches
three states under BDF and RK, with independently checked position, velocity,
and force. Reversed and zero-residual forms agree; source assertions still
execute; a free-fall negative control retains its free position and velocity.
All fourteen contact tests, 162 structural library tests, 99 compiler library
tests, 587 core tests, seventeen gates, formatting, and structural Clippy pass.

An initial broader component-definition candidate made six existing contact
tests structurally singular by introducing unrelated scalar definitions into
holonomic discovery. The final profile admits only inverse coordinates of the
explicit state-array definition. All affected tests pass again. The initial
free-fall fixture also needed two missing free horizontal variables to balance
its intended equations. RED/intermediate evidence remains in
`rolling-wheel/component-definition-{red-1,probe-1,probe-2,contact-1}.log`.

The normal `multibody-component-definition-origin` attempt failed its unchanged
sixty-second worker startup budget during source loading, before model
compilation; its parity is unmeasured. A separate archived-DAE diagnostic and
the controlled actual-worker `component-definition-profile-1` both complete
and produce byte-identical eleven-state traces with SHA-256
`91cd3b96b253f0b05162a29f22564644ef595acece2884f844a37dffafed7184`.
The position array is reconstructed; the three body velocity coordinates are
still integrated. Existing `plot-compare --reuse-traces` compares all 184
trajectory channels against OMC: the maximum bounded channel score is
1.188e-4, below the unchanged high threshold. These diagnostics do not replace
the failed normal attempt or provide cohort coverage credit.

The controlled profile measures Sim at 1.062014 seconds (1.05 seconds user CPU).
This does not establish a meaningful speed gain over the preceding 1.088924
seconds, and remains slower than OMC. Projection Jacobian programs consume
15.60% of sampled CPU self time, manifold Jacobians 12.43%, and typed
directional calls 11.82%; all JIT sample addresses map to generated code.
Worker SHA-256:
`bca1176e41b141f86e3d49c2cd9f56b15ce9128b24012b2f7286bf67351ef5fc`.
The next unresolved proof is reconstruction of the remaining velocity vector
from the no-slip constraints and independent angular motion. The original
scalar-state `TensorContact` still retains three states.

The fixed `multibody-component-definition-canary` retains every phase, status,
and band from `multibody-direct-auxiliary-canary`: nine models compared high,
175 initialization channels high, eleven unchanged refusals, and zero skipped,
missing, nonidentifiable, or deviating results. Receipt:
`rolling-wheel/component-definition-canary-delta.json`; worktree digest:
`b98745ba9ee53b6a18d5d99a9ef2b3501a67c8cc9922b7f0b9fd9995f15edbc1`.
The complete cohort above validates this component reconstruction change.

## Previous complete cohort: auxiliary derivative reconstruction

The complete `target/msl/multibody-direct-auxiliary-full` gate passes at
`ae6dbf5bab8be88da2b203ad38698ab98ef32adf`, with eleven Rumoca workers,
unchanged budgets and tolerances, and the same 566-model roster. It measures
158/566 strict-high models (27.92%), including 22/42 MultiBody examples.
All 158 compared models and all 20,379 initialization channels are high;
seventeen reviewed exclusions remain, with zero missing, nonidentifiable,
or deviating traces. Every previously strict-high model retains its band.

`GyroscopicEffects` completes Solve in 9.963 seconds and simulation in 1.506
seconds, gaining a high comparison where the preceding run timed out in Solve.
This is close to the unchanged ten-second Solve budget; it does not establish
a robust performance margin or attribute the gain to this structural change.
`IMC_Transformer` reaches its existing structural refusal instead of timing out.
The already excluded `ThyristorBridge2Pulse_RLV_Characteristic` times out during
simulation instead of completing: its failure remains visible and accounts for
the exclusion count dropping from eighteen to seventeen, without changing a
comparison band. No coverage claim relies on a retry.

Receipt: `rolling-wheel/direct-auxiliary-full-delta.json`; worktree digest:
`48672dfdec9c3416aac00ca9d56725958329a60236f921974bbf966a09020c35`.
OMC version is `a96aa1a-cmake`. RollingWheel's OMC speed target and combined
quick/full remain outstanding.

## Previous focused work: preserve auxiliary derivatives across reduction rounds

The explicit-height control of `TensorContact` retained three states despite
the existing auxiliary proof determining its contact vector from orientation.
Direct-state derivative preflight did not consume that proof. Once admitted,
height demotion succeeded, but velocity demotion stopped at a generated
`LinearSolve` expression. Supporting its derivative then exposed a Solve
ownership rejection: a bare aggregate solve cannot enter scalar lowering.
The retained RED and intermediate failures are
`rolling-wheel/direct-auxiliary-{red-1,candidate-1,candidate-2}.log`.

Preflight now follows the auxiliary block's exact operand contexts and excludes
the state being demoted. First and second derivatives of a checked linear solve
use the implicit matrix equations on the same nonsingular domain. Reconstructed
solves retain aggregate function owners, generated once per checked Real
matrix/vector signature, with distinct source-bound invocation identities.
SPEC_0007's STRUCT-T03 profile and its SPEC_0040 reconstruction rows govern this
extension; the existing supplied-derivative rule moved into those referenced
rows to keep the parent within its word budget.

The explicit-height control now uses one state and preserves all analytical
channels under both BDF and RK. The reciprocal-motion regression verifies
`q''=2/(1+theta)^3` when `theta'=1`: the matrix and right-hand side have zero
second derivatives, so the mixed term is essential. `AngularRateContact` also
drops from three state declarations to two while preserving its analytical
motion. All eleven contact tests, 162 structural tests, 99 compiler library
tests, 584 core tests, 17 gates, formatting, and structural-package Clippy pass.
The reciprocal fixture's first attempts needed the checked optional `fixed`
attribute and an unfixed algebraic start guess; these were fixture corrections,
not compiler failures or additional coverage.

The normal `target/msl/multibody-direct-auxiliary-origin` gate compares one
RollingWheel trace: all 184 trajectory and initialization channels high, zero
skipped/missing/nonidentifiable/deviating results. Sim measures 1.055705 seconds,
with the same fourteen integrated coordinates and byte-identical trace SHA-256
`f46cbcf4bf00620007139eb17416c2ece8c3ddf5665e577a3c4e22ef2dadc47c`.
This does not establish a meaningful RollingWheel speed improvement. The
controlled `direct-auxiliary-profile-1` attempt expired after 180 seconds in
source loading, before Sim; it provides no profiling measurement.

The fixed `multibody-direct-auxiliary-canary` retains every phase, status, and
band from `multibody-nested-call-sharing-canary`: nine models compared high,
175 initialization channels high, eleven unchanged refusals, and zero
skipped/missing/nonidentifiable/deviating results. Receipt:
`rolling-wheel/direct-auxiliary-canary-delta.json`; worktree digest:
`90691544006cba292c109a5c2d88f90f894836c0920f6d0eb7d1f3ae706ec964`.
The original tensor-form contact model still retains three states. Its missing
component-definition proof, RollingWheel's extra state coordinates, the OMC
speed target, and combined quick/full remain open. The complete cohort above
validates this focused structural change.

## Previous complete cohort: execution optimizations preserve parity

The complete `target/msl/multibody-nested-call-sharing-full` gate passes at
`eb7f32ec05bc98eed453a5ad643f9b777d968e44`, using eleven Rumoca workers and
the unchanged budgets, tolerances, and 566-model roster. It confirms the three
focused optimizations below: 157/566 models remain strict-high (27.74%),
including 21/42 MultiBody examples. The comparator measures 157 models,
retains eighteen reviewed exclusions, and reports zero missing traces,
nonidentifiable traces, or deviating channels. All 19,412 initialization
channels are high. Every model retains its preceding comparison band.

Seven unsuccessful models change phase/status: the six Fluid startup failures
from `multibody-causal-ready-full` reach their existing typed frontend refusals,
while `IMC_Transformer` times out instead of reaching its prior `EL005` refusal.
These are recorded as failures, with no new semantic coverage credit. Receipt:
`rolling-wheel/nested-call-sharing-full-delta.json`; worktree digest:
`48672dfdec9c3416aac00ca9d56725958329a60236f921974bbf966a09020c35`.
OMC version is `a96aa1a-cmake`. The OMC speed target and combined quick/full
remain outstanding.

## Previous focused work: share exact nested source call owners

On top of `0b273fb17391f0ce960e718a4ce20cd1d0670884`, pure-call registration
now reuses completed nested DAE call owners within the same Solve table.
Previously only root calls shared this lookup, expanding a shared source call
graph into an invocation tree. The reduced five-owner example produced seven
owners before the repair. Distinct source calls remain distinct even with
identical names, spans, or bodies; the active-function recursion check still
precedes reuse. Actual arguments, directional seeds, and each invocation's
assertion predicates retain their own execution coordinates. Finishing a table
discards the registration scope. This refines the existing pure-call
construction contract in SPEC_0043 under SOLVE-C51/C52.

All three focused regressions pass, covering increasing graph depth, different
arguments/seeds and predicate outcomes, and table-scope reset. All 315
Solve-phase/evaluator library tests, 99 compiler library tests, 582 core tests,
17 gates, formatting, and affected-package Clippy pass. RED/GREEN evidence is
in `rolling-wheel/nested-call-sharing-{red-1,green-2}.log`.

RollingWheel's canonical call table shrinks from 1,216 to 214 owners. In the
controlled actual-worker profile `nested-call-sharing-profile-1`, native build
time falls from 2.476045721 to 1.516248235 seconds. Sim measures 1.088924 seconds
(1.07 seconds user CPU), versus 1.134402 in the preceding controlled profile;
the trace remains byte-identical with SHA-256
`f46cbcf4bf00620007139eb17416c2ece8c3ddf5665e577a3c4e22ef2dadc47c`.
Worker SHA-256 is
`c8fc7489fda11426c4f43c3d228e3cc08866bc2c7c737f22e1f6e126ac98da6f`.
Solve construction itself does not improve in this measurement. Native
algebraic projection and manifold Jacobian programs still account for 14.30%
and 11.91% of CPU samples, respectively; these are self costs.

The normal `target/msl/multibody-nested-call-sharing-origin` gate measures
Sim at 1.073520 seconds and compares one model: all 184 trajectory and
initialization channels high, with zero skipped, missing, nonidentifiable,
or deviating results. The fixed `multibody-nested-call-sharing-canary` retains
every phase, status, and band from `multibody-typed-small-tensor-canary`:
nine compared models high, 175 initialization channels high, eleven unchanged
refusals, and zero skipped/missing/nonidentifiable/deviating results.
Receipt: `rolling-wheel/nested-call-sharing-canary-delta.json`; worktree digest:
`d928c2253b88de30dbe8b40378311c89483c6bafcba6943c944aeec04e686481`.
The complete cohort above confirms these three execution/construction
optimizations. The OMC speed target and combined quick/full remain outstanding.

## Previous focused work: emit small native tensor arithmetic directly

On top of `6306f702d820717ad506fe705e0bb35253b43158`, typed native function
helpers now use the same bounded direct matrix arithmetic as scalar kernels.
Previously even a 3-by-3 product emitted two counted loops and dynamic tape
addresses. Products costing at most 64 terms now emit ordered multiply/add
operations; short elementwise operations emit at most sixteen elements directly.
Larger operations retain loops. This is final backend emission under
SPEC_0032 §4 and SOLVE-C53: canonical tensor owners, call identities, and
floating-point accumulation order stay unchanged.

An isolated million-call 3-by-3 diagnostic improves from 86.9 to 51.8 ms;
the temporary throughput probe is removed. The two permanent regressions cover
vector/matrix orientations, both sides of the emission-size boundary,
cancellation-sensitive accumulation, and changing directional seeds. All 86
native backend tests, affected-package Clippy, 99 compiler library tests,
582 core tests, and 17 gates pass.

The controlled actual-worker profile `rolling-wheel/typed-small-tensor-profile-2`
measures Sim at 1.134402 seconds versus 1.347903 before this change (15.8% lower),
with 1.12 seconds of user CPU. DAE and canonical Solve artifacts are byte-identical
to the prior profile, as is the trace (SHA-256
`f46cbcf4bf00620007139eb17416c2ece8c3ddf5665e577a3c4e22ef2dadc47c`). Worker
SHA-256 is `2f61244a39e14ed4b7c1f0c51d2b0a68e080dc28f4d6285e408fae7e39675c10`.
The first controlled profile reached its 180-second outer limit during source
loading and measured no simulation; those artifacts remain retained.

The normal `target/msl/multibody-typed-small-tensor-origin` run measures Sim
at 1.142239 seconds. Its first comparator step lacked OMC on PATH; using the
pinned `nix develop .#modelica` shell compares the existing trace without
rerunning Rumoca. One model is compared, all 184 trajectory and initialization
channels high, with zero skipped/missing/nonidentifiable/deviating results.
The fixed `multibody-typed-small-tensor-canary` preserves every phase, status,
and band from `multibody-manifold-selection-canary`: nine compared models high,
175 initialization channels high, eleven unchanged refusals, and zero
skipped/missing/nonidentifiable/deviating results. Receipt:
`rolling-wheel/typed-small-tensor-canary-delta.json`; worktree digest:
`ad3a2ce8142fe8847c6f41d663005a5b9601ea229c7727364b87f8a00fee0608`.
The OMC speed target and combined quick/full remain outstanding. The latest
complete cohort below predates these two native execution optimizations.

## Previous focused work: select manifold derivative programs

RollingWheel's three independent manifold blocks previously replayed all six
directional programs for every state direction and discarded unrelated rows.
Solve construction now binds each checked sparsity color to its exact program
outputs, including shared tensor outputs. Execution calls each selected program
once per color. Full residual certification and atomic state correction remain
unchanged; native failures propagate without retrying the complete block.
The governing owners are SPEC_0007's Solve contract, SPEC_0032's tensor ownership,
and SPEC_0036/0039's constructed output and sparsity proofs.

The reduced independent-block test fails before the repair: it observes four
program executions where the two constraints require two
(`manifold-selection-red-2.log`). Thirteen focused regressions pass afterward,
covering permuted output placements, shared tensor programs, changing parameters,
native/reference execution, and rollback after a later block fails. All 982
IR/evaluator/solver library tests, 99 compiler library tests, 582 core tests,
17 gates, formatting, and affected-package Clippy pass.

The controlled actual-worker Sim profile falls from 1.498229 to 1.347903 seconds
(10.0%); the trace remains byte-identical with SHA-256
`f46cbcf4bf00620007139eb17416c2ece8c3ddf5665e577a3c4e22ef2dadc47c`.
The normal `target/msl/multibody-manifold-selection-origin` gate measures Sim
at 1.356090 seconds and compares one model: all 184 trajectory and initialization
channels are high, with zero skipped, missing, nonidentifiable, or deviating
results. This is still slower than OMC; differing timing boundaries preclude
equating these measurements to OMC's reported integration-only time.

The fixed `target/msl/multibody-manifold-selection-canary` retains every phase,
status, and agreement band from `multibody-causal-ready-canary`: nine models
compared high, all 175 initialization channels high, eleven unchanged refusals,
and zero skipped/missing/nonidentifiable/deviating results. The receipt is
`rolling-wheel/manifold-selection-canary-delta.json`, worktree digest
`a38f20cd00a5bed30dc189c4850eac38882b424ba6eab04ebbfd064d39610004`.
Profile artifacts are in `rolling-wheel/manifold-selection-profile-1`; worker
SHA-256 is `e2f9cd7b5dd61591b47bd0d8709fe0f1561b298e486d8193abb825b2d6454b60`.
This completes focused/canary validation. The latest complete cohort below
predates this optimization; combined quick/full and the OMC speed target remain
outstanding.

## Previous focused work: bound construction cost

The `RollingWheelSetDriving` Solve timeout below was traced to repeated
operation-prefix scans and repeated searches through causal candidates. A
compact, source-bound destination-range index now supplies assignment producer
lookups; a dependency queue preserves the exact earliest-ready causal order.
Neither transformation expands tensor coordinates or changes numerical order.
The normal `target/msl/multibody-causal-ready-origin` gate compares one model:
all 892 trajectory and 892 initialization channels are high, with zero skipped,
missing, nonidentifiable, or deviating results. Solve takes 8.149 seconds under
the unchanged 10-second budget; Sim takes 1.206 seconds. This restores the
originating focused case; the complete cohort confirmation is recorded below.

The fixed `target/msl/multibody-causal-ready-canary` has no phase, status, or
band changes against `multibody-pure-input-reuse-canary`: nine models compared
high, all 175 initialization channels high, zero skipped/missing/nonidentifiable
or deviating results, and eleven unchanged refusals. The receipt is
`rolling-wheel/causal-ready-canary-delta.json`, worktree digest
`8681bf2a90c29ab9a8f0a860f95a5f9b783baef09a95fed4fc82a76ce6388ca2`.
The 519 IR/Solve/native and 162 structural library tests pass. Formatting,
all-target/all-feature Clippy for the five affected compiler/runtime crates,
and `rumoca` library/core/gate suites pass. The first Clippy attempt rejected
a nested test closure; extracting its linear reference lookup fixed that lint.

The new controlled RollingWheel Sim profile measures 1.498 seconds and retains
the identical trace. It collects 1,490 CPU samples with none lost. Native
manifold Jacobian rows account for 14.37% of samples, algebraic projection rows
12.44%, and the full implicit Jacobian 1.75%; typed-call helpers add further
work. These are self costs, not inclusive estimates. The OMC speed target
remains unmet.

## Latest focused work: reuse geometry without changing the trace

On top of `0dc3858bd6f0a8864f354df4560157395634c192`, scalar Solve construction
and AD share repeated unary arithmetic within each exact context. Native pure
calls retain a result only under a construction-issued complete input
coordinate; every input bit, including directional seeds, must match.
Failure and assertion behavior remain covered by regressions.

RollingWheel's focused Sim falls from 2.164 to 1.568 seconds. The separate
actual-worker profile falls from 2.225 to 1.506 seconds, with an identical
trace. Trig calls fall from 32,623,972 to 5,187,414. The normal focused gate
`target/msl/multibody-pure-input-reuse-origin-2` compares one model with all
184 trajectory and 184 initialization channels high, and zero skipped,
missing, nonidentifiable, or deviating results. The first attempt hit a
source-loading startup limit and measured no parity; its artifacts are kept.

The fixed `target/msl/multibody-pure-input-reuse-canary` has no phase, status,
or band deltas against `target/msl/multibody-auxiliary-primal-canary`: nine
models compared high, 175 initialization channels high, no skipped/missing/
nonidentifiable/deviating results, and eleven unchanged refusals. Worktree
digest: `e333300748c075ba3708f0749572e75e452536c98726a4e6d610e4603c73b8dc`.
The exact receipt is `rolling-wheel/pure-input-reuse-canary-delta.json`.

The OMC performance gap remains open. Details and reduced RED/GREEN evidence are in the
[generated-kernel comparison](rolling-wheel-kernel-comparison.md).

## Latest complete measurement

`target/msl/multibody-causal-ready-full` passes at commit
`4585575a610fd989625d2cd12fa63df398c70247`. The complete 566-model comparison
measures **157/566 strict-high (27.74%)**: 157 models compared, eighteen
reviewed exclusions skipped, zero missing/nonidentifiable traces, and zero
deviating channels. All 19,412 initialization channels are high. **MultiBody is
21/42 high**. Every previously high model stays high; `RollingWheelSetDriving`
regains high parity under the unchanged Solve budget. The sole band change is
that restoration. Ten phase/status rows change: six unpassed Fluid cases now
report harness failures, and three existing simulation refusals reach typed
structural failures instead of timing out. Those changes earn no parity credit.

The gate takes 463.64 seconds with eleven Rumoca simulation workers; the OMC
reference/comparison stage takes 313.85 seconds. Host I/O pressure was substantial
during trace handling, so this gate duration is not a solver benchmark. The
per-model receipt is `rolling-wheel/causal-ready-full-delta.json`; comparator
SHA-256 is `ba7faf4f96e4f2c38e1a8c76ef10ddb4ccc0cda74e35f2652690c50b7e098e53`.
Worktree digest is
`48672dfdec9c3416aac00ca9d56725958329a60236f921974bbf966a09020c35`;
the preserved untracked communication file accounts for the dirty marker.
Combined quick/full and the OMC speed target remain outstanding.

## Previous complete measurement: exact pure-call input reuse

`target/msl/multibody-pure-input-reuse-full` passes the checked-in quality
gate at commit `716bba8bef01f04259655e88dcb0e6112b66011a` in 185.80 seconds.
The comparator measures **156/566 strict-high (27.56%)**: 156 models compared,
eighteen reviewed exclusions skipped, zero missing/nonidentifiable traces,
and zero deviating channels. All 18,520 initial-condition channels are high.
MultiBody remains **20/42 high**, with a changed membership.

Against `multibody-seed-linearization-full`, `Rectifier`,
`CompareTransformers`, and `DoublePendulumInitTip` advance from refusal to
strict-high. `RollingWheelSetDriving` loses its previous strict-high result:
the worker exceeds the 10-second Solve construction budget before simulation.
The aggregate gain does not excuse that regression. The newer complete run above
confirms its repair; no baseline promotion is claimed. Fourteen models change
phase/status details in total. The exact
receipt is `rolling-wheel/pure-input-reuse-full-delta.json`.

The worktree digest is
`48672dfdec9c3416aac00ca9d56725958329a60236f921974bbf966a09020c35`;
the comparator artifact SHA-256 is
`dbc9374983f1918a29fb78850d05593a7a3becee194c72d0c000789727fe7b56`.
The tracked source was committed; the preserved untracked communication file
accounts for the dirty-worktree marker. Combined quick/full remain pending.

## Previous complete measurement: shared seed linearization

`target/msl/multibody-seed-linearization-full` passes at commit
`5fbcc036d50f1124b99069797a64a41fcd500d00`, working-tree digest
`48672dfdec9c3416aac00ca9d56725958329a60236f921974bbf966a09020c35`.
The full 566-model gate takes 153.51 seconds with eleven Rumoca workers.
All **154 compared models remain strict-high (27.21%)**, with eighteen unchanged
reviewed exclusions skipped, zero missing/nonidentifiable traces, and zero
deviating channels. All 18,693 initial channels remain high. **MultiBody stays
20/42 high**. Every model retains its previous phase outcome and agreement band;
the exact delta is `rolling-wheel/seed-linearization-full-delta.json`.

The isolated profile measures RollingWheel Sim at 1.248 seconds, versus 1.309
before fixed-coordinate matrix reuse, with a byte-identical trace. The critical
OMC performance gap remains open. Combined `verify quick` and `verify full`
remain pending; no PR, release, or baseline promotion is claimed.

The next structural investigation compares the requested and selected state
coordinates. Flat and the checked DAE retain `StateSelect.always` on
`wheel1.x`, `wheel1.y`, `wheel1.angles`, and `wheel1.der_angles`, but classify
these eight coordinates as algebraic. Rumoca integrates twelve other
coordinates; OMC integrates those eight requested coordinates. This establishes
a selection difference, not yet its causal contribution to the runtime gap.
The 21-variable dynamics block versus OMC's six-variable block remains a
related lead, recorded in `rolling-wheel/algebraic-kernel-next-target.json`.

## Previous complete measurement: on-demand derivatives

`target/msl/multibody-on-demand-derivatives-full` passes at commit
`c1145fa9a50e0af44e4bc4ac02c74a5317bd14f9`, working-tree digest
`48672dfdec9c3416aac00ca9d56725958329a60236f921974bbf966a09020c35`.
The full 566-model gate takes 137.07 seconds with eleven Rumoca workers.
All **154 compared models remain strict-high (27.21%)**, with eighteen unchanged
reviewed exclusions skipped, zero missing/nonidentifiable traces, and zero
deviating channels. All 18,693 initial channels remain high. **MultiBody stays
20/42 high**. Every model retains its previous phase outcome and agreement band;
the exact delta is `rolling-wheel/on-demand-derivatives-full-delta.json`.

The isolated profile below improves RollingWheel Sim from 1.640 to 1.309 seconds
with a byte-identical trace. The critical OMC performance gap remains open.
Combined `verify quick` and `verify full` remain pending; no PR, release, or
baseline promotion is claimed. Next: repeated algebraic matrix work at fixed
directional-derivative coordinates and source-backed elimination in the dynamics
block. `rolling-wheel/algebraic-kernel-next-target.json` records the comparison
with OMC's six-variable system: Rumoca retains 21 unknowns, and its existing
tearing only reduces that to fourteen. That is a structural lead, not proof that
changing the solve dispatch preserves the earlier electrical fixes.

## Previous complete measurement: deferred observation evaluation

`target/msl/multibody-deferred-observation-full` passes at commit
`9639fad96b733e94fb4aadc454cdb04e0eedcf55`, working-tree digest
`48672dfdec9c3416aac00ca9d56725958329a60236f921974bbf966a09020c35`.
The full 566-model gate takes 152.48 seconds with eleven Rumoca workers.
All **154 compared models remain strict-high (27.21%)**, with eighteen existing
reviewed exclusions skipped, zero missing/nonidentifiable traces, and zero
deviating channels. All 18,693 initial channels remain high. **MultiBody stays
20/42 high**; every preceding high model is retained.

RollingWheel takes 1.657 seconds in concurrent Sim and RollingWheelSetDriving
takes 2.060 seconds. The isolated RollingWheel profile below establishes the
performance improvement separately. ThyristorBridge2Pulse_RLV_Characteristic
now completes in 11.181 seconds under the unchanged budget; its pre-existing
exclusion concerns insufficiently converged OMC period-aggregate reference
values, so it earns no high-parity credit. This accounts for the eighteenth
skipped completion. Inverse_sh_TX returns its previous Flatten refusal; no model
changes agreement band. Exact deltas are in
`rolling-wheel/deferred-event-left-full-delta.json`.

All seventeen repository inspection gates also pass, including spec budgets,
source citations, and production file-size checks
(`deferred-event-left-repo-gates-1.log`).

The critical RollingWheel runtime gap remains open. Combined `verify quick`
and `verify full` remain pending; no PR, release, or baseline promotion is
claimed. The next measured target is proactive completed-step derivative
evaluation, followed by repeated algebraic linearization work.

## Previous complete measurement: grouped Jacobian outputs

`target/msl/multibody-grouped-jvp-full` passes at commit
`2c637488b3c88703d4f729eb30f7d6453b58543e` (compiler change `b51acb48`),
working-tree digest
`48672dfdec9c3416aac00ca9d56725958329a60236f921974bbf966a09020c35`.
The full 566-model gate takes 147.38 seconds with eleven Rumoca workers.
All **154 compared models remain strict-high (27.21%)**; seventeen unchanged
reviewed exclusions remain skipped, with zero missing/nonidentifiable traces
and zero deviating channels. All 18,693 initial channels remain high.
**MultiBody remains 20/42 high**, and every previous high model is retained.

RollingWheel takes 2.209 seconds in concurrent Sim; RollingWheelSetDriving
takes 2.803 seconds. The isolated RollingWheel profile below remains the
performance evidence, and its critical OMC gap remains open. No model changes
agreement band. Three absent models change failure classification:
Inverse_sh_TX reaches DAE construction refusal, BevelGear1D reports a runtime
contract failure instead of timeout, and GenerationOfFMUs returns its previous
structural refusal. Exact deltas are in
`rolling-wheel/grouped-jvp-full-delta.json`.

Combined `verify quick` and `verify full` remain pending. The immediate priority
is RollingWheel execution cost; no coverage expansion, PR, release, or baseline
promotion is claimed.

## Previous complete measurement: affine seed removal

`target/msl/multibody-affine-seed-full` passes the local fallback gate at commit
`9fb1b2476977ea27225287751e2d8adbb4afafea`, working-tree digest
`48672dfdec9c3416aac00ca9d56725958329a60236f921974bbf966a09020c35`.
The full 566-model comparison takes 152.98 seconds with eleven requested,
admitted, and pinned Rumoca workers. All **154 compared models remain
strict-high (27.21%)**, with seventeen unchanged reviewed exclusions, zero
missing/nonidentifiable traces, and zero deviating channels. All 18,693
initial channels remain high. Every prior high model, including all nine
electrical counterexamples and DCPM_Cooling, is retained; **MultiBody remains
20/42 high**.

RollingWheel retains **184/184 high channels** and the same maximum channel
bounded normalized L1 `1.0279228736061218e-4`. Concurrent Sim takes 2.394
seconds, with 3.887 seconds of separate source/Solve/backend preparation.
RollingWheelSetDriving retains 892/892 high channels and takes 4.385 seconds
in Sim. These cohort timings are not isolated benchmarks; the clean profile
pair below measures the seed-removal change separately.

No model changes agreement band. GenerationOfFMUs remains absent: this run
hits the unchanged ten-second Solve budget before returning the previous
structural `EL005` refusal. That phase outcome is retained in
`rolling-wheel/affine-seed-full-delta.json`; it earns no coverage credit.
The complete MSL gate is green, but the critical RollingWheel performance
gap remains open. Combined `verify quick` and `verify full` remain pending
while that compiler performance work continues. No PR, release, or baseline
promotion is claimed.

## Previous complete measurement: affine tensor functions

`target/msl/multibody-affinity-full` passes the local fallback gate at commit
`aecb01dc7038c7cedede5fe6dbc85c62b0c3c74e`, working-tree digest
`48672dfdec9c3416aac00ca9d56725958329a60236f921974bbf966a09020c35`.
The complete 566-model run takes 139.77 seconds with eleven requested,
admitted, and pinned workers. All **154 compared models remain strict-high
(27.21%)**, with seventeen unchanged reviewed exclusions, zero missing or
nonidentifiable traces, and all 18,693 initial channels high. There are zero
deviating trajectory channels. Every prior high model, including the nine
electrical counterexamples and DCPM_Cooling, remains high. **MultiBody stays
20/42 high**.

RollingWheel's changed trace passes with **184/184 channels high**, maximum
channel bounded normalized L1 `1.0279228736061218e-4`. Its concurrent Sim time
is 3.597 seconds; source/Solve/backend preparation is separately 3.722 seconds.
RollingWheelSetDriving retains 892/892 high channels and takes 4.850 seconds
in Sim. No model changes band. The sole phase change is GenerationOfFMUs,
which remains absent and returns its earlier structural `EL005` refusal rather
than the preceding run's Solve-stage timeout. The complete delta is
`rolling-wheel/affinity-full-delta.json`.

The isolated profile pair below supports the RollingWheel improvement; this
cohort timing is not a head-to-head benchmark. The critical runtime gap remains
open. Combined `verify quick` and `verify full` have not run for this change;
no PR, release, or baseline promotion is claimed.

## Previous complete measurement: prepared algebraic refresh

`target/msl/multibody-prepared-stages-full` passes the local fallback gate at
commit `440f4466fec6987eb1cbcb6c68f34500566319ae`, working-tree digest
`48672dfdec9c3416aac00ca9d56725958329a60236f921974bbf966a09020c35`.
The complete 566-model run takes 196.38 seconds with eleven requested and
admitted workers; the OMC reference/comparison stage accounts for 91.87 seconds.

All **154 compared models are strict-high (27.21% of 566)**, with seventeen
unchanged reviewed exclusions, zero missing/nonidentifiable traces, and all
18,693 initial channels high. There are zero deviating trajectory channels.
Every previous high model remains high, including the nine electrical
counterexamples. **MultiBody remains 20/42 high**.

`DCPM_Cooling` advances from a simulation timeout to 7.911 seconds in Sim,
with all 195 channels high and maximum channel bounded normalized L1
`1.518715114241044e-5`. This is the only band change. The other phase change is
`GenerationOfFMUs`, which remains absent: its previous structural refusal is
replaced by a ten-second Solve-stage timeout in this run. The complete delta is
`rolling-wheel/prepared-stages-full-delta.json`.

RollingWheel takes 4.556 seconds in Sim and RollingWheelSetDriving 5.015 seconds;
all 184 and 892 respective channels remain high. The clean isolated RollingWheel
pair below remains effectively unchanged at 4.283 versus 4.303 seconds. The
critical OMC runtime gap remains open. Combined `verify quick` and `verify full`
have not run for this change; no PR, release, or baseline promotion is claimed.

## Previous complete measurement: selected native residual execution

`target/msl/multibody-native-residual-full` passes the local fallback gate at
commit `a2cd4deae1c50e9650c0ca3124452c0d72013b2b`, working-tree digest
`48672dfdec9c3416aac00ca9d56725958329a60236f921974bbf966a09020c35`.
The complete 566-model run takes 120.83 seconds with eleven requested and
admitted workers. OMC reuses all 170 existing reference results.

All **153 compared models remain strict-high (27.03% of 566)**, with seventeen
unchanged reviewed exclusions, zero missing/nonidentifiable traces, and all
18,498 initial channels high. There are zero deviating trajectory channels.
All 566 phase/simulation outcomes and bands match the native-manifold full run;
all nine electrical counterexamples retain their high bands. MultiBody remains
**20/42 high**. `rolling-wheel/native-residual-full-delta.json` records the
complete comparison and artifact hashes.

RollingWheel takes 4.380 seconds in Sim and RollingWheelSetDriving 4.927 seconds;
build times are separately 4.188 and 10.873 seconds. The isolated profile pair
below establishes the selected-residual dispatch improvement. The OMC gap
remains a critical runtime bug. The fixed 145-model median speed ratio is 2.635
and the expanded 170-model median is 1.730; this concurrent run with cached OMC
timings does not establish an isolated cohort speedup. Combined `verify quick`
last passed before ten subsequent implementation changes; `verify full` has
not run. No PR, release, or baseline promotion is claimed.

## Previous complete measurement: native manifold execution

`target/msl/multibody-native-manifold-full` passes the local fallback gate at
commit `55acfbbdbef06c7d3b86820ce3e2b71deaa5cf40`, working-tree digest
`48672dfdec9c3416aac00ca9d56725958329a60236f921974bbf966a09020c35`.
The complete 566-model run takes 133.14 seconds with eleven requested and
admitted workers. OMC reuses all 170 existing reference results.

All **153 compared models remain strict-high (27.03% of 566)**, with the same
seventeen reviewed exclusions, zero missing/nonidentifiable traces, and all
18,498 initial channels high. There are zero deviating trajectory channels.
All 566 phase/simulation outcomes and bands match the preceding full run;
all nine electrical counterexamples retain their high bands. MultiBody remains
**20/42 high**. `rolling-wheel/native-manifold-full-delta.json` records the
complete comparison and artifact hashes.

RollingWheel takes 5.215 seconds in Sim and RollingWheelSetDriving 5.423 seconds;
build times are separately 4.375 and 10.568 seconds. The isolated profiles below
establish the native-manifold dispatch improvement. The remaining OMC gap is
still a critical runtime bug. The fixed 145-model median speed ratio is 2.164
and the expanded 170-model median is 1.463; this concurrent run with cached OMC
timings does not establish an isolated cohort speedup. Combined `verify quick`
last passed before nine subsequent implementation changes; `verify full` has
not run. No PR, release, or baseline promotion is claimed.

## Previous complete measurement: state-only manifold projection

`target/msl/multibody-state-only-manifold-full` passes the local fallback gate
at commit `f485ef13f2e3a79c9ae136c193eb3560f05114dc`, working-tree digest
`48672dfdec9c3416aac00ca9d56725958329a60236f921974bbf966a09020c35`.
The complete 566-model sweep takes 175.94 seconds. Eleven Rumoca workers were
requested; available memory limited execution to ten. OMC reuses 169 cached
results and runs one new model in one persistent session. The run metadata's
14-worker OMC setting describes its configured budget, not fourteen active
new simulations.

**All 153 compared models are strict-high (27.03% of 566)**, with seventeen
reviewed exclusions, zero missing/nonidentifiable traces, and all 18,498 initial
channels high. No compared trajectory channel deviates. All nine electrical
counterexamples retain their high bands. **MultiBody returns to 20/42 high**;
RollingWheelSetDriving is the only phase/simulation and band change from
`multibody-singleton-degree-full`. Every preceding high model remains high.
The delta, electrical checks, and artifact hashes are in
`rolling-wheel/state-only-manifold-full-delta.json` under the campaign directory.

| Model | Previous full simulation | Current full simulation | Current trajectory parity |
|---|---:|---:|---|
| RollingWheelSetDriving | Timeout at 12 s | 6.741 s | 892/892 channels high |
| RollingWheel | 9.234 s | 6.407 s | 184/184 channels high |

The wheel-set build still takes 10.708 seconds, separately from simulation.
Its complete trace is byte-identical to the earlier singleton-degree diagnostic
(`3dc2333f4b9d6d69bead292633f3304aa57204f430040d50c00fb62753006a5b`),
with maximum channel bounded normalized L1 `5.061492181479045e-6`.
RollingWheel's trace changes slightly and remains all-high, maximum channel
bounded normalized L1 `1.1053709819053231e-4`. OMC's cached RollingWheel
simulation timing is 0.134 seconds: substantial runtime overhead remains.
The fixed 145-model performance cohort median is 2.585 versus 2.245 before;
the expanded 170-completion median is 1.688. Memory pressure, concurrency,
and cached OMC timings prevent interpreting this pair as an isolated speedup.

The runtime repair and its focused/canary checks are complete. The remaining
22 MultiBody models still need investigation. Combined `verify quick` last
passed before eight subsequent implementation changes; `verify full` has not
run. No PR, release, or baseline promotion is claimed.

## In progress: requested states and repeated tensor reduction

The candidate based on `eb325ac7173ddd0757783f6c3ed5cc77e894c251`
promotes continuous Real declarations marked `StateSelect.always` through one
checked whole-DAE reconstruction before matching. MLS §4.8.7.1 and STRUCT-T07
govern this selection; SPEC_0032 governs preservation of tensor owners.
The initial reduced alias model selects `q[1:2]` before the change and `x[1:2]`
after it, agreeing with OMC. Output causality, parameter roles, assertions, and
contradictory fixed initial values have focused regressions.

Promotion alone regressed RollingWheel to structural refusal
(`rolling-wheel/requested-states-origin-1`). The actual reduction recorder
shows the demoted path reaching one unmatched `der(wheel1.y)`. A previous
component replacement had wrapped the position residual in `ArrayUpdate`;
the next component proof did not read through that checked update to its
untouched y component. The pristine retry was less reduced and is not evidence
that the earlier angular-state demotion failed.

The six-equation `SelectedComponents` regression reproduces the same gap with
`der(p)=-p; p={x,y,z}` and requested x/y states. It fails structural preparation
before the component-selection repair (`requested-components-red-1.log`).
Following the exact literal, full-rank update selection makes RollingWheel
structurally regular (`requested-states-reduction-probe-2.log`). The prepared
DAE retains all eight requested coordinates plus body velocity and joint
position tensors: fourteen integrated coordinates versus OMC's eight.
Complete elimination of these dependent coordinates remains open.

The reduced model then exposed Solve derivative extraction failing on the
updated tensor (`requested-components-green-1.log`). Reusing its existing
constant update selector repairs that boundary. Both integrators now reproduce
all six analytic channels (`requested-components-green-2.log`). OMC also
simulates this exact source, selecting x, y, and p[3]; its maximum analytic
channel error is below 1e-5 (`requested-components-omc-1`).

The next actual-model gate, `target/msl/multibody-requested-components-origin`,
still fails Solve and compares zero models. Differentiating the first no-slip
constraint produces a sum of products equal to zero, but the derivative
extractor required a subtractive root. A second reduced model,
`der(q)=-q; 0=x*q+1`, with x requested, reproduces that error
(`requested-product-red-1.log`). Passing the complete residual to the existing
scalar affine proof makes both integrators agree with its analytic solution
(`requested-product-green-1.log`).
OMC also simulates the exact product fixture successfully
(`requested-product-omc-1/comparison.json`).

RollingWheel remains refused after this repair
(`requested-components-lowering-probe-3.log`). The equivalent dot-product
fixture `0={x,1}*{q,1}` also reproduces this refusal
(`requested-dot-product-red-1.log`). Inspecting the actual prepared BLT shows
a more fundamental problem: block 484 contains three angular accelerations and
21 algebraics (`requested-state-blocks-probe-1.log`). Solve drops the derivative
entries from the algebraic projection and then tries to isolate those
derivatives separately. A scalar pivot need not be invertible even when this
joint system is regular.

The proposed STRUCT-T09 normalization introduces exact whole-tensor algebraic
aliases for implicit derivatives, substitutes their typed coordinate reads,
and appends `der(state)=alias`. The joint equations then remain one compiled
algebraic system. MLS Appendix B and §8.3.1 justify the equivalent extension;
§8.6 initialization obligations and existing runtime rank/convergence checks
remain required. No coefficient is estimated by subtracting residual samples.
The normalization is implemented and its originating-model gate now completes:
`target/msl/multibody-requested-aliases-origin` compares one model, with all 184
channels and initialization channels high and zero skipped, missing,
nonidentifiable, or deviating traces. This focused result is not a new cohort
measurement. Its isolated Sim time is 2.324479 seconds, slower than the prior
1.248-second candidate. The generated-C comparison and diagnostic counters
identify substantial repeated geometry evaluation; see
[RollingWheel kernel comparison](rolling-wheel-kernel-comparison.md).

The new mixed derivative/dot-product reproduction fails before normalization
(`requested-mixed-dot-red-1.log`) and passes both integrators afterward
(`requested-aliases-green-2.log`). The same source also passes OMC, with maximum
analytic invariant error below 4e-6 (`requested-mixed-dot-omc-1/comparison.json`).
All six requested-state regressions pass (`requested-aliases-selected-tests-2.log`).
Broader validation exposed native tensor linear-solve, explicit derivative
family, and scalar affine paths being unnecessarily replaced by aliases. The
selection now retains those owners, using exact matching and compact family
bodies. Outside mixed BLT blocks, admission requires a derivative consumed
through a tensor expression. Scalar-only equations keep their existing affine
construction and rejection checks. No existing regression was weakened.

All 961 library/core tests pass (`requested-aliases-libraries-4.log`), as does
all-target/all-feature Clippy for structural, Solve, and the public compiler
crate (`requested-aliases-clippy-1.log`). Temporary compiler probes were removed.
The fixed canary `target/msl/multibody-requested-aliases-canary` has no phase,
simulation-status, or comparison-band changes against
`target/msl/multibody-requested-states-canary`: nine compared models are high,
all 175 compared initialization channels are high, and skipped, missing,
nonidentifiable, and deviating counts are zero. The eleven remaining targets
retain their prior refusals. The candidate worktree digest is
`3dd6c3edbc76ce9f91b6cfb4c4f3203d91c8e29e72c84e93d42cd187b231dc39`;
`requested-aliases-canary-delta.json` records the exact comparison and artifact
digests. This is Tier 1 regression evidence only.
The final originating-model rerun after native-owner preservation,
`target/msl/multibody-requested-aliases-final-origin`, also compares one model
with all 184 channels and 184 initialization channels high, zero skipped,
missing, nonidentifiable, or deviating traces, and Sim time 2.358592 seconds.

Combined quick/full and a new full cohort gate remain pending while the
RollingWheel kernel work continues. The original-model refusal is closed by
the focused comparison; the performance regression remains open.
Private logs in this section are under `.git/multibody-campaign/rolling-wheel/`.

## Inspect the actual structurally transformed equations

The worker wrote the pre-transform input into `ir-structural-dae.json` and
`ir-structural-dae.mo`. This made an apparent DAE-to-DAE comparison compare the
same input twice. The build observer now borrows the immutable
`LoweredSolveModel` pair already retained by phase-solve and serializes its
`prepared_dae()`. No structural pass is repeated, and no numerical program or
simulation calculation changes. A failed lowering leaves no structural artifact;
recognized prior structural files are cleared before a new request.

The focused source model `der(x)=v; der(v)=a; v=-x`, with `x` initially fixed,
reduces from two states to one. Before the fix its structural artifact still
names both (`structural-artifact-red-2.log`); afterward the artifact and Solve
both retain only `x`. The first RED attempt was a fixture compilation error.
A second regression checks that failed reduction leaves neither prior nor
mislabelled structural products while preserving unrelated files. Its initial
fixture failed too early in ToDae; the corrected balanced singular fixture
reaches the intended structural failure (`structural-artifact-green-2.log`).

All 186 simulation, worker, parameter-override, and legacy simulation-worker
tests pass (`structural-artifact-libraries-1.log`). The obsolete import noted
by that run was removed; all-target/all-feature Clippy for the four affected
crates and rustfmt pass (`structural-artifact-clippy-1.log`). The fixed
`multibody-structural-artifact-canary` retains all twenty phase and band outcomes
against `multibody-seed-linearization-canary`: nine compared models and 175
initial channels high, zero skipped/missing/nonidentifiable/deviating traces.
Source: `40a22015` plus digest
`905e884225824a8e240af7db6c6b1d69b028a79547ac099698336b15c893642a`;
receipt: `rolling-wheel/structural-artifact-canary-delta.json`.

`structural-artifact-origin-1` records the actual RollingWheel transformation:
3,199 input expressions become 4,024, with all 255 continuous equation owners
retained. The body-frame position changes from state to algebraic; the twelve
remaining state coordinates and all eight unselected `always` coordinates are
now verified in the transformed DAE. Canonical Solve JSON and the complete trace
remain identical to the preceding profile. Trace SHA-256:
`61e23f642f39489afa0717c767501558972f2e29419b77b088befbe8b2aff3dd`.
The diagnostic profile takes 1.265 seconds in Sim with 237 samples; this is no
performance improvement. Worker SHA-256:
`dc0a892657723018dfdc4685e32a549ca7cf1dd97debe5c94ff3c1fc29f28019`.

The reduced `PreferredTensorState` model has `x=q; der(q)=-q`, with whole
array `x[2]` marked `StateSelect.always` and `q[2]` marked `avoid`. Rumoca keeps
`q` as its state array through structural lowering; OMC selects `x[1:2]`.
This rules out lost parameter binding or a MultiBody-specific cause. The
[MLS StateSelect contract](https://specification.modelica.org/maint/3.6/class-predefined-types-and-declarations.html#stateselect)
and SPEC_0007/STRUCT-T07 place the missing selection in structural lowering.
No state-promotion fix or causal runtime benefit is claimed yet. Source, actual
stage dumps, OMC generated equations, and the next proof obligation are retained
in `rolling-wheel/state-selection-diagnosis.json`. These small diagnostic runs
are not cohort parity or comparative performance measurements; their process
executions briefly overlapped, so their timings are not used for comparison.

## Retain algebraic linearizations across directional seeds

The state-only Jacobian path already retains its settled algebraic coordinate,
but `project_algebraic_seed_with_plan_inner` rebuilt and factorized every
selected block for every direction. Its Y/P/time point is unchanged across
successive seeds. The focused nonlinear tensor-output regression records a
second matrix evaluation for the second seed (`seed-linearization-red-3.log`).
The preceding RED attempts exposed an incomplete fixture and a counter that
missed the scalar reverse path; neither is the behavioral reproduction.

Derived structural construction now certifies repeatability from the complete
primal and both directional output catalogs, including otherwise discarded
effects. The runtime retains the same block matrix, row scales, and LU for
that exact complete Y/P/time coordinate. Constructor-issued block indexes keep
the matrices attached to their original systems. Each requested seed still
propagates its own right-hand side and passes the original residual check.
Coordinate changes invalidate retained matrices, failure clears the entries and
restores the caller's seed, and cloned runtimes start with empty numerical
caches. The model's table/call context is immutable. Neither canonical programs
nor tensor structure changes; SPEC_0038 records the numerical reuse condition.

Regressions cover state and algebraic values, parameter and time changes,
adjacent state bits, signed-zero time, parameter seeds, a singular coordinate,
missing construction proofs, clone isolation, and impure or ambiguous primal/JVP
output owners. All 1,110 tests across Solve IR, evaluation, solver, simulation,
BDF, and RK45 pass (`seed-linearization-libraries-1.log`), together with their
all-target/all-feature Clippy (`seed-linearization-clippy-1.log`).
All seventeen repository inspection gates pass
(`seed-linearization-repo-gates-2.log`); the first pass caught SPEC_0038 twelve
words over budget, resolved by shortening the new rule without changing it.

`multibody-seed-linearization-canary` preserves all twenty phase and band
outcomes against `multibody-on-demand-derivatives-canary`. Its nine compared
models and 175 initialization channels remain high, with no skipped, missing,
nonidentifiable, or deviating traces. Source: `24c1d944` plus digest
`7f0bb9aae11fcef6efc474ae27774035517cde8c1be898636d9a512d69ffe1d4`;
the exact delta is `rolling-wheel/seed-linearization-canary-delta.json`.

The isolated `critical-seed-linearization-profile-1` measures RollingWheel Sim
at 1.248 seconds against 1.309 previously; user CPU decreases from 1.30 to 1.23
seconds. The roughly 4.7% change is a single-pair measurement. The complete trace
is byte-identical, SHA-256
`61e23f642f39489afa0717c767501558972f2e29419b77b088befbe8b2aff3dd`,
and canonical Solve JSON is unchanged. Worker SHA-256:
`7b4d386c05122e0f57dbacb1085b4ff495d361ec2fe80e28d00267820bbd49db`.
The profile retains 236 CPU samples with zero lost samples. Residual/JVP program
223 and trigonometric calls remain prominent. This modest change does not close
the critical OMC gap. The complete `multibody-seed-linearization-full` gate
passes at `5fbcc036` with all 566 phase and band outcomes unchanged; see the
latest complete measurement above.

## Request derivatives only when the numerical method needs them

`SolveMeKernel::completed_integrator_step` evaluated state derivatives solely to
warm its private accepted-point cache. This executes the derivative algebraic
closure even when the numerical method has no derivative request at that point.
The component now performs only its required event/history work and cache
invalidation during completion. The ordinary derivative getter still evaluates
and caches requested points. SPEC_0038 records this ownership rule; no equation,
integration tolerance, or required observation changes.

The focused harmonic-oscillator regression uses a counting native-interface
adapter executing the real prepared residual. Before the fix the first
completion makes one unrequested RHS call (`on-demand-derivatives-red-2.log`).
Afterward completion makes none, explicit requests return the expected
derivatives, and identical requests reuse their cache across successive steps.
The first attempted RED exposed a fixture type error and is not behavioral
evidence. All 607 solver, simulation, BDF, and RK45 tests pass, as does
all-target/all-feature Clippy (`on-demand-derivatives-libraries-2.log`,
`on-demand-derivatives-clippy-1.log`). All seventeen repository inspection
gates pass (`on-demand-derivatives-repo-gates-1.log`). The first library build
exposed the now test-only allocating derivative wrapper; production uses the
existing borrowed slice operation.

The fixed `multibody-on-demand-derivatives-canary` preserves all twenty phase
and agreement-band outcomes against `multibody-deferred-observation-canary`.
Its nine compared models and 175 initialization channels remain high, with no
skipped, missing, nonidentifiable, or deviating trace. Source: `e6017e33` plus
working-tree digest
`ff2f7b90df633432c6fa82da8d08a77d5558d48c8f3c8169d4cac837f374c542`;
the exact delta is `rolling-wheel/on-demand-derivatives-canary-delta.json`.

The isolated `critical-on-demand-derivatives-profile-1` improves RollingWheel
Sim from 1.640 to 1.309 seconds, with user CPU decreasing from 1.61 to 1.30
seconds. This single pair supports an approximately 20% improvement. The trace
remains byte-identical, SHA-256
`61e23f642f39489afa0717c767501558972f2e29419b77b088befbe8b2aff3dd`,
and canonical Solve JSON is unchanged. Worker SHA-256:
`23b491debb8eb362a357762d42d5d3a46ac3f94acc1dae24f590caa8c070405a`.
The profile retains 255 CPU samples with zero lost samples. Algebraic JVP and
typed pure-call directional programs remain prominent; this is still much
slower than the existing OMC reference. The subsequent complete cohort passes
at `c1145fa9`, as recorded above; no coverage increase or release readiness is
claimed.

## Defer speculative event-left outputs

An actual-worker census at `94d0d2ce` identifies a host-level source of
unnecessary algebraic work. RollingWheel takes 1,432 accepted steps, 3,571
derivative-kernel calls, and 444 directional calls. The full algebraic refresh
runs 1,932 times: 500 requested positive-time samples plus an unconditional
event-left output candidate for every accepted step. Uneventful endpoints
discard those candidates. OMC's repeated reference runs take 1,028 steps and
1,329 ODE calls; step count alone cannot explain the runtime gap.

`commit_accepted_endpoint` now retains the admissible left coordinate, native
sampled states, and opaque pre-callback FMU snapshot. It evaluates outputs only
when a known time event or returned step event needs publication. Evaluation
restores that snapshot, uses the ordinary output operation, and restores the
current complete component state on success, failure, or unwind. No derivative,
root, event, or required observation is omitted. SPEC_0038 and SPEC_0044 §6
record this host rule; FMI 3.0.2's state restore and completed-step semantics
remain the component boundary.

The reduced RED has a valid endpoint and an unrequested interior observation
whose algebraic branch is undefined. The old host fails solely on that discarded
candidate (`deferred-event-left-red-1.log`); the repaired host advances. A known
time event still propagates the same getter failure. Separate regressions prove
saved-input evaluation, complete current-state restoration after errors and
panic, original panic propagation, and foreign-snapshot rejection. All 606
solver, BDF, RK45, and simulation tests pass, as does all-target/all-feature
Clippy (`deferred-event-left-libraries-2.log`, `deferred-event-left-clippy-1.log`).

The fixed `multibody-deferred-observation-canary` takes 47.10 seconds, preserving
all twenty phase/band outcomes. All nine compared models and 175 initialization
channels remain high; no trace is skipped, missing, nonidentifiable, or deviating.
The measured source is `94d0d2ce` plus digest
`b18a0e7f309402e31be3b624c5510115e4eefb7f8d261f8e9b63daaf774eff1e`.

The clean `critical-deferred-observation-profile-1` run improves RollingWheel
Sim from 2.138 to 1.640 seconds (user CPU 2.11 to 1.61 seconds). The complete
trace remains byte-identical, SHA-256
`61e23f642f39489afa0717c767501558972f2e29419b77b088befbe8b2aff3dd`,
and canonical Solve JSON is unchanged. The native worker SHA-256 is
`22a5ba80fa1bde722f5898d9f1b59d468592ba288721372345d1b2f228666d2a`;
309 CPU samples have zero lost samples. This single isolated pair supports an
approximately 23% improvement, not parity with OMC. Temporary probes are removed;
`kernel-census-probes` preserves their patch, original files, and manifest.
The subsequent full cohort passes at `9639fad9`, as recorded above. The critical
performance gap remains open.

## Reuse all tensor JVP outputs within a prepared color

The selected-row interface recomputed an entire JVP program to return each
requested output. The earlier actual RollingWheel census recorded 40,821
evaluations of each of program 195's three selected outputs. A three-column
Jacobian therefore invoked the same tensor program nine times. OMC's generated
linear-system matrix functions fill their selected entries directly; they do
not replay a whole tensor derivative separately to retrieve each component.

Under SPEC_0007 SOLVE-C56, derived structural-artifact construction now binds
each projection color to ordered program/output groups in both solver-Y and
solver-Y/parameter seed spaces. It uses the checked logical output catalogs,
retains their nonidentity placement, and does not rewrite the scalar/tensor
programs. The runtime consumes those groups at one immutable numerical point,
omits reverse-completed rows, and retains all selected outputs of each native
or interpreted program invocation. Native failures propagate without a retry.
There is no persistent numerical cache or runtime grouping by hashes, pointers,
or operation equality.

The construction also checks operation repeatability exhaustively, including
compact nested branches and folds. Impure random operations prevent reuse even
when their values are discarded. Ambiguous or missing logical output ownership
cannot issue a grouped selection. Table access retains the same immutable
external-table context for each invocation.

`rolling-wheel/grouped-jvp-red-1.log` records the coupled-system regression:
the numerical solution was correct, but the native interface received four
single-output calls instead of two grouped calls. The repaired regression
reuses the runtime while changing the matrix coefficient through `2,-3,0,2`.
`grouped-jvp-effects-red-1.log` separately captures the initially missing guard
for an unused impure operation. Regressions cover both seed-space maps,
reverse-completed rows, input extents, unrelated-program isolation, table
failures, and actual JIT call counts. All 307 Solve-IR, 190 evaluator, 78 native,
463 solver, and 128 simulation tests pass (`grouped-jvp-libraries-2.log`).
All-target/all-feature Clippy for those crates passes
(`grouped-jvp-clippy-3.log`); its earlier two attempts exposed nesting only.

The fixed `target/msl/multibody-grouped-jvp-canary` preserves all 20 phase and
agreement-band outcomes against `multibody-affine-seed-canary`. Its nine
compared models and 175 initialization channels remain high, with zero
skipped, missing, nonidentifiable, or deviating traces. Eleven requested,
admitted, and pinned workers ran the canary. The delta receipt
`rolling-wheel/grouped-jvp-canary-delta.json` binds dirty-tree digest
`b8a0a484309d158bbaac8d2cbee7ba859808690a379cd7a918934abd4708e1b2` to parent
`52d578f101f0f992eebc9e7b4bf15366a8acab54`.

The clean isolated `rolling-wheel/critical-grouped-jvp-profile-1` takes
2.138 seconds in Sim and 2.11 seconds of user CPU, versus 2.315 and 2.28
seconds in `critical-affine-seed-profile-1`. This single pair supports roughly
8% less runtime. The trace is byte-identical
(`61e23f642f39489afa0717c767501558972f2e29419b77b088befbe8b2aff3dd`), and the
entire emitted canonical Solve JSON compares equal. The profile retains 414
samples with zero loss and named JIT symbols. Program 223's residual and JVP,
plus their trigonometric work, remain material costs; the OMC gap remains open.
`rolling-wheel/grouped-jvp-diagnosis.json` pins the evidence and worker hashes.

## Omit singular guesses before prepared affine solves

RollingWheel's positive-time runtime census found 5,540 failures of the optional
scalar seed for `wheel1.rollingWheel.delta_0[3]` in projection block 823. Each
failure restored the incoming snapshot and abandoned the staged refresh for
the complete projection plan. The earlier census recorded 2,015,815 block
evaluations, including 13,607 executions of the angle-rate block. These are
instrumented diagnostic counts, not performance measurements.

The preserved tensor equations locate the first divergent producer precisely:
the seed isolates `delta_0[3]` from `dot(delta_0, e_long_0) = 0`. The source
defines `e_long_0 = normalize(cross({0,0,1}, e_axis_0))`, so that scalar pivot
is zero. The coupled contact equations use the orthogonal directions
`e_axis_0`, `e_long_0`, and their cross product; the complete matrix remains
nonsingular within the source assertion domain. OMC's generated linear system
664 also retains a coupled three-variable solve, using body height in place
of `delta_0[3]`. The equations and seed owner are pinned in
`rolling-wheel/affine-seed-diagnosis.json`.

Under SPEC_0007 SOLVE-C56, `ContinuousRefreshOwners` now omits projection seeds
after deriving affinity from the canonical residuals and before emitting exact
assignment schedules. The same construction applies to algebraic, derivative,
root, event, clock, and remainder owners. An affine solve already computes its
solution independently of the incoming guess. Nonlinear blocks retain their
seeds, and the original tensor residuals, assertions, coupled matrices, and
coordinate refinement remain authoritative.

The reduced regression uses `y - k*x = 1`, `x + y = 3`: scalar isolation of
`x` divides by zero at `k=0`, while the coupled determinant is `-(k+1)`.
`affine-seed-red-1.log` records the failing schedule assertion. The repaired
test reuses the prepared runtime across `k=0,1,3,0`; companion tests cover
nonlinear seed retention and every owner/remainder's emitted schedule.
All 303 Solve-IR, 190 evaluator, 459 solver, and 128 simulation tests pass
(`affine-seed-libraries-{1,2}.log`), as does all-target/all-feature Clippy for
these crates (`affine-seed-clippy-1.log`). Temporary probes were removed.

The fixed `target/msl/multibody-affine-seed-canary` preserves all 20 phase and
band outcomes against `multibody-affinity-canary`: all nine compared models
and 175 initialization channels remain high, with zero skipped, missing,
nonidentifiable, or deviating results. The run used 11 requested/admitted/pinned
workers; `rolling-wheel/affine-seed-canary-delta.json` binds its dirty tree
digest `4e0d8507938500c5264e1c551dbb49281b070abb631e9bf27c53fd0554f99056`
to parent `2db3389d45583ba3551b1cb19124a75eeef2ef0c`.

The clean isolated profile `rolling-wheel/critical-affine-seed-profile-1`
takes 2.315 seconds in Sim and 2.28 seconds of user CPU, compared with
3.439 and 3.40 seconds in `critical-affinity-symbols-profile-1`. Both use
the unchanged 12-second budget, one worker, and named JIT symbols. This
single pair supports a roughly 33% reduction, not a broad speedup claim.
The trace is byte-identical (`61e23f642f39489afa0717c767501558972f2e29419b77b088befbe8b2aff3dd`),
and the emitted canonical implicit programs, pure-call bodies, and complete
projection plan compare equal. The new artifact has empty seeds on every
affine projection stage. The critical performance gap to OMC remains open.

## Prepared linear solves through conditional tensor functions

RollingWheel's generated plan formerly lacked affine certificates for block
813 (three angle rates) and block 858 (twenty-one forces, torques, and
accelerations). Both therefore entered the torn Newton solver, whose reduced
Jacobian perturbs each tear variable and repeats the causal/residual sweep.
OMC's generated equations use linear systems, including the six-unknown
dynamics system 731; the compilers have different state bases and block
partitions, so these dimensions are not directly equivalent.

The first certificate refusal is inside the typed `Frames.axesRotations`
owner 563: its `axisRotation` callees contain comparisons, conditional regions,
concatenation, and numeric conversion. The constructor's affinity walk did not
cover those operations. The source formula computes a rotation matrix from
the known sequence/angles and multiplies known axis/rotation coefficients by
the unknown rates. Program 223, which owns the acceleration constraint at
row 814, also requires concatenation, transpose, and array-patch degree bounds.

The repair extends the constructor-owned proof through both conditional
regions and compact tensor operands. Unknown-dependent selectors remain
nonlinear; both branch results contribute even if one is inactive at a
particular point. Conversion is conservatively nonlinear in its operand.
Tensor bounds cover whole ranges without per-coordinate IR expansion. This
follows SPEC_0007 SOLVE-C36/C56, SPEC_0032 §2/4, and MLS §10.4, §10.6.13,
and §12.4.4. No numerical tolerance, source equation, or execution program is
changed.

Checked reconstruction of the newly emitted Solve model now certifies all
four coupled blocks (813, 823, 858, 869) as affine. The pure-call bodies and
projection plan are identical to the previous artifact; the implicit programs
differ only in their derived affinity certificates. The earlier corrected
row-814 dependency and all non-invalidating BLT flags remain intact.
`rolling-wheel/affinity-pattern-proof-1.log` records the complete block census.

Two typed-call regression assertions and three tensor-operation assertions
fail before their corresponding repairs. The negative controls cover unknown
selectors, nonlinear coefficients, an unknown-dependent inactive branch,
runtime indices/slices, and a million-element tensor. All 302 IR, 190 evaluator,
457 solver, and 128 simulation library tests pass; all-target/all-feature
Clippy passes for the four crates. The fixed canary at parent `34c6ec78`,
working-tree digest
`ef383da198f992c4e6421b7dc4ecfeaff2ebde90524c490117d23b1287430aca`,
retains all twenty phase outcomes and bands: nine compared high, all 175 initial
channels high, zero missing/skipped/nonidentifiable/deviating traces, eleven
workers. Its receipt is `rolling-wheel/affinity-canary-delta.json`.

One clean isolated profile pair, with the same twelve-second simulation budget,
shows the following changes:

| Declared phase / counter | Prepared-stage baseline | Affinity repair |
|---|---:|---:|
| SimBuild | 2.184 s | 1.700 s |
| IC | 0.369 s | 0.061 s |
| Sim | 4.303 s | 3.435 s |
| Sim user CPU delta | 4.24 s | 3.41 s |

Both runs have zero Sim major-fault and system-CPU deltas. The new worker SHA is
`f81a7c6ae2193579dbd7ecd7470612bf17a7590a8d322a14d6e345d04ea3c80f`.
Its trace SHA is
`61e23f642f39489afa0717c767501558972f2e29419b77b088befbe8b2aff3dd`;
the changed arithmetic path passes the subsequent complete OMC comparison
recorded above. Perf records 675
user-CPU samples without loss; trigonometry, row evaluation, and allocation
remain visible costs, with incomplete native/JIT stack unwinding. The critical
OMC runtime gap remains open. Combined `verify quick` and `verify full` are
pending.

A second profile uses Cranelift 0.125.4's built-in `PERF_BUILDID_DIR` support
to emit the JIT symbol map, with no source probes. Sim takes 3.439 seconds and
the trace is byte-identical to the first affinity profile. Its 672 samples
have no loss; the largest named generated self costs are
`rumoca_jacobian_row_223` (5.65%), the directional owner 563 (4.61%), and
residual program 223 (2.98%). Sine/cosine together account for 11.01% self
samples. Stack unwinding still stops at generated frames, so these are not
inclusive caller fractions. The exact map, perf data, and reports are retained
under `rolling-wheel/critical-affinity-symbols-profile-1`. This points the next
investigation at coefficient/Jacobian evaluation and repeated rotation work;
it does not establish how much further runtime can be recovered.

## OMC profile and remaining algebraic refresh overhead

Forty sequential executions of the unchanged, xtask-generated RollingWheel
OMC executable confirm that solver step count does not explain the gap.
The original initialization file, horizon, DASSL settings, and CSV output are
retained; OMP/OpenBLAS threads are fixed to one. The executable SHA-256 is
`a563bc0a18b6ae9baed42ad78014c27044f94cea56cfaaae8f45ceb22793dd0a`.
All forty executions succeed with 1,028 DASSL steps, 1,329 ODE evaluations,
42 Jacobians, eight error-test failures, and zero convergence failures.

| OMC measurement | Median across forty executions |
|---|---:|
| Reported simulation time | 16.582 ms |
| Reported output time | 31.356 ms |
| Reported solver time, excluding callbacks | 1.097 ms |
| Reported total simulation/output time | 50.655 ms |
| Whole-process user CPU | 67.345 ms |
| Whole-process wall observation | 114.236 ms |

The whole-process wall observation includes the diagnostic driver's polling;
it is not an isolated solver timer. The simulation timer ranges from 15.868 to
21.956 ms. A 999 Hz user-CPU `perf` capture across these runs retains 2,502
samples with no lost samples. CSV emission accounts for 43.25% of inclusive
samples and generated `functionAlgebraics` for 5.32%. Floating-point formatting
is the largest self-time cost. The generated ODE entry invokes 44 top-level
equation routines, which may themselves call other routines. Raw profiles,
per-run statistics, source-artifact hashes, and commands are retained under
`rolling-wheel/omc-perf-repeated-1` and `rolling-wheel/profile_omc_repeated.py`.

Rumoca's latest clean isolated declared-Sim measurement remains 4.283 seconds.
Its profile excludes build/startup, while the OMC profile includes those phases
and CSV output; their sampled percentages must not be treated as having the
same denominator. A separate temporary positive-time Rumoca assignment census
counts 2,224,551 calls: 2,168,913 Direct assignments and 55,638 Zero assignments.
There are no TensorAffine assignments, ruling out that suspected path for this
model. Instrumentation adds overhead, so its timing is not a benchmark.

The stage diagnostic identifies two refusal reasons. Coupled projection blocks
without an isolatable seed for every coordinate fail the runtime seed-coverage
guard. Separately, the Solve dependency certificate says projection block 868,
which assigns Y114 (`wheel1.rollingWheel.r_road_0[3]`) to zero, invalidates
earlier row 814 in coupled block 858. The runtime therefore refuses staged
execution and uses the broader refresh projection. No native assignment compile
or execution failures were observed. These findings are retained in
`rolling-wheel/critical-assignment-census-{1,4}`; diagnostic probes are removed.

Inspection of the actual prepared DAE disproves an upstream BLT-order defect.
Prepared expression 4023 updates component three of vector residual 3107 with
expression 4021; row 814 no longer depends on algebraic coordinate 72/2, the
road-height component. The lowered program preserves that exact TensorUpdate
at operation 764. However, `DependencyWalk::tensor_update` unions the whole patch
into every output and retains every original base dependency, including the
overwritten component. This introduces the false reverse edge. The prepared
DAE and scalar incidence are retained in `rolling-wheel/ordering-prepared-dae.json`
and `rolling-wheel/ordering-probe-2.log`.

The candidate dependency derivation follows fixed index/whole-axis projections
exactly, including separate AD lanes; runtime indices and slices retain their
conservative dependencies. The tensor owner and operation sequence are
unchanged. Three reduced vector/matrix tests fail before the fix; all four
tests, including the dynamic-selector control, pass afterwards. Governing
contracts are MLS §10.5/§10.6.9, SPEC_0032 §2, and SPEC_0007 SOLVE-C17/C36/C56.
The separate runtime repair relies on the constructor's complete projection
block ownership instead of requiring an isolatable seed for every unknown.
Missing producers still reject runtime construction; genuine backward
dependencies still reject one-pass execution. Two reduced tests with derived
structural artifacts fail the old dispatch requirement (`coupled-stage-seeds-red-2.log`);
the earlier fixture omitted those artifacts and is not the decisive RED proof.

Enabling the stages exposes another runtime defect: an admitted native
assignment execution error was silently retried through the interpreter.
`prepared-stages-libraries-3.log` records the existing simulation discriminator
incorrectly succeeding with an always-failing native backend. Assignment errors
now propagate, restore the incoming snapshot, and do not permanently disable
native execution. The discriminator now observes the assignment error, the
first native failure reached by the prepared stage order. A separate regression
checks restoration after a partial native write and a second attempted call.

All 297 IR, 190 evaluator, 457 solver, and 128 simulation library tests pass
(`prepared-stages-libraries-4.log`); all-target/all-feature Clippy passes for all
four crates (`prepared-stages-clippy-4.log`). The fixed
`target/msl/multibody-prepared-stages-canary` passes at parent `3799c0c8`,
working-tree digest
`e5d98371f99a43da88bd65b89d3defbea7e92a14d58081eda8e0042e77f704ad`.
All twenty phase/simulation outcomes and bands are unchanged: nine compared
models high, all 175 initial channels high, and zero skipped, missing, excluded,
nonidentifiable, or deviating comparisons. Eleven workers are requested and
admitted. The exact delta is `rolling-wheel/prepared-stages-canary-delta.json`.

Replaying the unchanged RollingWheel residual programs through the repaired
artifact derivation removes the row814/Y114 edge and every algebraic reverse
invalidation (`prepared-stages-pattern-proof-1.log`). The first isolated run
takes **4.303 seconds**, versus 4.283 seconds before; user CPU remains 4.24
seconds, with zero major faults during Sim. Its trace is byte-identical to the
baseline, SHA-256
`340e0a5bab67e9a05b160f3b50a623126edd2660863fa9cd0f25461b16bc334b`.
This pair establishes no speed improvement. The corrected dependency and
dispatch contracts do not close the critical performance gap; the remaining
prepared coupled-block execution needs profiling. The subsequent named-commit
complete cohort sweep passes as recorded above; combined `verify quick` and
`verify full` have not run for this change.

## Selected algebraic residual native execution

RollingWheel's selected algebraic residual callback always interpreted its
source program, although selected directional callbacks already used native
code. Program 223 retains 766 operations and three aggregate outputs; only one
of those outputs participates in the large simultaneous block, so batching
that block alone cannot remove its interpreter cost. The first responsible
layer is runtime/native dispatch, not Modelica lowering.

The runtime now requests a selectable native expression for the unchanged
implicit residual block. Cranelift emits each source program once and uses its
entry for both selected and full evaluations. Selection uses the checked
program/output coordinate, preserves complete tensor programs, and does not
execute unrelated programs. Shared conditional owners retain their aggregate
batch and explicitly decline selection. An admitted execution error propagates.
This is target-local emission under SPEC_0032 §4 and SPEC_0007; it changes no
equations, assignment ownership, solve order, state selection, or tolerances.

The isolated diagnostic pair goes from **5.144 to 4.283 seconds** in declared
Sim, with user CPU decreasing from 5.09 to 4.24 seconds and zero major faults
during either simulation. Both complete traces have SHA-256
`340e0a5bab67e9a05b160f3b50a623126edd2660863fa9cd0f25461b16bc334b`.
The roughly 17% reduction is one profile pair, not a stable benchmark. The OMC
gap remains critical; repeated tensor assignment and directional evaluations
remain visible in `perf`. Source hashes, worker hashes, timings, and profiles
are recorded in `rolling-wheel/native-residual-diagnosis.json`.

Three reduced dispatch tests fail before the repair and pass afterwards.
All **76 Cranelift, 456 solver, and 128 simulation library tests** pass, including
aggregate/sparse outputs, table context and errors, pure-call owners, shared
conditional batches, explicit decline, and error propagation. All-target,
all-feature Clippy passes for the three crates.

`target/msl/multibody-native-residual-canary` passes at parent `ab36e867`,
working-tree digest
`bb06dbc1af4f6e03fcbcea70241f5220c0734b7379bcda5d7d2c95588bcd9926`.
All twenty phase/simulation outcomes and bands match the native-manifold
canary: nine compared models high, all 175 initial channels high, and zero
skipped/missing/excluded/nonidentifiable/deviating comparisons. Eleven workers
were requested and admitted. Logs and the exact delta are
`rolling-wheel/native-residual-{libraries-2,clippy-1,canary-1}.log` and
`rolling-wheel/native-residual-canary-delta.json`. The subsequent named-commit
full sweep passes with all 566 phase/simulation outcomes and bands unchanged,
as recorded above. Combined `verify quick` and `verify full` have not run for
this change.

## Critical RollingWheel performance investigation

The 48x cached-OMC runtime gap is an open critical performance bug. An isolated
worker at `8b0794d8` takes 6.094 seconds in its declared Sim phase: 6.05 seconds
of user CPU, no system CPU at the counter resolution, and zero major page
faults. A fresh execution of the existing OMC artifact takes 0.114 seconds
including process startup and output. OMC reports 1,028 DASSL steps and 1,329
ODE calls. The earlier Rumoca census has 1,404 BDF steps; that difference does
not explain the runtime gap. These diagnostic runs are not stable benchmarks.

`perf` and a temporary positive-time program census identify repeated tensor
execution. Implicit program 223 has 766 operations and three outputs, and runs
66,589 times. Four state-manifold directional programs each run 48,880 times.
The first concrete dispatch defect is in runtime preparation: manifold residual
and directional blocks were never offered to the execution backend, and every
projection unconditionally used the interpreter. The diagnostic receipt is
`rolling-wheel/critical-runtime-diagnosis.json`; temporary probes are removed.

The repair offers both unchanged, source-bound manifold blocks to the existing
backend once during preparation. A backend may decline compilation; an admitted
native execution error propagates without an interpreter retry. This follows
SPEC_0007's checked-product ownership and SPEC_0038's non-semantic native
execution boundary. State selection, tensor owners, constraints, tolerances,
and the numerical projection algorithm are unchanged. Two reduced tests fail
on the old code and pass with native dispatch, interpreter-decline, state-only
input, and execution-failure controls.

One isolated candidate run takes 5.144 seconds (5.09 user CPU seconds, zero
major faults). Its trace is byte-identical to the baseline and to the last
full run's RollingWheel trace:
`340e0a5bab67e9a05b160f3b50a623126edd2660863fa9cd0f25461b16bc334b`.
The remaining runtime gap is still critical. Repeated full tensor evaluations
for selected algebraic residuals and sensitivities remain the next target;
this improvement is not closure of the performance issue.

All 453 solver and 127 simulation library tests pass, as do all-target,
all-feature Clippy checks for both crates. The fixed
`target/msl/multibody-native-manifold-canary` passes at parent `8b0794d8`,
working-tree digest
`6f6c0b37c0cf4b88fb8d6adc7061ca3c9a2f46d5e944857ed7c73a0be2bfb74e`.
All twenty phase/simulation outcomes and bands match the indexed-alias canary.
Nine models compare high, all 175 initial channels are high, and there are
zero skipped, missing, excluded, nonidentifiable, or deviating comparisons.
Eleven workers were requested and admitted. Logs and the exact delta are
`rolling-wheel/native-manifold-{libraries-1,clippy-1,canary-1}.log` and
`rolling-wheel/native-manifold-canary-delta.json`. The subsequent named-commit full sweep passes with all
566 phase/simulation outcomes and bands unchanged, as recorded above; combined
`verify quick` and `verify full` have not run for this change.

## Fourbar indexed-alias definition repair

The original Fourbar_analytic DAE contains both `position_b[i].y = k` and
`prismatic.position_b[i] = position_b[i].y`. Causal analysis interpreted the
second equality as another definition of the scalar output, then discarded
both definitions as duplicates. The source rows are retained in
`fourbar-analytic/mixed-alias-source-rows.json`. OMC's saved equation inventory
assigns the three outputs `0.0`, `0.2`, and `0.0` during initialization
(`indexed-alias-omc-equations.json`, equations 799–801).

The governing requirements are MLS §9.2 connection equalities and §10 array
coordinates, SPEC_0007 Stage 3's orthogonal input/output causality, and
SPEC_0032 §1–2's source-owned arrays and structural scalar views. Exact
scalar/element aliases now defer their direction until independent whole
definitions have been considered. A complete component definition may follow
the already proved, transitively closed whole-definition graph. An externally
solved algebraic cannot seed that proof, and a continuous dependency cannot
become event-held. The same dependency walk supplies closure and variability;
this adds no second traversal of whole-definition expression graphs.

The initial three-test reduction fails two assertions on the old code
(`indexed-alias-red-1.log`). Five strengthened tests now cover either equality
orientation, scalar alias chains, output arrays, an independently defined
whole array supplying scalar outputs, incomplete coverage, duplicate scalar
and component equations, cycles through the array, and a transitive continuous
source. All 161 structural tests and all-target/all-feature structural Clippy
pass in `indexed-alias-green-4.log`. Earlier fixture-construction and lint
failures remain in the numbered logs.

The original DAE inspection now proves all three scalar output definitions and
complete coverage of variable 476, `jointSSP.prismatic.position_b`. The exact
before/after records and hashes are in `indexed-alias-definition-delta.json`.
Its 829 structural reduction records remain byte-identical: differentiation
still lacks consumption of complete component definitions and filters out
output definitions. The original 1622/1659 structural refusal remains; this
repair alone establishes neither compilation nor a new model's trace parity.
Downstream checks pass: 115 GALEC library and six integration tests, 120 Solve
tests, 127 simulation tests, 572 compiler-core tests, 243 architecture tests,
and seventeen size/spec gates. The log is
`fourbar-analytic/indexed-alias-consumers-canary-1.log`.

The fixed `target/msl/multibody-indexed-alias-canary` passes at parent
`84aa78ccb64c88647244a15b5d624732e68b2104`, working-tree digest
`053bb1f4e3cd33b143fe730e7905586165adde1a91bb1db1f2fea592bbb74cd4`.
All twenty phase and simulation outcomes and bands match
`multibody-state-only-manifold-canary`; nine models compare high with all 175
initial channels high and zero missing, skipped, excluded, nonidentifiable, or
deviating comparisons. Eleven workers were requested; the memory limiter
admitted four. `indexed-alias-canary-delta.json` retains the comparison and
artifact hashes. Tier 1 is complete; the latest full-cohort measurement remains
the `f485ef13` run above. No additional model pass or isolated speedup is claimed.

## Previous complete measurement: singleton degree queries

`target/msl/multibody-singleton-degree-full` passes the local fallback gate at
commit `39cb7f2384de9d15cf3d022bf16d1d2c5b1d168c`, working-tree digest
`48672dfdec9c3416aac00ca9d56725958329a60236f921974bbf966a09020c35`.
The complete 566-model sweep takes 130.23 seconds with eleven Rumoca workers.
All 152 compared models are strict-high (26.86% of 566), with seventeen
reviewed exclusions, zero missing/nonidentifiable traces, and all 17,606 initial
channels high. No compared trajectory channel deviates; all nine electrical
counterexamples remain high. MultiBody remains **19/42 high**.

There are no band changes from `multibody-derivative-chain-full`.
PrismaticConstraint and IMC_Transformer reach structural refusals within budget.
LineForceWithTwoMasses now exceeds the Solve budget instead of panicking;
the separate completed DAE inspection below establishes the preflight repair,
but this full attempt does not reach that final structural refusal.
RollingWheelSetDriving still times out after twelve simulation seconds
(12.110 seconds including call overhead), with 10.715 seconds of build time.
The receipt is `rolling-wheel/singleton-degree-full-delta.json` under the
campaign directory. The earlier twentieth high model remains a performance
regression requiring repair.

The fixed 145-model performance cohort reports 2.245 median simulation speedup
over OMC; the expanded 169-completion cohort reports 1.523. The medians have
different populations. Neither summarizes the expensive MultiBody tail:
RollingWheel takes 9.234 seconds of simulation versus OMC's 0.134 seconds;
DoublePendulum takes 1.482 versus 0.122 seconds. These exclude Rumoca build time.
Combined `verify quick` last passed before six subsequent implementation
changes; `verify full` has not run. No PR or baseline promotion is claimed.

### RollingWheel runtime census and state-only projection

The MSL worker uses the in-process FMI Model Exchange host, Diffsol BDF for
this model, Cranelift where execution is admitted, and prepared interpretation
for remaining work. These measurements do not execute an exported C FMU.
The current Solve artifact retains twelve scalar states, 899 algebraic slots,
and four state constraints; OMC selects eight states. A temporary numerical
census records 1,404 BDF steps, 27 resets, 336 linear setups, and 2,324 nonlinear
iterations through 4 seconds. Accepted orders 1 through 5 have counts
54/255/242/339/514. OMC's generated DASSL executable takes 1,028 steps, 1,329
ODE calls, 42 Jacobian evaluations, and zero events. A permanently low BDF
order therefore does not explain the large runtime gap.

The refresh census records 6,843 full-algebraic refreshes taking 6.748 seconds,
3,670 derivative refreshes taking 2.279 seconds, and 2,512 root refreshes taking
0.189 seconds. These counters include preparation, initialization, and simulation;
they are not exclusive declared-Sim samples. Both instrumented trajectories are
byte-identical to the complete sweep's RollingWheel trace. The temporary probes
are removed; exact patches, binary/source hashes, logs, and scope are retained
in `rolling-wheel/rolling-wheel-runtime-census.json`. The first OMC executable
invocation lacked its relative sparsity assets; that setup failure and the
corrected working-directory invocation are both retained.

The concrete ordering defect is in the FMI kernel's state projection: it
reconstructs all algebraics before evaluating the retained state-only manifold.
Solve lowering already rejects algebraic/derivative dependencies in that
manifold (`lower.rs::manifold_state_slots`, SPEC_0007 structural/Solve boundary).
The reduced regression projects an off-manifold state from -1 to 4 before
observing `a*a=x`; the old code fails trying to find a real `a` at -1. Its
negative control keeps an invalid post-projection observation as an error.
The candidate passes only the state prefix to the unchanged residual/JVP
projection, preserving tensor owners, numerical tolerances, and the accepted
step's existing restart policy. An invalid manifold that reads an algebraic
warm start is refused by the checked evaluator and leaves the state unchanged.
All 451 solver tests, all-target/all-feature solver Clippy, formatting,
243 architecture tests, and seventeen size/spec gates pass. The positive
regression first failed on the old premature algebraic solve; the invalid-output
control already passed. Logs are `rolling-wheel/state-only-manifold-red-1.log`,
`state-only-manifold-green-2.log`, and `state-only-manifold-canary-1.log`.
The fixed twenty-model `target/msl/multibody-state-only-manifold-canary` has no
phase or band changes: nine comparisons and all 175 initial channels high,
zero missing/skipped/excluded/nonidentifiable traces. It ran at parent `39cb7f23`
with dirty digest `a3031f39be8bb4047180ee0e1142d4a5c9a43b65010c3b9b861017d54a8008a0`.
Eleven workers were requested, but the memory limiter admitted one; this is
not an eleven-worker timing comparison. The delta is retained in
`rolling-wheel/state-only-manifold-canary-delta.json`. The completed full sweep
at `f485ef13` is recorded above.

## Previous complete measurement: supplied derivative chains

`target/msl/multibody-derivative-chain-full` passes the local fallback gate at
commit `7fa18fa1c93b4b65c588e98da6509aaf794dc03d`, working-tree digest
`48672dfdec9c3416aac00ca9d56725958329a60236f921974bbf966a09020c35`.
The complete 566-model sweep takes 124.74 seconds with eleven Rumoca workers.
**All 152 compared models are strict-high (26.86% of 566)**, with seventeen
reviewed exclusions, zero missing/nonidentifiable traces, and all 17,606 initial
channels high. No compared trajectory channel deviates; all nine electrical
counterexamples retain their high bands.

MultiBody measures **19/42 high**. RollingWheelSetDriving initializes but again
exhausts its twelve-second simulation budget (12.113 seconds including call
overhead). It is the only band loss from `multibody-shared-call-full`.
PrismaticConstraint and IMC_Transformer change from structural refusals to
Solve timeouts. LineForceWithTwoMasses reaches a compiler panic instead of its
previous Solve timeout: `holonomic preflight proves a causal algebraic definition`.
The panic is the immediate triage priority; the wheel-set performance loss also
remains open. A passing fallback gate does not discharge either defect.
`fourbar-analytic/derivative-chain-full-delta.json` retains all four phase deltas,
the band delta, electrical checks, and result/comparison/band artifact hashes.

Fourbar_analytic still reports 1622/1659 structural matches. The combined
`verify quick` passed before the last four implementation changes; focused
checks and the fixed canary pass as recorded below. `verify full` has not run.
No PR, release, or baseline promotion is claimed.

## Previous complete measurement: restored wheel-set completion

`target/msl/multibody-shared-call-full` passes the local fallback gate at
commit `51459c19c3aa59db8925b3a0d8eb40846d666d67`, working-tree digest
`48672dfdec9c3416aac00ca9d56725958329a60236f921974bbf966a09020c35`.
The complete 566-model sweep takes 140.11 seconds with eleven workers.
**All 153 compared models are strict-high (27.03% of 566)**, with seventeen
reviewed exclusions and zero missing or nonidentifiable traces. All 18,498
initial channels are high and no trajectory channel deviates. All nine
named electrical counterexamples retain their high bands.

MultiBody returns to **20/42 high**. RollingWheelSetDriving completes in
11.304 seconds, with all 892 trajectory and initial channels high. Its worst
channel's bounded normalized L1 score remains `5.061492181479045e-6`.
Build takes 11.199 seconds, including 7.809 seconds of Solve lowering.
RollingWheel remains high at 8.682 seconds of simulation. The wheel-set
restoration is the only phase/simulation or band change from
`multibody-parameter-branch-full`; the receipt and artifact digests are in
`rolling-wheel/shared-call-full-delta.json`. The solver budget remains twelve
seconds. This completes the shared-interface equality fix's cohort check.

Thirty-five of 42 MultiBody examples reach the DAE boundary. The remaining
22 without high parity comprise thirteen timeouts (two Flatten, ten Solve,
one simulation), four structural refusals, and five earlier producer errors
(three record derivative-specialization refusals, one function-shape refusal,
one array-valued `fixed` attribute refusal). Fourbar_analytic still reports
1622/1659 structural matches; its OMC derivative chain is the next focus.
At that checkpoint, the combined `verify quick` passed before the preceding three implementation changes;
the focused suites, canary, and full MSL checks pass as recorded below.
`verify full` has not run. No PR, release, or baseline promotion is claimed.

## Previous complete measurement: wheel-set timeout

`target/msl/multibody-parameter-branch-full` passes the local fallback gate
at commit `c89fde703f82afc323d9f38bdf1e8f316452d723`, working-tree digest
`48672dfdec9c3416aac00ca9d56725958329a60236f921974bbf966a09020c35`.
The complete 566-model sweep takes 123.99 seconds with eleven workers.
**All 152 compared models are strict-high (26.86% of 566)**, with seventeen
reviewed exclusions and zero missing or nonidentifiable traces. Every
compared initial channel is high and no trajectory channel deviates. All
nine electrical counterexamples retain their high bands.

MultiBody returns to **19/42 high** because RollingWheelSetDriving times out
after successful initialization. Its enclosing simulation call takes 12.130
seconds and reports `timeout after 12.000s`; build takes 11.051 seconds,
including 7.646 seconds of Solve lowering. This is the only phase/simulation
or band change from `multibody-native-singleton-full`, retained in
`fourbar-analytic/parameter-branch-full-delta.json`. The earlier twentieth
high model is a real trace comparison but not yet reliable completion.
The passing fallback gate does not excuse this loss. Performance work takes
priority over further capability changes; no timeout or comparator change
is proposed.

A declared-Sim `perf` diagnostic on RollingWheelSetDriving completes in
11.875 seconds, capturing 2,342 samples with zero lost. This diagnostic does
not replace the cohort timeout. Of 163 `memcmp` samples, captured direct
return addresses identify 95 dependency-slice comparisons, 28 input-type
comparisons, 23 output-interface comparisons, and seventeen comparisons
inside native `call_cells`. The original DWARF caller unwind fails. The
bounded replacement reads the first user-stack word of leaf libc `memcmp`,
using the ELF load bias derived independently from a resolved Rust symbol;
it makes no recursive-unwinding claim. Evidence is under
`rolling-wheel/wheel-set-runtime-profile`, including
`memcmp-return-addresses-corrected.json`. Source inspection and disassembly
show repeated value comparisons of immutable interface slices already shared
by `Arc`. A sharing-aware equality fast path is the next hypothesis.

### Shared call-interface equality

The owner-table equality query now recognizes identical immutable `Arc`
slice allocations before comparing their values. The helper requires `Eq`,
whose reflexivity justifies the shortcut; a merely `PartialEq` element could
not justify it. Independently reconstructed interfaces still undergo the
same full value comparison, and primal/directional owner checks remain
required. This is a read-only data-integrity optimization under SPEC_0029 §3,
not a new semantic identity, wire format, or execution policy.

The focused RED visits all three elements when comparing shared storage
(`rolling-wheel/shared-call-red-2.log`). After the fix that count is zero;
independent equal and unequal slices still compare all three values. Wire
replay retains value equality and rejects altered input, output, and dependency
slices in both primal and directional interfaces. All 293 Solve IR tests,
190 evaluator tests, 72 native-backend tests, and 447 solver tests pass.
All-target/all-feature IR Clippy, 243 architecture tests, seventeen size/spec
gates, formatting, and whitespace checks pass
(`rolling-wheel/shared-call-green-1.log`, `shared-call-gates-build-1.log`).

The matched declared-Sim diagnostic completes in 11.474 seconds versus
11.875 before. `memcmp` falls from 6.96% to 1.11% of samples; the new capture
has 2,250 samples with zero lost. The emitted traces are byte-identical,
SHA-256 `3dc2333f4b9d6d69bead292633f3304aa57204f430040d50c00fb62753006a5b`.
Build time rises from 11.613 to 12.307 seconds, including Solve lowering
from 7.371 to 8.051; the change targets repeated runtime comparisons.
The single pair proves the targeted cost was removed without changing this
trace, not a stable speedup or restored cohort coverage. Receipt:
`rolling-wheel/shared-call-profile-pair.json`.

The fixed twenty-model `target/msl/multibody-shared-call-canary` has no phase
or band changes from `multibody-parameter-branch-canary`. Its nine compared
models and all 175 initial channels remain high, with zero skipped, missing,
excluded, or nonidentifiable traces. This is Tier 1 evidence only, captured
on `f3db93c41e049e5d9eb16720cf36bf4f68c0cc6f` with candidate worktree digest
`80dfdfee5fdfc0c2453ca7e42c7f1168148124844e8d2cc68aaf294a5083f04b`;
`rolling-wheel/shared-call-canary-delta.json` retains the receipt. The next
complete sweep must decide whether the wheel-set completion loss is closed.

## Previous complete measurement: first RollingWheelSetDriving high trace

`target/msl/multibody-native-singleton-full` passes the local fallback gate
at commit `27bc2c8d711703fc679b492a26e542486bb19019`, working-tree digest
`48672dfdec9c3416aac00ca9d56725958329a60236f921974bbf966a09020c35`.
The complete 566-model sweep takes 138.19 seconds with eleven workers.
**All 153 compared models are strict-high (27.03% of 566)**, with seventeen
reviewed exclusions and zero missing or nonidentifiable traces. Every
previously high model retains its band, including all nine original
electrical counterexamples. All 18,498 initial channels are high and no
trajectory channel deviates.

MultiBody reaches **20/42 high**, with twenty compared and no trace boundaries.
RollingWheelSetDriving advances from a twelve-second simulation timeout to
high parity on all 892 trajectory and initial channels. Its worst channel's
bounded normalized L1 score is `5.061492181479045e-6`. The enclosing simulation
call takes 12.050 seconds, including its own initialization work; the solver
budget remains twelve seconds and the parent watchdog remains fourteen.
Build takes 10.467 seconds, including 7.194 seconds of Solve lowering, and
the separate initialization check takes 0.142 seconds. This is a measured
high result with little runtime margin, not proof of reliable completion
under every host load. RollingWheel retains all 184 high channels at 9.261
seconds of simulation.

IMC_Transformer returns from a Solve timeout to its structural refusal.
These are the only two phase/simulation changes relative to
`multibody-quick-after-architecture-full`; RollingWheelSetDriving is the only
band change. The retained receipt and artifact hashes are in
`rolling-wheel/native-singleton-full-delta.json`. No tolerance, baseline,
timeout, target-list, or exclusion changes were made. `verify quick` passed
before this implementation, and its focused tests and gates pass as recorded
below. `verify full` has not run. Twenty-two MultiBody examples still lack
high parity; Fourbar_analytic's structural refusal is the next investigation.

### LineForceWithTwoMasses: retained-value preflight

The full derivative-chain sweep exposed a panic while restoring a previously
retained manifold after direct state demotion. A bounded replay identifies
`body1.w_a`, RHS expression 7059, as differentiable but not reconstructible as a
state-only value. Reconstruction reaches `jointUPS.axisLength` without a causal
value anchor and hits an `expect` whose alleged preflight never ran. This is a
structural proof/consumer mismatch, not a Modelica source error or solver issue.
The earlier candidate for the same state, RHS 5693, does have a value proof.

OMC's flat, optimiser, and backend XML captures under `line-force/omc-stages`
all succeed. Its flat source defines `body1.w_a = angularVelocity2(body1.frame_a.R)`;
the function returns the record's `w` field. Its backend represents axisLength
as a dummy state, retains the geometric square-root definition, and issues its
first and second derivatives. Rumoca's panic is earlier than numerical execution;
these captures do not establish a Rumoca trace comparison.

The reduced checked-DAE test returns a whole vector from a pure function whose
other argument is a matrix. Differentiation can follow the returned argument,
but retaining the complete call needs the matrix's exact value. The RED reproduces
the same panic (`line-force/manifold-preflight-red-1.log`). Before reconstructing
a demotion, structural analysis now proves an exact RHS value if a surviving
manifold references that state. The value must use remaining state/invariant
anchors. A missing proof rejects this candidate through the existing
`WouldInvalidateManifold` outcome, allowing other candidates to be considered.
A removed lifted constraint requires no retained-value substitution. Tensor
arguments, source equations, assertions, and the final manifold check remain
under their existing owners (SPEC_0007 structural scope / STRUCT-T03).

The negative case now rejects without panic; a control with an exact whole-matrix
definition reconstructs a state-only manifold, and demotion without a retained
constraint remains available. All 156 structural tests, 572 compiler-core tests,
243 architecture tests, seventeen size/spec gates, structural Clippy, formatting,
and whitespace checks pass. No temporary probes remain. The original DAE replay
now completes with a typed structural refusal: its best intermediate residue is
one equation/unknown pair, but no complete reduction is established. All 1,143
reduction records, including two manifold rejections, are retained with the source
DAE digest in `line-force/manifold-preflight-inspection.json`.

The fixed twenty-model `target/msl/multibody-manifold-preflight-canary` has no
phase or band changes from `multibody-derivative-chain-canary`. Its nine compared
models and 175 initial channels are high, with zero skipped, excluded, missing,
or nonidentifiable traces. This is Tier 1 evidence on parent
`c7d85fc3b9b068f249d7f2972895c01559932e7b`, candidate worktree digest
`9be3cae1653ab969bc61f1a89a7dea73b0bee0567ac6638d7538bc096fb0f3f5`;
receipt `line-force/manifold-preflight-canary-delta.json`. The latest cohort
number remains the named derivative-chain sweep above. Wheel-set performance
remains open before further capability expansion.

### Wheel-set follow-up profile after the preflight repair

One declared-Sim `perf` diagnostic at `d3c414d8` completes in 11.670 seconds,
with 2,303 samples and zero lost. Build takes 9.824 seconds, including 6.957
seconds of Solve lowering. The trace is byte-identical to the earlier high
wheel-set trace, SHA-256
`3dc2333f4b9d6d69bead292633f3304aa57204f430040d50c00fb62753006a5b`.
This single-worker diagnostic does not replace the eleven-worker cohort timeout.
The profiler is attached only during the worker's declared Sim phase, with the
same twelve-second solver budget (`rolling-wheel/manifold-profile-receipt.json`).

The main leaf costs remain prepared-row interpretation (9.08%), sparse LU
(5.17%), native call payload conversion (2.74%), projection-affinity lookup
(2.52%), assignment-certificate queries (2.26%), and row/output lookup (1.65%).
The earlier shared-interface comparison fix remains effective: `memcmp` is
1.13%. Caller unwinding is still almost entirely absent, so these are leaf
samples, not inclusive cost attribution. Next hypotheses are preparing repeated
immutable projection queries once and examining aggregate projection execution.
The existing native singleton path deliberately excludes multi-output tensor
rows; any extension must preserve one tensor program owner and its checked
assignment semantics. No runtime optimization is claimed from this capture.

### Untorn singleton projection before degree queries

For a block without a tearing, both degree classifications attempt the same
singleton assignment before residual projection. Runtime now performs that
common attempt first, then asks for the degree certificate only if it declines.
It does not repeat a declined assignment. A block with a tearing retains the
previous affine/torn/assignment ordering and its branch selection. The residual
Newton path is factored into its own helper without changing its arithmetic.
This is a control-flow optimization over existing constructor-issued facts
(SPEC_0029 §5 and SPEC_0036/SPEC_0043 §6a); tensor source programs, assertion
execution, assignment acceptance, and rollback semantics retain their owners.

The focused RED records one redundant degree query for an exact singleton;
the candidate records zero with either an affine or unproved classification.
A torn-singleton control still requests degree selection, and an inexact
candidate still reaches residual Newton. All 448 solver tests, all-target/
all-feature solver Clippy, 243 architecture tests, seventeen size/spec gates,
formatting, and whitespace checks pass (`rolling-wheel/singleton-degree-red-1.log`,
`singleton-degree-green-2.log`, `singleton-degree-canary-1.log`).

The matched declared-Sim wheel-set profile takes 11.183 seconds versus 11.670.
The degree-query hotspot falls from 2.52% of samples to zero recorded samples;
2,214 samples are captured with zero lost. The complete trace remains
byte-identical, SHA-256
`3dc2333f4b9d6d69bead292633f3304aa57204f430040d50c00fb62753006a5b`.
Build takes 11.098 seconds, including 7.008 seconds of Solve lowering. The
single diagnostic pair proves the targeted cost and unchanged trace, not a
stable speedup or restored cohort completion. Its receipt and exact candidate
patch digest are in `rolling-wheel/singleton-degree-profile-pair.json`.

The fixed twenty-model `target/msl/multibody-singleton-degree-canary` has no
phase or band deltas from `multibody-manifold-preflight-canary`. Its nine
compared models and 175 initial channels remain high, with zero skipped,
excluded, missing, or nonidentifiable traces. This Tier 1 result is on parent
`45b3507e00b0b073573864eeffd8a0ea64d40e6d`, candidate worktree digest
`e5ce5b73a334ab799b53e6003fdd031e353d60e285e0aaf268529c25b75ebb77`;
receipt `rolling-wheel/singleton-degree-canary-delta.json`. The following full
cohort sweep must decide whether the wheel-set timeout is closed.

### Fourbar_analytic: supplied derivative chains

MLS §12.7.1 permits a higher-order annotation only in the differentiation
chain that issued its arguments. The candidate follows checked predecessor
links in source priority order and records each original argument together
with its derivative order. Each link appends only derivatives of the previous
link's last tangent group. Reconstruction consumes this same compact plan;
whole tensor arguments remain whole. Governing contracts are SPEC_0007's
structural stage, SPEC_0032, and SPEC_0036/SPEC_0043 §10.

The reduced oscillator has `der(q)=v`, `der(v)=-q`, a position function with
the same max-like branch as the MSL joint, and both kinematic derivatives.
Its first-order control passes, while requesting acceleration fails at 4/5
structural matches (`fourbar-analytic/derivative-chain-red-1.log`). OMC accepts
both branches and the first-order control. Additional quadratic-function and
direct-call controls verify the full argument history; all seven OMC cases
match their analytic trajectories within `4.40e-8` absolute error. The two
`analytic-comparison.json` receipts are under `derivative-chain-omc` and
`derivative-chain-quadratic-omc`.

Extending selection alone is insufficient: holonomic value materialization
and its first-derivative proof also looked through the primal function body.
Value reconstruction now retains the checked pure call after proving every
argument reconstructible from state/invariant values. Its supplied first
derivative is checked through the same selected argument plan. This preserves
the original assertion behavior and avoids requiring a parameter-only guard
proof inside the supplied function. The reduced second-order system retains
two dependent states with two manifold constraints, leaving two independent
states; both BDF and RK traces agree with the analytic oscillator.

Six focused tests cover both branches, a nonlinear second derivative, whole
tensor argument/result shapes, a direct call lacking the higher-order context,
and both valid and invalid assertion domains. The quadratic direct-call
control supplies `q_d=2*v`; incorrectly assuming this is `der(q)` would change
its derivative by `4*v*v`. It retains the structural refusal. All 154
structural tests and the then-571 compiler-core tests pass, followed by the
expanded six-case suite and thirty derivative regressions. Structural and
compiler-core Clippy, 243 architecture tests, seventeen size/spec gates,
formatting, and whitespace checks pass. Logs include
`derivative-chain-gates-build-{1,2}.log`, `derivative-chain-green-6.log`, and
`derivative-chain-final-checks-2.log` under `fourbar-analytic`.

The fixed twenty-model `target/msl/multibody-derivative-chain-canary` has no
phase or band changes from `multibody-shared-call-canary`. Its nine compared
models and all 175 initial channels remain high, with zero skipped, missing,
excluded, or nonidentifiable traces. This Tier 1 capture uses parent
`5d0f62d147caa94e04befb9532f437b0b16cb9de` and candidate worktree digest
`7a0bca0656d146c6225b43f0262d9039b35092b9b6966bda0ed399af6cd1b641`;
`fourbar-analytic/derivative-chain-canary-delta.json` retains the exact delta.

The originating `target/msl/multibody-derivative-chain-fourbar` attempt still
reports 1622/1659 structural matches, with 7.872 seconds of build time and no
simulation. Its gate correctly fails as parity unmeasured. The current
inspector again emits the same 829 reduction records
(`reduction-after-derivative-chain-1.log`). A bounded generic rejection probe,
removed after capture, identifies an earlier obstruction: the joint's
`prismatic.position_b` input is driven by three Constant-block outputs.
Flatten emits three indexed connection equations (`source-stages/ir-flat.mo`
lines 991–993), but variable 476 has no whole-vector causal definition.
The differentiation proof stops on this vector before reaching the supplied
derivative chain. `rejection-probe-2.log` retains that evidence. The next
investigation is preservation of the source aggregate connection through
Flatten and ToDAE; no coverage gain is attributed to this reduced fix yet.

### Fourbar_analytic: differentiating a parameter-selected position

The existing `omc-structure` inspector reproduces the original 1622/1659
structural refusal. An observed reduction of the exact source DAE shows why
that final diagnostic alone is insufficient: direct demotion reaches six
unmatched equations, while the later pristine holonomic path reaches eight.
The six-equation frontier names orientation/position equations and leaves
`jointSSP.totalPower`, body accelerations, and `b2.body.frame_a.t[1]` unmatched.
The reducer ultimately returns its preserved earlier refusal. Logs are
`fourbar-analytic/structure-1.log` and `fourbar-analytic/reduction-1.log`.

Rebuilding the structural inspector after the parameter-branch change
and replaying the same checked source DAE produces exactly the same 829
reduction records. Both lanes reach a minimum residue of twelve (six unmatched
equation/unknown pairs); the later holonomic lane finishes at sixteen. The
record-stream SHA-256 is
`0936b7735c12449841f9fb2a55e5f1092ca8aea833ed3dcbab0cc62b2ed6434c`
before and after. `fourbar-analytic/parameter-branch-reduction-delta.json`
binds this result to the current capture. The reduced parameter case is
repaired, but this evidence gives it no credit for advancing Fourbar_analytic.

OMC stage captures include flattened Modelica, flat/optimiser/backend XML,
and transformation-debugger equations in `fourbar-analytic/omc-stages`.
OMC selects exactly `j1.phi` and `j1.w` as states, with one linear torn system
(one iteration variable and thirty inner variables), no nonlinear torn
system. Its equations explicitly propagate first and second derivatives
through the prismatic joint's quadratic position solution.

The source joint computes `distance = -k1 + (if positiveBranch then k2 else
-k2)`, where `positiveBranch` is a Boolean parameter solved during
initialization. Structural differentiation had no conditional-expression
case unless the guard could already be selected as a literal. Parameter
bindings deliberately are not literals: initialization and parameter updates
must still choose the branch.

The reduced `ParameterBranchKinematics` oscillator preserves two forced
states, observes a parameter-selected signed position, and requests both
kinematic derivatives. The unconditional alias control passes. Both a bound
Boolean parameter and a `fixed=false` initialized Boolean fail structural
analysis at 4/5 matches, leaving the position equation and acceleration
unmatched (`fourbar-analytic/parameter-branch-red-2.log`). OMC accepts the
literal, initialized-positive, and initialized-negative sources; all five
channels match the analytic oscillator within `9.44e-9` absolute error
(`fourbar-analytic/parameter-branch-omc/analytic-comparison.json`).

The candidate derives guard invariance from parameter coordinates and their
checked operations/call-argument substitutions, requires every branch value
to satisfy the relevant differentiation/materialization proof, and retains
the original conditions and branch order around the differentiated values.
Tensor branches retain their whole shape, including zero derivatives.
Governing requirements are MLS §3.6.5, §3.8.3, §8.6, SPEC_0007's structural
stage contract, SPEC_0032, and SPEC_0036. All six focused tests pass, including
function substitution, both tensor branches, and rejection of time/state
guards from this parameter-only proof. The tensor test audits the prepared
DAE: the position and both derivative conditionals retain vector shape `[2]`.
All 154 structural and 566 compiler-core tests pass
(`fourbar-analytic/parameter-branch-gates-1.log`). All-target, all-feature
structural Clippy, 243 architecture tests, and seventeen size/spec gates pass
after replacing a manual even-index test with `is_multiple_of`
(`fourbar-analytic/parameter-branch-gates-build-2.log`). Formatting and
whitespace checks pass.

The normal-budget `target/msl/multibody-parameter-branch-fourbar` attempt
still returns 1622/1659 structural matches, in 8.389 seconds of build time.
It produces no simulation or comparison: the focused gate correctly reports
parity unmeasured. This reduced repair does not close the original model.
The fixed `multibody-parameter-branch-canary` passes with all twenty
phase/simulation and band rows unchanged from `multibody-native-singleton-canary`.
All nine compared models and 175 initial channels remain high, with zero
deviating, skipped, missing, excluded, or nonidentifiable comparisons. It runs
at `b102b3f7`, dirty-tree digest
`93d23c8089bdbfe881e2f3c80952ec46d792d48a9955f051fc6a39558860c837`;
the receipt is `fourbar-analytic/parameter-branch-canary-delta.json`.
Tier 1 is complete. A named-commit cohort sweep and continued equation-level
investigation of Fourbar_analytic remain next.

The DAE JSON capture succeeds, but readable DAE rendering refuses a function
assertion (`unsupported-feature:dae-modelica-function-statement:assertion`).
The flattened text and exact DAE JSON remain available for this investigation;
that rendering limitation is separate from structural compilation.

## Previous complete measurement: quick-suite success

The MSL step of `verify quick --early-exit` at commit
`be391b3f25e216616af9167a16c48f5e95c6eb1d`, working-tree digest
`48672dfdec9c3416aac00ca9d56725958329a60236f921974bbf966a09020c35`,
passes the local fallback gate in 296.88 seconds with the quick suite's four
model workers. Its result, comparison, and band table are preserved in
`target/msl/multibody-quick-after-architecture-full`.
**All 152 compared models are strict-high (26.86% of 566)**, with seventeen
reviewed exclusions and zero missing or nonidentifiable traces. All 17,606
initial channels are high; no trajectory channel deviates. Every high model
from `multibody-function-scope-index-full` retains its band, including all nine
original electrical counterexamples.

MultiBody is **19/42 high**, with nineteen compared and no trace boundaries.
RollingWheel completes with all 184 channels high and 9.656 seconds of
simulation including initialization. Its earlier twelve-second timeout remains
recorded below: this run follows an architecture repair, not a demonstrated
runtime performance fix. IMC_Transformer still changes from a structural
refusal to a Solve timeout relative to `multibody-function-scope-index-full`;
the full delta is `rolling-wheel/verify-quick-architecture-msl-delta.json`.

The combined `verify quick --early-exit` suite passes in 1396.72 seconds:
workspace lint, MSL parity, all 28 pinned corpus rows (89.3 seconds), 243
architecture-hardening tests, seventeen size/spec gates, all 7,516 nextest
tests across 146 binaries, and workspace doctests. Existing ignored doctest
examples remain ignored. The log is
`.git/multibody-campaign/verify-quick-after-architecture-1.log`; the timing
receipt is `rolling-wheel/verify-quick-after-architecture-passed.json`.
This validates the implementation at `be391b3f`; the subsequent `6ba7bb80`
commit only records evidence. `verify full` has not run. Further explicit MSL
runs retain the approved eleven simulation workers.

### Native execution of singleton projection assignments

A fresh RollingWheel `perf` capture attaches at the worker's declared `Sim`
phase, after build and the separate initialization check. The pre-change
`rolling-wheel/quick-rolling-runtime-profile` capture records 1,799 samples,
zero lost, and 9.115 seconds of simulation. Prepared row interpretation has
12.95% self samples. Caller unwinding is incomplete, so this is not an
inclusive cost attribution to any particular projection caller.

Source inspection identifies an interpreter-only consumer in
`RefreshProjectionModel::eval_implicit_target_value`. The candidate hands its
existing constructor-derived exact assignment to the optional native backend
and caches compilation by immutable program/target identity. Only source
programs with one stored scalar output are admitted; aggregate programs keep
their shared owner and existing execution path. The compiler's assignment
materializer preserves independent tiny offsets and singularity guards
(SPEC_0036 and SPEC_0043 §6a, SPEC_0032). No equation, state selection,
tolerance, or model-name policy changes.

The focused regression first records two failures: no native execution and
an injected native error hidden by the interpreter-only path. All six new
tests pass, covering reuse, compilation refusal caching, error propagation,
aggregate retention, tiny roots, and identical singular-coefficient declines.
The simulator's existing injected-failure test initially expected successful
interpreter completion; it now requires the native projection error while
retaining its zero-success and nonzero-failure assertions. All 447 solver and
127 simulator library tests pass. Logs are
`rolling-wheel/native-singleton-red-2.log`,
`rolling-wheel/native-singleton-tests-build-1.log`, and
`rolling-wheel/native-singleton-tests-build-2.log`.

The matching post-change capture,
`rolling-wheel/native-singleton-runtime-profile`, completes simulation in
8.677 seconds against the same twelve-second budget. Its complete trace JSON
is byte-identical to the pre-change trace
(`f8df74c085fa7a397df10ebdf9754cc48bd2bf34776aa81ebb85b9e5ac23e31c`).
This single pair observes a 4.8% reduction; it does not establish a stable
speedup or replace a cohort comparison. Prepared interpretation has 11.39%
self samples. Build and separate initialization check change from 4.146 /
0.471 seconds to 3.841 / 0.451 seconds. The timing receipt is
`rolling-wheel/native-singleton-runtime-timing-delta.json`.

The fixed `target/msl/multibody-native-singleton-canary` passes in 9.49
seconds with eleven workers at HEAD `6ba7bb80`, working-tree digest
`7c13adb6fd70e33d052159b2b017ed0def8711e7f1d85defd32be26d33a4d81a`.
All twenty phase/simulation and band outcomes match
`multibody-function-scope-index-canary`. All nine compared models and 175
initial channels remain high, with no deviating, missing, skipped, excluded,
or nonidentifiable comparisons. The retained receipt is
`rolling-wheel/native-singleton-canary-delta.json`. Tier 1 is complete;
the next full cohort measurement remains pending. All-target, all-feature
Clippy for solver/simulator, 243 architecture-hardening tests, and seventeen
size/spec gates pass (`rolling-wheel/native-singleton-gates-1.log` and
`rolling-wheel/native-singleton-size-gates-1.log`). The first combined command
named the nonexistent `code_size_test` target after its successful Clippy and
architecture steps; the subsequent command runs the actual `suite_gates`
target. Formatting and whitespace checks pass.

## Previous complete measurement: quick-suite gate failures

The MSL step of `verify quick --early-exit` at commit
`03865c4e9e2fb3e4c6cae86b1b9b233cd5237e91`, working-tree digest
`48672dfdec9c3416aac00ca9d56725958329a60236f921974bbf966a09020c35`,
compares **151 models, all strict-high (26.68% of 566)**, with seventeen
reviewed exclusions and zero missing or nonidentifiable traces. All 17,422
initial channels are high, and no trajectory channel deviates. Its result,
comparison, and band table are preserved in
`target/msl/multibody-quick-after-scalar-contact-full`; the delta is
`rolling-wheel/verify-quick-msl-delta.json`.

MultiBody falls to **18/42 high** because RollingWheel exceeds the unchanged
twelve-second simulation budget, after successful initialization. This is
an execution regression, with no observed deviating trace. PrismaticConstraint
and IMC_Transformer also change from structural refusals to Solve timeouts.
This invocation used the quick suite's automatic four-worker schedule, while
the preceding explicit full run used eleven. The timeout is retained as a
failure; neither the passing fallback gate nor earlier successful measurements
establish reliable completion. All nine original electrical counterexamples
retain high parity.

The combined quick suite passes lint, MSL parity, and the pinned external
corpus, then fails its architecture gate: Solve has 274 runtime totality
assertions against the unchanged ceiling of 272. Workspace tests do not run
after this early exit. Neither `verify quick` nor `verify full` is green.
The repair selects binary tensor builtin kinds and binary scalar operators
once in Solve lowering, then consumes those narrowed values. This removes
two impossible-case assertions without changing operand checks, operation
ordering, tensor ownership, or the gate. All 120 Solve library tests and 243
architecture-hardening tests pass. The next gate exposes oversized specs:
SPEC_0007 has 2680 words; SPEC_0036 has 2810 words and 367 lines. Their detailed
target-product, continuous-refresh, relation, and initialization requirements
move verbatim into the existing SPEC_0040/0043 catalogs, retaining mandatory
parent links and every requirement. No status or budget changes. The parents
now contain 2403/2414 words and 328/325 lines. All 243 architecture-hardening and seventeen size/spec gates pass.
`rolling-wheel/spec-catalog-move-receipt.json` verifies each moved passage
byte-for-byte against the prior commit. RollingWheel performance remains the next investigation
before MultiBody breadth work resumes.

## Previous complete measurement: function scope indexing

`target/msl/multibody-function-scope-index-full` passes the local fallback
MSL gate at commit `a2bf9abc7ea12a0c16a2674e22626d9abfd03c92`, digest
`48672dfdec9c3416aac00ca9d56725958329a60236f921974bbf966a09020c35`.
The 566-model run finishes in 136.49 seconds with eleven model workers.
**All 152 compared models remain strict-high (26.86% of 566)**, with seventeen
existing reviewed exclusions and zero missing or nonidentifiable traces.
All 17,606 initial channels are high; no trajectory channel deviates. Every
previously high model, including the nine original electrical counterexamples,
retains its band.

All four Solve completion regressions from the scalar-contact sweep recover.
DCPM_Drive completes again under its unchanged reviewed exclusion, with
9.185 seconds of Solve lowering and 9.383 seconds of build time. PrismaticConstraint,
GearConstraint, and GenerationOfFMUs again report their structural refusals
within budget. Raw simulation and initialization completions return to 169
and 183. Solve completion reaches 258: Fourbar_analytic now reports a
structural refusal, 1622/1659, instead of timing out. This gives a concrete
next diagnostic, not a new successful simulation.

MultiBody remains **19/42 high**, with nineteen compared, all 8,477 trajectory
and initial channels high, and no skipped, missing, excluded, or
nonidentifiable MultiBody traces. DAE completion remains 35/42. RollingWheel
reports 9.264 seconds of simulation including initialization. Relative to
the pre-contact `multibody-selected-jvp-full`, every simulation band and
execution outcome is retained; only Fourbar_analytic changes its refusal
classification. The full delta and hashes are in
`rolling-wheel/function-scope-index-full-delta.json`. Combined `verify quick`
and `verify full` success and complete MultiBody coverage are not established.

## Previous complete measurement: scalar contact reconstruction

`target/msl/multibody-scalar-contact-full` passes the local fallback-baseline
MSL gate at commit `adc5d4b2ac965d1b1778647ad78bc26dc16f5a6a`, digest
`48672dfdec9c3416aac00ca9d56725958329a60236f921974bbf966a09020c35`.
The full 566-model comparison finishes in 125.72 seconds with eleven model
workers. **All 152 compared models remain strict-high (26.86% of 566)**,
including all nine original electrical counterexamples. There are 16 existing
reviewed exclusions and zero missing or nonidentifiable traces. All 17,606
initial channels are high; trajectory channels comprise 17,589 high and
seventeen minor, with none deviating or severe.

MultiBody remains **19/42 high**, with nineteen compared and all 8,477
trajectory and initial channels high. There are no MultiBody trace boundaries;
DAE completion remains 35/42. No additional example closes in this run.
The repaired contact-coordinate core regression is independently green.

Four models lose completion of the Solve phase: PrismaticConstraint,
GearConstraint, GenerationOfFMUs, and DCPM_Drive. The first three previously
reported structural refusals; DCPM_Drive previously completed simulation
under an existing reviewed exclusion and now hits the ten-second Solve
budget. This lowers raw simulation completions from 169 to 168, initial
completions from 183 to 182, and Solve completion from 257 to 253. The
exclusion policy and all model bands are unchanged. The passing baseline
gate does not excuse these execution regressions; performance repair takes
priority over further capability work. The full delta and hashes are in
`rolling-wheel/scalar-contact-full-delta.json`.

A bounded standalone DCPM_Drive perf diagnostic captures 5,489 samples with
zero lost samples. It completes simulation, but Solve lowering takes 11.436
seconds and total build takes 12.168 seconds, confirming work beyond the
cohort's unchanged phase budget. Function reconstruction accounts for 16.25%
of self samples, causal-definition derivation 7.58%, and scalar affine
recognition 2.23%. Source inspection finds that orphaned-expression recovery
scans the full expression arena once per function. Indexing those existing
function scopes once is the next hypothesis; no repair is claimed yet.
This diagnostic completion does not replace the full-run timeout.

### Function scope indexing

The performance repair extends the existing function-expression inventory
pass with per-function scope lists. Orphan recovery visits each function's
existing branded expression identities in the same ascending source order;
the final completeness check remains intact. This removes the repeated
whole-arena scan without changing function statements, definitions, or
construction order. All 154 structural library tests pass.

The matched standalone DCPM_Drive diagnostic reduces Solve lowering from
11.436 to 7.804 seconds and build time from 12.168 to 8.302 seconds. The
new perf capture has 9,153 samples and zero lost samples. These are diagnostic
timings, not a statistical benchmark. Separate before/after artifact runs
produce byte-identical DAE, Solve IR, and simulation traces: the Solve SHA-256
is `095f50d80afa1cf221f3362fa4195d3b607b4899715bad02e9f6d7e4d926bcb3` and
the trace SHA-256 is
`b80f5af6dc932435adfc419704b75e0b5553f38cd520cedc316d78c8d29209ad`.
`rolling-wheel/function-scope-index-equivalence.json` records all hashes.
Affected all-target/all-feature Clippy checks and formatting pass. The fixed
`target/msl/multibody-function-scope-index-canary` passes with all twenty
phase, simulation, initial-condition, and band outcomes unchanged. Nine
models compare high, all 175 initial channels are high, and skipped, missing,
excluded, nonidentifiable, and deviating counts are zero. At HEAD
`09e66c8be6c5df71fbf964564516ff15eb678d0c`, its working-tree digest is
`94c4aafbbe07bf098a355f7069b749bfae1541297682197261500d1d2024eeca`.
`rolling-wheel/function-scope-index-canary-delta.json` records the delta.
The subsequent full run records all four Solve completion regressions as
recovered in the latest measurement; the underlying structural refusals remain.

## Previous complete measurement: selected native Jacobians

`target/msl/multibody-selected-jvp-full` passes the local MSL quality gate
at commit `45cd3b9054be6b57c80fcceba0feedb22818d315`, working-tree digest
`48672dfdec9c3416aac00ca9d56725958329a60236f921974bbf966a09020c35`.
The complete 566-model run finishes in 122.61 seconds with eleven requested,
effective, and pinned simulation workers. **152 models compare and all 152
remain strict-high (26.86% of 566)**; seventeen existing reviewed exclusions
account for the other simulation completions, with zero missing or
nonidentifiable traces. All 17,606 initial channels are high and no trajectory
channel deviates. Every model's simulation band and execution outcome is
unchanged from `electrical-affine-polished-full`, including all nine original
electrical counterexamples. The only phase-status change is Inverse_sh_TX
returning its earlier Flatten refusal instead of a DAE refusal.

MultiBody remains **19/42 high**, with nineteen compared, all 8,477 trajectory
and initial channels high, and no skipped, missing, excluded, or
nonidentifiable MultiBody traces. DAE completion remains 35/42; the remaining
23 examples have the same failure classifications listed below. RollingWheel
now reports 9.409 seconds of simulation including initialization, versus
12.601 seconds in the preceding full run. The selected native JVP change
retains every prior high model but does not yet unlock another example.
The per-model delta and artifact hashes are recorded in
`rolling-wheel/selected-jvp-full-delta.json`. That run precedes the scalar
contact-coordinate repair below; this MSL gate does not establish
`verify quick` or `verify full` success or complete MultiBody coverage.

## Coupled scalar contact coordinates

The outstanding core fixture replaces the direct circular constraint with
`s+w=x`, `s-w=y`, and `2*(s*s+w*w)=radius*radius`. Its source and DAE retain
all seven equations, but structural matching previously stops at 6/7.
The auxiliary recognizer admits aggregate vector maps and dot equations;
it cannot materialize these two separately declared scalar coordinates.
The direct-coordinate control already passes. This is the known reduced
core failure, not evidence that a remaining MultiBody example has closed.

The repair applies the existing SPEC_0007 STRUCT-T03 square linear auxiliary
profile to blocks of source-authored scalar residuals. A closed affine proof
admits sums, signs, and products with unknown-independent state/invariant
coefficients. Connected components must be square; nonlinear and
underdetermined components remain refused. Membership, equation identity,
and state anchors belong to one shared block. Its coordinates project one
aggregate `LinearSolve` result, with one function and derivative cache per
block. The recognizer never consumes tensor scalar views or enumerates a
tensor extent. Source residuals remain owners, and their own reconstruction
cannot be used to replace them with identities. This preserves MLS §8.3.1
equation semantics and §8.6 initialization obligations through the existing
checked DAE construction and SPEC_0032 aggregate execution.

For `A*q=b`, reconstruction uses the existing aggregate relations
`A*q'=b'-A'*q` and `A*q''=b''-A''*q-2*A'*q'`; runtime singularity remains a
checked solve failure. The original circular-motion regression now passes
with both BDF and RK. Thirteen manifold tests pass, including linear and
quadratic time-dependent coefficients, state-dependent coefficients, and
rejection of inconsistent fixed initial positions. All 153 structural
library tests pass, including shared membership and refusal controls; the
complete core suite passes 559 tests with zero failures. Affected
all-target/all-feature Clippy checks pass after extracting the component
membership update from an excessively nested loop.

Review then finds that memoizing affine decisions alone still copies shared
coefficient subexpressions into trees. A focused RED doubles source-DAG depth
from four to eight and grows the coefficient recipe from 174 to 2,574 nodes.
The corrected recipe shares source expressions and coefficient identities;
operand traversal visits each shared recipe once. Reconstruction caches each
coefficient with its source identity, coordinate/offset role, derivative
order, exact call context, reconstruction mode, and provenance. The DAG-growth
regression now passes, and all 154 structural library tests pass. Core and
canary validation are repeated for this final representation change: all
560 core tests and the affected all-target/all-feature Clippy checks pass.

An added producer test passes: source DAE has zero aggregate linear solves;
the prepared DAE has exactly one, retains seven continuous equation owners
and both position/velocity manifolds, and both original scalar definitions
still reference both auxiliary coordinates. The worker's current
`ir-structural-dae.*` writer serializes its input DAE before reduction, so
those files are not used as evidence of this producer change.

The OMC stage dump explicitly retains `der(s)`, `der(w)`, and their second
derivatives. Its original circular fixture produces twelve rows and agrees
with the analytic solution in all seven physical channels; maximum absolute
error is 6.485e-8. A separate changing-coefficient fixture fails OMC
initialization (`0 != 1 = $START.vy-vy`) and produces no data rows. It has
analytic Rumoca regression evidence only; no OMC trace agreement is claimed
for that fixture. An initial test written with powers also exposed the
existing structural refusal of `u^2`; the coefficient regression uses
multiplication to isolate this repair, and power differentiation remains
separate work. A native worker run of each fixture checks all seven channels
at eleven samples through 0.1 seconds against the analytic solution; maximum
absolute error is 2.731e-10. The original fixture also matches OMC at all
nine exactly shared timestamps, with maximum absolute difference 6.485e-8;
two Rumoca timestamps have different floating-point encodings from the OMC
grid and are covered by the analytic check only.

The fixed `target/msl/multibody-scalar-contact-canary` passes with all twenty
phase, simulation, initial-condition, and band outcomes unchanged from
`multibody-selected-jvp-canary`. Nine models compare high and all 175 initial
channels are high, with zero skipped, missing, excluded, nonidentifiable,
or deviating traces. At HEAD `705ad773801b86f51e01c1ba63822ea58e92703a`, its
working-tree digest is
`b4737e571d9ded6073b571cc86dd997c1acea27a5d08f620cc0585d9589e69eb`.
The delta is `rolling-wheel/scalar-contact-canary-delta.json`; fixture traces
and equations are under `rolling-wheel/scalar-contact-*`. The subsequent
complete comparison is recorded in the latest measurement. The repeated
`target/msl/multibody-scalar-contact-shared-canary` retains all twenty outcomes
and the same nine compared high models, 175 high initial channels, and zero
boundary/deviating counts. Its digest at the same HEAD is
`f1ab545d8ddecd19f40edda2b3a6cd8bd1c1cc179b9d2214d9c42d04d312934f`;
the delta is `rolling-wheel/scalar-contact-shared-canary-delta.json`.
Native fixture reruns retain all seven channels within 2.731e-10 of the
analytic solution. Formatting and `git diff --check` pass.

## Previous complete measurement: electrical parity recovery

`target/msl/electrical-affine-polished-full` passes the local MSL quality
gate against the checked-in fallback baseline. Its complete 566-model
comparison finishes in 132.74 seconds at commit
`3a7c0b12f46f94220cd56e7b8f64f0337d18a08a`, working-tree digest
`48672dfdec9c3416aac00ca9d56725958329a60236f921974bbf966a09020c35`, with
eleven requested and pinned model workers. **152 models compare and all
152 are strict-high (26.86% of 566)**. Of 169 simulation completions,
seventeen have existing reviewed exclusions; missing and nonidentifiable
traces are zero. There are 17,589 high and seventeen minor trajectory
channels, zero deviating or severe channels, and all 17,606 initial channels
are high. The tracked baseline and exclusion policy are unchanged.

All twelve high-model execution regressions from the first affine sweep
are restored. Every previously high model from the RollingWheel and
fixed-pre full runs remains high, and all nine original electrical
counterexamples now close as strict-high in this complete comparison:

| ACDC model suffix | Full-run outcome | High trajectory channels |
|---|---|---:|
| Rectifier1Pulse.Thyristor1Pulse_R_Characteristic | high | 95 |
| RectifierBridge2Pulse.ThyristorBridge2Pulse_RL | high | 146 |
| RectifierBridge2mPulse.ThyristorBridge2mPulse_RLV | high | 429 |
| RectifierCenterTap2Pulse.ThyristorCenterTap2Pulse_RL | high | 133 |
| RectifierCenterTap2mPulse.ThyristorCenterTap2mPulse_R | high | 417 |
| RectifierCenterTap2mPulse.ThyristorCenterTap2mPulse_RL | high | 423 |
| RectifierCenterTapmPulse.ThyristorCenterTapmPulse_R | high | 307 |
| RectifierCenterTapmPulse.ThyristorCenterTapmPulse_RL | high | 313 |
| RectifierCenterTapmPulse.ThyristorCenterTapmPulse_RLV | high | 316 |

MultiBody returns to **19/42 high**, with nineteen compared, all 8,477
trajectory and initial channels high, and zero skipped, missing, excluded,
or nonidentifiable traces. DAE completion remains 35/42. RollingWheel
completes its four-second horizon with all 184 channels high; its reported
12.601-second simulation runtime includes initialization and remains close
to the unchanged twelve-second integration budget. A perf investigation
is next to establish where runtime is spent before claiming a performance
repair or attributing earlier timeouts to host load.

The remaining 23 MultiBody examples comprise eleven Solve timeouts, three
structural refusals, two simulation timeouts, two Flatten timeouts, four
DAE refusals, and one Instantiate refusal. The known implicit-contact-circle
core regression still fails at structural matching, 6/7. Combined
`verify quick` and `verify full` are not green, and no PR, release approval,
or baseline promotion is made from this gate alone.

One previously completed reviewed exclusion,
`RectifierBridge2Pulse.ThyristorBridge2Pulse_RLV_Characteristic`, now times
out in simulation. This execution loss remains visible even though it was
never strict-high. GearConstraint reaches its structural refusal within
the phase budget instead of timing out; Inverse_sh_TX changes from Flatten
to DAE refusal without a corresponding producer edit. Those diagnostic
changes are recorded, not claimed as new language capability. Full per-model
deltas and artifact hashes are in `affine-polished-full-delta.json` and
`full-execution-delta.json` under the inverter investigation.

The subsequent RollingWheel perf diagnostic captures 4,001 samples with
zero lost samples at 199 Hz and completes under the unchanged solver budget.
It reports 11.776 seconds including 0.575 seconds of initialization. In the
last 45% of the capture, prepared row evaluation accounts for 22.42% of
self samples, tensor concatenation 5.82%, memory comparison 5.65%, matrix
multiplication 3.59%, and scalar pure-call payload adaptation 3.35%.
Source inspection finds selected projection residual/JVP evaluation uses
the interpreter even though the backend interface offers compiled whole-block
evaluation. Call metadata is also checked in both the interpreter and native
adapter. These are investigation leads, not established repair claims;
compiled-program availability and selected-output ownership need verification
before changing dispatch. `rolling-wheel/affine-polished-performance-receipt.json`
records the profile, request, result, and hashes. No gate result is replaced
by this diagnostic run, and host contention remains an unproven explanation
for the earlier timeout.

### Selected native Jacobian execution

The bounded availability probe confirms that RollingWheel's implicit residual,
Y-only JVP, and full-seed JVP all compile successfully. The first avoidable
work is in `RefreshProjectionModel::eval_implicit_jacobian_v_row`: it always
evaluates the selected prepared program in the interpreter. The native
adapter already owns one callable per Jacobian program, but its public
execution interface previously exposed only a whole-block sweep.

Two focused regressions reproduce the missing dispatch: four selected
requests make zero native calls, and a selected native failure is bypassed
while the interpreter returns a value. They pass after adding a selected
program-output entry point through the runtime, simulation adapter, and
Cranelift product. The coordinate comes from the existing prepared JVP view,
including its exact program index and aggregate output offset. This follows
SPEC_0007 SOLVE-C13/C14/C15, SPEC_0032 scalar-view ownership, and SPEC_0029's
runtime/backend boundary. It changes execution of the existing AD artifact;
it does not change Modelica equations, tensor ownership, or differentiation.

The native entry point checks the program, output extent, and input extents,
executes that program into reusable private aggregate storage, and returns
only after successful execution. Unrelated programs remain unevaluated.
External-table context, sparse whole-block output placement, static-gradient
reuse, and both seed spaces retain their existing ownership. An unavailable
selected entry point returns `None`; an execution failure propagates.

The library run passes 440 solver, 72 native-backend, and 127 simulation
tests. An added mapping test passes with reordered JVP programs and a
multi-output JVP program. The thirteen native policy tests also pass after
their counting wrapper delegates the new entry point. Native tests cover
changing Y/P/time/seeds, sparse output placement, aggregate output offsets,
invalid coordinates and extents, and an unrelated missing-table program
that must not execute. All affected all-target/all-feature Clippy checks
pass. Core validation has 556 passes and the same one contact-circle
structural failure, 6/7. Original-model OMC/perf, canary, and full-cohort
validation are recorded below and in the latest measurement. The red failures
are recorded in `rolling-wheel/selected-jvp-receipt.json`.

The originating comparison `target/msl/multibody-selected-jvp-origin` at
HEAD `7e05287df7eb7dede5cb95b9101968588f0b443b`, working-tree digest
`007525460aca735748b223cd2ad120e16fb7252da6e6d0ed39e68e9343ee3bae`,
compares one model and keeps all 184 trajectory and initial channels high.
Skipped, missing, excluded, nonidentifiable, and deviating counts are zero;
worst channel bounded normalized L1 is 1.272e-4. The reported simulation
runtime is 8.887 seconds including initialization.

A matching standalone perf capture at 199 Hz, with the same model settings,
four-second model horizon, and twelve-second solver budget, completes in
8.723 simulation seconds including 0.455 seconds of initialization. The
preceding capture reported 11.776 seconds including 0.575 seconds of
initialization: a 25.9% reduction across these two captures. Build time
remains about four seconds. The new capture has 3,400 samples and zero lost
samples. In its last 45%, prepared row evaluation falls from 22.42% to
11.79% of self samples and native Jacobian row functions appear among the
largest entries. Residual evaluation and call-interface work remain visible.
This is bounded diagnostic evidence, not a statistical benchmark or a new
cohort number; the original-model OMC comparison separately establishes
trace agreement. No temporary probes remain.

The fixed `target/msl/multibody-selected-jvp-canary` passes with all twenty
phase, simulation, initialization, and model-band outcomes unchanged from
`electrical-offset-ports-canary`. Nine models compare high; all 175 initial
channels are high, with zero skipped, missing, excluded, nonidentifiable,
or deviating traces. Its working-tree digest at the same HEAD is
`35c3a4205ecd131713a68486f37f4104d563cb5a5dd98500d450e4f73596e34a`.
The durable delta is `rolling-wheel/selected-jvp-canary-delta.json`.
Formatting passes. The subsequent complete cohort at `45cd3b90` uses eleven
simulation workers and retains all preceding high models and electrical
counterexamples, as recorded in the latest measurement.

## Previous complete measurement: first direct affine solve

`target/msl/electrical-affine-full` completes the full 566-model comparison
in 127.82 seconds at commit `82630a4d2361ef673d921c49ada8519ce51ed08b`,
working-tree digest
`48672dfdec9c3416aac00ca9d56725958329a60236f921974bbf966a09020c35`.
The quality gate fails: **139 models compare and all 139 are high**
(24.56% of 566), but twelve previously high models now fail execution.
The original center-tap rectifier gains high status. Of 150 simulation
completions, eleven have existing reviewed exclusions; missing and
nonidentifiable traces are zero. Of 14,471 trajectory channels, 14,461 are
high and ten minor; none deviate. All 14,471 initial channels are high.
Seven previously observed exclusions no longer complete;
the exclusion policy itself is unchanged. These failures are not closures.

MultiBody remains **18/42 high**, with 18 compared and zero skipped, missing,
or nonidentifiable traces. DAE completion remains 35/42. RollingWheel still
times out with eleven pinned workers. Its performance investigation remains
pending while the electrical execution regressions take priority.

The numerical follow-ups below restore all twelve lost high models in
focused comparisons, but a new complete measurement is still required.
Neither this full run nor the follow-up establishes release readiness.

## Previous complete measurement: fixed-pre iteration

`target/msl/electrical-fixed-pre-full` completes the full 566-model MSL/OMC
comparison in 149.53 seconds at HEAD
`49477020cc7706352fa314fe8384c08d9695b1d8`, working-tree digest
`ee6f7e2ea0ff9fd0f00ee2d581f7340c07b021e55635f049c1dcd961a9130d46`.
Of 168 simulation completions, **150 compare and all 150 are strict-high**
(26.50% of 566). There are zero near models and zero deviating or severe
channels among 17,115 compared trajectory channels; all 17,115 initial
channels are high. Eighteen skipped traces have existing reviewed policy
exclusions; missing and nonidentifiable traces are both zero. The exclusion
policy is unchanged: `RectifierBridge2mPulse.ThyristorBridge2mPulse_RL` now
completes and enters its already tracked exclusion, increasing the observed
excluded count from 17 to 18.

Eight of the preceding nine electrical counterexamples now compare high.
The remaining `RectifierCenterTapmPulse.ThyristorCenterTapmPulse_R` fails
with `EX002: event condition equations did not converge with fixed pre at
t=0.00333333333333337`. This is an execution regression, not counterexample
closure. It also loses baseline-certified high status, so the quality gate
fails. Investigation remains focused on this switching event.

MultiBody measures **18/42 strict-high**, with 18 compared and zero skipped,
missing, or nonidentifiable MultiBody traces. The previously high elementary
`RollingWheel` now exceeds the unchanged 12-second solver budget. Its
simulation timing is 12.598 seconds including initialization; the remaining
24 examples have no comparable successful trace. DAE completion stays
35/42. The scheduler requested 16 workers and memory-capped execution at 11
pinned workers, which the user accepted. The cause of the RollingWheel
timeout has not yet been profiled; the preceding 19/42 result is historical.
Budgets, thresholds, references, and the baseline were not relaxed.

### Fixed-pre event equation investigation

The originating `Rectifier1Pulse.Thyristor1Pulse_R_Characteristic` has the
source equation `off = s < 0 or pre(off) and not fire`. Its source, DAE, and
Solve program retain that equation and the separate `pre(off)` slot. The
old runtime turned the thyristor on at time 0.020000000000019592, before its
correct firing edge at 0.02002002002004838. OMC changes both `off` and `fire`
at 0.02002002002102569. At time 5.005, Rumoca previously conducted while
`fire` was false and OMC held the device off. The final firing pulses agree;
the defect is not a shifted controller waveform or lost source operator.

A periodic timer/reset/latch reduction reproduces the defect with both BDF
and RK: at time 1.01 the latch wrongly holds true, before the required 1.1
firing edge. A one-shot control already passes. OMC agrees with the periodic
fixture's analytical latch, timer, and integral, with maximum integral error
2.494e-13. Both solver regressions now pass. The governing contracts are
MLS Appendix B, SPEC_0022 SIM-001/SIM-008/SIM-009, and SPEC_0040 SOLVE-C22:
solve current discrete and condition equations with fixed `pre` before
advancing ordinary event history atomically.

The runtime had refreshed condition memory only after settling discrete
equations, then advanced `pre` before the changed conditions could correct
a transient latch value. The new inner iteration settles current equations
and condition memory together under the same history snapshot. It retains
the clocked first-pass restriction and fails explicitly on nonconvergence.
The original focused MSL comparison passes on all 95 trajectory and initial
channels, with zero skipped, missing, excluded, or nonidentifiable traces.
Its regenerated OMC trace is byte-identical to the preceding reference.

The fixed 20-model canary retains every preceding phase, simulation,
initialization, and band outcome: nine compared, nine high, all 175 initial
channels high, and zero skipped, missing, excluded, or nonidentifiable
traces. Solver tests pass 434/434. Core tests pass 552 with the one known
implicit-contact-circle structural failure; focused all-feature Clippy
passes. Combined `verify quick` and `verify full` are not green. Evidence,
artifact hashes, and the full delta are under
`buffered-relation-counterexamples/thyristor-1pulse` in the campaign directory.

### Remaining rectifier: stale affine coordinates

The next investigation maps the center-tap failure to thyristor 3: its `off`
value is Solve P230, its switching variable `s` is Y108, and root 5 writes
relation-memory P383. Its 39-variable algebraic block tears on Y113, the
common output voltage. OMC preserves the same `off` equation as regular
equation 329 inside coupled block 354. Rumoca's 32 fixed-pre iterations
alternate `off` between true and false: the projected `s` alternates between
3.7526e-15 and -9.0816e-14. No root overrides remain during the oscillation.

At the failing time, a 70-digit solution of the original resistor network
using its binary64 source voltages gives `s = -1.81523e-12` on the off
branch and `s = -9.08160e-14` on the conducting branch. Both are negative,
so the all-off branch is consistent. The runtime instead retains the
preceding conducting output voltage after changing branch: its residual
and correction are both below tolerance, but the resulting switching
variable has the wrong sign. The first divergence is the algebraic
projection, not the source equation, event history, or a physically
unsolvable switching state.

A two-variable affine regression reproduces the admission error without
MSL: a certified solve returns the incoming positive 1e-20 instead of the
unique negative -5e-21 solution. A source reduction using three instances
of a scalar switch also exposes BDF's delayed turn-off at time
0.10001431503167271. Both BDF and RK now match the analytical switching
state, current, and current integral. OMC matches the same fixture on 204
rows, with maximum current error 1.089e-13 and integral error 1.302e-14.
Two initial reduction variants encountered separate DAE array `pre` shape
and projected Boolean-assignment refusals; those are preserved as distinct
failures, not evidence of this runtime defect.

Construction-certified affine blocks now compute `A*x = -F(0)` and validate
the resulting residual using the existing tolerance. Exact singleton
assignments retain their direct path; nonlinear blocks retain their
existing branch-preserving iteration. This uses the existing block-affinity
proof, Jacobian, and factorization cache, without changing canonical IR,
relation thresholds, event budgets, or comparator policy.

The original center-tap model passes the focused run
`target/msl/electrical-affine-origin` at HEAD `49477020`, working-tree digest
`913bd843d9ec054f9f969571429aca3317a58a221bf2a7a56608cd8ecec6ad4a`:
one compared model, all 307 trajectory and initial channels high, zero
skipped, missing, excluded, or nonidentifiable traces. Worst channel
bounded normalized L1 is 9.174e-6. This is focused evidence; the latest
complete cohort above still records its failure. Projection tests pass
60/60, event integration tests 54/54, and core tests 554 with only the known
implicit-contact-circle failure. The existing affine refresh test is
strengthened from accepting a small-residual iterate to requiring its
unique exact zero solution. Evidence is under
`buffered-relation-counterexamples/center-tap-mpulse`.

The fixed 20-model canary `target/msl/electrical-affine-canary` retains every
preceding phase, simulation, initialization, and band outcome: nine compared,
nine high, all 175 initial channels high, and zero skipped, missing, excluded,
nonidentifiable, or deviating traces. Its working-tree digest is
`ec3bde07fcd915bdebb58cff20412e7250d4ac6ba2d04cb389cbf6ee809f8240` at the same
HEAD. Solver tests pass 435/435; focused all-feature solver and core Clippy
pass. The full 566-model sweep is next, with 11 simulation workers explicitly
accepted by the user. No full-cohort closure or release readiness is claimed
from these focused results.

### Affine residual scaling and refinement

The full sweep above exposes two defects in the direct affine path. In
`ChopperStepDown_R`, its first solution has opposing currents near three
million amperes during a switching iteration. The candidate's flow residual
is 1.979e-10, with a normalized residual of 1.184e-16 under the existing
coordinate-scale policy. The new path had instead certified it using the
temporary zero origin's scales and rejected it against 1e-10. A three-equation
resistor/current-source reduction reproduces the rejection. Certification
now derives scales from the actual candidate, as the existing Newton path
does; no model tolerance or comparator threshold changes.

In `RectifierCenterTap2mPulse.ThyristorCenterTap2mPulse_R`, the corrected
scaling still exposes a first-solve residual of 1.56146e-10 in a small
current, just above the unchanged 1e-10 tolerance. A four-variable circuit
with large opposing currents and small leakage reproduces that refusal.
The affine solve now refines against the original residual using the same
certified matrix until the normal certificate passes or the existing
projection iteration budget is exhausted. Failure preserves the incoming
coordinate. Both numerical regressions turn from red to green; all 437
solver tests and focused all-feature Clippy pass.

`target/msl/electrical-affine-refinement-focused` compares 14 of 15 selected
models, all high, with 3,340 high and three minor trajectory channels, all
3,343 initial channels high, and zero skipped, missing, excluded,
nonidentifiable, or deviating traces. Its HEAD
is `82630a4d`, working-tree digest
`b7d918c8e691a3b53dd091e6ac9b7fe5c99ec8455a9f8a63baecbfe09e1670b2`.
All nine original electrical counterexamples are high in this focused
check. Eleven of the twelve execution losses are restored; the remaining
`DCAC.SinglePhaseTwoLevel.SinglePhaseTwoLevel_RL` fails condition iteration
at time 0.0009362657394780009. It remains an execution regression requiring
repair, and the focused gains do not replace the latest full-cohort counts.
Artifacts and the two red regressions are under the `affine-regressions`
subdirectory of the center-tap investigation.

The follow-up canary `target/msl/electrical-affine-refinement-canary` keeps
all twenty members' phase, simulation, initialization, and band outcomes
unchanged: nine compared and high, 175 initial channels high, and zero
skipped, missing, excluded, nonidentifiable, or deviating traces. At the
same HEAD its working-tree digest is
`8db4964fc62f6158d73caf1c5118c43a19c4478185979bcc4b6936f96c2adacf`.
Core tests pass 554 with the same one implicit-contact-circle failure;
focused all-feature Clippy and formatting pass after extracting the
finite correction update to keep nesting within the existing limit.

### Inverter commutation beside offset port voltages

The remaining `DCAC.SinglePhaseTwoLevel.SinglePhaseTwoLevel_RL` failure is
a lower diode-to-transistor conduction transfer, rather than a PWM firing
change. OMC's regular equations 246/249/252/255 retain the same four switch
relations. At time 0.0009362657637039185, OMC transfers conduction with
lower firing true and upper firing false. Rumoca's source, DAE, and Solve
preserve these equations; the failing runtime state has inductor current
0.0020000011485478463 at time 0.0009362657394780009.

The lower transistor's switching coordinate is Solve Y43, root 2 writes
condition memory P251, and its source `off` is P168. Fixed-pre iteration
alternates the root between zero and -7.1054e-10. A 70-digit solution of
the source resistor network, using the captured binary64 inputs, instead
requires a negative junction voltage in both candidate configurations:
-1.14855e-14 on the transistor-off branch and -5.74274e-15 with both lower
devices on. Absolute port potentials near -50 V cause factorization to
lose the small independent junction coordinate. Its residual is already
within tolerance, so the prior refinement path accepts zero before trying
a correction. This localizes the divergence to runtime affine projection;
the source relations, history ownership, and upstream IR remain correct.

A four-variable numerical regression reproduces the failure with two offset
ports, junction voltage, and leakage current. It formerly returned zero
instead of the analytical negative voltage near -1e-17. The general affine
path now attempts one residual correction before accepting a nonzero
first residual, even when that residual already fits tolerance. Exact zero
requires no correction, finite stagnation retains the existing certificate,
and nonfinite corrections fail without publishing the private candidate.
Subsequent certification and iteration budgets remain unchanged. This
implements the existing MLS Appendix B and SPEC_0040 SOLVE-C22 obligation
to solve the current equations before accepting their relation values.

All 438 solver tests pass. An additional timed source control verifies
inductive current and complementary switching with both BDF and RK;
OMC matches its analytical current within 8.676e-11 across 84 rows, with
zero switching errors. The complete core suite passes 556 tests with only
the known implicit-contact-circle structural failure, 6/7 matched rows.
Temporary runtime probes are removed. These checks do not establish
`verify quick` or `verify full` success.

The originating focused comparison `target/msl/electrical-offset-ports-origin`
at HEAD `c4789fce2e90aaa0b33f1406007a6a90642ada43`, working-tree digest
`2833c01141a5261c5c48c7aa9f5c3301be1e661ab2e1f75e9ea998e54857c991`,
compares one model and classifies it high: 139 trajectory channels are
high and four minor, with all 143 initial channels high. Deviating, skipped,
missing, excluded, and nonidentifiable counts are zero. Runtime simulation
takes 0.868 seconds. Proof, red/green logs, source/IR hashes, and the OMC
control are under `buffered-relation-counterexamples/inverter-rl` in the
private campaign directory.

The fixed `target/msl/electrical-offset-ports-canary` retains all twenty
phase, simulation, initialization, and model-band outcomes from the preceding
refinement canary. Nine models compare high, with all 175 initial channels
high and zero skipped, missing, excluded, nonidentifiable, or deviating
traces. At the same HEAD its working-tree digest is
`dfec1300d8534c5246c51b960f04ee0b4a3891470b1a1fabdee91d6146cd3570`;
the delta is `offset-ports-canary-delta.json`. Solver/core all-target,
all-feature Clippy and formatting pass. The complete 566-model sweep is
next, using the user-approved eleven simulation workers.

## Previous complete measurement: RollingWheel completion

`target/msl/multibody-rolling-wheel-full` completes the full 566-model
MSL/OMC comparison in 298.66 seconds at HEAD
`bc71577f85df24957e5c9ab30fdaf4ed48da4311`, working-tree digest
`be5115934a4405aed33efc58bb7e96e93cd1f0f0cabd031311a603c7a50d1312`.
The quality gate still fails on the nine electrical counterexamples below.
Of 169 simulation completions, 152 compare: 143 are strict-high (25.27% of
566), and nine are near. Seventeen unchanged reviewed exclusions account for
all skipped traces; missing and nonidentifiable traces are both zero.
The 59 deviating trajectory channels, including five severe, are unchanged
from `multibody-torn-roundoff-full`. All 17,606 compared initial channels are
high. These are comparator bands; a near model can contain deviating channels.

MultiBody now has **19/42 strict-high examples**, up from 14/42. All 19
completed traces compare, with all 8,477 trajectory and initial channels high,
and zero skipped, missing, excluded, or nonidentifiable MultiBody traces.
The five gains are `HeatLosses` (798 channels), `PendulumWithSpringDamper`
(422), `RollingWheel` (184), `SpringDamperSystem` (605), and `Fourbar2` (585).
The same compiler changes also add high-band results for magnetic
`MovingCoilActuator.ArmatureStroke`, rotational `RollingWheel`, and
translational `InitialConditions`. ArmatureStroke has two minor channels and
zero deviating channels under the existing high-band policy. All 135 models
high in the preceding full sweep retain that band. No thresholds, budgets,
exclusions, or baseline were changed.

The remaining 23 MultiBody examples comprise eleven Solve-stage timeouts,
two simulation timeouts, three structural refusals (`PrismaticConstraint`,
`DoublePendulumInitTip`, and `GearConstraint`), two Flatten timeouts, four
DAE refusals, and one Instantiate refusal. All 42 remain in scope. DAE
completion remains 35/42. Fourbar_analytic advances from structural refusal
to a Solve timeout, while RollingWheelSetDriving advances from a Solve
timeout to a simulation timeout. PrismaticConstraint now returns its
structural refusal within budget. These failure-stage transitions are not
parity gains.

All fixed 20 canary members retain their preceding phase, simulation,
initialization, and band outcomes when selected from this full sweep;
nine compare high, including all 175 initial channels, with zero missing,
skipped, excluded, or nonidentifiable traces. This subset comparison is a
regression tripwire, not a separate canary execution or cohort measurement.
The full-run delta and artifact hashes are recorded in
`rolling-wheel/full-msl-rolling-wheel-delta.json` under the campaign directory.

The nine actionable counterexamples remain in
`Modelica.Electrical.PowerConverters.Examples.ACDC`:

| Rectifier family | Model suffix | Deviating channels | Severe channels |
|---|---|---:|---:|
| Rectifier1Pulse | Thyristor1Pulse_R_Characteristic | 9 | 0 |
| RectifierBridge2Pulse | ThyristorBridge2Pulse_RL | 0 | 0 |
| RectifierBridge2mPulse | ThyristorBridge2mPulse_RLV | 14 | 5 |
| RectifierCenterTap2Pulse | ThyristorCenterTap2Pulse_RL | 0 | 0 |
| RectifierCenterTap2mPulse | ThyristorCenterTap2mPulse_R | 20 | 0 |
| RectifierCenterTap2mPulse | ThyristorCenterTap2mPulse_RL | 16 | 0 |
| RectifierCenterTapmPulse | ThyristorCenterTapmPulse_R | 0 | 0 |
| RectifierCenterTapmPulse | ThyristorCenterTapmPulse_RL | 0 | 0 |
| RectifierCenterTapmPulse | ThyristorCenterTapmPulse_RLV | 0 | 0 |

Every row remains non-high, including rows with only minor channel errors.
These block merge/release and further MultiBody breadth. Original RollingWheel
is now repaired in the full cohort, so electrical triage resumes next. The
earlier four electrical closures remain high. Combined `verify quick` and
`verify full` are not green; the known implicit-contact-circle unit regression
also remains unresolved. The requested checkpoint commit preserves these
failures explicitly; it is not release approval or baseline promotion.
The completed run used four pinned model workers. The user explicitly
requests approximately 16 simulation cores next time, overriding the local
four-worker preset for that future execution; this run was left unchanged.

## Previous complete measurement and RollingWheel investigation

`target/msl/multibody-torn-roundoff-full` completes the full 566-model
comparison in 312.72 seconds but **fails the quality gate** at HEAD
`bc71577f85df24957e5c9ab30fdaf4ed48da4311`, dirty-tree digest
`f50ac09f35fdac3c6c65964542478e8ebdb0cf896e54a2176d257d880447ddad`.
Of 161 simulation completions, 144 compare: 135 are strict-high and nine near.
Seventeen reviewed exclusions remain visible; zero traces are missing or
nonidentifiable. There are 59 deviating trajectory channels, including five
severe. All 14,721 initial-condition channels remain high. MultiBody retains
35 DAE completions and 14 high models out of 42; its remaining outcomes are
seven compilation failures, nine solver failures, and twelve timeouts.

The recovered-coordinate roundoff repair closes four counterexamples in this
complete cohort: DiodeBridge2Pulse, HalfControlledBridge2Pulse,
ThyristorBridge2Pulse_R, and HBridge_R. No previously high model loses its
band. DCPM_Cooling remains a timeout. GyroscopicEffects changes its timeout
classification from an internal simulation timeout to a parent model-attempt
timeout, without a trace. Inverse_sh_TX advances from Flatten refusal to DAE
refusal without an owning-phase change in this repair; that classification
delta is retained and is not claimed as a capability improvement.

Before the new RollingWheel regression, all 434 solver and 526 core tests
passed, as did affected all-feature Clippy
checks and formatting. The original DiodeBridge2Pulse focused comparator has
all 91 trajectory/initial channels high, one comparison and zero missing,
skipped, excluded, or nonidentifiable traces; its OMC reference is
byte-identical to the prior full run. The fixed 20-model canary retains all
phase/band outcomes, nine compared models and 175 high channels including
initial conditions, with zero missing or skipped comparisons. Both use the
same dirty digest as the full run. Deltas and root-cause evidence are under
`buffered-relation-counterexamples/diode-bridge/` in the campaign directory.

Nine electrical counterexamples remain actionable release blockers. The user's
latest priority permits fixing one MultiBody model, then requires returning to
those electrical counterexamples before further breadth. The selected model
is RollingWheel: its balanced
DAE fails structural matching at 907/911. The investigation maps unmatched
equations and variables, state selection, initialization, and differentiated
constraints against OMC before choosing a repair. The 28 unsupported examples
comprise seven structural refusals, twelve Solve-stage timeouts, two simulation
timeouts, two Flatten timeouts, four DAE refusals, and one Instantiate refusal.
All 42 examples remain visible, together with the broader 111 ModelicaTest
and 171 nonpartial library inventories. Current combined `verify quick` and
`verify full` are not green; no baseline, commit, or PR is made.

The RollingWheel investigation now includes OMC's flattened equations, backend
index-reduction dump, selected states, and generated initialization system.
Rumoca's first unmatched contact row belongs to
`delta_0 = r_road_0 - frame_a.r_0`; the whole-family differentiation proof
stops at the implicit road coordinate `s`. A subsequent capture of the actual
matching corrects that initial interpretation: unmatched scalar row 814 is
the third component, which reads `r_road_0[3]=0`, not `s` or `w`. Its
alternating-path closure contains 26 rows over 25 unknowns: the three contact
constraints, implicit `delta_0` components, orientation, and normalization.
Rows 812 and 813 determine the other road coordinates outside this closure.
The proof must respect component demand as well as coupled constraints. OMC
differentiates a coupled contact constraint set, including intermediate
algebraic coordinates, normalized directions, and second derivatives. The
initial state-count difference alone is not proof of a lowering defect.

The new `implicit_algebraic_contact_coordinates_preserve_circular_motion`
regression is intentionally RED: replacing the passing circular constraint
with `s+w=x`, `s-w=y`, and `2*(s*s+w*w)=radius*radius` gives EL005, six of
seven equations matched. OMC simulates the same source; all six observed
coordinates agree with the analytic circle over 12 rows, with maximum
absolute error 3.26e-8. This isolates the inability to handle implicit
intermediate coordinates, but does not establish that a repair to this
fixture alone closes RollingWheel, or that its implicit road coordinates
cause the first unmatched scalar row. Exact source spans and OMC regular
equation owners are mapped in `rolling-wheel/coupled-constraint-source-map.json`
under the campaign directory. All temporary probes are removed.

A second reduced source isolates selection through a whole-array definition:
`road={s,w,x*x}; p=road[3]`, with nonlinear algebraic equations for `s` and
`w`, and derivative equations for `p` and its velocity. It first fails at
eight of nine structural matches. Structural admission and reconstruction now
use the same source-bound element projection through literal array constructors
and checked causal definitions. They preserve the original array equations,
do not freeze tunable indices, and do not bypass function calls or their
derivative annotations. The selected position, velocity, acceleration, and
unrelated algebraic channels now match the analytic solution with both BDF
and RK. OMC agrees over 12 rows and nine channels to 2.23e-16 maximum error.

All four new source regressions pass, including matrix selection, inconsistent
fixed initial values, and a retained assertion in an unselected element.
All 146 structural tests pass. The component-change core run has 530 passes
and one failure: the earlier implicit-contact circle is still RED. The fixed
20-model `multibody-component-differentiation-canary` passes with all phase,
simulation, and band outcomes unchanged; nine models compare, with zero
missing, skipped, excluded, or nonidentifiable traces and all 175 initial
channels high. Its dirty-tree digest is
`00a3fa1dd1b29a458dc7349ffffc52653e28e0052dda1203b3a2ac8fc85da186`.
The rebuilt original RollingWheel worker still reports EL005 at 907/911;
its Flat and DAE artifacts are byte-identical to the preceding capture.
RollingWheel's coupled contact constraints remain unresolved, and this
component repair does not establish another supported MultiBody model.

A third reduced source keeps a circular-motion constraint inside one mixed
vector equation: `road={s,w,x*x+y*y}; {s,w,road[3]}={1,2,radius*radius}`.
It first exposed an earlier Flatten defect: the second vector equation had
scalar count one instead of three, yielding ED001 at eight equations and ten
unknowns. Equation shape inference now consumes the existing Resolve/Instantiate
declaration-based scalar proof, which was already used by dimension evaluation.
Unknown references still remain unknown. The minimal array-equation test is
RED before this change and GREEN after it; all 631 Flatten tests pass.
With only the shape repair, the mixed circle reaches a balanced DAE and fails
at structural matching, nine of ten. OMC simulates the identical reduced source over 12 rows; its ten
observed channels match the analytic solution to maximum absolute error
1.27e-7, including exact algebraic values for both road coordinates.

The shape-change core run retains 530 passing tests, with only the implicit
contact and mixed-vector circle regressions RED (zero ignored or filtered).
The fixed 20-model `multibody-scalar-shape-canary` also passes with all phase,
simulation, and band outcomes unchanged, nine comparisons, no missing or
skipped traces, and all 175 initial channels high. Its dirty-tree digest is
`3aed21fa3f16d017bc663c79c5cbc2301fff033dc7716fc811fb559bb42b9b63`.
These are focused tripwires; the complete-cohort numbers above remain the
latest cohort evidence.

The subsequent tensor constraint repair makes that mixed circle pass with
both BDF and RK, including all ten analytic channels. The same equations
written as scalar equalities serve as a passing diagnostic control. A
source-bound component plan now supplies both the differentiability proof
and reconstruction. It retains the tensor owner, domain, body shape, and
untouched components through a checked array update; only the selected
component changes, and its position and velocity constraints remain on the
initial manifold. Reduction identities include the owner, body, and component.
This is an implementation within tensor owners, without scalar equation
owners or changes to the tensor-native requirement.

The first reconstruction exposed a redundant differentiated causal definition
and incorrect array-update incidence. Component admission now rejects
identical source projection trees before differentiation. DAE dependency
projection reads the replacement only at selected coordinates and reads the
old tensor at other coordinates; static whole-axis, scalar, slice, and record
field selections have focused regressions. A parameter-index RED test also
proves that default parameter values cannot justify removing a possible
dependency. Only constant selections receive exact selection; tunable or
runtime indices retain their possible dependencies.

The tensor owner, middle-component, consistent and inconsistent initialization,
and analytic simulation controls pass. The first complete core run after
this repair has 535 passes and only the implicit-contact circle RED, with
zero ignored or filtered tests. After the parameter-index guard, all 36 DAE
evaluator and 146 structural tests pass; the final core run retains 535 passes
and that one known RED. Affected all-target/all-feature library Clippy and
all-feature core Clippy pass. The fixed 20-model
`multibody-tensor-constraint-canary` passes with all phase, simulation, and
band outcomes unchanged from the shape canary: nine comparisons, no missing,
skipped, excluded, or nonidentifiable traces, and all 175 initial channels high.
Its dirty-tree digest is
`f60d484b24a4ac81748f927d9a5d6db54e330d3bf04de2c30e0c8bbfcd765ec6`.
The rebuilt original RollingWheel still fails EL005 at 907/911, with identical
Flat, DAE, and returned structural DAE artifacts. The tensor repair therefore
adds no demonstrated MultiBody support yet. The source
and producer receipts are in `rolling-wheel/tensor-constraint-triage.md` and
`rolling-wheel/tensor-constraint-producer-receipt.json` under the campaign
directory; temporary probes are removed. No further MultiBody breadth precedes
the electrical fixes after the selected RollingWheel repair.

The next regression now retains the actual contact geometry in
`tests/fixtures/index_reduction/TensorContact.mo`: a rotating axis, normalized
cross product, implicit road/contact vectors, and the same three scalar contact
constraints. The independent angle is prescribed; a vertical reaction replaces
the full rigid-body dynamics. OMC runs this exact final source without warnings,
selects only the angle as a state, and agrees with all 24 analytic observables
over 12 samples to maximum absolute error 7.78e-16. Its backend dump differentiates
the coupled auxiliary coordinates to first and second order.

Rumoca constructs 24 equations and 24 unknowns but matches only 23 on both BDF
and RK. The unmatched row is `delta[3] = road[3] - z`; its alternating closure
contains 17 equations and 16 unknowns. This corresponds to RollingWheel's
third contact-offset row 814 and its 26/25 closure. The same reduced source
with the three contact constraints explicitly solved for `s`, `w`, and `z`
passes all 24 analytic channels on both solvers. These three focused tests
therefore give two RED implicit cases and one GREEN control, after the preceding
535-pass core checkpoint. They remain unresolved regression obligations.

This isolates a coupled constraint limitation beyond component selection:
the current reconstruction requires state-based expressions for auxiliary
values and retained first derivatives, whereas the contact block defines
those quantities implicitly. The next structural change needs a proof for
the coupled block while preserving tensor owners and initialization constraints.
General dummy derivatives would also require the scope amendment stated by
SPEC_0007; no such cutover has been implemented here. Current-source evidence
is bound by `rolling-wheel/tensor-contact-receipt.json`. Two earlier fixture
variants separately exposed Integer-to-Real `cross` admission and dependent
fixed-start preservation limits; both are retained in the private evidence,
without weakening either checker. Full verification remains outstanding.

The next building block is now a checked whole-tensor `LinearSolve` operation
in Solve schema 70. Construction requires a positive square Binary64 Real
matrix and a matching Real vector, and wire replay derives the result type
through the same constructor. Its directional program applies
`A*dx = db - dA*x` with aggregate operands and results. Interpreter and native
execution use the existing pivoting kernels and report singular or non-finite
systems as errors. A scaling regression exposed absolute pivot cutoffs that
rejected a well-conditioned system after multiplying both sides by `1e-20`;
the kernels now reject zero or non-finite pivots instead. Trace comparison
tolerances are unchanged.

The affected suites pass (292 Solve IR, 190 evaluator, 69 native tests, plus
doctests), as does affected all-target/all-feature Clippy. The core checkpoint
is 536 passes and the same three unresolved implicit-constraint failures,
with no ignored or filtered tests. OMC solves the time-varying matrix fixture
at scales `1e-20`, `1`, and `1e20`; all six analytic channels over 12 samples
agree to maximum absolute error 8.89e-16. The fixed 20-model canary at HEAD
`bc71577f85df24957e5c9ab30fdaf4ed48da4311`, dirty digest
`b762c8cde08e72fb6bc9b5a2a6ca284daf695a66b793e2a4dda02127d85533b4`,
retains every preceding phase, simulation status, and band. Its nine compared
models are high, with zero missing, skipped, excluded, or nonidentifiable
traces. This is Tier 1 evidence only, recorded in
`rolling-wheel/tensor-linear-solve-canary-delta.json`; it adds no cohort claim.
The coupled structural proof and RollingWheel repair still remain to be done.

DAE schema 37 now carries the internal auxiliary `LinearSolve` with a
constructor-derived Real vector result. Function lowering produces one checked
tensor kernel; the extent-4096 tests retain three registers and four operations.
Dependency projection includes both complete operands, and unsupported scalar,
compile-time numeric, and GALEC paths refuse the intrinsic explicitly. This
does not add a Modelica source builtin or complete coupled index reduction.
The final bridge suites pass: 187 DAE unit tests, 17 DAE integration tests,
eight compile-fail doctests, 37 DAE evaluator tests, and 120 Solve-phase tests.
Affected all-target/all-feature Clippy and formatting checks pass.

The bridge canary at the same HEAD, dirty digest
`039d158b9cec066a1bdd2c6f5010f339bb22d9875c5316caad59e3df0ddb847b`,
retains all 20 phase/status/band results from the preceding kernel canary.
Nine models compare high, with all 175 initial channels high and zero
missing, skipped, excluded, or nonidentifiable traces. The delta and evidence
hashes are in `rolling-wheel/tensor-linear-solve-dae-canary-delta.json` and
`rolling-wheel/tensor-linear-solve-receipt.json`. The structural proof still
needs coupled auxiliary definitions and general higher derivatives of the
contact geometry; no original MultiBody model is newly supported by this work.


### Coupled tensor auxiliaries and shared equation normalization

The reduced contact now simulates on BDF and RK with all 24 analytic channels
correct. A two-dimensional block and a varying-coefficient variant retain all
six analytic channels, variable/state counts, and original equation-owner
counts. Square source-authored dot blocks supply one aggregate `A*q=b` proof;
first and second derivatives use `A*dq=db-dA*q` and
`A*ddq=ddb-ddA*q-2*dA*dq`. Product, quotient, trigonometric, square-root, cross,
and outer-product rules retain tensor operations. Nonlinear coefficients
remain outside this proof, and singular matrices produce a checked error.

The source-value proof now retains exact state dependencies through algebraic
coefficient definitions. Losing those dependencies had caused an unnecessary
orientation-state lift whose zero initial guess made the contact solve singular.
The repaired reduced model selects its original angle/height anchors directly.
Original function identities also rebuild through their explicit mapping when
an auxiliary function is inserted before them.

RollingWheel's radius equation is written `0 = radius - dot(...)`. A regression
proved that the initial block recognizer missed this equivalent equation form.
STRUCT-T08 now specifies one shared signed-zero equation normalization layer.
Causal definitions, derivative definitions, state constraints, algebraic lifts,
and auxiliary blocks consume its borrowed equality operands. It preserves the
original numerical residual, provenance, tensor domains, and source owners;
no division, cancellation, reassociation, or effect-erasing rewrite is admitted.

Validation: all ten contact/auxiliary tests and all 146 structural tests pass;
structural all-target/all-feature Clippy passes. The complete core suite reports
545 passes and the same one existing scalar implicit-circle failure (EL005,
6/7). OMC independently matches the six analytic channels of the constant,
varying, and signed-equation auxiliary fixtures to within 1.2e-16. The earlier
same-source OMC contact receipt remains the 24-channel reference. A separate
valid scalar/array `.+` fixture is still refused by ToDAe (ED020), although OMC
matches its analytic solution; its private reproduction is retained without a
coverage claim.

The fixed `multibody-auxiliary-normalization-canary` passes at HEAD
`bc71577f85df24957e5c9ab30fdaf4ed48da4311`, dirty-tree digest
`30ad2e0ecb5f41e3beb4a08dc213f24e7572fe398f61915097e2fad24b06bd98`.
All 20 phase/status/band rows equal the preceding DAE-bridge canary. Nine
models compare high; missing, skipped, excluded, and nonidentifiable counts
are zero, and all 175 initial channels are high. This is Tier 1 evidence only.
Receipts and hashes are `rolling-wheel/auxiliary-normalization-receipt.json`
and `rolling-wheel/auxiliary-normalization-canary-delta.json`.

The fresh original RollingWheel worker still fails at 907/911 matching
(1.48 seconds compilation, 2.44 seconds Solve preparation). Its three dot rows
are now recognized, but reconstruction of their orientation coefficients is
not yet proved through function calls. The source DAG reaches the axis-selection
conditionals in `Frames.TransformationMatrices.axisRotation` through
`Frames.axesRotations`; a focused function-based contact reproduction is staged
for the next proof. No original MultiBody model is newly supported. Quick/full
remain unpassed; the priority remains RollingWheel, then the nine electrical
counterexamples, before additional MultiBody breadth.

### Exact function selection and source-state anchors

Shared function substitution now selects static branches under the exact caller
arguments, including literal array selectors, while leaving runtime coordinates
and tunable parameters unknown. A reduced function-based contact fixture passes
BDF and RK on all 24 analytic channels. Two further regressions distinguish a
causal definition from an independent constraint: substituting a definition into
itself must not erase its value equation, and following a definition from an
independent constraint must retain the original state dependencies.

The reduced rolling contact with both no-slip equations now passes both solvers
on all 36 analytic channels. Same-source OMC checks all 36 channels to within
9.4e-9. All 41 eval-dae and 148 structural tests pass, as does their all-target,
all-feature Clippy. The complete core suite has 547 passes and the same existing
implicit-circle EL005 failure; quick/full are still unpassed.

The fixed `multibody-definition-anchors-canary` passes at HEAD
`bc71577f85df24957e5c9ab30fdaf4ed48da4311`, dirty-tree digest
`4d57de86845b09ff78fbf1125d6de9904ed5bcddd6bccba35b9091c6112cea40`.
All 20 phase/status/band rows match the preceding function-selection canary.
Nine models compare high, zero are missing/skipped/excluded/nonidentifiable,
and all 175 initial channels are high. This is Tier 1 evidence only; receipts
are `rolling-wheel/definition-state-anchors-receipt.json` and the corresponding
`definition-state-anchors-canary-delta.json` in the private campaign directory.

Original RollingWheel still returns EL005 at 907/911 matching. Its diagnostic
Solve preparation takes 19.13 seconds, exceeding the normal 10-second budget.
The observer now selects the two actual no-slip constraints and reaches one
remaining angular-velocity/vertical-acceleration pair. The contact constraint
has a proof only through derivative order one: its angle-rate vector cannot yet
be reconstructed from the selected angular-velocity and angle states. OMC
differentiates the corresponding radius constraint twice. The next focused
repair is the tensor linear map inside `Frames.axesRotations`, after which the
original model must be rerun. No original MultiBody parity gain is claimed.

### Tensor angular-rate reconstruction and higher contact derivatives

The original `RollingWheel` now completes structural reduction, Solve lowering,
native execution preparation, and initialization. Its remaining failure is the
unchanged 12-second simulation budget; there is no completed trace or new
strict-high claim for this model.

Two reduced source models isolate the structural defects. `AngularRateContact`
requires reconstructing a vector of angle derivatives from a state-dependent
linear map. Structural analysis now proves that map directly through source
array operations and exact function substitutions, retaining aggregate matrix
and outer products. It neither enumerates a tensor basis nor replaces the
original equation owners. Nonlinear unknown-dependent coefficients are refused.
`TensorContactAnnotated` exposes a separate order-dispatch defect: a supplied
first derivative incorrectly prevented a provable second derivative of the
function body. Both fixtures pass BDF and RK checks against their analytic
channels; OMC agrees with those channels to maximum absolute errors of
`4.59e-8` and `5.56e-16`, respectively.

The newly differentiated original DAE exposed repeated traversal of shared
expression dependencies. Query-local visitation now keys on expression,
scalar, record field, and lexical domain point; function argument contexts keep
their separate handling. A twelve-layer shared graph previously emitted one
dependency 4,096 times. The original structural preparation fell from 54.510
seconds to 0.617 seconds, producing byte-identical DAE JSON with 12,134
expressions, 368 variables, and 255 continuous equation owners.

Validation: 43 DAE-evaluator and 150 permanent structural tests pass, as do all
eight contact tests and Clippy for the changed crates. The core suite has 549
passes and the previously recorded implicit-circle `EL005` failure. The fixed
20-model canary at `target/msl/multibody-angular-rate-canary` has no changes in
phase, simulation, initialization, or comparison band relative to
`multibody-definition-anchors-canary`. Its nine compared traces remain high;
all 175 compared initial channels are high, with zero skipped, missing,
excluded, or nonidentifiable traces. The run binds HEAD `bc71577f` and working
tree digest `c47d4fdc15130307b1e62c5248438311ff425890b8bf7b5a84e7ce7efe579388`.
This is Tier 1 regression evidence; the preceding full cohort remains the
source of cohort counts. Quick and full verification are not yet green.

### Shared differentiation graph and original RollingWheel completion

`perf` identifies repeated evaluation of expanded contact derivatives as the
remaining runtime cost. Plain reconstruction already preserved shared nodes,
but differentiation, exact-value substitution, and function instantiation
re-emitted shared subexpressions. The scalar/vector scaling regression grows
from 61 to 789 resulting expressions when shared source depth doubles from
four to eight before the repair. Reconstruction now caches successful results
by source expression, complete call context, derivative order, mode, and
provenance. It preserves expression sharing without changing equation owners,
tensor structure, floating-point operations, or function-call identity.
The scaling test now passes, and `ScopedContact` checks distinct arguments
through nested calls against four analytic channels on both BDF and RK.

The unchanged 12-second solver budget now admits the original four-second
RollingWheel simulation. The focused comparator and subsequent full cohort
both give 184/184 high trajectory and initial channels, with zero skipped,
missing, excluded, or nonidentifiable traces. Maximum channel bounded
normalized L1 error is `1.1923173202530037e-4`. Rumoca and OMC use different
state coordinates; trace parity does not establish identical state selection.

The first focused OMC launch separately exposed an empty endpoint-file race:
the file existed before OMC wrote its endpoint. The launcher now waits for
nonempty publication within the existing deadline and cleans up its own
child on startup failure. The empty-file regression fails before the repair
and passes afterward. The failed run is retained; the canonical reference
stage subsequently compares the unchanged successful Rumoca trace, without
rerunning that simulation. The complete cohort above exercises this fix too.

Focused validation has 151 structural, nine contact, and 169 MSL-tooling tests
passing. Temporary performance probes are removed. The latest complete core
checkpoint predates graph sharing and has 549 passes plus the known
implicit-contact-circle failure. The test-helper nesting lint discovered in
the graph-sharing Clippy run is fixed. The final checkpoint reruns all 151
structural tests successfully and passes all-target/all-feature Clippy for
`rumoca-phase-structural` and `rumoca-test-msl`, formatting, and diff checks.
No current quick/full pass is claimed.

## Preceding parameter-relation sweep and torn-roundoff repair

`target/msl/multibody-parameter-relations-full` completes the full 566-model
comparison but **fails the quality gate** at HEAD
`bc71577f85df24957e5c9ab30fdaf4ed48da4311`, dirty-tree digest
`cf88904a126e6030665feb95ab352e0462bfe150eeec74fcc42305c5fa23e1c8`.
Of 161 simulation completions, 144 compare: 131 are strict-high and 13 near.
Seventeen reviewed exclusions remain visible; zero traces are missing or
nonidentifiable. There are 63 deviating trajectory channels, including nine
severe channels. All 14,721 compared initial-condition channels are high.
ForceAndTorque is restored to all 527 trajectory/initial channels high, and
MultiBody again has 14 high models out of 42. DAE/balance totals remain
288/276. DCPM_Cooling loses completion to a timeout, so the strict-high total
does not increase. PrismaticConstraint changes from structural refusal to a
timeout; neither outcome establishes support.

The thirteen remaining counterexamples are power-converter models. The next
is DiodeBridge2Pulse: 90/91 channels are high, but `ground.p.i` has mean absolute
error about 5.4e-4 A against OMC's zero-current solution. Its torn-Newton
certification defect is now reproduced and repaired in focused tests; the
original complete trace and cohort confirmation remain pending.
The complete per-model delta is retained in
`buffered-relation-counterexamples/force-and-torque/parameter-relations-full-delta.json`
under the campaign evidence directory. Breadth remains paused; no baseline,
timeout, or tolerance is changed, and no commit or PR is made.

At the first bad sample, 0.0102 seconds, the positive DC current-balance
residual equals the erroneous ground current, -1.429416585e-4 A. Replaying the
same Solve artifact proves that certified observation refresh accepted this
point. Its Newton correction, 7.147075780e-16, rounds away in a tear voltage
near -9.77; comparing recovered coordinates after that rounded update falsely
reports zero change. Reduced-Jacobian sweeps now retain recovered-coordinate
sensitivities, so certification also bounds the linearized correction before
addition. A stalled line search declines and restores the incoming seed for
the existing full-system solve. The exact captured point then yields zero
ground current. Temporary diagnostic source probes are removed.

Two reduced regressions first fail on currents of 0.050004445 A instead of
0.05 A; all three new controls now pass, including equation rescaling and
continued reduced solving of a well-conditioned loop. OMC's equivalent source
preserves total current exactly across 12 rows, though individual currents
differ from the analytic split by 4.44e-6 A in opposite directions; that
numerical difference remains recorded. Evidence is in
`buffered-relation-counterexamples/diode-bridge/torn-roundoff-triage.md` under
the campaign directory. This is a general numerical projection repair, with
no change to equation lowering or trace policy.

## Preceding buffered-relation sweep and parameter-rotation repair

`target/msl/multibody-buffered-relations-full` completes the 566-model comparison
but **fails the quality gate** at HEAD `bc71577f85df24957e5c9ab30fdaf4ed48da4311`,
dirty-tree digest `3ce52135f14b3144926bbe9252d3e80586b0ac43aba597e7385429a239c9b5f3`.
Of 162 simulation completions, 145 compare: 131 are strict-high, 13 near, and
one deviating. Seventeen reviewed exclusions remain visible, with zero missing
or nonidentifiable traces. There are 203 deviating trajectory channels, including
17 severe channels. Initial conditions are high for 144 models; ForceAndTorque
has 27 deviating initial channels. MultiBody has 35 DAE completions, 14 simulation
completions, and only 13 high models out of 42. Its other outcomes are seven
compilation failures, eleven solver failures, and ten simulation timeouts.

IdealTriacCircuit is repaired in this full comparison: all 55 trajectory and
initial channels are high. Its focused semantic regressions, 1,070 core/Solve/
solver tests, affected Clippy checks, and unchanged fixed 20-model canary pass.
However, eight previously high models lose that band: ForceAndTorque and seven
power-converter examples. Fourteen actionable counterexamples remain, including
the five preceding electrical counterexamples and a newly completed near trace.
The next triage is ForceAndTorque's incorrect initial force/torque resolution;
unrelated breadth remains paused. The complete per-model delta is retained in
`tensor-affine-counterexamples/ideal-triac/buffer-full-delta.json` under the
campaign evidence directory. Current `verify quick` and `verify full` remain
outstanding; this run is not release evidence, and no baseline is promoted.

ForceAndTorque's parameter-rotation regression now has a general DAE fix under
validation. The flattened model is byte-identical before/after. Six relations
over parameters incorrectly received crossing roots, including the enum tests
selecting `fixedRotation.R_rel`. Solve's initial parameter seed was correct, but
its initialization update read zero-initialized relation buffers and replaced
the rotation with the fallback identity. The source event planner was
conservative about qualified enum literals; both expression-event and activation
lowering now consume the constructed expression variability and preserve
parameter-only tests as literal conditions without roots (MLS §3.8.3/§8.5).

Two reduced tests first failed: two parameter roots remained, and horizontal
force was zero instead of -500. OMC's flattened equations and all four sampled
rows establish the expected force. The strengthened suite checks all three
record-binding branches on Auto/Interpreter and a parameter `when` activation.
All 524 core and 820 DAE/Solve/solver library tests pass, as does affected-library
Clippy. `target/msl/multibody-parameter-relations-origin` restores all 527 original
ForceAndTorque trajectory and initial channels to high, with one compared model
and zero skipped, missing, excluded, or nonidentifiable traces. It uses the same
HEAD and dirty digest `aca1281986332acfef3e30a9139e3a223e457992873655b4eb4cd4d6a03b7b27`.
The fixed `multibody-parameter-relations-canary` has all 20 phase/band rows
unchanged from `multibody-buffered-relations-canary`: nine compared models and
175 channels high, with zero missing or skipped comparisons. Evidence and the
producer/canary deltas are in
`.git/multibody-campaign/buffered-relation-counterexamples/force-and-torque/`.
This is focused closure evidence; the next complete cohort comparison must
confirm it and measure the remaining electrical counterexamples.

The three targeted FMI assertion tests also pass with package execution enabled
(`parameter-relations-fmi-2.log`, 12.71 seconds): FMI 2/3 schema and VDM checks,
FMPy parameter validation and analytic free fall, plus continuously varying
predicate and state-event rejection controls. The first general-shell attempt
skipped package execution because CMake/FMPy/Java were unavailable; the passing
attempt uses the FMI shell and the existing pinned FMPy environment. OMC also
accepts all three reduced rotation branches, with maximum force error below
1.2e-13 against their analytic values (`omc/comparison.json`).

## Preceding tensor-affine failure and relation-buffer repair

`target/msl/multibody-tensor-affine-full` completes the 566-model comparison
but **fails the quality gate** at HEAD `bc71577f85df24957e5c9ab30fdaf4ed48da4311`,
dirty-tree digest `5ab62ca833eef338e1ad7948e0c8d90fdea446ab04f54d7c8093c476d72ca8fb`.
Of 157 simulation completions, 143 compare: 137 are strict-high and six have
ten deviating channels. Four of those channels are severe. Fourteen existing
reviewed exclusions remain visible; no traces are missing or nonidentifiable.
All initial-condition comparisons are high. MultiBody retains 14 high models
out of 42 and all 5,883 shared channels high, without any new completion.

The six actionable counterexamples are IdealTriacCircuit, DiodeBridge2Pulse,
HalfControlledBridge2Pulse, ThyristorBridge2Pulse_R, ThyristorBridge2mPulse_RLV,
and HBridge_R. ThyristorCenterTapmPulse_R also loses its preceding high result
to an event-iteration refusal. Further MultiBody capability work is paused
while the first counterexample is traced against OMC. The 300.23-second test
and complete per-model delta are retained as `tensor-affine-full-1.log` and
`tensor-affine-full-delta.json` under the campaign evidence directory. No
baseline, tolerance, or exclusion was changed. Current combined `verify quick`
and `verify full` are not green; no commit or PR is made.

IdealTriacCircuit now has a reduced source-level regression independent of
MSL. Both Auto and Interpreter reproduce a negative `forwardSwitch` while
`forwardOff` remains false at 0.238 seconds. OMC simulates the same source and
satisfies both turn-off implications on all 320 rows. The original model's
first lasting divergence is the turn-off near 0.48522 seconds: the preceding
implementation and OMC turn off, while the new exact solve reaches zero and
retains the on state.

Temporary instrumentation confirms that the component detects the reduced
model's crossing at 0.2363029383566202 with indicator -1.5349943538467414e-7.
The source DAE retains both exact relation identities, but Solve assigns no
relation-memory target to either. Consequently, there is no relation-memory
slot on which event iteration can retain the detected post side. A third RED
test records this construction gap. This rejects a missed-crossing explanation
and a defect confined to native code generation; the next task is a general
buffered-relation lowering repair under MLS §8.5/Appendix B and
SPEC_0007/0038/0044. Relation truth must remain distinct from condition-edge
history and from the whole Boolean assignment. The probes are removed, and
all three focused tests initially failed, providing the pre-repair evidence.
Artifacts are under `.git/multibody-campaign/tensor-affine-counterexamples/ideal-triac/`.

The in-progress repair now allocates relation truth by exact source expression,
separately from Boolean results and condition-edge history. Root programs still
evaluate the source operands. A shared-expression `noEvent` regression first
failed and now has a distinct literal-evaluation context, including cache scope.
The reduced circuit's persistent latch error is repaired, but its first trace
still had two wrong event-left rows. An independent affine-loop test then
proved that derivative-coordinate reuse returned a root of zero instead of
-5e-6: a small tear residual did not bound the recovered variable's error.
Derivative and root refresh now require existing coordinate-convergence checks.
The four switching/root controls pass without tolerance changes.

A fifth RED control showed that non-strict relations were false at exact-zero
initialization. Relation-buffer updates now use the same compiler-owned zero
orientation as event indicators; all five controls pass. The complete core
check found three homotopy branch-selection regressions (517/520 passed).
Continuation now settles relation truth and ordinary Boolean definitions at
each lambda, reprojects initialization when they change, and restores both
numeric and discrete coordinates after a rejected step. All 12 homotopy controls
pass. The later full comparison above closes IdealTriacCircuit but exposes
further regressions, so MultiBody breadth remains paused.

## Previous passing complete measurement

`target/msl/multibody-additive-assignment-full` passes the complete 566-model
MSL/OMC gate at HEAD `bc71577f85df24957e5c9ab30fdaf4ed48da4311`, dirty-tree digest
`8df44f7325df0da96fdf02f5061d3792822f91a563d7cbeace922a1791b2cdad`.
The 275.22-second test passes the unchanged quality and performance floors.
Flatten passes 491 models, DAE construction 288, and balance 276;
initialization succeeds for 176 and simulation for 160. The comparator measures
144 models, all high, with 16 existing tracked exclusions and zero missing,
nonidentifiable, or deviating comparisons. All 143 preceding high models remain
high. DemoPowerSupplyWithBuffer newly passes all 42 compared channels; its
preceding failure was an event-boundary algebraic projection refusal at
`dcPowerSupply.i`. Of 14,721 trajectory channels, 14,706 are high and 15 minor;
none deviate. All 14,721 initialization channels are high.

| MultiBody stage | Models out of 42 |
|---|---:|
| Instantiated | 41 |
| Flattened | 39 |
| Balanced DAE | 35 |
| Initialization succeeds | 17 |
| Compared at strict-high trace parity | 14 |

All 5,883 compared MultiBody channels are high, with zero minor, deviating,
missing, skipped, excluded, or nonidentifiable comparisons. The 28 remaining
failures are one Instantiate refusal, two Flatten timeouts, four
DAE-construction refusals, eight structural-analysis failures, ten Solve
timeouts, and three simulation timeouts. PointGravityWithPointMasses2 refuses
its heterogeneous `fixed` array. PrismaticConstraint reports its structural
refusal instead of the preceding Solve timeout; GyroscopicEffects completes Solve and initialization
but exceeds the 12-second simulation budget. No new MultiBody completion or
coverage of the wider 111-model ModelicaTest / 171-model library scope is claimed.

The current performance gate passes with system-speedup median 1.716 and
wall-speedup median 44.839, against baselines 1.559 and 52.45468. It uses 14
single-threaded OMC workers and the unchanged 35% tolerance. The preceding
`multibody-verify-quick-checkpoint-6` performance failure remains retained;
these later standalone MSL passes do not rewrite it. The complete current
delta is `additive-assignment-full-delta.json` under
`.git/multibody-campaign/gyroscopic-sim-timeout/`. Apart from the new high model,
all model agreement bands remain unchanged. Inverse_sh_TX now reaches its DAE
refusal instead of failing in Flatten; ThyristorBridge2Pulse_DC_Drive reports a
runtime refusal instead of its previous simulation timeout. No baseline was
promoted, and neither unresolved failure earns coverage credit.

The last combined `verify quick` run, before the zero-assignment repair,
completed in 1,688.257 seconds. Lint, all 28
pinned corpus cases, 243 architecture tests, 17 repository gates, and all 7,395
workspace tests pass, with no workspace tests skipped. Documentation tests pass
30 examples, with 23 existing ignored examples. Its only failed step is the MSL
performance gate. The earlier formal-statement registry failures are repaired
and pass that combined run. The latest standalone full MSL gate passes, as do
1,409 focused/core/solver tests and affected Clippy; combined `verify quick` and
`verify full` still require current runs. No commit,
baseline promotion, or PR is made. Exact timings and the complete measurement
receipt are under `.git/multibody-campaign/gyroscopic-sim-timeout/` as
`verify-quick-6-quick.json`, `verify-quick-6-receipt.json`, and
`quick-6-msl-delta.json`; the delta's original `target/msl/results` directory
was renamed to the checkpoint above after verification finished.

## In progress: affine tensor moment equations

The next GyroscopicEffects equation is `Body.mo:261`,
`frame_a.t = I*z_a + cross(w_a,I*w_a) + cross(r_CM,frame_a.f)`.
Canonical program 426, logical row 1399, output 2 lacks an isolator for solver
Y871, `bodyCylinder1.body.frame_a.f[2]`. The exact program and OMC regular
equation 1688 differ by zero after the XML torque alias and the two declared
zero `r_CM` components are applied symbolically. Its force coefficient is
`-r_CM[1]`; the instance value 0.125 is not a compiler assumption. This follows
MLS 3.6 §§10.3.5, 10.6.4 and Appendix B, with SPEC_0032 compact tensor ownership
and SPEC_0036/0043 source-bound assignment proofs.

A minimal cross-product regression initially returns no assignment where
`force[2] = (torque[3] + r[2]*force[1])/r[1]` gives 15, independently of starts
at ±1e30. The new `TensorAffine` certificate retains one checked rule per source
producer, compact independent register ranges, and the selected output. Final
execution adapters materialize offset/coefficient pairs using the original
tensor operations; shared DAG nodes are not unrolled, calls are not replayed
with a substituted target, and the original evaluation prefix remains. Scalar
products/division, matrix products, cross products, fills, transposes and
linear arithmetic share this construction. Dependent products and denominators
remain unproved. Dynamic zero/nonfinite coefficients retain implicit fallback;
the complete-seed and nonlinear branch guards are unchanged. Solve schema is 69.

The initial library pass covers 475 tests. A source-level `TensorAffineMoment`
regression proves the force seed, rejects forged source-selection/output/cut
metadata, and checks seven analytic channels under BDF/RK and Auto/Interpreter.
A second RED test exposed materialized coefficient overflow returning -0 while
the per-row evaluator declined it. The existing nonfinite-coefficient guard now
belongs to the shared materializer, removing the evaluator's duplicate emitter.
The library suite, all 516 core tests, and 429 solver controls pass after that
change: 1,420 tests total. Affected all-target/all-feature Clippy passes. OMC
regular equation 13 isolates the same force expression, and the normal trace
comparator measures all seven trajectory and initialization channels high
(score 1.6705e-7), with no minor or deviating channels. The largest absolute
analytic error is 1.4753e-6 for OMC's torque channel with scale 54, and 1.0202e-8
for Rumoca; no solver or comparator tolerance was changed. The fixed 20-model
canary retains every preceding phase/status/band row, with all 175 trajectory
and initial channels high across nine comparisons and no missing, skipped,
excluded, or deviating comparisons. The normal GyroscopicEffects run still
times out in simulation. Its DAE is byte-identical to the previous DAE; ten
additional exact seeds reduce missing assignments from 26 to 16. A separate
60-second diagnostic completes in 21.81 seconds and compares all 967 shared
OMC trajectory and initial channels high, but does not earn normal-budget
coverage. The full cohort uncovers the counterexamples reported above, so the
capability is not ready to land. Detailed logs and the changing receipt are
`gyroscopic-sim-timeout/tensor-affine-*` under the campaign evidence directory.

## Replaceable record dimensions

The PositionControlledDCPM reduction now typechecks for both a default
replaceable record and an explicitly redeclared scalar record, without needing
the numerical value of `asin`/`acos` bindings. The eleven matrix-dimension
controls pass. The complete MSL measurement above includes this repair and
confirms the recovered Typecheck stage without additional high-parity models.

Source-to-instance inspection also found that a direct component modifier
(`Holder h(redeclare DriveData driveData[2])`) lost the replacement type and
shape, while the equivalent extends modifier retained them. Instantiate now
applies a declaration-ID-keyed replacement in the current occurrence before
array expansion, using the existing cached class template. A reduced sibling
control proves that an unmodified instance keeps its original type and rank.
Dimension expressions are resolved and evaluated in the enclosing source scope.

A second RED test showed that the first array element lost an `each` field
modifier: the parser had keyed the modifier by `driveData[2]`, mistaking the
new declaration's dimensions for an element selection. The modifier now targets
the whole component. OpenModelica confirms the five reduced cases' record
counts and field values, including shared `{3,3}` and distributed `{3,4}`
values. Another RED parser test proves retention of nested `each`, `final`, and
`redeclare` flags. These follow MLS §§7.2–7.3 and §10.1; no model-specific path
or changed tolerance is involved.

The shared dimension proof may use a scalar fact only when every observed
occurrence of that declaration is scalar. Any array occurrence vetoes that
fact, including a zero-length array recorded before expansion removes all
its elements. Unapplied nested redeclarations remain explicitly unknown and
invalidate descendant shape facts. This does not claim complete support for
nested component redeclarations or other unreviewed redeclaration prefixes.

Evidence is retained under `.git/multibody-campaign/`: the original RED tests,
`replaceable-record-overlay-probe-2.log`/`-4.log`,
`component-redeclare-dimensions-modifiers-red-2.log`,
`component-redeclare-dimensions-omc/comparison.json`,
`replaceable-record-consensus-unit-1.log`, and
`redeclare-nested-prefixes-red-1.log`/`-green-1.log`. Final checks pass all 501
core tests and 824 IR/typecheck/Flatten tests, all-target/all-feature Clippy for
seven affected crates, and workspace formatting. The broader affected-crate
run also passed 1,377 tests with three existing ignored documentation examples
before the final marker cleanup.

`multibody-replaceable-record-origin` restores PositionControlledDCPM from
Typecheck ET004 to its earlier Flatten EF024 (`motorData` is missing structured
identity). The other eight original models retain their DAE-construction
frontiers. All nine pass Typecheck; none simulate, so this run reports parity
unmeasured and exits 1. Its delta is `replaceable-record-origin-delta.json`.

The fixed canary `target/msl/multibody-replaceable-record-canary` exits 0 at
HEAD `bc71577f85df24957e5c9ab30fdaf4ed48da4311`, dirty-tree digest
`e79e68e65df477423e50f5e9ed3a9955605da310cf93108ceb7bc37238884fd0`.
All 20 model phases/statuses and band rows match the preceding canary. Nine
models are compared; all 175 trajectory and initial channels are high, with
zero minor, deviating, missing, skipped, excluded, or nonidentifiable results.
The durable Tier 1 delta is `replaceable-record-canary-delta.json`.
The complete checkpoint and its outstanding verification failures are recorded
above and in `verify-quick-checkpoint-5-status.json`.

## Generalization audit: explicit constraining interfaces

The current audit compares language rules, source identities, and independent
record and differential-equation examples. A textual scan of added compiler,
evaluator, solver, and simulation lines against the branch base finds no added
model-name special cases; that scan alone does not prove semantic generality.

The audit reproduced a defect in `inheritance::validate_redeclaration`: it read
the default component type identity before the explicit `constrainedby` identity.
MLS §7.3.2 requires comparison with the constraining interface. New reduced
tests check a record and a model replacement that omit default-only fields,
plus negative controls for missing interface members and an omitted explicit
constraint. Both positive tests failed with EI027 against the default type,
while their resolved constraint identities were correct. The shared validator
now reads the explicit constraint identity and uses the default type only when
the clause is absent. Missing identities are rejected without name recovery.
All four tests pass, including both solvers' traces, as do all 505 compiler-core
tests and 216 instantiation unit tests. Architecture, specification gates, and
affected all-target/all-feature Clippy also pass. The fixed canary
`target/msl/multibody-constraint-interface-canary` passes at dirty-tree digest
`3a95cca45f5dee3d3d74f846c99d43d19e9959460b0ca55ec602a03f0e5c225b`:
all 20 phase/status/band rows are unchanged; nine models compare high with all
175 trajectory and initialization channels high and zero missing, skipped,
excluded, nonidentifiable, or deviating results. Its durable delta is
`constraint-interface-canary-delta.json`. The complete measurements include this
fix and retain all prior high-parity models; the canary
does not establish a new cohort parity count.

OpenModelica accepts both positive sources and simulates `y = 2*time` exactly.
Its default frontend also accepts both negative probes, so its acceptance is
not used to relax the MLS requirement. The initial oracle runner exits 1
because its expected negative rejections did not occur; both positive
simulations succeeded. Evidence is in `constraint-interface-omc/comparison.json`
and `constraint-interface-receipt.json`. Broader structural-interface limitations remain
separate open proof obligations, and no additional MSL support is claimed.

## Current semantic repair: computed fixed attributes

The fresh GyroscopicEffects comparison uses the same pinned MSL 4.1.0 sources
and OMC `a96aa1a-cmake`: frontend flattening, backend XML snapshots,
initialization dumps, and a successful five-second reference simulation.
Both frontends report 2,276 scalar equations and unknowns. OMC's 16 selected
states versus Rumoca's 18 coordinates with two quaternion norm constraints is
not, by itself, a semantic discrepancy. Fixed-rotation inverse matrices also
have parameter bindings in Rumoca corresponding to OMC's initial equations.
The DAE Modelica renderer refuses `promoted_cat1` in `axisRotation`; the DAE
JSON remains the authoritative Rumoca artifact for this comparison.

The first proven divergence is earlier, in instantiation. `Spherical.mo:97`
declares `w_rel[3]` with `fixed=fill(w_rel_a_fixed,3)`. GyroscopicEffects sets
`w_rel_a_fixed=true` for both spherical joints. OMC retains `fixed=true` on
all six scalar components, but Rumoca's Flat and DAE attributes were absent.
`extract_attributes` only accepted Boolean literals, discarding computed
values or allowing an unresolved outer modifier to fall through to a local
literal. Under MLS §8.6 this loses required initialization equations; MLS
§7.2.4 also requires the modifier's written scope and overriding precedence.

The reduced regression first fails with `enabled.x.fixed=None` instead of
`Some(true)`. The replacement evaluates uniform Boolean expressions using
the instantiation environment, preserving instance-specific parameter values
and outer modifier scope. A supplied but undecidable or nonuniform attribute
returns EI035 instead of disappearing. Heterogeneous per-component `fixed`
values still need a checked owner; this repair does not claim that capability.
PointGravityWithPointMasses2 uses mixed fixed arrays and now refuses them
explicitly. ControlledMixingUnit has the same limitation, and
Transformer3PhaseYyWithHysteresis refuses indexed `HFixed[1]`. All three were
already failing later stages; this repair exposes the earlier semantic gaps
without claiming support for their attributes.

The dynamics regression uses `der(x)=-x` and `y=2*x`, with `x.start=0` and
`y(start={1,2,3}, fixed=fill(pinned,3))`, `pinned=true`. Both Rumoca solvers and
OMC initialize `x={0.5,1,1.5}`, `y={1,2,3}` and follow the analytical exponential
solution on all six channels. The nine modifier integration tests and 956
affected library tests pass; six tensor initialization tests pass with the
new dynamic case. The real-model DAE changes only ten fixed attributes: the two three-component
angular velocity arrays become true and eight arrays become false. All other
canonical storage is exactly equal. Solve restores six initial rows (8 to 14);
continuous programs and both layouts remain exactly equal. Pure-call site
owner ordinals in discrete/event programs shift by two without other program
changes. Focused all-target/all-feature Clippy passes. The fixed canary retains
all 20 phase/status and band rows: nine models compare high across all 175
trajectory and initialization channels, with no missing or deviating channels.
The standard full-cohort gate passes with all 143 high-parity models retained,
16 existing exclusions, and zero missing, nonidentifiable, or deviating
comparisons. GyroscopicEffects still times out in simulation. No additional
model support is claimed. Diagnostic artifacts are in
`.git/multibody-campaign/gyroscopic-omc-stages/` and
`.git/multibody-campaign/gyroscopic-fixed-attribute/`.

## Current performance repair: shared pure-call interfaces

After restoring GyroscopicEffects' computed `fixed` attributes, a 0.01-second
diagnostic agrees with a fresh OMC simulation on all 967 shared channels;
initial values differ by at most 1.43e-14 on the diagnostic relative scale.
A separate full five-second diagnostic completes in 38.68 seconds of runtime
under an explicit 60-second budget. The repository comparator reports a
bounded normalized L1 score of 1.037e-6 and a worst channel score of 1.841e-5.
This is diagnostic evidence only: the standard 12-second simulation budget
still fails, and no additional model is counted as supported.

The actual standard worker's simulation profile identifies repeated typed-call
interface equality and payload work. The Solve artifact contains 2,070 primal
and embedded directional site records referring to 253 owners. Each issued
site deep-copied its immutable type, dependency, projection, and affinity
inventories. A reduced storage-sharing test fails before the repair.

Call sites now share those private immutable inventories with their owner.
Equality remains value-based; deserialized sites with separate allocations
are still accepted only when their complete metadata matches. No runtime
value cache, semantic identity, equation, or numerical policy changes. The
33 focused call tests and 522 IR/evaluator/Cranelift library tests pass,
including altered dependency/projection/affinity rejection controls and native
execution tests. Affected all-target/all-feature Clippy passes. The first normal
origin run still fails, reaching a Solve timeout rather than simulation; that
failure is retained. In the separately bounded diagnostic, the complete Solve
problem is exactly equal and all 2,276 trace channels at 501 samples are exactly
equal before and after sharing. Runtime falls from 38.68 to 33.95 seconds in
these diagnostic runs; this is not a standard-budget recovery or a cohort
performance claim. An explicit BDF short-horizon probe also succeeds, so the
initial BDF capability boundary is not the cause of the timeout.

The fixed canary `target/msl/multibody-shared-call-interface-canary` passes at
dirty-tree digest
`9994da98fd6de62d7850bb34f93f9b7603d88b13a1400ee5e84ac0ae4ad21548`.
All 20 phase/status and band rows match the previous canary: nine models compare
high, with all 175 trajectory and initialization channels high and zero
missing, skipped, nonidentifiable, or deviating comparisons. Its durable delta
is `gyroscopic-sim-timeout/shared-interface-canary-delta.json`. The complete
`verify quick` outcome is recorded above; `verify full` remains outstanding.

A controlled diagnostic rebuilds the pre-sharing and shared implementations
with the same Cargo command, then alternates four executions of each version
on CPU 6 with one worker thread. All 24 runs complete, and each model's entire
trace is exactly equal across both versions and all repetitions. Median runtime
changes are 84.663 to 80.835 ms for CompareSincExpSine, 731.556 to 723.295 ms for
DemonstrateSignalExtrema, and 1.07647 to 1.05386 seconds for DoublePendulum.
These three controls do not reproduce the broad slowdown seen in the cohort.
They do not discharge the failed performance gate or establish its host-level
cause. Both diagnostic binaries, source snapshots, and every attempt are kept
in `controlled-interface-comparison/`; the current source and canonical build
artifacts are restored.

A read-only audit identifies the next GyroscopicEffects runtime lead. All 51
algebraic projection stages and both derivative projection stages lack direct
seeds for some coupled coordinates, making the runtime's staged-execution
predicate false. It therefore uses the causal sweep followed by full projection
plans containing 2,004 and 479 blocks respectively. OMC's saved compiler dump
classifies the corresponding physical rotational systems as linear after
reduction, with 39 and 36 unknowns before tearing; mapping those equations to
Rumoca's 79- and 72-row blocks remains a proof obligation. Replaying the saved
SolveModel through the phase-owned checked constructor confirms that every
one of these 51 algebraic and two derivative projection stages already carries
an affine proof. This does not establish nonsingularity: ordinary projection
can accept an exactly zero residual before factoring its Jacobian. A staged
execution repair must preserve singular-system behavior, nonlinear branch
selection, and coordinate certification. No seed check has been relaxed and
no speedup is claimed for this hypothesis. Evidence
is `.git/multibody-campaign/gyroscopic-sim-timeout/`, including
`value-stage-coverage-audit.json`, `constructed-refresh-facts.json`,
`staged-refresh-candidate.json`, and `omc-coupled-runtime-blocks.json`.

### Exact zero-flow assignments

The next producer-level reduction identifies a simpler missing seed before the
coupled blocks. GyroscopicEffects logical row 2159 owns
`bodyCylinder2.frame_b.f[1]` (Y1264); its compact program loads three Y values
and stores them as residuals. The previous isolator issued `AffineResidual`,
which the declared-target seed constructor excludes. OMC's saved equations
1323–1325 assign these three forces to `0.0`.

Solve assignment construction now issues `Zero` for an exact target load or
certified total copy, including a scalar negation. It retains the original
evaluation prefix and target identity. Runtime evaluation and final scalar
materialization consume that certificate directly. Dynamic or zero
coefficients, nonlinear products, overwritten registers, and unproved calls
do not acquire this certificate. Solve schema 67 and checked wire replay bind
the new representation. A second RED regression fixes scalar materialization's
register allocator to advance past every tensor lane before appending a
constant.

All 462 IR/evaluator library tests pass. Seven source integration tests pass,
including a reduced unconnected array-flow connector, all seven analytic
channels in BDF/RK and Auto/Interpreter modes, and rejection of a forged
zero-assignment prefix. OMC independently generates the same zero-flow
equations and matches those seven analytic channels with maximum absolute
error `8.197e-8`. This follows MLS §9.2 and Appendix B.1a. An explicit written
zero equation used an existing Direct path; the retained connector reduction
exercises the originating missing seed.

All 512 core integration tests and affected all-target/all-feature Clippy pass.
The fixed canary `target/msl/multibody-zero-assignment-canary` passes at HEAD
`bc71577f85df24957e5c9ab30fdaf4ed48da4311`, dirty digest
`f6a825d032b0c4c0fbea179225d9380e9990a6125be82b0122b7710dee3be07b`.
All 20 phase/status and band rows match the preceding shared-interface canary:
nine compared models, all 175 trajectory and initial channels high, and zero
deviating, skipped, missing, excluded, or nonidentifiable comparisons.

The normal-budget origin still fails the 12-second Sim limit after successful
initialization; Solve takes 7.107 seconds. A separate 60-second diagnostic at
the source experiment's `rtol=atol=1e-8` completes five simulated seconds in
19.251 seconds of runtime. Its 967 shared OMC channels are all high, including
initialization; the worst channel bounded normalized L1 error is `8.861e-6`.
This remains diagnostic evidence, with no normal-budget high-coverage credit.
An earlier diagnostic mistakenly used `rtol=atol=1e-6`; its separate artifacts
are retained and excluded from performance conclusions.

The new Solve artifact preserves every canonical continuous program and
changes only the continuous refresh owners. Algebraic seed rows increase from
2,030 to 2,045 and numerical projection stages decrease from 51 to 36. The
complete-seed staged guard is unchanged. The two coupled blocks still lack
74 seeds. One remaining residual is now mapped through OMC alias signs:
Rumoca row 1426, after evaluating this model's rotation parameters, is the
negative of OMC regular residual 1716 and therefore has the same zero set.
The typed call owner is a transpose followed by matrix multiplication; OMC
chooses different tearing assignments. This establishes one equation mapping,
not equivalence of the complete coupled systems.

Evidence is under `.git/multibody-campaign/gyroscopic-sim-timeout/`:
`zero-assignment-receipt.json`, `zero-assignment-canary-delta.json`,
`zero-assignment-origin-receipt.json`, `zero-assignment-solve-trace-delta.json`,
`zero-assignment-strict-omc-comparison.json`, and
`coupled-torque-origin-omc.json`. The subsequent full 566-model gate passes;
its measurement and remaining scope are recorded at the top of this ledger.

### Independent additive assignments

The next reduction exposes a numerical defect in the old `AffineResidual`
certificate: the residual `x + 4 - 13` with a starting guess of `1e30` returns
an isolated value of zero, although its solution is nine. The former evaluation
`x - residual/coefficient` loses the small independent terms. The RED test is
`additive-offset-red-1.log`; this is a general equation-isolation defect rather
than a model-specific exception.

Solve construction now issues `Additive` with a finite, nonzero constant target
coefficient and weighted, target-independent source registers. Reverse producer
order combines shared contributions before visiting their children, so repeated
DAG expressions cannot expand into an exponential term tree. The original
program prefix still executes; the isolated value uses only the independent
register selection. Shape metadata is immutable and borrowed during evaluation.
One IR-owned final scalar materializer serves both canonical refresh owners and
the evaluator's compiled schedules. The old residual-correction representation
and its readers are removed; Solve schema 68 replays the new certificate.
Complete seed coverage, coefficient singularity, and nonlinear branch guards
remain required under SPEC_0036 / SPEC_0043 §6 and MLS Appendix B.1a.

The reduced `AdditiveTorque.Probe` retains the actual moment-equation form
`zeros(3) = torque_a + transpose(R)*torque_b - cross(r,force)`, with the rotation
inside a typed function call. At `R=[0,-1,0;1,0,0;0,0,1]`, `r={1,2,3}`,
`torque_b={x,2*x,3*x}`, and `force={2*x,3*x}`, the exact result is
`torque_a={-3*x,3*x,-4*x}`. All ten channels follow `x=exp(-time)` under BDF/RK
with Auto/Interpreter, despite torque starts of `1e30`. OMC's flattened equation
preserves the same rotation and cross product; regular assignments 13–15
isolate the independent torque expressions. Its twelve CSV rows agree with
the ten analytic channels to at most `1.093e-7` absolute error.

Focused validation passes 466 IR/evaluator tests, all 514 core integration
tests, and 429 solver control tests. The mode-dependent pivoting fixture now
asserts its newly available additive `y=3-x` seed and explicitly verifies that
incomplete seed coverage still requires the complete coupled solve; its
singular-coefficient mode and final solution assertions are unchanged. Regressions cover the original cancellation, bounded sharing after forty
doublings, prefix input errors, materialized execution, cancelled/nonlinear
coefficients, and rejected wire changes to terms, coefficient, and prefix.
The fixed 20-model canary passes with all phases and agreement bands unchanged:
nine compared models, 175 trajectory and initialization channels high, and no
missing, skipped, excluded, or deviating comparisons. Four models have small
error-score changes, recorded in the delta; the band rows are not identical.
The full 566-model gate then passes as reported above.

The originating GyroscopicEffects normal attempt still times out at 12 seconds,
after successful initialization and 6.651 seconds of Solve. A diagnostic from
freshly recompiled DAE, at `rtol=atol=1e-8` with a 60-second runtime limit,
finishes five simulated seconds in 18.024 seconds. All 967 shared OMC channels
are high, including initialization; 1,309 Rumoca-only channels remain outside
that comparison. This is diagnostic evidence, not normal-budget coverage or a
controlled speedup measurement. An initial diagnostic accidentally consumed a
DAE predating the fixed-attribute repair; it is preserved and excluded from
current-source correctness/performance conclusions.

The fresh source export changes provenance identifiers because of source path
spelling. Matching all 2,668 source files by canonical path and identical bytes
allows the span IDs to be mapped explicitly. After that mapping, the only Solve
changes are schema version and continuous refresh owners: the original equations,
initialization, events, typed calls, and other products are identical. Algebraic
seed rows increase from 2,045 to 2,127, numerical projection stages decrease from
36 to two, and missing seeds decrease from 108 to 26. Derivative seed rows grow
from 554 to 602; its missing seeds decrease from 74 to 26. Both plans remain
uncertified as complete causal solutions.

The next concrete residual is logical row 1399, program 426, output 2, matched
to `bodyCylinder1.body.frame_a.f[2]` (Y871). Its target is inside
`cross(r_CM,frame_a.f)[3]`, with a runtime coefficient `r_CM[1]`. OMC's regular
moment assignment 1688 contains the same product; its alias map negates the
body's torque into `bodyCylinder1.frameTranslation.frame_a.t[3]`. OMC solves the
force using a different translational equation (1686). Symbolic expansion of
the canonical tensor program, after that XML torque alias and the two zero
center-of-mass components, gives exactly the negative of OMC's assignment
residual; their sum is zero. The target coefficient is `-r_CM[1]`.
A general tensor affine projection is a hypothesis for the remaining isolation gap; a zero or nonfinite
coefficient must retain the original residual, and the complete-seed guard must
remain. No compiler change for that next hypothesis has been made.

Artifacts are under `.git/multibody-campaign/gyroscopic-sim-timeout/`:
`additive-offset-libraries-2.log`, `additive-offset-core-1.log`, and
`additive-torque-omc/` (source, flattened equations, backend info, CSV, analytic
comparison). Current validation receipts include
`additive-assignment-canary-delta.json`, `additive-assignment-full-delta.json`,
`additive-assignment-origin-receipt.json`, `additive-assignment-solve-delta.json`,
`additive-assignment-source-provenance-delta.json`,
`additive-assignment-diagnostic-configuration.json`, and
`additive-assignment-current-omc-comparison.json`. The original equation mapping remains in
`coupled-torque-origin-omc.json`.

## Parameter preparation during shape inference

An isolated FullRobot diagnostic takes 8.83 seconds in Flatten, then reaches
its prior ToDae ED019 refusal. The diagnostic does not replace its timed
cohort failure. CPU samples show parameter-map construction and copying;
the sampled DWARF stacks are incomplete, so no inclusive CPU percentage is
claimed. A debugger stop on the 32nd `build_eval_context` call establishes the
path from `Context::build_parameter_lookup` through `DimensionScope::value`
to a fresh `ParamEvaluator` for an individual shape query.

Reduced tests return the correct shapes but fail the work bound: a matrix
walk prepares the inventory 32 times, and two comprehension branches prepare
it six times. `DimensionScope` now lazily prepares one existing evaluator per
immutable parameter inventory in a walk. Cloned lexical scopes share that
preparation but retain separate index bindings; separate roots and function
invocations prepare their own inventories. This caches no expression results.
The three focused tests pass, including sibling scopes and index shadowing,
as do 145 evaluator tests, 630 Flatten tests, all 505 compiler-core tests, and
affected all-target/all-feature Clippy. Formatting and diff checks pass.

The seven-model origin run recovers each model's earlier stage under the
normal budgets: FullRobot, IMS_Start_Polyphase, and
LightningSegmentedTransmissionLine reach their prior DAE refusals;
PrismaticConstraint, PlanarFourbar, and PointGravityWithPointMasses2 reach
Solve; GyroscopicEffects initializes before failing simulation. There are
zero completed simulations or comparisons, so the focused run exits 1 with
parity unmeasured. FullRobot and IMS still take 9.15 and 9.63 seconds in
Flatten; the full cohort must test whether the recoveries hold under its load.

The fixed canary `multibody-dimension-preparation-canary` passes at dirty-tree
digest `29302162fb394ef5bd448c433d4286a3ac32c2595d6af2e21b717ec8fafdc6ac`.
All 20 phases/statuses and channel-band rows are unchanged. Nine models compare
high, with all 175 trajectory and initialization channels high and zero
missing, skipped, nonidentifiable, or deviating comparisons. Tier 1 is complete.
The preceding `multibody-dimension-preparation-full` measurement confirmed six
of the seven focused recoveries; IMS_Start_Polyphase remained a Flatten timeout.
All prior high-parity models and
channel bands are retained. No new high-parity model is claimed.

Evidence is under `.git/multibody-campaign/fullrobot-flatten-profile/`,
`fullrobot-flatten-callstack/`, and `dimension-preparation-{red,green}-1.log`.
The origin and canary deltas are `dimension-preparation-origin-delta.json` and
`dimension-preparation-canary-delta.json`.
The governing ownership rules are SPEC_0029 §5 and SPEC_0033 §§2–3/6a;
the existing MLS array and lexical-scope rules remain unchanged.

## Borrowed inventories in the shared constant evaluator

The remaining IMS_Start_Polyphase regression exposes copying between separate
shape walks. Its isolated diagnostic spends 8.51 seconds in Flatten; leaf CPU
samples again identify parameter-map preparation. Eight reduced independent
queries return the right shapes but materialize the 128-binding inventory eight
times, failing the zero-copy work bound.

`EvalEnvironment` now supplies read-only values, dimensions, and functions to
the existing constant interpreter. Owned and borrowed inventories share
`EvalContext`'s single scoped lookup order. `ParamEvaluator` borrows the stable
Flatten maps, prepares enumeration identities and function-name aliases, and
materializes only scalar values actually read. It retains no expression-result
cache and cannot outlive the borrowed inventory. Function definitions and shape
vectors are borrowed, including through short function aliases.

The reduced tests compare owned and borrowed interpretation across sibling and
root scopes, unknown values, enums, function formals, and shape-only metadata.
Resource tests count actual owned-context parameter insertions and require
zero for scalar-only shape queries. Pointer checks prove that function aliases
and dimensions retain their original backing data. All 147 evaluator and 630
Flatten tests pass; all 506 compiler-core tests pass.

An identical source probe in Rust and OpenModelica `a96aa1a-cmake` preserves
2×2 and 5×2 arrays in sibling components, despite a different root-scope
parameter. Both Rumoca solvers and OMC produce state slopes 1.5 and 12, with
their sum at 13.5. OMC emits no warnings or errors; its six trace rows differ
from those affine trajectories by at most 1.34e-15. This checks MLS §§5.3,
10.1, and 12.4; no compiler branch names an MSL model.

Evidence is `.git/multibody-campaign/borrowed-parameter-receipt.json`,
`borrowed-parameter-omc/comparison.json`, and the corresponding test logs.
All-target/all-feature Clippy passes for the evaluator, Flatten, and main
crate. The final evaluator rerun passes all 147 tests, including the insertion
and borrowing checks. The fixed canary passes with all 20 phase/status and
channel-band rows unchanged: nine compared models, all 175 trajectory and
initial channels high, and zero missing/skipped/nonidentifiable/deviating
comparisons. Its durable delta is `borrowed-parameter-canary-delta.json`.

The full 566-model gate passes in 249.09 seconds with 493 Flatten completions,
286 DAE models, and 256 Solve models. IMS_Start_Polyphase recovers its earlier
ToDae balance frontier. All 566 model bands and channel-band counts are unchanged
from the preceding full run: 143 compared high, 16 tracked exclusions, zero
missing/nonidentifiable/deviating comparisons. MultiBody remains at 40 Flatten,
33 balanced DAE, 17 initialized, and 14 strict-high models out of 42. The full
receipt is `borrowed-parameter-full-delta.json`; no new high-parity model,
baseline promotion, commit, or completed quick/full verification is claimed.

## Boolean parameters determined at initialization

Fourbar_analytic's ED020 refusal is traced through a debug build to ordinary
initial-equation lowering, then arithmetic binary-expression construction.
This excludes the competing function-builtin and record-field explanations.
Flat initial row 0 subtracts a Boolean-valued `selectBranch` call from
`jointSSP.prismatic.positiveBranch`. The row's left-hand reference and the
declared Boolean parameter carry the same occurrence identity; its source
declaration has `fixed=false` in `PrismaticWithLengthConstraint.mo`.

A reduced source with no MultiBody dependency reproduces ED020 on
`branch = q > 0` in an initial equation. Another initial equation sets `q=-2`,
despite a start guess of 5. OpenModelica accepts the identical source without
warnings and produces `q=-2`, `branch=false`, and `y=-time` in all six rows.
The correction must preserve MLS §8.6's initialization semantics: selecting
the branch from the start guess or converting Boolean equality into numeric
subtraction would be incorrect. A typed initialization owner and regression
now pass focused validation. The DAE owner preserves the exact unbound,
non-Real `fixed=false` parameter, its type/shape, and source provenance. Wire
replay and structural transformation reconstruct it through the checked API.
Solve substitutes the definition into initialization residuals and commits its
value after solving. Cyclic parameter definitions and initialization-determined
String storage retain explicit unsupported diagnostics.

A stronger independent probe exposed a second issue: initialization grouped
all rows sharing an unknown into one numerical block even when their matched
dependencies were one-way. With a Boolean branch changing between the state
guess and solution, the combined line search stalled. The planner now uses the
shared dependency-first SCC decomposition to order matched rows, retaining
mutually dependent rows together and all unmatched consistency checks.

The three reduced core tests pass with BDF and RK-like simulation. They cover
opposing guesses and solved signs, Boolean/Integer dependency chains, a function
call, reversed equation orientation, and a state crossing zero after
initialization while its parameters stay constant. OpenModelica accepts the
three identical positive source variants without warnings or errors; six rows
per variant agree with the analytic trajectories within 1.78e-15. The complete
compiler-core suite passes all 509 tests. The seven affected libraries pass
1,451 tests; four initial-owner integration tests pass. All 243 architecture
checks, 17 repository gates, and affected all-target/all-feature Clippy pass.
The schema pin now names template version 6, and the DAE module review below
accounts for its new owner. No coverage claim was made from focused tests alone.
The following complete
measurement closes that validation. Details are in
`boolean-initial-parameter-receipt.json`.

The fixed 20-model canary passes with every phase and band unchanged: nine
compared models, 175 trajectory/initial channels, and no missing, skipped, or
deviating comparisons. The three analytic origins now produce balanced DAEs.
Fourbar_analytic reaches EL005 with 1,622 of 1,659 equations matched;
Engine1b_analytic and PlanarLoops_analytic reach the 10-second Solve limit.
That focused origin run has no simulations or comparisons and reports parity
unmeasured, not success.

The full `multibody-initial-parameter-full` gate passes in 312.16 seconds at
the commit and digest stated above. DAE completions increase from 286 to 289,
and balanced DAEs from 274 to 277. All 566 model bands and channel-band counts
are unchanged: 143 compared high, 16 existing exclusions, and zero missing,
nonidentifiable, or deviating comparisons. No new high-parity model is claimed.
GyroscopicEffects now hits the Solve limit before initialization; its preceding
full run spent 8.65 seconds in Solve before reaching a 12-second simulation
timeout. This regression remains visible and requires profiling. The complete
delta is `boolean-initial-parameter-full-delta.json`. The speed gate passes,
but combined `verify quick` and `verify full` remain outstanding.

### Scalar-row owner lookup

The latest full run exposes GyroscopicEffects exceeding its unchanged
10-second Solve budget. It has no initialization-defined parameter owners.
A separate diagnostic profile attributes 15.44% of leaf samples to
`DaeView::continuous_owner` and 6.54% to `continuous_owner_for_scalar_row`.
GDB stops inside that scan with the actual caller
`explicit_derivative_preferences` during structural preparation. This proves
the repeated lookup cost, but does not attribute the entire timeout to it.
The profile is diagnostic evidence, not a successful normal-budget attempt.

The reduced resource test makes 256 row queries in reverse order and fails
because the old lookup materializes 32,896 owner views. The checked equation
constructor now derives each owner's exclusive scalar-row end from its
predecessor and checked row count. Overflow is rejected before insertion.
The immutable owner sequence supports binary search, including duplicate ends
for empty families. It retains one entry per semantic owner; it neither
expands domains nor changes residuals, source order, or wire operations.
SPEC_0032 §§1–2 and SPEC_0036's construction/index rules govern this change.

Four focused tests pass: the resource bound, mixed scalar/family boundaries
with multiple body equations and canonical JSON/binary replay, empty families,
and compact domains with more than `u32::MAX` total scalar rows. The four
affected libraries pass 479 tests; compiler core passes 509, architecture 243,
and repository gates 17. All-target/all-feature Clippy passes after extracting
a repeated-body test helper.
Diagnostic compilation of the unchanged GyroscopicEffects source produces
byte-identical DAE and Solve exports before and after this optimization;
`dae-equality.json` and `solve-equality.json` retain the hashes. The first
post-change diagnostic omitted the sibling ModelicaServices source root and
stopped at name resolution; the corrected command loads the complete pinned
library root. These exports are separate from normal-budget gate attempts.
The fixed 20-model canary passes with all phase, model-band, and channel-band
counts unchanged: nine compared models and 175 high trajectory/initial
channels, with no skipped, missing, or deviating comparisons. The focused
GyroscopicEffects origin still exceeds the 10-second Solve budget and has no
parity measurement. This failure remains recorded.

The complete `multibody-scalar-owner-full` gate passes in 283.02 seconds at
the commit and digest stated above. All 566 model bands and channel-band
counts are unchanged: 143 compared high, 16 existing exclusions, and zero
missing, nonidentifiable, or deviating comparisons. DAE completions remain
289, balanced DAEs 277, and successful simulations 159. Successful
initializations recover from 175 to 176: GyroscopicEffects completes Solve in
7.74 seconds, with 7.68 seconds in lowering, then initializes and reaches its
12-second simulation timeout. This full-run result does not erase the focused
timeout or establish reliable performance headroom. The speed gate passes;
no new high-parity model, baseline promotion, or combined quick/full pass is
claimed. Evidence is in `.git/multibody-campaign/gyroscopic-solve-timeout/`,
including `scalar-owner-canary-delta.json`, `scalar-owner-origin-delta.json`,
and `scalar-owner-full-delta.json`.

A separate diagnostic attaches sampling to the actual `msl-fast` worker only
during Solve, using the worker's one-thread compiler setting. Its 670 CPU
samples put 13.43% of leaf cost in `FunctionRebuilder::rebuild_function`.
Inspection finds a whole-expression-arena scan for orphaned scoped expressions
inside each function reconstruction. This is the next resource-cost hypothesis,
not a proven attribution of all sampled time: it needs a reduced RED test with
orphaned scoped expressions and unrelated model expressions before changing
the producer. The worker was stopped after Solve; this diagnostic has no
coverage credit. Its receipt and analysis are under `worker-profile/`.

The SPEC_0043 module review measures 17,347 core + 5,431 wire = 22,778 lines.
Against the initialization-owner inventory below, `equations` increases from
799 to 835 lines and `model` excluding wire from 8,373 to 8,376; every other
group is unchanged. The 39-line increment retains checked overflow handling
and immutable derived lookup metadata. The old linear scan is removed, and
serialization still emits only the existing semantic operations. There is no
second equation representation or compatibility reader to retain. Only the
total acknowledgment crosses a 250-line step; review triggers are unchanged.

### Initialization-owner module review

Under SPEC_0036's DAE milestone review and SPEC_0043 §1, production DAE source
is 17,308 core + 5,431 wire = 22,739 lines, compared with the preceding
derivative-owner review's 17,163 + 5,407 = 22,570. This adds 145 core and 24
wire lines; only the core acknowledgment crosses a 250-line step.

| Production module group | Physical lines |
|---|---:|
| `model` excluding wire | 8,373 |
| `expression` | 3,889 |
| `discrete_values` | 1,216 |
| `equations` | 799 |
| `model_event_transactions` | 440 |
| `conditions` | 436 |
| `error` | 398 |
| `events` | 328 |
| `clocks` | 321 |
| `expr_query` | 293 |
| `temporal` | 286 |
| `lib`, `ids`, `provenance` | 529 |
| Wire | 5,431 |

The new 128-line `model/initial_parameters.rs` contains the checked constructor
and immutable view. Remaining core additions register the arena, identity,
error, count, and freezing. The owner cannot be an ordinary parameter binding:
its value may depend on initialization unknowns. It cannot reuse the discrete
initial-value owner, whose reads must already be settled. Wire replay shares
`InitialValueWire` with discrete values, replacing the old discrete-only record
name, but retains separate checked construction and ordinal checks. The wire
accepts only the current schema. Review found no obsolete compatibility reader,
duplicate parameter-binding path, or removable semantic check.

The evidence is `.git/multibody-campaign/boolean-initial-equation-callstack/`
(`run-debug.log`, `initial-branch-flat.json`, `row-identity.json`) and
`boolean-initial-equation-probe/` (`Source.mo`, the Flat export, ED020 diagnostic,
and `comparison.json`). The initial optimized-worker debugger attempt lacked
source symbols; the recorded stack comes from the subsequently rebuilt debug
worker. Engine1b_analytic and PlanarLoops_analytic share the error category;
their exact rows have not yet been independently traced.

## Scope and initial census

The source is MSL 4.1.0, with ModelicaTest pinned at
`8ae3d35c24e519cb2996cab20f3b13daf2b0c50a`. An OpenModelica semantic inventory
(`getClassNames`, `getClassRestriction`, `isPartial`) identified:

- 42 MultiBody root examples;
- 111 non-partial models under `ModelicaTest.MultiBody`;
- 171 non-partial models in the MultiBody library, including components that
  require an enclosing model and connections.

The library also contains functions, connectors, types, and partial classes.
The inventory is a scope record, not evidence that those declarations are
covered. Component and function coverage still needs to be mapped to reviewed
test embeddings; the 42 examples alone do not establish complete library
coverage.

The initial 42-example diagnostic run at the branch base used the normal
10-second phase budgets and 12-second simulation budget with a 14-second
parent watchdog. OpenModelica `a96aa1a-cmake` simulated every selected example.
Rumoca reported 20 balance failures, six structural-analysis failures, six
timeouts, five DAE-construction failures, one runtime-contract failure, and
one instantiation failure. The remaining three simulations were compared;
none had a deviating channel, skipped comparison, or missing trace.

Artifacts are retained locally under `target/msl/multibody-baseline`, with
provenance and hashes under `.git/multibody-campaign`. This focused diagnostic
does not supply a full 566-model cohort parity claim or baseline promotion.

## Array constructor equation dimensions

The originating model is
`Modelica.Mechanics.MultiBody.Examples.Elementary.SpringMassSystem`.
Its two `Joints.Prismatic` instances contain this torque balance:

```modelica
zeros(3) = frame_a.t + frame_b.t + cross(e*s, frame_b.f);
```

The expected equation count is three per joint. At the branch base, Flat
equation rows 10 and 57 each recorded `scalar_count = 1`. This explains
exactly the observed deficit: 1465 equations for 1469 unknowns.

The first divergence is in flattening. The array-only dimension context does
not establish the scalar shape of `s`, so the right-hand cross product cannot
prove its shape there. The flattener also omitted the constructor shape on
the left. Connection-edge removal is not the cause: the model's
`oc_break_edge_scalar_count` is zero. Inspection of the Flat artifact found
no mismatched cardinalities for direct variable-reference left sides.

The semantic basis is MLS 3.6 §§10.3.3, 10.3.5, 10.6.1, and 10.7; the phase
owner follows SPEC_0007 and identity handling follows SPEC_0001. The fix
recognizes constructor calls through their exact predefined declaration IDs
and carries their complete dimensions into equation cardinality and the Flat
tensor domain. `fill` retains the dimensions of an array-valued element after
its new axes. Unknown or invalid extents do not establish a constructor
shape. DAE balance checks are unchanged.

The regression `scaled_cross_product_preserves_all_three_torque_equations`
reduces the defect to seven scalar unknowns. Before the fix it fails with
five equations. Its independent runtime oracle is
`torque(t) = {4*(2+t), -3*(2+t), 0}`. Additional focused checks cover matrix
and higher-rank constructors, empty extents, structural dimensions, unknown
dimensions, same-spelling non-predefined declarations, and the scalar result
of a vector dot product.

The repaired real-model Flat artifact changes only the two equation
cardinalities and their structured domains. Each new domain has one binder
over `1:3`; the original equations, declaration identities, and spans remain
intact.

Tier 1 validation:

- All 624 flattening tests pass.
- The end-to-end torque regression passes its equation-balance and analytic
  trace checks.
- Clippy passes for `rumoca-phase-flatten` and the `rumoca` library and core
  integration suite, with all features and warnings denied.
- The fixed canary has no model-band or failure-category transitions. All
  eight completed simulations are compared with no deviating channels,
  skipped comparisons, or missing traces. The other twelve targets retain
  their failures; the command's successful exit does not make them supported.

The canary's before evidence is the same twenty members of the complete
Tier 2 run at `82d87a16d11f6c6e86069510cfec471874da480a`, whose source tree
`0436b778ec9a36a53a8d935fe29a98a89344edec` is identical to the branch base.
After evidence is in `target/msl/multibody-constructor-canary`; the per-model
delta and hashes are in `.git/multibody-campaign/constructor-canary-delta.json`.
This is a Tier 1 regression check, not a cohort parity claim.

The follow-up 42-example diagnostic removes 19 of the 20 balance failures.
`Constraints.PrismaticConstraint` still has a two-equation deficit, down from
sixteen. `SpringMassSystem` now has 1469 equations and 1469 unknowns, but
structural matching still fails (1394 matches for a 1406-by-1406 system).
These remaining failures are not exceptions or passing models.

`Rotational3DEffects.ActuatedDrive` newly completes simulation. All 455
compared channels, including initialization, agree with OpenModelica within
the high band. Every completed simulation in this focused run is compared;
there are four comparisons, no skipped or missing traces, and no deviating
channels. Per-model changes and artifact hashes are retained in
`.git/multibody-campaign/constructor-multibody-delta.json`.

Manual review of `ActuatedDrive` identifies two equivalent inertias of 2,
driven by `sin(2*pi*time)` with zero initial angle and angular velocity and
zero gravity. An independent check of all 501 Rumoca samples verifies both
`revolute` and `rotor1D` against
`w=(1-cos(2*pi*t))/(4*pi)` and
`phi=t/(4*pi)-sin(2*pi*t)/(8*pi^2)`. Maximum absolute errors are below
`1.3e-6` for velocity and `7.7e-7` for angle, with exact zero initialization.
The check and trace hash are in
`.git/multibody-campaign/actuated-drive-analytic.json`. This reviewed case
exercises the new constructor-shape path together with the existing
revolute-joint, rigid-body, one-dimensional rotor, mounting, connection,
initialization, and integration paths; it does not establish unrelated joint
or state-selection behavior.

The complete 566-model Tier 2 run at
`3dec54b64813bfb4e50d6177ab6b664cccb59ef4` passes. It compares 130 models,
all strict-high, with zero deviating channels or missing traces; 17 unchanged
reviewed policy exclusions remain skipped and do not count as supported.
The only model-band transition from the preceding full run is
`ActuatedDrive`, absent to high. Evidence is retained in
`target/msl/multibody-constructor-full` and
`.git/multibody-campaign/constructor-full-receipt.json`. The tracked source
stayed unchanged throughout the run; the local untracked FastDyn communication
file accounts for the comparator's dirty-worktree flag. Release gates have
not yet been rerun for this branch.

## Declared function result dimensions

The remaining balance failure after the constructor fix was
`Constraints.PrismaticConstraint`: 2427 equations for 2429 unknowns.
Its `freeMotionScalarInit.initAngularVelocity` component contains:

```modelica
Frames.angularVelocity2(R_b) =
  Frames.resolve2(R_b, Frames.angularVelocity1(R_a)) + w_rel_b;
```

`Frames.angularVelocity2` declares the output `w[3]`, but Flat row 160
counted this equation as one scalar. The precollected executable function
already retained the resolved declaration identity and fixed output shape;
equation shape inference did not consume that evidence. Connection-edge
removal again contributes zero and cannot explain the deficit.

MLS §§12.4.3 and 10.6.1 establish the first output and equation dimensions.
MLS §12.4.6 permits automatic vectorization only for a function with one
scalar result. A fixed array result therefore establishes its declaration's
axes; a scalar result alone cannot establish the call's dimensions.
SPEC_0007 assigns this work to flattening, using SPEC_0001 declaration
identity.

The flattener now indexes declaration-proven array results by `DefId` before
flattening equations. Every exposure of a declaration must agree on the
shape. Conflicting exposures, deferred extents, and scalar results remain
unknown at this boundary. Equation cardinality and the structured domain
consume the same shape. The real-model Flat difference is exactly row 160's
cardinality, one to three, and its corresponding `1:3` domain.

The end-to-end angular-velocity regression fails before the fix with seven
equations for nine unknowns. After the fix it balances and follows the
independent analytic trace `{3*time, 4*time, 5*time}`. A scalar-function
vectorization control verifies all three components of `shift(x)={1,2,3}`.
Unit controls distinguish same-spelling declarations, refuse conflicting
exposures in either order, and refuse deferred dimensions even when provisional
effective dimensions look fixed.

Tier 1 validation passes: all 628 flattening tests, the angular-velocity and
vectorization regressions, the previous torque regression, formatting, and
focused all-feature Clippy with warnings denied. The fixed twenty-member
canary has no band, phase, or failure-category changes relative to those same
members in the complete run at `3dec54b6`. All eight completed traces are
compared with zero skipped, missing, or deviating channels; twelve targets
retain their failures. Evidence is in
`target/msl/multibody-function-shape-canary` and
`.git/multibody-campaign/function-shape-canary-delta.json`.

The same 42-example diagnostic now proves `PrismaticConstraint` balanced at
2429 equations and 2429 unknowns. It remains structurally singular, with
2287 matches in a 2313-by-2313 system, and is not supported. The other 41
models retain their phase and failure classifications. All four completed
traces are compared, with zero skipped, missing, or deviating channels.
There are no remaining balance failures in this focused set. The remaining
38 failures are 18 structural-analysis failures, twelve timeouts, six
DAE-construction failures, one runtime-contract failure, and one instantiation
failure. Evidence is in `target/msl/multibody-function-shape-after` and
`.git/multibody-campaign/function-shape-multibody-delta.json`; producer hashes
are in `.git/multibody-campaign/function-shape-proof.json`.

The complete 566-model Tier 2 sweep at
`3bc697611242f10c19d93dcc3e93eb26d55172aa` passes: 130 models compared,
all strict-high, 9659 channels compared with zero deviations, and zero missing
traces. The same 17 reviewed policy exclusions remain skipped and unsupported.
No model changes agreement band relative to the complete run at `3dec54b6`.
The tracked source remained unchanged throughout this sweep. Evidence is in
`target/msl/multibody-function-shape-full` and
`.git/multibody-campaign/function-shape-full-receipt.json`.

## State definitions through exact coordinate aliases

`SpringMassSystem` reaches a balanced DAE after the shape fixes but originally
matches only 1394 of 1406 structural rows. The direct state-definition search
accepts a state on the left of a kinematic equation, but the model gives its
kinematics through `p1.frame_b.r_0`, an algebraic coordinate exactly equal to
`body1.frame_a.r_0`. The unique causal-definition map cannot supply this fact:
the geometry equation and the alias equation both define that coordinate.

The structural change consumes the existing exact value-equality
anchor, preserving its sign, before applying the existing state-demotion
checks. It accepts either equation orientation and reconstructs a checked DAE.
This is SPEC_0007/SPEC_0040 STRUCT-T04, with MLS §§8.3.1, 8.6 and `der`
semantics; it does not implement general state selection or alias elimination.

The real model now matches all 1406 rows. Its 644 variables, 445 continuous
equation owners and source map keep their original order and provenance.
Six body-coordinate arrays become algebraics; the four fixed joint initial
values remain 0.1, 0, 0.1, 0. At this step runtime still exceeds the 12-second
simulation budget. The subsequent tensor assignment repairs below let the
same model complete in 2.134 seconds with every compared channel strict-high
in `target/msl/multibody-gravity-complete-examples`. An independent analytic
check of both prismatic joints uses
`s = 0.1 + g/30*(1-cos(sqrt(30)*time))`, with `g = 9.80665`.
Maximum displacement, velocity, and acceleration errors over all 501 samples
are 1.94e-5, 1.08e-4, and 5.79e-4, respectively, under the model's original
1e-6 tolerance.

Three analytic kinematic regressions cover vector coordinates, a signed alias,
and reversed equation orientation, including the initial displacement and
velocity. All 141 structural tests and 13 initial-value alias-transfer runtime
tests pass. Five holonomic-specific fixtures now declare `StateSelect.always`
to continue exercising their intended reduction; their proof assertions are
unchanged. Focused structural/core Clippy and formatting passed.

The diagnostic canary in `target/msl/multibody-alias-state-canary` adds
`FirstGrounded` with every compared channel high and leaves its other members
unchanged. The subsequent 42-example run at
`target/msl/multibody-alias-state-after` exposes the counterexample below.
These are focused working-tree measurements, not complete cohort claims. The
state-demotion change was held until that counterexample was closed.

## Receiver function redeclaration counterexample

The originating model is
`Modelica.Mechanics.MultiBody.Examples.Elementary.UserDefinedGravityField`.
The state-demotion work enables it to complete, but the comparison reports
48 deviating channels among 193, including five at initialization. Rumoca
keeps gravity and the pendulum motion zero; OpenModelica's initial vertical
gravity is -9.780263581798753. This paused unrelated capability work until the
closure recorded below.

The source redeclares `world.gravityAcceleration` to
`theoreticalNormalGravityWGS84(phi=geodeticLatitude)`. Before the receiver fix,
Flat equation 27 still calls the default `standardGravityAcceleration`
implementation with `world.gravityType=NoGravity`. That explains zero gravity
without invoking a solver, initialization, or mechanical-reduction hypothesis.

The minimal receiver regression replaces a default `Double` with `Triple`.
Instantiate records the correct slot and implementation DefIds, but Flatten
selects `Double` through direct and inner/outer receivers. The first divergent
producer is Flatten's function override selection: it only consults the
caller's modification environment. MLS §§5.4, 7.2.2 and 7.3, SPEC_0001 and
SPEC_0007 Stage 2 govern the repair.

The working repair supplies the receiver's instance modification environment
and retains the enclosing instance scope of bound modifier arguments. Three
Flat identity regressions pass. Two runtime regressions check direct and
inner/outer calls across every sample, including nested instances with distinct
parameter values. The nested test caught a second defect in the intermediate
repair: both instances used the declaration's default value. Carrying the
modifier's enclosing instance identity and path closes that focused regression.
The complete Flatten test suite passes (687 tests across its unit, integration,
and documentation groups), as does Flatten's all-feature, all-target Clippy
check. The originating model's Flat output now selects only the WGS84
implementation and carries the latitude argument from the correct instance.

The normal 12-second run in `target/msl/multibody-receiver-gravity` and a
separate 60-second diagnostic run both reach consistent initialization, then
time out without a complete trace. The extended diagnostic changes no supported
coverage count or benchmark budget. Public live stepping through 0.5 seconds
produces nonzero pendulum motion and the expected initial gravity, but it is
not a replacement for the complete OMC comparison. Algebraic output observation
is expensive: at 0.02 and 0.04 seconds it takes 26 and 32 full projection
sweeps respectively. Diagnostic tracing identifies two unsettled singleton
blocks; the exact producer and assignment certificates are under investigation.
The missing proof was in Solve's exact-assignment constructor: scalar additive
residuals admitted an isolator, but the equivalent `TensorBinary` sum did not.
The repair projects the operands of the requested tensor element through the
existing stride/lane checks and applies the same additive proof. It retains
the compact program and rejects cancelled targets and unisolated dependencies.
Both positive tests failed before this change; all five tensor shape tests now
pass, including stride identity and negative controls.

That repair reduced the full simulation to 1.69 seconds, but the comparison in
`target/msl/multibody-gravity-tensor-isolators` still found five torque-channel
deviations. The generated assignment builder allocated new registers after the
largest destination **start**, overwriting live tensor lanes. A standalone
materialization regression reproduced `-1` instead of `4`. The builder now
uses the existing checked register-flow extent. Its single-output and grouped
assignment regression passes; the Solve IR, evaluator, and solver unit suites
pass all 823 tests.

`target/msl/multibody-gravity-register-ranges` completes the originating model
under the unchanged normal budget: 193/193 channels high, all 193 initial
channels high, zero missing, skipped, excluded, or deviating channels. Runtime
is 1.690 seconds and build plus runtime is 3.396 seconds. This closes the
originating counterexample. An independent two-state pendulum calculation uses
`phi_dot = w` and
`w_dot = (-10000*g(20 + 10*sin(phi))*cos(phi) - 0.1*w)/100000.001`, with the
source WGS84 gravity function. SciPy DOP853 at `rtol=1e-12`, `atol=1e-14`
agrees at all 501 samples: maximum angle, speed, and acceleration errors are
below 5.88e-7; gravity error is below 9.81e-12; all eight checked transverse
torque channels are exactly zero. The fixed 20-model canary in
`target/msl/multibody-gravity-complete-canary` retains nine compared models,
all strict-high, with zero missing, skipped, excluded, or deviating comparisons.
Every member's compilation and simulation status is unchanged from the earlier
state-alias canary. The additional repeated-prefix receiver regression passes,
bringing the receiver identity suite to four tests. The full-cohort milestone
remains pending. The affected packages pass all-target, all-feature Clippy.

## Moving drive angular-acceleration counterexample (closed)

The combined 42-example validation in
`target/msl/multibody-gravity-complete-examples` completes ten models. All ten
have reference comparisons, with zero missing, skipped, or excluded models.
Nine are strict-high. `MovingActuatedDrive` has one deviating channel among
624: `bodyCylinder.body.z_a[3]`; all 624 initial channels are high. The other
32 examples still fail before producing a complete trace. This counterexample
paused unrelated capability work and the milestone commit until closure.

The model's angular velocity satisfies
`w_a[3] = -r1.w*sin(revolute.phi)`, and its source declares `z_a = der(w_a)`.
Rumoca's acceleration instead satisfies
`z_a[3] = r1.a*sin(revolute.phi) - r1.w*cos(revolute.phi)*revolute.w`
to 8.89e-16 over the complete candidate trace. The first term has the wrong
sign under the product rule. Before structural reduction, DAE owner 401 retains
`z_a = der(w_a)`. After reduction its product-rule expansion puts
`(r1.a*r1.e)*revolute.R_rel.T` where the source requires
`revolute.R_rel.T*(r1.a*r1.e)`. Structural differentiation treated multiplication
as commutative in the second term of the product rule. Both forms typecheck for
a square matrix and vector, so shape checks alone cannot establish correctness.

Three polynomial regressions reproduce wrong derivatives for matrix–vector,
vector–matrix, and matrix–matrix products. The repair preserves both
operand positions in the first- and second-order product rules and tests the
value plus both derivatives over every sample. These fixtures construct dynamic
matrices from fixed parameter matrices and time coefficients, within the
existing differentiability rules; no unsupported operation is admitted to make
the test pass. All 419 core integration tests pass, including these three
regressions. This follows SPEC_0007's structural transformation contract and
MLS §3.7.2 (`der`) and §10.6.4 (ordered vector and matrix multiplication).

The originating run in `target/msl/multibody-moving-drive-product-order`
compares all 624 channels as strict-high, including all 624 initial channels,
with zero missing, skipped, excluded, or deviating comparisons. Runtime is
3.220 seconds under the unchanged normal budget. Its corrected acceleration
satisfies `z_a[3] = -r1.a*sin(phi) - r1.w*cos(phi)*revolute.w` to 8.89e-16
over all 501 candidate samples. This closes the originating counterexample;
the regenerated structural DAE retains owner 401 and residual 4450, while
product node 4446 now has the required matrix on the left. All 141 structural
tests and the affected packages' all-target, all-feature Clippy checks pass.
The fixed canary in `target/msl/multibody-product-order-canary` retains nine
compared models, all strict-high, with zero missing, skipped, excluded, or
deviating comparisons. All 20 members' stage and simulation statuses are
unchanged from `multibody-gravity-complete-canary`. The full example-set
regression run in `target/msl/multibody-product-order-examples` compares all
ten completed models as strict-high: 3759 channels, including initialization,
with zero missing, skipped, excluded, or deviating comparisons. All 42 stage
and simulation statuses are unchanged from the preceding example run; the
angular-acceleration channel changes from deviating to high. The other 32
examples remain failures. The named-commit full 566-model milestone is next;
these focused runs do not establish a cohort claim or baseline promotion.

The full 566-model gate in `target/msl/multibody-state-tensor-full` passed at
`4716a8291f26302853aef2010b97b5e8d84e4fa2` (tree
`5161f078ed87db562a551d149c351b83cc660da9`). It compares 138 models, all in
the high trajectory band, with 12126 channels and zero deviating channels or
missing traces. Sixteen completed models retain reviewed comparator exclusions.
Every previously high model remains high. Eight additional models enter that
band: MultiBody `DoublePendulum`, `ForceAndTorque`, `Pendulum`,
`SpringMassSystem`, `UserDefinedGravityField`, `MovingActuatedDrive`, plus
Rotational `FirstGrounded` and Translational `Sensors`.

This result needs two explicit qualifications before further breadth work:

- `DCPM_Drive`, previously completed under a reviewed comparator exclusion,
  now fails Solve lowering: continuous algebraic row 133 cannot substitute
  `der(dcpm1.airGapDC.flange.phi)` because its matched state residual is not a
  subtraction. Its disappearance from the completed set explains the exclusion
  count changing from 17 to 16; the policy file is unchanged. This is a new
  execution regression to repair, not improved comparator coverage.
- `Clocked.Examples.Elementary.RealSignals.AssignClockToTriggerHold` retains
  an existing near initial channel: `triggeredSampler.y` is 0 while OMC gives
  0.1 after the first clock tick at time zero. The error persists until the
  next tick at 0.02 seconds. Its trajectory score still falls in the high band,
  but that aggregate must not hide a potential clock/event semantic defect.
  Rumoca already samples the sine and toggles/holds the Boolean at time zero;
  the triggered non-clocked sampler fails to respond. The following repair
  closes this counterexample before resuming the MultiBody compile frontier.

The full run takes 272.260 seconds. Source files were unchanged during the
sweep; artifact digests and exact counts are recorded in
`.git/multibody-campaign/state-tensor-full-receipt.json`. No baseline is promoted.

## Complete event iteration after the first clock tick

A minimal held-Boolean-clock fixture reproduces the missing sample at time
zero. The same fixture with a first tick shifted to 0.01 seconds passes. Both
include an independent initialization counter that must remain one, and their
analytic sample oracle preserves exact event timestamps rather than moving a
nearby sample across a tick.

DAE retains the `hold`, `change`, and Boolean `when` semantics in its typed
temporal owners. Solve scalar row 2 computes `trigger = held != pre(held)`
with a `Fixed` pre-read policy; guarded assignment owner 0 correctly reads
the current input on the trigger's rising edge and otherwise holds the
sampler output. The first wrong operation is the runtime's
`PostInitialClockTick` filter: it excludes the `Fixed` trigger equation even
after the held clock value changes. A pre-read policy does not identify an
initialization-only equation.

The runtime now uses the complete SOLVE-C22 event pass after initialization
clears for the first clock tick, as required by MLS §§16.3, 16.5.1 and 8.3.5.
Initialization still defers clock-owned equations, and the existing event
history commit and once-per-tick clock execution remain authoritative. The
obsolete filter is removed. Both focused tests pass, along with all 421 core
integration tests and all 417 solver unit tests.

The originating comparison in `target/msl/multibody-clock-trigger-repair`
is high on all eight channels, including all eight initial channels with zero
initial error. There are no missing, skipped, excluded, or deviating
comparisons. This closes the clock-trigger counterexample; its fixed canary
in `target/msl/multibody-clock-trigger-canary` retains all nine compared models
as strict-high, with zero missing, skipped, excluded, or deviating comparisons.
All 20 stage and simulation statuses are unchanged. Formatting and the affected
packages' all-target, all-feature Clippy checks pass. The `DCPM_Drive`
execution-regression repair remains pending.

## Signed derivative residuals

The `DCPM_Drive` failure comes from its matched state equation 491. Prepared
DAE residual 4335 is `+(der(dcpm1.airGapDC.flange.phi) -
dcpm1.inertiaRotor.w)`: a unary plus around subtractive residual 4334.
The state variable is 278 and its right-hand side is state 235. This is a
valid equation under MLS Appendix B.1; neither the DAE nor structural
reconstruction has lost its derivative definition. Solve's `derivative_rhs`
requires a subtraction at the root and incorrectly rejects the unary wrapper
when algebraic row 133 needs that derivative.

Solve lowering now reads through unary plus and negation around a complete
zero residual before applying its existing derivative-isolation checks.
This preserves the equation's solution and keeps executable derivative
analysis in the SPEC_0007 Solve owner. Three checked-DAE regressions cover
positive, negative, and nested wrappers. All fail with the original refusal
before the repair. Afterward they verify both the derivative and its algebraic
use at four assignments, including one outside the solution manifold.
All 113 Solve tests and all 421 core integration tests pass, as do formatting
and the affected packages' all-target, all-feature Clippy checks.

The originating run in `target/msl/multibody-derivative-wrapper-repair`
restores `DCPM_Drive` completion in 11.839 seconds, including 6.597 seconds
of simulation preparation and 5.224 seconds of integration. The unchanged
reviewed comparator exclusion still applies. A separate diagnostic using the
same production comparator and fresh OMC reference checks all 590 common
channels: 586 are high and all 590 initial values agree exactly. The four
non-high channels remain precisely the previously reviewed
`idealDcDc.feedback.y` and `idealDcDc.powerController.u` aliases in the two
inverters. Against this reference, the prior candidate has four near channels;
the restored candidate has two near and two deviating channels, all inside
that existing integration-residual boundary. No other channel becomes
non-high. The exclusion is unchanged and this model does not count as
strict-high.

The clock model in that same originating run remains high on all eight
channels with exact initial values. The fixed canary in
`target/msl/multibody-derivative-wrapper-canary` retains nine compared models,
all strict-high, with zero missing, skipped, excluded, or deviating
comparisons. All 175 initial channels are high and all 20 stage and simulation
statuses are unchanged from `multibody-clock-trigger-canary`. The execution
regression is closed; the complete 566-model milestone follows at the repair
commit. No baseline is promoted from these focused results.

### Reference generation and cache evidence repair

The complete 566-model run at `7069c44e` in
`target/msl/multibody-regression-restored-full` failed its quality gate during
reference-cache persistence. It supplies no accepted cohort parity number.
Ten OMC attempts had been labeled successful despite an empty `resultFile`
and no trace. Their runtime messages reported execution failures, but the
reference producer discarded the wording `Simulation execution failed`.
This was a producer error; the cache correctly rejected the incomplete
success records. No candidate comparison was missing, and no compiler
simulation status regressed from the preceding full run; `DCPM_Drive`
returned to `sim_ok` under its unchanged reviewed exclusion.

The first reproduction, `Modelica.Electrical.Digital.Examples.RAM`, also
identified an earlier reference setup error. Explicitly loading the generic
MSL `ModelicaServices` made its resource loader pass `modelica://` URIs to
`fullPathName`, so initialization could not read the memory data file.
The identical pinned model succeeds with OMC's tool-specific services.
Reference generation now lets OMC supply those services, requires a result
file before reporting success, and retains runtime failure diagnostics.
An independent warning-level assertion model confirms that warnings which
permit successful simulation remain nonfatal. A cache-policy fingerprint
invalidates references produced under the previous setup.

Regenerating JSON now replaces the destination after writing the complete
payload, preserving historical traces which share a cache hard link.
The empty-result, runtime-assertion, warning, and hard-link regressions each
exercise their observed failure boundary. All 168 tooling unit tests pass,
as do formatting and all-target, all-feature tooling Clippy.

The originating harness run in `target/msl/multibody-omc-reference-origin`
produces successful OMC references for RAM, `readRealParameterModel`, and
the MultiBody `Pendulum`. Only Pendulum currently completes in Rumoca;
its 144 compared channels, including initial values, are high, with no
missing or skipped comparison. This does not claim Rumoca support for the
two resource examples. The final fixed canary in
`target/msl/multibody-omc-reference-final-canary` retains all 20 stage and
simulation statuses, nine strict-high comparisons, 175 high initial
channels, and zero missing, skipped, or deviating comparisons. All 19
recorded historical reference-file hashes remain unchanged. The complete
566-model milestone follows at the repair commit; no baseline is promoted.

### Complete milestone after reference repair

The complete 566-model gate at commit
`13f0f133fac8edc80b7f55b53d820259bad74347` passed in 393.62 seconds. Its
artifacts are in `target/msl/multibody-reference-repaired-full`, with the
source audit and artifact hashes recorded in
`.git/multibody-campaign/reference-repaired-full-receipt.json`.
All 138 compared models are strict-high; 17 other completed models retain
their reviewed exclusions, and zero candidate comparisons are missing.
All 12,126 compared initial channels are high. Every previously strict-high
model, including all 129 baseline-certified models, remains strict-high.

The run compiles 286 models, balances 274, attempts 267 simulations, and
completes 155. Six attempts time out and 106 fail in the solver path.
These stage classifications are unchanged from the preceding full run;
the clock and DCPM repairs remain effective. OMC supplies successful
references for all 155 completed candidates. The other 411 models remain
visible in the full roster without an OMC simulation attempt in this run.
The performance gate also passes with its complete 145-model cohort.
The tracked source tree matches the named commit; the recorded dirty flag
comes from the user-owned, untracked `comm_fastdyn.md` file. No baseline
was promoted. These results do not establish complete MultiBody coverage.

### Record fields in retained state constraints

`Modelica.Mechanics.MultiBody.Examples.Constraints.PrismaticConstraint`
previously stopped in checked index-reduction reconstruction with a missing
state-only substitution for expression 5431. This identity belongs to the
reconstructed DAE at that reduction round, not the original DAE. It projects
the orientation matrix from `Frames.absoluteRotation` into the retained
position constraint from `Joints.Prismatic`:

```modelica
frame_b.r_0 = frame_a.r_0 + Frames.resolve1(frame_a.R, e*s);
```

The differentiability analysis already understood this field projection.
The first missing operation was exact value materialization in the structural
phase, whose matching holonomic preflight also omitted record fields.
Both now use the existing checked projection resolver and preserve nested
function argument bindings. Unsupported projections still fail at the same
typed boundary. The semantic basis is MLS 3.6 §§12.4, 12.6, and Appendix B;
the owner follows SPEC_0007 and STRUCT-T03/T04 in SPEC_0040.

Three checked-DAE regressions reproduce the old rejection. They exercise
both matrix fields of a record returned through nested calls with permuted
arguments. Independent numeric expectations cover four parameter pairs,
including negative, zero, and fractional values. The retained equation still
relates the remaining matrix state, passes the state-only invariant, and
has a computable structural system. All 144 structural tests, 421 core
integration tests, formatting, and all-target, all-feature structural Clippy
pass.

The production diagnostic inspection of the original Prismatic DAE now
completes the exact formerly rejected state-296/RHS-5573 substitution,
reducing the unmatched residue from 38 to 32. Subsequent exploratory
reductions reach residue 20 before exhausting admissible candidates.
The incomplete transformed system is not published; the model still reports
its original structural singularity, 2287 matches for 2313 equations and
unknowns. This repairs a construction boundary and adds no supported model.
Before/after identities and artifact hashes are retained in
`.git/multibody-campaign/prismatic-manifold-triage.json`.

The origin run in `target/msl/multibody-prismatic-field-origin` retains high
parity for Pendulum and MovingActuatedDrive: two comparisons, all 768 channels
and initial values high, zero missing or skipped comparisons. Prismatic's
OMC reference succeeds while Rumoca still refuses it. The fixed canary in
`target/msl/multibody-prismatic-field-canary` has no changes to any of its
20 phase or simulation statuses relative to `multibody-omc-reference-final-canary`.
All nine compared models and 175 initial channels remain high, with zero
missing, skipped, or deviating comparisons. The recorded Tier 1 delta is
`.git/multibody-campaign/prismatic-field-canary-delta.json`.


### Exact scalar incidence in initialization

The next shared structural blocker is the initial-manifold guard: the
GyroscopicEffects example reaches a holonomic constraint incident on the
source-fixed `revolute.phi`. Its initial value is present in the DAE. The
earlier preflight rejects the candidate because the existing post-initialization
manifold projection may move that state; the later transferred-pin check is
not the originating failure. The guard remains intact pending a joint
initialization owner. MLS 3.6 §8.6 and Appendix B, SPEC_0007, and the
SPEC_0036 draft / SPEC_0043 §4 describe the required ownership.

A prerequisite repair makes initialization incidence use the exact scalar
projection already used by structural analysis. Matrix coordinates, structured
domain points, substituted parameter bindings, and derivative definitions now
retain the same scalar identity as their emitted residual programs. A transferred
pin determines only its actual scalar. The prior whole-variable walk rejected
three analytic matrix-initialization fixtures before simulation.

Five end-to-end regressions cover direct and structured matrix initial equations,
derivative initial equations, permuted array parameter bindings, and inconsistent
fixed matrix initial data. Both BDF and RK paths are checked against independent
exponential trajectories or the required initialization refusal. All 426 core
integration tests and 113 Solve tests pass, along with focused all-target,
all-feature Clippy. The fixed canary in
`target/msl/multibody-initial-tensor-canary` retains all 20 phase and simulation
outcomes relative to `multibody-prismatic-field-canary`: nine comparisons and
all 175 initial channels high, zero missing, skipped, or deviating comparisons.
The artifact hashes and delta are in
`.git/multibody-campaign/initial-tensor-canary-delta.json`. This is prerequisite
coverage; it does not claim that the initial-manifold blocker is resolved.

### Joint initialization and retained constraints

GyroscopicEffects first reaches the old guard at continuous owner 363,
residual 6775, incident on state coordinates 474 and 502. Coordinate 474 is
`revolute.phi`; its declaration carries `fixed = true` and a start expression.
The source span is 2576..2588 in source 13533509827142484477. This identifies
the admission problem before reconstruction, independently of the later
initial-value preservation checker, which remains intact.

The replacement lowering includes every retained position and velocity
constraint in the initialization residual, using the same scalar incidence
as the emitted programs. Source starts proved independent of initialization
unknowns supply given coordinates. Dependent starts and transferred pins
remain equations alongside the unknown parameters they read. An opaque
initialization owner derives row targets and unknown inventories from its
checked plan and rejects overlapping projection, update, or given ownership.
Wire schema 62 replays that constructor; the old independent inventories are
removed. Root assembly checks the retained manifold count and storage bounds.

After initialization settles, manifold certification receives read-only state
storage, including at the initial event boundary. Continuous-time correction
retains its existing state projection. The early blanket admission guard is
removed only with this replacement; inconsistent initial conditions remain
errors.

Five end-to-end cases check uniform circular motion against independent
analytic trajectories on both BDF and RK, partially fixed positions and
velocities, and a fixed start that depends on a parameter solved during
initialization. They also require rejection of inconsistent circle, time,
and invariant constraints. All 248 Solve IR, 113 Solve lowering, 144 structural,
and 417 runtime tests pass with the final given-coordinate handling, as do all
430 core integration tests. Focused all-target, all-feature Clippy passes
across the changed compiler/runtime crates.

Lowering every fixed start to a numerical residual initially regressed ordinary
FMI C export: `Real x(start=2,fixed=true); der(x)=-x;` was rejected by the
existing parameter-only C initialization profile. Given-coordinate ownership
restores this export without widening that profile. The packaged tensor decay
conformance test now uses fixed starts and checks that legal FMI start overrides
survive initialization. FMI 2.0.5 and 3.0.2 schema/VDM checks, source compilation,
direct lifecycle checks, and FMPy ME/CS analytic traces all pass. The receipt is
`.git/multibody-campaign/joint-given-fmi-conformance-4.log`; earlier attempts
without the configured tools are not counted as validation. The combined fixed-canary validation is recorded below; this entry does not
claim a MultiBody coverage gain.

### GyroscopicEffects: forwarding-function equalities

The one-model run `target/msl/multibody-joint-initial-gyroscopic` gets past
the initial-value admission guard, but fails structural analysis before
numerical initialization. The frontend is balanced at 2276 scalar equations
and unknowns. The structural reducer starts with 37 unmatched equations and
unknowns, reaches 15 of each, then transactionally returns its original
singularity error. The remaining equation owners are generated frame
orientation and angular-velocity connections, rather than integration errors.

OpenModelica's successful reference in
`target/msl/multibody-regression-restored-full` uses the same MSL 4.1.0 source.
Its 16 scalar states include two three-coordinate dynamic state sets selected
from the four quaternion coordinates of `bodyCylinder1.body` and
`bodyCylinder3.body`. Rumoca activates quaternion equations for those same
two bodies; incorrect orientation-root activation does not explain this
failure. The reference's revolute and Rotor1D angular velocities both start
at 10 and agree to about 4e-9 at the five-second endpoint.

At the final unreduced frontier, checked DAE function 30 is
`Frames.angularVelocity2`. Its sole output assignment reads parameter ordinal
1, the whole three-element `R.w` input after record-argument lowering. It has
no other statements or external implementation. Five body/rotor velocity
equations call it, but equality closure reports no state anchor for their
frame angular-velocity connectors. The missing fact belongs to structural
equality analysis under SPEC_0007 / STRUCT-T03–04 and MLS FUNC-002–005, not
to simulation or a model-specific exception.

The focused regression first fails because the checked forwarding function
does not anchor its second vector argument to the state. Nonforwarding and
asserting functions remain rejected by this proof. A separate four-vector
Modelica fixture reproduces the structural failure at 9 of 12 matched rows:
`x = forward(a); a = b; y = forward(b); der(x) + der(y) = -2*x`.
Its solution is independently known as `x = y = a = b = {1,2,3}*exp(-time)`.
OpenModelica with the same `Inline=true` annotation used by MSL retains three
scalar states and matches all twelve channels within 6e-9. Without that
annotation, this OpenModelica build fails its array-equation code generation;
that failed attempt is not counted as reference validation.

Equality closure now consumes the existing checked single-assignment
forwarding proof and reads the returned caller argument with its original
sign and shape. Original function and equation ownership is retained. All
146 structural unit tests pass. The ordinary originating-model attempt in
`target/msl/multibody-forwarding-gyroscopic` exceeds the unchanged ten-second
Solve phase budget and remains a failure. A separate diagnostic replay shows
the final unmatched set shrink from 15 equations and unknowns to six of each:
all nine unmatched angular-velocity connection rows disappear. The diagnostic
replay is root-cause evidence, not a successful timed gate.

Once the forwarding fixture matches, state demotion leaves a repeated
derivative in `der(x) + der(y)`. Scalar Solve lowering previously accepted only
an isolated derivative or one scaled derivative product, so the end-to-end
regression then failed at that later boundary. It now derives the coefficient
and offset symbolically for a proved scalar affine residual. The checked plan
retains source parameter loads and emits ordinary arithmetic. It does not
recover coefficients by subtracting two residual evaluations, which can lose
them beside a large offset. Repeated-derivative tests use an offset of 1e20,
change the runtime parameter, and require rejection of zero coefficients and
nonlinear derivative products. All 116 Solve tests pass. The original
four-vector fixture now agrees with the analytic trajectory on both BDF and
RK, including all twelve initial values.

### GyroscopicEffects: indexed matrix functions

With forwarding equalities present, the first refusal at both remaining
orientation connection families is expression 1077, `PromotedCat1`, in checked
function 33, `Frames.from_Q`. This function constructs its matrix from indexed
quaternion products. Its single result assignment owns expression 1078.
The diagnostic reports are
`.git/multibody-campaign/gyroscopic-orientation-proof-refusals.log` and
`gyroscopic-forwarding-step-26-residue-12.dae.json` in the same directory.

Structural differentiation now preserves the checked maps of linear tensor
builtins and invariant index projections. Their original shapes and zero
operands remain explicit; changing runtime indices acquire no such proof.
Singleton equality substitution constructs the declared aggregate shape before
a later projection, including when its anchor is scalar. This is the
SPEC_0032 compact tensor rule applied inside the SPEC_0007 structural owner.

The matrix regression uses `y = [x[2]^2, 7; x[1]^2, 11]`, `der(x) = -x`, and
`a = der(y)`, with the matrix expression owned by a Modelica function. A replay
through the pre-map structural implementation reproduces 6 of 10 matched
equations. Both solvers now match all ten analytic channels, including the
constant column and its zero derivative. OpenModelica agrees within 4.2e-8.
The original scalar-anchor projection regression also verifies successful
reconstruction with the declared array shape. All 433 core tests pass with the final
function-size refactor, as do all 116 Solve and 146 structural tests.

An earlier reduced probe also used `vector()` inside its function; that
separately reaches the existing unsupported typed pure-call builtin boundary.
The source is retained as `.git/multibody-campaign/indexed-vector-unsupported.mo`.
The final matrix fixture follows the actual `from_Q` matrix-constructor form;
support for that separate pure-call `vector()` boundary is not claimed.
The new diagnostic replay in
`.git/multibody-campaign/gyroscopic-tensor-map-structural-report-2.log` now clears
the orientation connection frontier. It still stops with six unmatched
equations and unknowns, now at the two three-dimensional
`frame_b.r_0 = frame_a.r_0 + Frames.resolve1(frame_a.R, r)` families. The final
holonomic candidate raises the unmatched count and is not accepted. This is
the next proof boundary; the model still has no Rumoca trace. Focused
all-target/all-feature structural and Solve Clippy passes after extracting the
proof-walk operations into bounded functions. The fixed 20-model canary in
`target/msl/multibody-gyroscopic-focused-canary` preserves all 20 phase,
simulation, and agreement-band outcomes relative to
`multibody-initial-tensor-canary`: nine comparisons are high, all 175 initial
channels are high, and no comparisons are missing, skipped, or deviating.
The artifact-bound delta is
`.git/multibody-campaign/gyroscopic-focused-canary-delta.json`. Core Clippy also
passes. None of these focused results establishes a new MultiBody or
full-cohort parity count.

### GyroscopicEffects: supplied kinematic derivatives (in progress)

The final tensor-map replay is retained in
`.git/multibody-campaign/gyroscopic-tensor-step-32-residue-12.dae.json`.
The first differentiation refusal in position rows 1400 and 1591 is a
derivative coordinate: expressions 8454 and 8806 read `der(Q)` of
`bodyCylinder1.body` and `bodyCylinder3.body`. Their quaternion derivatives
are constrained implicitly by the angular-velocity equations. This rules out
the earlier tentative trigonometric-builtin hypothesis at this frontier.
The focused walk is `gyroscopic-position-proof-refusals.log` in that directory.

The MSL source for `Frames.resolve1` supplies
`derivative(noDerivative=R) = Internal.resolve1_der` and requests inlining
after index reduction. Its supplied derivative uses
`resolve1(R, v2_der + cross(R.w, v2))`, retaining the angular-velocity
relationship instead of requiring a second quaternion derivative.
The [MLS §12.7.1 contract](https://specification.modelica.org/maint/3.6/functions.html#using-the-derivative-annotation)
distinguishes omitting a tangent under documented argument assumptions from
proving an argument constant. The proposed construction contract is in
SPEC_0036 / SPEC_0043 §10, under Function Derivative Ownership.

The five-variable regression declares `2*q*der(q)=v`, `der(v)=-1`,
`y=position(q,v)`, `velocity=der(y)`, and `acceleration=der(velocity)`.
The position function returns `q*q`; its supplied derivative returns `v`
under the stated kinematic relationship. With `q(0)=v(0)=1`, the exact
position is `1+t-t*t/2`. Rumoca fails at four of five matched equations in
`annotated-constraint-red.log`. OpenModelica succeeds and matches all five
channels within 8e-9, including initial values; the CSV and analytic receipt
are under `annotated-constraint-omc/`. This records the original failure;
the implemented DAE and structural derivative path now passes the regression.

Two earlier defects are reproduced and repaired: extraction discarded the
actual parsed assignment form of restricted derivative annotations, and
Resolve left their function and input references without declaration IDs.
The red receipts are `derivative-metadata-red.log` and
`derivative-resolution-red.log`. Resolve's 152 tests pass after attaching
identities in lexical scope. Flat metadata now retains exact callable
references and ordered input roles, collects derivative dependencies, and
expands each excluded record's input role across exactly that record's
fields. The record-input integration test passes in
`derivative-flat-records-final.log`. All 630 flattening unit tests and focused
all-target/all-feature core, Resolve, and Flatten Clippy pass with the final
metadata parser and role mapping. The preceding canary predates this work.

DAE schema 35 adds a checked differential owner. It requires complete,
pure functions with matching original input names, types, and shapes, followed
by precisely the required tangents. Record tangents preserve compact extents
and recursively omit fields without Real values. The read-only view derives
tangent input ordinals and the filtered result mapping; JSON and binary replay
repeat the constructor checks. Structural reconstruction reattaches the links
using its issued function identities. The preceding schema-34 implementation
passed all 180 DAE and 146 structural unit tests in
`derivative-dae-structural-1.log`. These include five derivative-owner
tests for priority, omitted tangents, record types, malformed wire, and purity;
the vector-state regression also verifies preservation during state demotion.
All-target/all-feature DAE and structural Clippy passes in
`derivative-dae-structural-clippy-1.log`; formatting and `git diff --check` pass.

Higher-order links now require a constructor-issued predecessor, retain source
priority, and append tangents only for the preceding derivative group. Common
`zeroDerivative` restrictions remain consistent. The six focused owner tests
pass in `derivative-chain-owner-1.log`, including rejection of treating the
second-order signature as an ordinary first derivative and rejection of a
cyclic predecessor in wire data. Earlier schema-34 validation above predates
this extension.

Shape discovery follows exact annotated callable identities separately from
executable call dependencies. This allows a supplied derivative to call its
primal without creating a false recursive component. Input value relevance
also propagates through annotation dependencies. Structural first-derivative
selection checks `zeroDerivative` applicability, retains omitted original
arguments, and emits the selected checked function call before primal-body
differentiation. Higher-order structural selection/emission is still pending;
the checked chain alone is not an execution claim.

The restored first derivative closes the reproducer's four-of-five structural
failure. Its next rejection, recorded in `derivative-integrated-core-3.log`,
was Solve's requirement that the coefficient in `2*q*der(q)=v` be compile-time
numeric. Under MLS B.1 and SPEC_0007's Solve lowering contract, scalar affinity
does not require a constant coefficient. Scaled and repeated affine forms now
retain state-dependent coefficients and evaluate the quotient on its finite,
nonzero domain. A non-finite coefficient cannot yield a plausible zero result:
the generated arithmetic reaches the runtime's existing non-finite rejection.
Declared zero/non-finite constant coefficients remain rejected at compilation.

`derivative-integrated-core-4.log` passes three tests: all five analytic channels
with BDF and RK, refusal to apply `zeroDerivative` to the varying argument, and
failure at the singular initial point `q=0,v=1`. Broad affected suites and a
fresh GyroscopicEffects run were pending at that checkpoint.

`derivative-focused-core-1.log` now passes all 437 core tests. The fourth new
regression verifies array specialization, dimensions used only by derivative
functions, and predecessor identity for higher-order links; it does not claim
higher-order structural execution. Affected library results are 181 DAE IR and
271 DAE lowering tests in `derivative-focused-libraries-1.log`, then 117 Solve
and 146 structural tests in `derivative-focused-libraries-2.log`. The first
library run exposed one outdated assertion banning every scalar `Select`;
the repaired test instead proves the translation-time guard is absent from
runtime dependencies and the coefficient remains live. Clippy found complexity
violations in derivative replay, discovery, and proof walks; the affected
operations are now separate helpers. The final four-crate Clippy run passes in
`derivative-focused-clippy-5.log`. All 715 affected library tests pass after
these refactors in `derivative-focused-libraries-3.log`, including the additional
zero/non-finite scaled-coefficient cases. Core integration Clippy also passes
in `derivative-core-clippy-1.log`.

The normal fresh-source run in `target/msl/multibody-annotated-gyroscopic`
compiles GyroscopicEffects to a balanced 2,276-equation DAE in 3.32 seconds,
then times out in Solve under the normal 10-second phase budget. It produces
no trace, and the gate correctly reports parity unmeasured. Its log is
`derivative-origin-gyroscopic-1.log`; this is a failure, not a coverage gain.
The fixed 20-model run in `target/msl/multibody-annotated-derivative-canary`
preserves every phase, simulation, and agreement-band outcome relative to
`multibody-gyroscopic-focused-canary`. All nine comparisons are high and all
175 initial channels are high, with zero missing, skipped, excluded, or
deviating comparisons. The artifact-bound receipt is
`.git/multibody-campaign/derivative-canary-delta.json`, with working-tree digest
`d4a20c205dc516ba64a8c590aa2d6f40729a2d9dfca9c5cbfa5893e788f9f4d6`.
No new MultiBody or full-cohort coverage is claimed. `verify quick`,
`verify full`, and the next full 566-model milestone remain pending.

A separate compile-only diagnostic exports fresh schema-35 source artifacts to
`.git/multibody-campaign/derivative-origin-diagnostic/`. The DAE owns checked
links for `Frames.resolve1`, `resolve2`, and `resolveRelative`, including the
correct omitted tangents after record decomposition. The independently rebuilt
structural inspector completes under its diagnostic 60-second cap; its report
is `derivative-origin-structural-report-1.log`. The last exploratory reduction
round has three unmatched equations, `f_x[2138..2140] (Real T)`, and three
unmatched unknowns: `der(fixedRotation1.frame_b.R.w[2])`,
`der(revolute.R_rel.w[3])`, and `bodyCylinder2.body.a_0[3]`. It discovers no
further direct or holonomic candidates. The position-equation frontier has
advanced, but the failed reduction returns its original singular-system error;
this diagnostic is neither a successful prepared system nor a simulation.
The recorded normal-budget timeout above remains unchanged. The next focused
step is to retain the final rejected candidate and inspect these three rows.

### GyroscopicEffects: bilinear manifold values

The final exploratory candidate is retained as
`.git/multibody-campaign/derivative-final-candidate.dae.json`, with its exact
manifold and matching failure in the adjacent metadata file. The scratch pass
copies the current structural implementation and adds diagnostics only; none
of that instrumentation enters the production compiler. In the saved
candidate, row 2138 has residual 8241, the connection between
`revolute.frame_b.R.T` and `fixedTranslation.frame_a.R.T`.

The first refusal is expression 898, `outerProduct(e,e)` in
`Frames.planarRotation`. Differentiation of the root succeeds, but retained
value preflight rejects that builtin before it can reconstruct the record's
matrix field. The exact chain is in
`derivative-final-proof-refusals-3.log`. The existing value emitter already
supports the builtin; the preflight's separate list omits it. This identifies
the structural preflight as the first divergent layer and does not establish
a state-selection or higher-order-derivative defect at this frontier.

MLS §10.3.5, catalog rows ARR-037/ARR-042, define the cross and outer-product
operations. SPEC_0007's structural contract requires retained manifold values
to preserve those checked operations; SPEC_0029 requires one semantic helper
owner. `builtin_profiles` now supplies the same value profile to preflight and
reconstruction. DAE arity and shape checks, state-only operand proofs, and
runtime-index refusals remain requirements of the path.

The reduced `BilinearConstraint` model returns a record containing
`outerProduct(x,v) + identity(3)` and `cross(x,v)`, constrains matrix/vector
states to those fields, and observes their derivatives. Before the fix,
`bilinear-constraint-red.log` fails at 27 of 39 matched equations. OpenModelica
compiles and simulates that exact source, including its state-selection
attributes, without warnings. All 39 channels match the independent
exponential solution within 6.6e-8, including initial values; the source, CSV,
log, and comparison receipt are in `bilinear-constraint-omc/`.

Restoring the value profile exposes a second reconstruction defect. The same
model passes with Real literals for the function's vector argument but fails
with Integer literals, although the declared input is `Real[3]` in both cases.
Both retained-manifold and direct-state substitution lose the declared Real
type when replacing a formal parameter with the actual Integer expression.
`bilinear-constraint-types-3.log` records those two failures and the successful
Real-literal control. This rules out `identity(3)` and record shape as the
source of the new type mismatch.

MLS §10.6.13 (ARR-009) requires Integer-to-Real conversion in Real contexts,
including arrays. Structural substitution now preserves that source type in
both value-rebuilding paths. The checked mixed-numeric identity `1.0 * value`
derives the Real result with the same compact extents; it introduces no scalar
expansion or new IR vocabulary and does not relax builtin constructor checks.
The existing initialization lowering uses the corresponding scalar identity
`value + 0.0` for the same language conversion.

All three reduced regressions now pass with BDF and RK, checking all 39
channels against the analytic solution, including initial conditions
(`bilinear-constraint-green-2.log`). The broader run passes all 440 core,
146 structural, and 117 Solve tests (`bilinear-focused-core-1.log` and
`bilinear-focused-libraries-1.log`). Structural Clippy passes with all targets
and features (`bilinear-focused-clippy-1.log`); core Clippy also passes
(`bilinear-core-clippy-1.log`).

The rebuilt production structural inspector now sorts the saved candidate
after two direct substitutions (`bilinear-final-structural-report-1.log`).
That snapshot omits the accumulated external manifold list, so this is a
focused before/after proof, not a claim about complete initialization.
The normal originating-model run supplies the stronger check:
`target/msl/multibody-bilinear-gyroscopic` completes Solve construction in
8.715 seconds and reports `ic_ok`, then hits the unchanged 12-second simulation
budget. Its gate fails with parity unmeasured: no Rumoca trace was produced.
This advances the failure from Solve to simulation; it is not a model pass.

The fixed 20-model canary, `target/msl/multibody-bilinear-canary`, preserves all
20 phase, simulation, and agreement-band outcomes against
`multibody-annotated-derivative-canary`. All nine compared models remain high,
with 175 high initial-condition channels and zero missing, skipped, excluded,
non-identifiable, or deviating comparisons. The durable receipt is
`.git/multibody-campaign/bilinear-canary-delta.json`, binding the artifacts to
worktree digest `1029dc260202fd272d4b3a4586bef573db23b0dc9de57769c9d55cbdb5465b32`.
These are focused results; no new full-cohort or MultiBody coverage number is
claimed. The next investigation follows this same model into its first
simulation steps.

The independent session probe (`bilinear-runtime-probe-1.log`) uses the
original source DAE, the model's 1e-8 relative and absolute tolerances, and a
12-second solver deadline. It records observations at 0, 0.01, and 0.02 seconds
without changing the production compiler or the failed gate result. Integration
advances in 38 and then 16 additional steps, with no root hits. All 967 exact
name matches against the successful OpenModelica CSV in
`multibody-regression-restored-full` agree within 2.5e-13 at initialization,
7.3e-8 at 0.01 seconds, and 4.4e-7 at 0.02 seconds. The other 1,309 Rumoca
observation names have no exact CSV-name match in this diagnostic; this is
neither full observable coverage nor a comparator agreement-band claim.
Samples and the explicit comparison inventory are retained in
`.git/multibody-campaign/bilinear-runtime-samples/`.

Profiling records 257,047 scalar-row evaluations through the second output,
including 214,061 target-assignment evaluations. The emitted Solve model has
18 scalar states. Its derivative refresh contains 548 exact assignment rows
but represents them through 479 projection stages: 477 singleton blocks and
two coupled blocks of 79 and 72 equations. The algebraic refresh similarly
contains 2,002 singleton projection stages and those two coupled blocks. The
global causal-order certificate is false for both plans. In
`refresh_plan/schedule.rs`, that global condition suppresses every exact
assignment run, even around the coupled blocks. This is the next performance
hypothesis, not yet a verified fix. A correction must prove dependency order
for each local assignment run, retain the coupled solves, and preserve the
existing rejection of uncertified seed order; simply removing the guard would
discard the protection against stale algebraic values.

The six-variable `CoupledRefresh` regression confirms the scheduling defect:
only `a = sin(x) + b` and `b = 0.25*a` are coupled, but all five algebraic
variables enter projection stages. `coupled-refresh-red-1.log` records that
failure while both solvers already satisfy the six-channel analytic solution.
The proposed correction follows SPEC_0007 / SOLVE-C56 and SPEC_0029: construct
local assignment stages from the existing exact-assignment dependency query,
admit a row only after its dependencies in the projection inventory have
settled, and retain the original BLT order where no global ordering proof
exists. A coupled block remains a projection barrier. The existing checked
Solve-owner stage-coverage and dependency checks still validate the result;
runtime scheduling and tolerance rules do not change.


The local refresh schedule now batches assignments whose dependencies have been
settled before each coupled solve. Validation: 417 IR/evaluator tests and 442
core tests passed, as did both focused Clippy checks. The six-channel analytic
fixture agreed with OpenModelica to `2.733e-8`. The fixed 20-model canary
`target/msl/multibody-local-refresh-canary` preserved every phase, simulation,
and comparison outcome against `multibody-bilinear-canary` (nine compared,
all strict-high; zero missing, skipped, excluded, or deviating models; all 175
initial channels high). The worktree digest was
`2670cb6c872308b21c3dcb66d5841287fd75977adf68a55434f189a42ac530f2`.
This is focused regression evidence, not a new cohort coverage claim.

The ordinary GyroscopicEffects run at
`target/msl/multibody-local-refresh-gyroscopic` still timed out in Sim after
successful Solve construction (8.840 seconds), backend construction (7.650
seconds), and initialization. Its comparator compared zero models, so parity
remains unmeasured. A diagnostic replay showed fewer emitted stages but the
same evaluation counts: the runtime correctly declined a schedule whose
structural dependencies could invalidate earlier equations.

Tracing the first such dependency identified Solve projection block 703,
`bodyCylinder1.body.frame_a.R.w[1]`, as apparently invalidating rotation-matrix
rows 1124–1132 in earlier blocks 600–608. The checked call owner and MSL
`Frames/from_Q.mo` both show the matrix depends only on `Q`; `w` is forwarded
to the separate angular-velocity output. `StructuralPattern` previously united
every call input into every output, losing that separation. The reduced
`record_output_dependencies` test reproduced the same false edge before the
fix. Governing contracts are MLS §12.3 / FUNC-005 and SOLVE-C17/C51/C56;
SPEC_0039's dependency derivation describes argument substitution into a
checked function summary.

Solve now derives a compact summary per typed output leaf from each issued
body, substitutes nested summaries, and derives directional summaries from the
same checked tangent body. Aggregates remain compact; within an aggregate the
summary is conservative. Call-site claims are checked against these derived
summaries during model replay (Solve schema 63). Runtime admission guards have
not been relaxed. Validation of this dependency change is recorded below.

The reduced fixture also exposed an OpenModelica code-generation defect when
its function lacked an inline annotation: the generated nonlinear residual
copied four matrix entries to `res+0` and two vector entries to `res+1`,
overwriting matrix residuals and leaving two coordinates unwritten. Simulation
reported success, but two trace channels were invalid. That run is retained
as failed oracle evidence in `record-dependency-omc`. The regression now uses
`Inline=true`, matching MSL `from_Q`, without changing its equations; the exact
nine-channel source in `record-dependency-inline-omc` agrees with its analytic
solution at all 12 output rows to `4.441e-16`. Both oracle artifacts are under
`.git/multibody-campaign/`; this is diagnostic evidence, not an MSL exclusion.


The dependency change passes 255 IR and 167 evaluator tests, all 445 core
tests, and both focused Clippy checks. The ordinary originating-model run,
`target/msl/multibody-record-dependency-gyroscopic`, constructs Solve in 9.345
seconds and its backend in 7.972 seconds, initializes successfully, and still
hits the 12-second Sim limit. No trace comparison is available from that run.
The fixed canary `target/msl/multibody-record-dependency-canary` preserves all
20 outcomes against `multibody-local-refresh-canary`: nine compared, all
strict-high; zero missing, skipped, excluded, or deviating models; all 175
initial channels high. Its recorded worktree digest is
`a6e671e284a11effa5da2f0933d9f1511c7454a6fd45e0d8d3c95359e4127146`.

The full-model diagnostic confirms the original angular-velocity-to-matrix
edge disappeared. Remaining reverse-invalidation flags fell from 21 to 14 for
algebraic refresh and 15 to 10 for derivative refresh. Evaluation counts remain
257047 scalar rows and 214061 target assignments through time 0.02; all 2276
observables at times 0, 0.01, and 0.02 are unchanged from the pre-fix diagnostic.
These are three samples, not a full trace or a new model pass.

The next false edge is at block 704, updating velocity component 2 after
component 1 in block 703. Logical implicit row 1388 maps through
`ScalarPrograms.output_indices` to stored program 424. That program calls owner
141, the three-operation body of `Frames.angularVelocity2`: load the rotation
matrix, load angular velocity, and return angular velocity. The new summary
correctly omits the matrix but still unites the three vector components. The
next construction obligation is to retain compact component mappings through
such typed copies and their directional/nested calls. Incomplete projection
seed coverage remains a separate admission condition. Neither guard can be
removed merely to enable the optimized path. Full 42-example, 566-model, and
quick/full verification remain pending while this focused runtime frontier is
unresolved; the complete MultiBody goal and PR gate are unchanged.

The component-copy regression now reproduces that second false edge directly:
each returned vector coordinate previously depended on all three input
coordinates. Solve schema 64 retains compact affine coordinate relations in
the owner-derived output summaries. Typed copies, pointwise operations,
transpose, static projections, and matrix products propagate their checked
coordinate relations; nested calls compose them, including independent
contraction axes. The same derivation applies to the checked directional body.
Other operations retain explicit conservative input dependencies. Coordinate
enumeration occurs only in the scalar structural-pattern view. The governing
contracts remain SOLVE-C17/C51/C56 and SPEC_0032 §6, with MLS §10.6.1 / ARR-027
for array assignment and §12.3 / FUNC-005 for pure calls.

Ten dependency tests pass, including all four matrix-product rank combinations,
nested contractions, transpose/slice composition, primal/tangent separation,
forged wire claims, and metadata independent of a million-element extent. All
260 Solve IR and 167 evaluator tests passed before the mechanical Clippy helper
extraction; all 447 core tests and both focused Clippy checks pass after it.
The new reduced source solves through `angularVelocity2` in the same direction
as GyroscopicEffects and checks all eight observables with both solvers and
both execution policies. Its exact OpenModelica source in
`.git/multibody-campaign/component-copy-omc` has 12 output rows, includes the
initial point, and agrees with the analytic solution to `1.3573e-10` absolute
error.

The current full-model diagnostic removes all remaining reverse-invalidation
flags: algebraic 14 to zero, derivative 10 to zero. Scalar-row evaluations
through time 0.02 fall from 257047 to 229999, and target assignments from
214061 to 187349, with the same 54 integration steps. All 2276 observables are
identical at initialization and differ by at most `1.095e-12` at the two later
sample times. These remain three diagnostic samples, not a full trace.
The normal run `target/msl/multibody-component-dependency-gyroscopic` constructs
Solve in 9.926 seconds and its backend in 8.043 seconds, initializes, and still
times out after the standard 12-second Sim budget. It compares zero models;
parity remains unmeasured. The remaining staged-refresh admission failure is
projection seed coverage. The first projection block, 703, has target Y867
and no seed row; coupled blocks also retain incomplete seed coverage. This
needs a separate correctness argument and regression before changing admission.

The fixed canary `target/msl/multibody-component-dependency-canary` preserves
all 20 phase, simulation, and band outcomes against
`multibody-record-dependency-canary`. Nine models are compared, all strict-high,
with zero missing, skipped, excluded, nonidentifiable, or deviating models;
all 175 initial channels are high. The receipt
`.git/multibody-campaign/component-dependency-canary-delta.json` binds worktree
`56d06da8111e4d948cd1620c007aa88760998934591ef0e9ddc5b34b76a2ed8e`.
This is Tier 1 regression evidence, with no new model pass or cohort claim.
Full 42-example, 566-model, and quick/full verification remain outstanding
while the originating model still fails; no new commit or PR has been made.

The first missing seed is now reproduced and repaired at the assignment
certificate owner. GyroscopicEffects row 1388 declares Y867 as its target,
but `Frames.angularVelocity2` returns that coordinate through a typed call,
which the isolator previously could not follow. The reduced copied-record
fixture failed because no exact refresh assignment existed for `R.w[1]`.
Solve schema 65 derives a separate exact value-projection certificate from
checked copies, finite constants, static projections, fills, transposes, and
nested certified calls. Arithmetic, dynamic indexing, control flow, and
assertion-bearing bodies remain outside this proof. Dependency information
alone never establishes value equality. Assignment isolation consumes the
certificate while retaining the call's evaluation prefix; runtime admission
checks are unchanged. Wire replay rederives the certificate and also binds
the paired directional call to its exact owner, closing a separately reproduced
owner-identity forgery.

Validation passes 268 Solve IR, 167 evaluator, and 449 core tests, plus both
focused Clippy checks. The reduced source remains byte-identical to the
eight-channel OpenModelica fixture above and passes both solvers with both
execution policies. Positive and negative tests cover nested coordinate
composition, compact million-element metadata, evaluation-prefix retention,
nonlinear rejection, and forged primal/directional wire claims.

The full-model diagnostic restores six exact velocity seeds. Derivative
refresh blocks with incomplete seed coverage fall from eight to two; those
remaining coupled blocks contain 79 and 72 unknowns. Algebraic refresh still
has 51 blocks with incomplete coverage. Scalar-row evaluations through time
0.02 fall slightly, from 229999 to 228861; target assignments remain 187349.
Across all 2276 observables, initialization is unchanged and the two later
samples differ by at most `1.140e-11`. These are diagnostic samples, not a
complete trace. The normal originating run
`target/msl/multibody-forwarded-value-gyroscopic` constructs Solve in 9.862
seconds and its backend in 8.572 seconds, initializes successfully, and still
hits the standard 12-second Sim limit. Zero models are compared, so parity
remains unmeasured and no additional model pass is claimed.

The fixed canary `target/msl/multibody-forwarded-value-canary` preserves all
20 phase, simulation, and band outcomes against
`multibody-component-dependency-canary`. All nine compared traces remain
strict-high, with zero missing, skipped, excluded, nonidentifiable, or deviating
models and all 175 initial channels high. The receipt
`.git/multibody-campaign/forwarded-value-canary-delta.json` binds worktree
`e1276912dd733b7489f2faf313b5307c9147c9689a4428804dd57f918f687504`.
This closes Tier 1 validation of the value-projection change. The remaining
coupled-block admission failure needs its own source reproduction and
correctness argument; complete 42-example/566-model and quick/full verification
are still pending. No new commit or PR has been made.

The next reduced defect is block-relative affinity. The exact schema65
GyroscopicEffects artifact in
`.git/multibody-campaign/forwarded-value-runtime-samples/solve.json`
(SHA256 `b84179b46a0324e4ae8bb256b9c3a1e2b9306e539204ee41dd1692b9a2cfbe18`)
contains coupled blocks 1794 and 1855, with 79 and 72 unknowns. At fixed
outside-block values, every selected residual is affine in its own block's
unknowns. The saved OpenModelica regular systems 1514 and 1760 are also
classified linear, but their different state and alias selection does not
establish a one-to-one block mapping. Initial quaternion equations remain
nonlinear. The diagnostic receipts are `coupled-block-affinity.json` and
`coupled-block-omc-systems.json` in the campaign directory.

The actual runtime predicate previously required a parameter-static gradient
with respect to all solver variables. A two-variable regression with an
earlier block supplying a coefficient rejects that unnecessarily strong
condition. Solve schema66 derives compact typed-call input-interaction
summaries and checks residual degree against the exact canonical block
inventory. Wire replay rederives the facts and rejects altered call claims or
mismatched canonical rows and unknowns. The runtime uses the issued fact to
remove the block's Newton step cap; it proves neither nonsingularity nor
parameter-static gradients, and does not change staged-refresh seed admission.
Tensor ranges stay compact. Unsupported compute forms remain unproved.

Review reproduced an output-identity error in the new checker before acceptance:
two compute nodes each using local output zero swapped the affine/nonlinear
classification. `block-affinity-output-identity-red-1.log` retains that failure.
The corrected checker uses the existing compute-block output mapping and
refuses duplicate output ownership. All 281 Solve IR, 167 evaluator, and 418
solver tests now pass in `block-affinity-libraries-final.log`; all 452 core
tests pass in `block-affinity-core-full-1.log`. The reduced tensor fixture
checks all nine analytic observables with both BDF/RK and Auto/Interpreter.
Its exact source SHA256 is
`93928e0f5f69b4f37e9859c1915c79298501e444420a683f63769003f3c65d5f`.
Default OpenModelica tearing refuses the non-inlined call while generating
linear-system Jacobians; that failure remains in `block-affinity-omc-1.log`.
The identical source succeeds with `--tearingMethod=noTearing` (the pinned
OMC version's deprecated alias for `minimalTearing`): all nine channels and
12 rows, including initialization, agree with the analytic solution to
`2.142e-10` absolute error. The alternate-oracle receipt is
`block-affinity-no-tearing-omc/analytic-comparison.json`; this diagnostic
changes neither MSL references nor gate settings. Both final focused Clippy
checks pass, and temporary profiling tests have been removed.

The production diagnostic certifies both actual coupled blocks affine and
retains 18 states. Its short run produces bitwise-identical values for all
2276 observables at times 0, 0.01, and 0.02, with unchanged work counts:
228861 scalar-row evaluations, 187349 target assignments, and 54 solver steps.
This is not a complete trace and shows no speed gain. The normal run
`target/msl/multibody-block-affinity-gyroscopic` compiles in 3.388 seconds but
exceeds the 10-second Solve-construction budget; the parent observes the
failure after 13.644 seconds. This is an earlier failure than the preceding
Sim timeout, with no initialization or trace result. It remains an unresolved
performance regression; the run compares zero models and parity is unmeasured.

The fixed canary `target/msl/multibody-block-affinity-canary` preserves all
20 phase, simulation, and band outcomes against `multibody-forwarded-value-canary`.
All nine compared models remain strict-high, all 175 initial channels are high,
and missing, skipped, excluded, nonidentifiable, and deviating counts are zero.
The receipt `block-affinity-canary-delta.json` in the campaign directory binds
worktree `1e78ae01f6a0f6e65b247bb3367da214ca7122a54d4809cba2292575e7fc4d79`.
This is Tier 1 regression evidence, with no coverage gain.

Isolated construction measurements rule out degree derivation as the main
multi-second cost: all 2004 projection proofs take 3.4–3.7 milliseconds, and
primal/directional interaction derivation over 1967 function owners takes
6.0–6.8 milliseconds. Replaying the complete checked function table takes
6.53–6.70 seconds in the test profile. These measurements do not include all
interface-cloning costs in the degree-derivation totals. The before/after
function-table wire is identical, and the 251 root owner references reach
all 1967 owners through typed call edges. Unused-owner pruning is therefore
not a remedy for this inventory. The next hypothesis is repeated construction
and deep cloning of previous-owner interfaces, which needs direct measurement
and a reduced regression before changes. Complete 42-example/566-model and
quick/full verification remain outstanding; no new commit or PR has been made.

The interface-construction hypothesis is now measured. Rebuilding all previous
owner interfaces for each of the 1967 owners takes 682.699 milliseconds for
primal signatures and 3.687 seconds for directional interfaces alone
(`call-interface-prefix-profile-before.log`). The borrowed-storage regression
fails on the former `interface()` implementation because it copies the owner's
input array (`call-interface-borrow-red-1.log`).

Typed construction now uses a `Copy` borrowed `SolvePureCallTableView` over
already-issued owners. Constant-time lookup retains the exact owner ID and
selects primal or directional interfaces without allocating previous-owner
arrays. Program construction, nested regions, wire replay, and all three
summary derivations share that view. Future owner IDs and suffix/index
rebinding are rejected. The finalized program and schema66 wire remain owned;
this change neither skips wire checks nor merges function identities.

All 284 Solve IR, 167 evaluator, 418 solver, and 452 core tests pass. All nine
Solve IR doctests pass, including an actual `E0521` rejection of a register
crossing construction scopes. Both affected-library and compiler Clippy pass;
formatting and `git diff --check` pass.

The fresh diagnostic lowers and saves the model in 8.677 seconds, versus
10.204 seconds before this change, about 15% faster. The complete serialized
Solve artifact is byte-identical, with SHA256
`240ca32f5e37ec32f45f435594d0f8a994de904b1255ff8ebbb35f492ec9c8c4`.
The ordinary run `target/msl/multibody-call-interface-gyroscopic` now clears
the unchanged construction budget: compilation takes 3.477 seconds, Solve
construction 8.725 seconds, and backend preparation 7.685 seconds.
Initialization succeeds, but simulation still exceeds its 12-second budget.
This repairs the construction timeout, with no additional model pass:
zero models are compared and parity remains unmeasured.

The fixed canary `target/msl/multibody-call-interface-canary` passes and
preserves every phase, simulation, and band outcome of the preceding
`multibody-block-affinity-canary`. All nine compared models remain strict-high;
all 175 initial channels remain high. Missing, skipped, excluded,
nonidentifiable, and deviating counts are zero. The campaign receipt
`call-interface-canary-delta.json` binds worktree
`ecf962178d1838402b99034a846ab01ba621b84191d3f821180793700e4df5f1`.
This is Tier 1 regression evidence, not a cohort coverage gain.

The remaining investigation is runtime cost in repeated algebraic projection,
including reduced Jacobian sweeps. Block affinity alone does not justify
reusing finite-difference Jacobians or removing causal-seed admission; both
proof boundaries remain intact. Complete 42-example/566-model and quick/full
verification remain outstanding while the originating timeout is unresolved.
No new commit or PR has been made.

Runtime profiling next isolated a separate allocation defect in
`rumoca-eval-solve::typed_program::InvocationScope`. Every function invocation
and Map/Fold domain point created and destroyed a result array sized for every
function owner in the model, including leaf calls with no nested calls. Of
209 CPU samples in a final 1.05-second integration window, 27.27% were in that
array's initialization and 13.40% in its destruction. These are self-symbol
samples, with incomplete stack unwinding, from a short diagnostic rather than
a full-model benchmark. `runtime-projection-perf-integration.txt` and
`invocation-scope-triage.json` retain the attribution and limitations.

The resource regression in `invocation-scope-red-1.log` invokes a leaf through
the actual evaluator in a checked 1024-owner table: its correct output still
retains 1024 unused result slots. Invocation storage now contains only executed
calls, keyed by the same typed owner ID. The former owner bound remains an
explicit check; duplicate insertion fails without replacing the issued result.
The lookup is private and never determines output order. Conditional regions
still share their enclosing invocation, while function and Map/Fold scopes
remain fresh. No value survives its existing invocation lifetime. A leaf's
result lookup now has zero allocated capacity, and a high owner ID consumes
only one entry when actually invoked. Unknown and duplicate owner regressions
pass. This changes evaluator storage, with no IR, arithmetic, solver tolerance,
or seed-admission changes, under SPEC_0029 sections 5/12 and SPEC_0043 sections
6/9's existing execution, call-identity, and scope contracts.

All 169 evaluator and 418 solver tests pass in `invocation-scope-libraries-1.log`;
all 452 core integration tests pass in `invocation-scope-core-1.log`. The
all-target/all-feature evaluator/solver Clippy check, formatting, and
`git diff --check` pass. The fresh diagnostic's two integration intervals take
503.549 and 213.184 milliseconds, versus 910.202 and 380.146 milliseconds in
the preceding profiled run, about 44% less time. Work remains exactly 228861
scalar-row evaluations, 187349 target assignments, and 54 solver steps. All
2276 observables at times 0, 0.01, and 0.02 remain bit-identical, including the
serialized sample files (`invocation-scope-runtime-comparison.json`). A separate
direct-name check against the saved OpenModelica CSV covers 967 observables at
these times, with maximum absolute error below 4.391e-7; 1309 Rumoca channels
are unmatched in this diagnostic without alias expansion. This incomplete
three-sample check is not a trace-parity band or a model pass.

The normal `target/msl/multibody-invocation-scope-gyroscopic` run compiles in
3.252 seconds, constructs Solve in 8.648 seconds, and prepares the backend in
7.056 seconds. Initialization succeeds, but simulation still exceeds its
unchanged 12-second budget. Zero models are compared and parity is unmeasured.
The fixed `target/msl/multibody-invocation-scope-canary` passes with every one
of its 20 phase, simulation, and band outcomes unchanged from
`multibody-call-interface-canary`: nine compared models remain strict-high,
all 175 initial channels remain high, and missing, skipped, excluded,
nonidentifiable, and deviating counts remain zero. The Tier 1 delta receipt
`invocation-scope-canary-delta.json` binds worktree
`c522ffb348ed1e921b780fe1ccc1bb362969741ed66025d5b865be3b3bb29a79`.
There is no additional model pass or cohort coverage claim. Full 42-example,
566-model, and quick/full verification remain outstanding.

The follow-up profile uses a realtime-clock window bounded by the first and
last sample markers: 361 CPU samples over 0.724 seconds. The removed dense
invocation-array symbols are absent; the largest remaining self symbol is
typed `EvalFrame::run` at 15.51%, with typed arithmetic and value allocation
also prominent (`invocation-scope-perf-integration.txt`). Source inspection
shows that `RefreshProjectionModel::eval_implicit_target_value` always enters
the interpreted target-assignment evaluator. The next investigation is that
execution boundary and the existing compiler-issued exact assignment/native
call products. This is a hypothesis for the next fix, with no change yet to
projection scheduling, nonlinear seeds, or Jacobian policy.

The next reduced regression confirms a missing execution binding:
`RefreshProjectionModel::eval_implicit_target_value` passes the model's row
context to the exact target-assignment evaluator, but that context supplied
only the reference call table. Both scalar-call evaluators consequently
interpreted typed functions even when the native backend already owned their
compiled helpers. The fixture uses the real compiled table for `f(x)=x*x+2`
and the actual target-assignment evaluator. Before wiring the consumer, it
returns 11 at x=3 but records zero native invocations where one is required
(`native-call-projection-red-2.log`). The preceding red-1 log is only a fixture
compile error, not regression evidence. This establishes execution dispatch
as the responsible layer: checked call construction and the numerical result
are already correct, and the compiled table is available in this fixture.

The evaluator context now accepts the model backend's pure-call executor.
Both checked and prepared scalar paths validate the same issued call site and
marshal through the same typed input conversion as reference evaluation.
The Cranelift adapter reuses the existing model-wide primal or directional
helper and its checked ABI; it builds no per-target wrapper or duplicate IR.
Outputs are invocation-local and are published only after successful execution.
An attached executor's error propagates with the source span instead of
retrying reference evaluation. The interpreter policy still supplies no native
executor. Governing rules are SPEC_0029 sections 5/12 and SPEC_0007 Stage 4,
especially SOLVE-C33/C37/C45/C51/C56; this is execution binding, not a new
Modelica transformation, scheduling rule, or result-cache lifetime.

All three new regressions pass (`native-call-projection-regressions-2.log`):
the real compiled owner executes on repeated target assignments with changing
inputs; primal/directional outputs agree with reference evaluation and the
analytic derivative for changing inputs and seeds through both row entry
points; and an injected execution failure publishes no partial output and
retains its source span. The complete affected library run passes 169 evaluator,
66 Cranelift, 127 simulation, and 418 solver tests, and all 452 compiler core
tests pass: 1232 in total. All-target/all-feature Clippy for the four affected
libraries, formatting, and `git diff --check` pass. The library run also found
an inspection test expecting the old fixed-start pendulum refusal. Its positive
replacement checks the issued given-state ownership, inspection agreement,
exact initial x=1, and the trajectory's circle constraint. A separate
contradictory fixed-start alias fixture retains the reduction, inspection,
and diagnosis refusal checks. All five structural-report tests pass.
The fresh short diagnostic takes 116.603 and 51.368 milliseconds for its two
integration intervals, versus 503.549 and 213.184 milliseconds before binding
the native calls: about 76–77% less time. All 2276 values at times 0, 0.01,
and 0.02 remain bit-identical, including the complete sample JSON files
(`native-call-runtime-comparison.json`). The solver still takes 54 steps and
evaluates 187349 target assignments. These three samples are not a full trace
or a parity band.

The normal `target/msl/multibody-native-call-gyroscopic` run compiles in 3.974
seconds, constructs Solve in 9.168 seconds, and prepares the backend in 6.173
seconds. Initialization succeeds, but simulation still reaches its unchanged
12-second timeout. The failure bucket is `Timeout`; the raw `sim_solver_fail`
tag does not establish a new numerical failure. Zero models are compared, and
parity remains unmeasured.

The fixed `target/msl/multibody-native-call-canary` passes with all 20 phase,
simulation, and band outcomes unchanged from `multibody-invocation-scope-canary`.
All nine compared models remain strict-high, all 175 initial channels remain
high, and missing, skipped, excluded, nonidentifiable, and deviating counts are
zero. `native-call-canary-delta.json` binds worktree
`22295d5f1c68acb5c36c1f742917f53c1f44039663ef6340ffc954aaa73f7507`.
This is completed Tier 1 evidence for the execution binding, with no additional
model pass or cohort coverage claim. Complete 42-example/566-model and
quick/full verification remain outstanding; no new commit or PR is made.

### Fresh 42-example checkpoint after native call binding

`target/msl/multibody-native-call-examples` completes the unchanged 42-example
target list at HEAD `bc71577f85df24957e5c9ab30fdaf4ed48da4311`, with worktree
digest `c5ccf781508b2d716b052ea45022e60275984fdbc101d598ca94b3f3128ed7d2`.
All 12 completed models are compared and strict-high across 4837 channels,
including initialization. Missing, skipped, excluded, nonidentifiable, and
deviating counts are zero. `FreeBody` and `ThreeSprings` change from absent to
high; all ten previously high examples remain high. This comparison spans
the accumulated branch changes since `multibody-record-field-examples`, so
the two gains cannot be attributed solely to native execution binding.
These are measured example results, not a new golden-cone certification or
a 566-model cohort claim.

The remaining failures stay in the target list:

| Failure bucket | Models | Current location |
|---|---:|---|
| Timeout | 16 | Ten Solve construction, four simulation, two Flatten |
| Structural analysis | 6 | Solve |
| DAE construction | 6 | Three derivative record inputs, three numeric/Boolean mismatches |
| Runtime contract | 1 | PointGravity initialization has a non-finite gravity residual |
| Instantiation | 1 | Surfaces colorMap constrainedby subtype check |

The campaign receipts `native-call-examples-receipt.json` and
`native-call-examples-delta.json` retain the exact failures and phase/band
changes. This replaces the older 10-of-42 checkpoint for this example scope.
The broader library/test inventory and complete 566-model and quick/full gates
remain unfinished.

The subsequent longer GyroscopicEffects diagnostic advances to time
0.9079194955562074 after 392 solver steps before reporting the same 12-second
timeout. A realtime-bounded profile contains 5032 self-symbol samples over
10.090 seconds: native scalar Jacobian rows account for 39.07% and native
typed directional helpers for 32.91%. There are no lost samples. These are
self-symbol classifications, not a call-tree attribution; repeated JIT symbol
names are not semantic owner identities. The shorter observed advance interval
versus the reported solver timeout also remains to be explained.
`native-call-full-perf-window.json` records the window and qualifications.
The next investigation is the actual Jacobian caller and selected-row/full-JVP
dispatch, with a reduced reproduction before changing execution or reuse.

The next stack trace stops at the first native Jacobian evaluation after the
private probe's integration marker. The actual call chain is BDF state reset
and Jacobian assembly → FMI directional derivative → projected state
sensitivity → `algebraic_plan_row_scales` → `algebraic_block_jacobian` → the
compiled full-model JVP (`jvp-callstack-1.log`). The colored branch of the shared
block assembler always invoked the full JVP; unlike the ordinary branch, it
never requested the available selected-row evaluator. This is runtime
execution ownership under SPEC_0029 sections 5/12 and SOLVE-C17/C45/C56, not
an IR sparsity or differentiation change. The existing coloring still proves
that columns in a color affect disjoint rows.

The reduced `colored_rows` fixture has three local rows mapped to global rows
4/1/3, local columns mapped to Y[2]/Y[0]/Y[1], and two checked colors. The
actual row-scale helper returns the correct scales but makes two whole-model
JVP calls rather than selecting the four required row/color pairs. All four
tests fail before the fix (`colored-jvp-red-2.log`); red-1 is only a fixture
compile error. The colored branch now passes its active global rows to the
same selected-row helper as ordinary projection. Local matrix placement,
reverse-computed rows, coloring, parameter-zero seed lanes, and scaling are
unchanged. An unavailable row retains one full evaluation for that color; an
error propagates rather than selecting that fallback.

All four focused regressions, all 422 solver library tests, and all 452 core
tests pass, along with all-target/all-feature solver Clippy, formatting, and
`git diff --check`. All 2276 short-diagnostic values at 0/0.01/0.02 remain
bit-identical (`colored-jvp-runtime-comparison.json`). The normal
`multibody-colored-jvp-gyroscopic` run clears initialization, with Solve
construction at 8.735 seconds and backend preparation at 4.512 seconds, but
still reports a 12-second simulation timeout: zero compared, parity unmeasured.
The fixed `multibody-colored-jvp-canary` passes with all 20 outcomes unchanged
from `multibody-native-call-canary`: nine compared strict-high models, 175 high
initial channels, and zero missing/skipped/excluded/nonidentifiable/deviating
counts. The delta receipt binds worktree
`ba078c68234cc2490ad2541b1c68b3681b5ef0cdddb14020edfa475ff1f7832d`.

The longer diagnostic now reaches time 4.48058911338337 after 1906 steps,
versus 0.9079194955562074 after 392 steps before this change. It still times
out under the unchanged budget. Its realtime window has 5801 self-symbol
samples over 11.634 seconds, with zero samples in the formerly dominant native
Jacobian-row functions (`colored-jvp-full-perf-window.json`). This completes
Tier 1 evidence for selected colored JVP execution, without claiming a model
pass. Inspection of the same sensitivity path also finds that it reconstructs
each block's Jacobian for final row scaling after having formed it at the
identical immutable Y/P/time point for the sensitivity solve. The next reduced
check tests retaining those row scales from the existing matrix within that
one invocation; it does not propose cross-step caching.

The reduced two-row sensitivity case confirms the duplicate work: twelve
selected JVP calls instead of eight (`seed-scale-reuse-red-1.log`). The primal
Y/P slices and time remain fixed while only the tangent seed changes, so final
row scaling can consume the same matrix used to solve each block. Review of
the initial implementation caught a contract regression: its separate seed
matrix builder did not validate the structure dimensions that the old final
scaling pass checked. A mismatched 3x3 structure attached to a 2x2 block was
incorrectly accepted (`seed-scale-shape-red-1.log`).

Sensitivity projection now uses the shared `algebraic_block_jacobian` and
computes row scales before LU consumes that matrix. The duplicate per-column
builder and its unused unit-seed scratch plumbing are removed. Shape checking,
selected-row errors, singularity rejection, final scaled residual validation,
and restoration of the caller's unknown seeds on failure remain in place.
This is runtime execution ownership under SPEC_0029 sections 5/12 and
SPEC_0007 Stage 4, not a new compiler transformation or cross-call cache.
The colored fixture also checks a nonzero projected direction through the
nonidentity row/column layout while retaining the caller's known directions.
All 424 solver library tests and 452 compiler core tests pass
(`seed-scale-reuse-solver-3.log`, `seed-scale-reuse-core-1.log`), along with
all-target/all-feature solver Clippy, formatting, and `git diff --check`.
The short diagnostic retains bit-identical values for all 2276 channels at
0/0.01/0.02 (`seed-scale-reuse-runtime-comparison.json`), with the same 54
solver steps. The normal `multibody-seed-scale-gyroscopic` run compiles in
3.268 seconds, constructs Solve in 8.612 seconds, and prepares the backend
in 4.427 seconds. Initialization succeeds, but simulation still reports the
unchanged 12-second timeout: zero compared and parity unmeasured.

The fixed `multibody-seed-scale-canary` passes with every phase, simulation,
and band outcome unchanged from `multibody-colored-jvp-canary`. All nine
compared models are strict-high, all 175 initial channels are high, and
missing, skipped, excluded, nonidentifiable, and deviating counts are zero.
`seed-scale-canary-delta.json` binds worktree
`5cd1f9433dfd1ed236bf84fe2a4f8b89b897bc15ae69b2ce79cb5a0ff2f2435a`.
This completes Tier 1 validation for this runtime change without adding a
full-model parity claim. The complete 42-example checkpoint remains twelve
strict-high examples; complete 566-model and quick/full verification remain
pending.

The subsequent longer diagnostic reaches time 4.900310731426584 after 2098
steps, versus 4.48058911338337 after 1906 steps before matrix/scale reuse.
It still reports the same timeout. `seed-scale-full-perf-window.json` records
5871 self-symbol samples over 11.775 seconds. The profile includes 2.37% in
`certifies_exact_target_assignment_output`; an actual debugger stack places
that call in branch-continuity projection during a BDF derivative evaluation
(`assignment-certification-callstack-1.log`). Inspection finds it rescanning
the prepared block's immutable operation list for non-causal operations on
each query. This is the next reduced performance investigation, not an
implemented optimization. The profile's separate metadata-comparison cost
does not authorize removing call-site ownership checks.

The reduced prepared-block fixture contains two independent assignment
outputs and one seed-dependent residual. Its valid/invalid row, output, and
target checks pass before the resource assertion fails: repeated queries over
the original and cloned block perform 408 operation visits after preparation
(`prepared-causality-red-1.log`). The predicate reads only immutable owned
operations, so its result belongs with the evaluator's existing prepared row
metadata. The output/target shape checks remain specific to each request.

`PreparedScalarProgramBlock::new` now records the existing non-causal-operation
predicate once per program. Its metadata allocation remains fallible, and
cloning retains the same result for the cloned program. Prepared certification
and isolator materialization consume that result. Standalone program helpers
still check their raw input. The regression also checks materialization and
proves that preparation performs the scan while subsequent queries and cloning
perform none. The per-thread counter exists only in unit-test builds. This
is evaluator preparation under SPEC_0029 sections 5/12 and SPEC_0007 Stage 4;
it changes no accepted operations, runtime coordinates, call-site ownership,
or arithmetic. All 170 evaluator, 424 solver, and 452 compiler core tests
pass (`prepared-causality-libraries-1.log`, `prepared-causality-core-1.log`),
along with all-target/all-feature evaluator/solver Clippy, formatting, and
`git diff --check`. The MSL results follow below.

The short diagnostic retains all 2276 values bit-identically at 0/0.01/0.02.
The fixed `multibody-prepared-causality-canary` passes with all 20 outcomes
unchanged, nine compared strict-high models, 175 high initial channels, and
zero missing/skipped/excluded/nonidentifiable/deviating counts. Its receipt
binds worktree `130cbc109b21ef9d040a23b119ffb26ce31e606b4128885219de7bb8e0607399`.

The complete `multibody-prepared-causality-examples` run has 13/42 strict-high
models and 5754 high channels including initialization, with zero missing,
skipped, excluded, nonidentifiable, or deviating channels/models. All twelve
previous high examples remain high; `Constraints.SphericalConstraint` newly
passes all 917 compared channels. Its simulation takes 7.484 seconds.
These gains span the colored JVP, matrix/scale reuse, and prepared-causality
changes since `multibody-native-call-examples`, not just the last edit.
GyroscopicEffects still reaches the unchanged simulation timeout after
successful initialization; the origin remains unmeasured. The 29 remaining
example failures comprise 15 timeouts, six structural-analysis failures, six
DAE-construction failures, one initialization runtime-contract failure, and
one instantiation failure. The example receipt and delta preserve each result.

An independent source-grounded review of SphericalConstraint checks the two
equivalent spring/body assemblies over all 501 samples from 0 to 10 seconds.
The maximum locked-position residual is 2.17e-16 m; the relative-position
sensor residual is 9.73e-15 m; constraint torque and power are zero. Rotation
orthogonality residuals are at most 2.10e-8 for the joint body and 6.66e-16 for
the constrained body. Computing mechanical energy from the source-defined
mass, inertia, center of mass, spring, and gravity gives maximum drifts of
4.63e-5 and 5.91e-5 J from the common initial 1.91568 J. The largest difference
between corresponding body position/orientation/angular-velocity channels is
8.37e-5. `spherical-physics-review.json` records raw maxima and source/trace
hashes; these diagnostics are additional physical evidence, not a new tolerance
policy or complete golden IR-cone certification.

The subsequent complete 566-model run `multibody-runtime-prepared-full`, at
HEAD `bc71577f85df24957e5c9ab30fdaf4ed48da4311` and the same worktree digest,
**fails** the soundness gate after the comparator executes. It measures 142
models: 141 strict-high, one near, zero missing, and 18 skipped under existing
reviewed policy exclusions. All 138 previously high models remain high.
FreeBody, ThreeSprings, and SphericalConstraint account for the three high
gains. The additionally completed single-phase `RLV_Characteristic` activates
an already tracked exclusion; no exclusion policy was changed or baseline
promoted. All 14550 compared initial channels are high.

The newly completing
`Electrical.PowerConverters.Examples.ACDC.RectifierBridge2mPulse.ThyristorBridge2mPulse_RLV`
has 409 high and **20 deviating** channels, 17 severe, despite its aggregate
near band. This is one actionable refinement counterexample and takes priority
over further MultiBody performance/capability work. Its largest errors are
aliases of neutral-point voltage: Rumoca's error reaches roughly 0.003 V
while the OMC reference range is about 1.5e-9 V. The source connects the star
point through a 1e6-ohm resistance to ground. This suggests sensitivity to
small current-balance errors, but the responsible compiler/runtime layer is
not yet proven. No noise exception, tolerance change, or closure is claimed.
`runtime-prepared-full-delta.json` retains the complete phase/band delta;
the next task is a reduced reproduction and root-cause proof for this model.

A separate diagnostic worker reproduces the neutral-voltage error and saves
Flat, DAE, structural DAE, and Solve artifacts without changing the model or
solver settings. At t=0.0594 the neutral voltage is 0.002971540144 V and the
1e6-ohm resistor current is 2.971540144e-9 A, retaining the resistor's exact
constitutive relation. The first sample above 1e-6 V occurs at t=0.002.
The neutral voltage/current belong to Solve projection block 50, a 110-unknown
coupled block with ten tear variables; the reported ground current is a later
singleton assignment. This narrows the next investigation to how the coupled
projection converges and how its settled values reach observation. It does
not yet distinguish residual-only convergence, ill-conditioned numerical
elimination, or an incorrect producer from one another.

## Broad verification checkpoint

The requested `cargo xtask verify quick` run at HEAD
`bc71577f85df24957e5c9ab30fdaf4ed48da4311` fails four stages: MSL parity,
the corpus pin, architecture, and workspace tests. Workspace-wide all-feature
Clippy and formatting pass. The MSL results are preserved in
`target/msl/multibody-verify-quick-checkpoint-1`, with worktree digest
`1acce4d1c34423640ac465849a4300cf34ac817dcc2a1b5c2c89c25f6e4c1e18`.
The complete 566-model comparison retains all 141 previously strict-high
models: 142 compared, 141 strict-high, the same one rectifier counterexample,
17 existing policy exclusions, zero missing, and zero nonidentifiable.
No model changes comparison band. The already excluded single-phase
`ThyristorBridge2Pulse_RLV_Characteristic` times out after completing in the
preceding run; that execution loss remains recorded separately.

The workspace runner initially stops after 372 tests on the architecture
failures. A subsequent run with `--no-fail-fast --test-threads 4` executes all
7294 tests: 7290 pass, four fail, and none are skipped. The failures concern
the two totality-debt counters, test-only module classification, and the DAE
LOC review ledger. The classifier misses external child modules beneath an
inline `#[cfg(test)]` module; added production assertions also need review.
The DAE ledger reports core/total measurements of 17164/22571 lines.
The separately completed workspace doctests pass 30 tests; 23 existing
documentation examples are ignored. No gate ceiling is changed here.

The 28-row corpus check passes 27 rows and rejects the obsolete `EL005`
refusal pin for Rotational `FirstGrounded`, which now simulates and has all
46 channels high in the complete OMC comparison. Its pin now checks four
states at 0, 0.01, and 0.02 seconds against independent numerical values.
From the source gear ratio and inertias, the motor reflected onto inertia2
gives `J = 0.1*10^2 + 2 = 12`. With all four initial states zero, the reduced
equations are `12*der(w2) = 100*sin(10*pi*t) - 10*w2 - 10000*(phi2-phi3)`
and `2*der(w3) = 10000*(phi2-phi3)`, with `der(phi2)=w2` and `der(phi3)=w3`.
A 60-decimal matrix exponential, augmenting the system with sine/cosine
states, gives identical values with 80 and 100 terms. The largest Rumoca
state error over the three samples is 8.91e-7. The new observations use the
existing recorder's initial and stepped tolerance formulas; the other 27
corpus rows are unchanged. The focused corpus check then passes with all
12 observations (`verify-quick-firstgrounded-pin-2.log`, 14.2 seconds).
`verify-quick-checkpoint-1.json` and its logs in the campaign ledger retain
the full checkpoint and follow-up validation. The full quick suite has not
been repeated after the pin update; the rectifier and four gate failures
remain open, and no baseline is promoted.

## DAE module review

The 2026-09-11 production inventory uses SPEC_0043's physical-line convention,
including its test/generated exclusions. Relative to branch HEAD
`bc71577f85df24957e5c9ab30fdaf4ed48da4311`, core grows from 16,776 to 17,163
lines and wire from 5,292 to 5,407: 22,068 to 22,570 total, a 502-line increase.
The mechanically derived ledger ceilings are 17,250 core, 5,500 wire, and
22,750 total. This acknowledges reviewed source size; no totality-debt ceiling
or source-size review trigger is relaxed.

| Production modules | Lines | Review finding |
|---|---:|---|
| `expression.rs`, `expression/` | 3,889 | Eight added lines expose borrowed record fields; all other expression construction is unchanged |
| `model/function_derivatives.rs` | 352 | New checked derivative identity, chain, signature, and tangent-shape owner |
| Other `model/` core and `model.rs` | 7,885 | Function storage, schema, and read views integrate the new owner; existing construction remains shared |
| `clocks.rs`, `conditions.rs`, `events.rs`, `temporal.rs` | 1,371 | Unchanged temporal/event ownership |
| `discrete_values.rs`, `equations.rs`, `model_event_transactions.rs` | 2,455 | Unchanged equation and transition construction |
| `error.rs`, `ids.rs`, `lib.rs`, `provenance.rs`, `expr_query.rs` | 1,211 | Typed derivative error/identity and exports; existing shared provenance/query ownership |
| `model/wire/function_replay.rs` | 1,444 | 102 added lines serialize derivative source facts and replay their predecessor order through checked constructors |
| Other wire modules | 3,963 | Thirteen added lines define the source-fact record and invoke derivative replay |

The derivative constructor proves complete functions, pure external bodies,
the exact original-input prefix, role coverage, recursively filtered Real
record fields, compact tangent shapes, priority uniqueness, and the issued
predecessor chain before appending a link. First and higher derivatives use
one insertion and signature-checking path. Consumers receive a borrowed view
of those checked facts; no second annotation table is added downstream in DAE.

Wire stores target, input roles, predecessor identity, priority, and provenance.
It does not serialize the derived order or tangent-prefix length: replay
reconstructs them through the same constructors. Its pending traversal permits
cross-function predecessor ordering and rejects a stalled or cyclic chain.
This is distinct from semantic derivative selection in the structural phase.
Review found no redundant validator, unchecked compatibility format, or second
signature checker to remove.

The record traversal did redundantly reconstruct field indices and assert that
lookups succeeded. A borrowed field iterator now carries each name and type
together, eliminating both assertions while preserving order, nested Real
filtering, and rejection of missing or extra tangent fields. The remaining
production increment carries the construction and replay obligations above.
The complete file inventory is retained as
`.git/multibody-campaign/dae-module-review-inventory.json`.

## Verification follow-up

All four workspace failures from the broad checkpoint are repaired. The
totality scanner now follows Rust module items, including external descendants
of inline test modules, while preserving any file also reachable through a
production declaration. Files without a proved test-only path stay counted.
Filesystem regressions reproduce both the missed test descendants and the
unsafe exclusion of a dependency shared by production and test roots. The
scope tests fail before the repair and pass afterward.

Production cleanup removes the actual added obligations separately: affine
coefficients use a closed arithmetic enum, expression reads reuse the existing
branded selector accessor, record traversal carries field names and types
together, initialization keys distinguish Y and P directly, and matrix
dependency construction produces the exact left/right coordinate maps as a
pair. Structured initialization uses the existing domain iterator and carries
its resolved scalar into body lowering. Totality ceilings are unchanged.

The affected libraries pass 582 tests; architecture passes 243 and repository
gates pass 17. Workspace all-target/all-feature lint, formatting, and traversal
checks pass. The subsequent complete workspace nextest run passes all 7294
tests with zero failures or skips, including the previously failing gates and
the required MSL simulation regressions. These results are recorded in
`totality-construction-{libs,gates,lint,workspace}-1.log` under the campaign
ledger. The earlier workspace doctest result remains 30 passed and 23 ignored;
doctests were not repeated for this private-API cleanup.

The fixed 20-model canary in `target/msl/multibody-totality-canary`, at HEAD
`bc71577f85df24957e5c9ab30fdaf4ed48da4311` with worktree digest
`65b0a8d54fae0879db4e3b2ee29b32da6e6073f6dfc006c1a18fb79602942703`,
has no phase, simulation-status, or comparison-band changes from
`multibody-prepared-causality-canary`. All nine compared models remain high,
including all 175 initialization channels; missing, skipped, excluded,
nonidentifiable, and deviating counts are zero. The remaining eleven canary
models retain their existing failures. The durable delta is
`totality-construction-canary-delta.json` in the campaign ledger.

The full 566-model evidence remains the preceding broad checkpoint: 141 high,
one actionable rectifier counterexample, and 17 existing exclusions. The full
quick suite has not been repeated after these repairs, and full verification
remains incomplete. Within that checkpoint, the 42 MultiBody examples retain
33 successful DAE constructions, 17 attempts reaching initialization, and 13
completed simulations with high trace parity. Counterexample investigation
resumes before any additional breadth work.

The next diagnostic replays the rectifier's exact checked SolveModel archive
and freezes all solver coordinates and parameter values at t=0.002 and
t=0.0594. Fresh reference-evaluator runtimes compare ordinary and
coordinate-certified refresh at the same 1e-10 tolerance. Ordinary refresh
retains neutral potentials of -1.35425e-6 V and 0.00297154 V; certified refresh
reduces them to 1.491e-11 V and 1.155e-9 V. The corresponding maximum residuals
fall from 1.91e-8 and 4.72e-9 to 4.26e-14. Both paths return success. This
isolates a convergence-policy lead without advancing time or changing the
stored inputs. A reduced public-observation regression and native runtime fix
are still required; this diagnostic does not close the counterexample.

`neutral-point-probe-2.log` and `neutral-point-probe-results.json` retain the
experiment. Its first setup attempt, `neutral-point-probe-1.log`, failed while
decoding the DAE archive with `InvalidArrayExtent`, before any numerical
evaluation. The successful follow-up uses the existing checked SolveModel
replay API; the separate DAE archive-replay defect remains to be triaged.

## Public observation convergence

The frozen rectifier evidence reduces to a two-variable source model:

```modelica
model CoupledObservation
  parameter Real epsilon = 1e-6;
  output Real x(start=1);
  output Real y(start=1);
equation
  x*x + y*y = 2;
  x*x + (1 + epsilon)*y*y = 2 + epsilon + epsilon*1e-4*time;
end CoupledObservation;
```

Subtracting the equations gives `y² = 1 + 1e-4*time` and
`x² = 1 - 1e-4*time`. Positive starts select the positive continuous branch.
The regression proves that lowering retains a coupled two-variable projection
block and checks both the public state observation and named getters at
0, 0.1, 0.5, and 1 second. Before the repair, all four combinations of BDF/RK
and native/interpreted evaluation return `x=1` at 0.1 seconds instead of
`0.9999949999875`. OpenModelica simulates the same source without warnings;
its 12 CSV rows agree with the analytic solution within `1.619e-10`.
The checked source equations are correct, and the failure first appears when
the public observation accepts a small equation residual despite a large
coordinate error. This is a runtime convergence defect rather than a compiler
equation defect or an integrator time-advance defect.

The linked FMI kernel now uses its existing coordinate-certified algebraic
refresh for public observations, including each pass of the discrete/algebraic
fixed point. It preserves the exact solver-vector width check and the frozen
continuous-state prefix. This implements the observable-result preservation
contract in SPEC_0038's Internal Solver Boundary, under SPEC_0033 §§2–3 and
§6a's counterexample workflow. It changes neither the source model nor the
comparison or numerical tolerances.

A second regression covers certification through causal back-substitution.
For `v = 1e6*i` and `i = 0.5e-6*v + 1e-11`, the unique solution is
`i = 2e-11`, `v = 2e-5`. Previously, the torn solver reported the zero vector
settled because the current correction alone met tolerance, concealing the
voltage error. The regression directly invokes the torn path and requires it
to converge without a dense fallback. Certification now checks the undamped
Newton candidate through the existing causal sweep and requires every
recovered coordinate, as well as every tear coordinate, to meet its existing
scaled correction tolerance.

Both fixes pass all 425 solver tests and all 456 core tests. Evidence is in
`observation-convergence-{red,green,solver}-1.log`,
`observation-torn-certificate-red-1.log`, and
`observation-convergence-core-2.log` under the campaign ledger. The source-level
regression is `suite_core/public_observation_convergence.rs`; the torn-path
regression is `runtime/projection/tests/certification.rs`. The independent OMC
receipt is `observation-convergence-omc/analytic-comparison.json`.
The normal-budget originating-model comparison in
`target/msl/multibody-observation-rectifier`, at HEAD
`bc71577f85df24957e5c9ab30fdaf4ed48da4311` with worktree digest
`8ffffe4abca43bb488dfccd9881270af6c4f84919cb8fe3f59e35e4e5a7f6aae`,
completes simulation in 1.276 seconds. It compares all 429 channels and all
429 initialization values, with zero missing, skipped, excluded, or
nonidentifiable traces. Seventeen formerly severe channels now agree, including
the neutral voltage; its maximum reference difference is approximately
`6.337e-9 V`. However, three linked `rootMeanSquareVoltage` channels remain
deviating: `mean.y`, `mean.y_last`, and `sqrt1.u`. Their mean absolute error is
2.018, with a maximum of 10.358. Thus 426 channels agree, but the model remains
non-high and the counterexample remains open. The focused command's zero exit
status does not override its comparison result. The complete cohort still
requires new validation; unrelated capability work stays paused.

The fixed 20-model canary in `target/msl/multibody-observation-canary`, at the
same HEAD with worktree digest
`6a91166f0e27748f4711fce1a6bfe8986f5c41ec6d586dfe4cd7e61012b48bdd`,
preserves every phase, simulation status, and comparison band from
`multibody-totality-canary`. All nine compared models remain high, including
all 175 initialization channels; missing, skipped, excluded, nonidentifiable,
and deviating counts are zero. Its eleven existing failures remain visible.
The durable delta is `observation-convergence-canary-delta.json`.

The remaining RMS error is present in the pre-repair trace as well. Its three
channels alias the mean block's held output: `der(x)=u`, with squared voltage
as the input, and `y_last=f*pre(x)` followed by `reinit(x,0)` at each 300 Hz
sample. The first diagnostic preserves the before/after Rumoca and OMC sample
values in `observation-rms-initial-triage.json`. Integral endpoint accuracy,
event pre-state sampling, and reset handling remain competing explanations;
none is yet established as the next defect.

## BDF accepted state and sampled integrals

The RMS investigation finds a discontinuity between the recorded integral one
representable time before a sample and the `pre(x)` value used at that sample.
The time gap is too small for the bounded derivative to explain the state gap.
A reduced model keeps only `der(x) = 50000 + 20000*sin(2*pi*300*time)` and a
300 Hz sample that stores `pre(x)` and resets `x` to zero. On the first tick,
BDF reports left `x=166.6647700240386` and sampled
`pre(x)=166.66516325266724`, across a time gap of `4.337e-19` seconds. The same
source passes on the RK host. OpenModelica runs all 30 resets with exactly
matching event-entry and sampled values; its maximum analytic integral error
is `2.182e-4`, at the same `1e-6` simulation tolerance.

A direct Diffsol regression removes Modelica, FMI, events, and Rumoca's
sampler entirely. For `x'=x`, its first accepted state is
`1.0001414213562374` at `t=0.00014142135623730943`, while native interpolation
at that exact time is `1.000141438235889`. This proves that the first divergent
owner is the numerical dependency's accepted-state publication. Diffsol updates
its backward differences with the Newton correction but publishes the
uncorrected predictor as `state.y`; an event reset then restarts from that
predictor. Rumoca's endpoint-copy special case hid the disagreement from the
common host's endpoint consistency check (SPEC_0038 / SPEC_0044 §6).

The dependency patch publishes column zero of the updated backward differences
as the accepted state. Rumoca removes its endpoint overriding copies and uses
the native extension throughout the accepted interval. This follows
SPEC_0033 §§2–3's first-owner rule and preserves the MLS §8.3.6 sampled-state
and reset semantics. The direct regression and both source-level host variants
pass, followed by all 458 core tests. Evidence is in
`native-bdf-endpoint-{red,green}-1.log`, `sampled-integral-{red,green}-1.log`,
`sampled-integral-omc/analytic-comparison.json`, and `bdf-endpoint-core-1.log`.

`vendor/diffsol/RUMOCA_PATCH.md` records the immutable upstream revision,
published archive checksum, one-line source change, license, and release
limitation. The archive audit verifies that every original package file is
present, with only `src/ode_solver/bdf.rs` changed. A workspace patch cannot
repair registry-published downstream crates: registry publication still needs
an upstream release carrying this fix or a published maintained dependency.
No upstream submission has been made. Origin, canary, and complete-cohort
comparison remain pending; the rectifier counterexample is still open.

The subsequent originating-model run,
`target/msl/multibody-bdf-endpoint-rectifier`, completes in 1.645 seconds and
compares all 429 channels, with all 429 initialization values high and no
missing, skipped, excluded, or nonidentifiable trace. At HEAD
`bc71577f85df24957e5c9ab30fdaf4ed48da4311`, worktree digest
`b8008171426889515029f6c7d99eb9c4b13a585732c126d02744d2797e0941c4`,
426 channels remain high and the same three RMS channels remain deviating
(mean absolute error 2.01643, maximum 13.68547). The integral's recorded left
value and sampled `pre(x)` now agree within `2.558e-13`, so the endpoint defect
is repaired, but it does not close the RMS counterexample.

The fixed 20-model canary in `target/msl/multibody-bdf-endpoint-canary` binds
the same worktree digest and preserves every phase, simulation status, and
comparison band from `multibody-observation-canary`. All nine compared models
remain high, including all 175 initialization channels; missing, skipped,
excluded, nonidentifiable, and deviating counts are zero. Eleven existing
failures remain visible. The delta is `bdf-endpoint-canary-delta.json`.

The next direct kernel regression finds a separate derivative-coordinate
defect. At a known scheduled boundary, `continuous_eval_time` moves physical
time backward by twice the state tolerance. For the smooth derivative
`1e6*time` at `t=1`, the fixture returns 999800 instead of 1000000. It agrees
before the boundary. The repair evaluates the scheduled left limit at the
adjacent representable coordinate, as the scheduled event-entry snapshot
already does, and leaves numerical root-bracket probing unchanged. The
regression fails before the repair and all 426 solver tests pass afterward
(`scheduled-derivative-coordinate-red-1.log` and
`scheduled-derivative-solver-1.log`). Broader and originating-model validation
of this additional repair remain pending.

The additional repair passes all 458 core tests and all-target/all-feature
Clippy for the solver and core packages. Its originating-model run in
`target/msl/multibody-scheduled-derivative-rectifier` completes in 1.517 seconds,
with 426 of 429 channels high, all 429 initialization channels high, and no
missing, skipped, excluded, or nonidentifiable trace. The three RMS channels
still deviate; their mean absolute error increases to 2.55457 and their maximum
error is 14.58486. This is a repaired derivative-coordinate defect, not closure
of the RMS counterexample.

The fixed 20-model canary in
`target/msl/multibody-scheduled-derivative-canary` retains every phase,
simulation status, and comparison band from `multibody-bdf-endpoint-canary`.
At HEAD `bc71577f85df24957e5c9ab30fdaf4ed48da4311`, its worktree digest is
`fd999cb8aa11cfb4b80bcc096e5fa9fd7ec336cfdf3e3f370873b20c7c1791f3`.
All nine compared models and all 175 initialization channels remain high;
missing, skipped, excluded, nonidentifiable, and deviating counts are zero.
The durable delta is `scheduled-derivative-canary-delta.json`.

The earlier rectifier trace also shows firing transitions hundreds of
nanoseconds after the OMC transitions. The source controller composes voltage
zero crossings, `Modelica.Blocks.Logical.Timer`, a gain, and a strict threshold
comparison. Root localization and recognition of the resulting timer deadlines
remain candidates for the residual averaging error. They require a reduced
proof before another production change. A new complete quick run is next to
measure the combined runtime changes across the full cohort and workspace.

## Verification checkpoint after observation and BDF repairs

The complete `multibody-verify-quick-checkpoint-2` archive binds HEAD
`bc71577f85df24957e5c9ab30fdaf4ed48da4311` and working-tree digest
`ae29d3016cdfffa208e9114e7c0ba58b4e294672c0ac756280ab53dec6a754a0`.
The 566-model comparison retains all 141 previously strict-high models, with
142 compared, 16 existing policy exclusions, zero missing traces, and zero
nonidentifiable traces. The rectifier remains the sole actionable model:
426 channels are high and the same three RMS channels deviate. All 14,550
compared initialization channels remain high. No baseline was promoted.

The 42 MultiBody examples retain 33 balanced DAEs, 17 initialization attempts,
16 successful initializations, and 13 strict-high completed simulations. All
5754 compared channels are high, with no missing or excluded comparisons.
The 29 failures retain their previous phase classifications. Outside MultiBody,
the already excluded CenterTap2Pulse `RLV_Characteristic` newly times out;
the complete phase delta also records three changed failure diagnostics. This
execution loss is not hidden by the unchanged strict-high band table.

`verify quick` completed in 2201.2 seconds, failing only its MSL soundness
stage. Workspace lint, all 28 corpus rows, 243 architecture tests, 17 repository
gates, and 7303 workspace tests pass. Documentation tests have 30 passes,
zero failures, and 23 ignored tests. These results precede the next event
relation repair; they do not close the remaining counterexample.

## Rectifier event relation, traced against OMC

The next investigation follows the original rectifier through its first
switching event, using the archived full-cohort traces and the source of
`Signal2mPulse`, `Logical.Timer`, and `Logical.Greater`:

1. At 50 Hz and a 30-degree firing angle, the threshold is 1/6 and the timer
   gain is 100. The first negative timer must stop when its voltage changes
   sign near 1/600 seconds. OMC's settled row sets the timer and its firing
   output to zero. Rumoca's row at 0.0016673391524120351 also sets the timer
   and `greaterNegative[1].u1` to zero, but leaves `greaterNegative[1].y=1`
   against `u2=1/6`. The false pulse persists until 0.0016676665409000395.
2. The retained Solve artifact has the correct root expression, `u2-u1`, at
   root 19, with an `AlgebraicDependent` relation target P359. The visible
   Boolean binding and its output program both read that same P359. This
   rules out an inverted comparison or a mismatched output slot.
3. The completed-step callback selects the crossed root's new side. Event
   iteration then turns off the upstream timer and projects `u1=0`, reversing
   root 19's sign. The old runtime unconditionally excludes the selected root
   from relation refresh and reapplies its override on every pass. It can
   therefore report convergence with a false relation equation. This violates
   the mixed-equation event solve in
   [MLS Appendix B](https://specification.modelica.org/maint/3.7/modelica-dae-representation.html).
4. A reduced source containing only a sine crossing, a conditional timer, and
   a threshold reproduces the pulse with both BDF and RK at unchanged 1e-6
   tolerances. OMC keeps `fire=0` on all 104 rows of the identical source.
   A runtime-only two-root test reproduces the stale Boolean without compiler
   lowering or numerical integration.

Event iteration now releases a selected side when the projected root becomes
strictly nonzero on the opposite side. The surviving selections pass through
post-commit canonicalization, preventing a released selection from returning.
The exact-zero control retains its selected side. Post-commit refresh still
preserves the separate frozen-parameter relation contract. The earlier test
that asserted unconditional override authority now checks both exact-zero
retention and release after a strict sign reversal.

The runtime regression and all 427 solver tests pass. Both reduced source
variants pass, as do all 460 core integration tests. These focused checks do
not establish closure of the original RMS deviation; its next OMC comparison
and the fixed canary delta are still required. The private receipts retain the
first-event comparison, red/green logs, and source-identical OMC CSV hashes.

All-target/all-feature Clippy for the solver and core crate also passes. The
post-commit fixture was extracted to satisfy the function-size limit; all 11
event-iteration tests pass afterward, and the formatting check passes.

The fixed 20-model canary is now complete at
`target/msl/multibody-event-relation-canary`: all phase and simulation statuses
match the preceding canary. The same nine compared traces are strict-high,
with all 175 initial channels high and no missing, skipped, excluded, or
nonidentifiable candidates. This is a focused regression result, not a cohort
coverage number.

The originating-model gate at `target/msl/multibody-event-relation-rectifier`
failed before compilation: the model worker exceeded the unchanged 60-second
source-root loading budget. It attempted no simulations and reports parity
unmeasured. That failed attempt is retained. A separate bounded one-shot
source diagnostic then compiled and simulated the original model. Its first
negative firing pulse is gone, but the comparator still finds the same three
RMS deviations among 429 channels; all 429 initial channels remain high. The
source diagnostic does not replace the failed gate. An attempted replay of the
older DAE JSON was also rejected at deserialization with `array extent must be
a nonnegative literal Integer`; that artifact round-trip limitation remains
untriaged.

## Rectifier switching-time counterexample

The repaired relation did not materially change the RMS values, so it was a
separate observable defect rather than the explanation for their remaining
error. The next source-to-OMC trace follows the first positive firing pulse:

| Quantity | Rumoca source diagnostic | OMC |
|---|---:|---:|
| First positive firing time, seconds | 0.003334224320793803 | 0.003333333334501213 |
| Second voltage-squared mean | 51295.33723503503 | 51309.92209359418 |

The 0.890986-microsecond delay keeps the old circuit branch active across a
voltage-squared jump of approximately 54449.996. Its missing integral,
multiplied by the mean block's frequency of 300, predicts an error of
-14.55426; the observed error is -14.58486. This calculation links root timing
to the accumulated error without adjusting any comparison threshold.

A reduced source integrates `if sin(100*(time-1/300)) > 0 then 10000 else 0`
over 0 to 0.01 seconds. Its exact solution is
`x(t)=10000*max(0,t-1/300)`. Both Rumoca integrators violate the existing 1e-6
state-error scale around the event; BDF still reports zero at
0.003333500000000001, where the exact integral is 0.001666666666674116.
OMC's source-identical 54-row trace has maximum absolute error about 1e-8.
The private `root-integral-omc` receipt retains the source, regression fixture,
CSV, and hashes.

The current host bisects to the session's time-width bound and then applies
the upper bracket endpoint. Its default constructor derives that time bound
from the absolute state tolerance. The bisector satisfies its present bracket
contract; these results expose a numerical accuracy problem across the
derivative jump, rather than proving an inverted root or bad interpolation
inside either integrator. FMI assigns this localization to the common host
before the completed-step callback
([FMI 3.0.2](https://fmi-standard.org/docs/3.0.2/#fmi3CompletedIntegratorStep)).

A bracketed secant prototype at unchanged tolerances repaired BDF but still
failed the RK reduced case. It was removed from production, with its patch and
failure log retained privately. The reduced counterexample remains open; no
root-policy change, tolerance adjustment, baseline promotion, or new
full-model support claim follows from this experiment.

## Root-time policy construction

The next proof isolates default option construction. Two metamorphic tests
fail without changing a model: changing the absolute state-error scale from
1e-9 to 1e-3 changes the default root-location duration by six orders of
magnitude, while multiplying the time coordinates by 1024 leaves that
duration unchanged. The constructor had supplied the absolute state tolerance
where a time-resolution policy was required.

Default batch and live construction now obtain that duration from the
existing `accepted_step_roundoff(start_time, scan_resolution)` policy, bounded
by the scan resolution. The helper that accepted a state tolerance has been
removed. The proposed contract is recorded in SPEC_0038 and SPEC_0044 §6:
state-unit changes leave time resolution unchanged, and time-unit changes
rescale it. This changes internal default root accuracy through dimensional
ownership; requested state-error settings, comparator thresholds, explicit
root-location options, scan cadence, and the host time-roundoff formula retain
their previous definitions. The bisection implementation is unchanged.

Both metamorphic tests and all 429 solver tests pass. The original analytic
switched-integral regression now passes for both BDF and RK with its unchanged
1e-6 settings. Its OMC source and trace remain bound by the earlier receipt.

The first broader core run passed 461 tests and exposed one timestamp snapshot
in the function-assertion message test. The diagnostic payload remains
`f rejects: u=2 y=6`. Its reported time moves from 0.095894271 to 0.095894015,
closer to the analytic crossing `ln(4/3)/3 = 0.09589402415059362`. That test now
checks the typed component assertion, its exact message, and the analytic state
at the reported time against the requested state-error scale. The focused test
passes. The complete core suite passes 462 tests, and the solver and compiler
all-target, all-feature Clippy checks pass.

The original-model run (`multibody-root-time-policy-rectifier`) compares all
429 channels: 426 high, three deviating, zero severe, and all initial channels
high. The same RMS channels remain counterexamples. Their mean absolute error
falls from 2.55 to 0.66, with maximum error 5.44. The fixed 20-model canary
(`multibody-root-time-policy-canary`) retains every status: nine compared
models remain entirely high with 175 high initial channels, and eleven
pre-existing failures remain visible. No baseline is promoted.

The remaining event rows narrow the next investigation. The positive timer's
entry is accurate at 0.0016666666666668587 s. At 0.0033333333333335708 s its
comparator input is 0.1666666666666712, above 1/6, yet the Boolean remains false
until 0.0033333651543144957 s. A separate phase changes voltage sign near
0.005 s but settles its Boolean only at 0.005000332031542508 s. These are
observations to trace through root evaluation and event iteration, not grounds
for a comparator exception or a claim of closure.

## A consumed sample tick backdates a state event

The first timer discrepancy is now reduced to `SampledTimer`: a sine crosses
zero at 1/600 s, a `when` stores its entry time, and a timer enables an
integrator after another 1/600 s. An independent `sample(1/300, 1/300)` counter
provides the coincident scheduled owner. MLS Appendix B and SPEC_0022 SIM-001,
SIM-008, and SIM-010 require the relation to settle at its event coordinate
without executing the sample owner again.

Before the fix, BDF reaches the timer root at 0.003333333333333348 s with an
accurate stored entry time, but the kernel applies the event at the consumed
tick, 0.0033333333333333335 s. The relation becomes false there. Its next
accepted step reaches 0.003334333333333348 s before enabling the integrator,
losing 0.01 from the exact integral. This excludes integration error and timer
lowering as the first cause: the runtime probe directly identifies the
coordinate change in `event_update_application_time`.

OMC completes the identical source with 61 rows and exactly three sample
activations. Its maximum absolute error against the analytic integral is
1.005e-6, compared with Rumoca's 0.01 loss at the delayed event. OMC's located
event row is just beyond the reduced test's 1e-6 analytic bound; the oracle is
evidence for the trajectory, not an exact-arithmetic reference.

The application-time selector now consumes the existing three-way
`StateTimeCoincidence` value. Only an unconsumed clock selects its semantic
tick; a consumed clock retains the component coordinate while the existing
row filter prevents replay. No root-search policy, state-error setting, or
comparison threshold changes. The regression checks the analytic integral
for every trace row under BDF and RK and checks exactly three sample
activations. Both reduced solver cases pass, as do all 429 solver tests, all
464 core tests, formatting, and all-target, all-feature Clippy for the solver
and compiler.

The original-model run `multibody-sampled-timer-rectifier` now compares all
429 channels high, with all 429 initial channels high and zero missing,
skipped, excluded, or nonidentifiable comparisons. This closes the three RMS
counterexamples for this model. RMS mean absolute error is 0.0121 and maximum
absolute error is 0.0953. The first positive firing settles at
0.0033333333333335708 s; the later phase-three voltage transition settles at
0.005000000000000596 s. No comparison policy changed.

The fixed 20-model canary `multibody-sampled-timer-canary` retains every phase
and simulation status and every channel band: nine compared models remain
entirely high, with 175 high initial channels, zero missing/skipped/excluded/
nonidentifiable comparisons, and eleven pre-existing failures. Both runs bind
HEAD `bc71577f85df24957e5c9ab30fdaf4ed48da4311` and dirty-tree digest
`c9243d92d1ce6253763bfd4f3f93b8675ba2cd81be293cffa371573bfaf39ee8`.
These focused results are not a cohort coverage claim. The original model
remains in the next complete 566-model comparison; no baseline is promoted.

## Complete cohort after the consumed-tick fix

`multibody-sampled-timer-full` completes the full 566-model gate with exit 0
at HEAD `bc71577f85df24957e5c9ab30fdaf4ed48da4311`, dirty-tree digest
`5364e55e1d435913b77da2a673065ab92736dcd208f90a140e42e19ea04eecb2`.
All 142 compared models are strict-high, with zero deviating or severe
channels, zero missing traces, 15 skipped models under the existing policy
exclusions, and zero nonidentifiable traces. All 14,550 initial channels are
high. The 141 previously strict-high models remain strict-high; the repaired
rectifier supplies the one additional model. The 15 existing minor channels
across three other models retain their previous classifications.

MultiBody's 42-example scope is unchanged: 33 balanced DAE constructions,
17 initialization attempts, 16 successful initializations, and 13 compared
strict-high simulations containing 5,754 channels. The other 29 examples
remain failures; this runtime fix adds no MultiBody breadth claim.

Raw execution falls from 158 completions to 157. The previously completing,
policy-excluded `RectifierBridge2mPulse.HalfControlledBridge2mPulse` now
reports EX001: BDF step-size exhaustion at 0.013333333333334703 s. This is an
execution regression retained for separate triage, despite having no effect
on the strict-high count. Four other already-failing models change failure
classification; their complete phase delta is retained in the campaign
receipt. There was no retry, policy change, or baseline promotion.

This milestone reruns the focused solver/core suites, Clippy, the fixed
canary, and full MSL comparison. It does not claim a new `verify quick` or
`verify full` pass; those complete workflows remain required before
finalization.

## BDF restart steps below its configured minimum

The excluded half-controlled rectifier's execution regression is independently
reproduced from its source: it fails at 0.013333333333334703 s. The retained
OMC trace has smooth load voltage through this event, as does the prior
completing Rumoca trace. A temporary derivative probe confirms smooth runtime
values: approximately 269.443602 V and 72599.854757 V² for the first two
integrals. This excludes a new voltage discontinuity as the first cause.

At the located root, the RMS integral is only 3.31e-11. Diffsol's initial-step
heuristic chooses approximately 4.56e-16 s, below its configured 1e-13 s
minimum. BDF accepts two steps and then raises `StepSizeTooSmall` while
increasing the step following successful solves. A direct Diffsol test with
the constant derivative 72599.85 and initial value 3.31e-11 reproduces the
exact failure time without Modelica, FMI, clocks, roots, or algebraic refresh.
A second constant-derivative test fails when continuing after a hard stop
whose interval is shorter than the configured minimum.

The numerical owner now applies its configured minimum at the beginning of
each new BDF attempt, then reapplies any closer hard stop. The difference
table is rescaled to the actual new step size. Error-driven reductions retain
the existing minimum-step failure; no model, state-error setting, comparator
policy, or FMI adapter workaround changes. The local dependency patch remains
confined to `vendor/diffsol/src/ode_solver/bdf.rs`, with provenance recorded in
`vendor/diffsol/RUMOCA_PATCH.md`. Current upstream commit
`a33f02a4952c6837979754cab92eef70763a2f41` retains the original interaction;
no upstream submission has been made.

Both direct regressions pass forward and backward. A separate stiff problem
still fails with the expected minimum-step error when its requested accuracy
cannot be resolved, and the prior accepted-endpoint regression remains green.
All 464 core tests, formatting, and all-target/all-feature Clippy for the
Diffsol adapter and compiler pass. The temporary probes have been removed.

The fixed 20-model canary `multibody-bdf-minimum-step-canary` retains every
phase and simulation status and every model band: nine compared models high,
175 high initial channels, zero missing/skipped/excluded/nonidentifiable
comparisons, and eleven existing failures.

The complete `multibody-bdf-minimum-step-full` gate passes with 566 targets,
142 compared strict-high models, zero deviating or severe channels, all
14,550 initial channels high, zero missing/nonidentifiable traces, and 16
skipped models under existing policy exclusions. All 142 previously high
models remain high. The half-controlled bridge completes in 1.75 s, restoring
the raw execution count from 157 to 158; its existing comparator exclusion
remains and it receives no parity credit. Two already-failing electrical
models now report timeout instead of their previous numerical failures.

Both runs bind HEAD `bc71577f85df24957e5c9ab30fdaf4ed48da4311` and dirty-tree
digest `807587dd97ad47a9c3466919fe4be5ae45ba9dc251466ef62f7e693055900a49`.
MultiBody remains at 33 balanced DAE constructions, 16 successful
initializations, and 13 strict-high simulations out of its 42 examples, with
5,754 compared channels. No baseline is promoted. Complete `verify quick` and
`verify full` workflows remain required before finalization.

## PointGravity: initialization guesses evaluated too early

With the execution regression closed and zero actionable trace deviations,
the next original model is `MultiBody.Examples.Elementary.PointGravity`.
Its fixed body positions are `{0,0.6,0}` and `{0.6,0.6,0}`. The Body equations
alias those positions to the integrated `frame_a.r_0` coordinates and compute
point gravity from the radius. Fresh OMC simulation reaches 5 s with 502 rows;
body two starts at `{0.6,0.6,0}` with gravity
`{-0.9820927516479827,-0.9820927516479827,0}`.

The retained Rumoca source/Flat/DAE/Solve diagnostic fails during initialization
with non-finite gravity. The declared body positions have the correct starts,
but the integrated position guesses are zero. Orientation matrices are already
valid. The initialization constraints retain the fixed values; the failure
occurs before those constraints can settle the state coordinates.

The reduced source has `a = b`, `b(start=0.6,fixed=true)`, and `der(a)=-1/a`.
That control passes both BDF and RK. Extracting the equivalent algebraic
`g=-1/a; der(a)=g` makes both fail with a non-finite `g` before initialization.
OMC initializes the exact reduced source with `a=b=0.6` and `g=-1.6666666667`
and completes its trajectory. This distinguishes premature algebraic
evaluation from lost fixed attributes or a derivative-solver failure.

`SolveMeKernel::initialization_solver_y` called the ordinary continuous
refresh while collecting its guesses, before `settle_initialization_system`.
It now copies the retained guess and current state prefix without evaluating
ordinary algebraics. The existing joint initialization solve remains the
owner of transferred pins, dependent starts, parameters, and manifolds under
MLS §8.6 and SPEC_0043's initialization construction contract. No IR contract,
pin classification, tolerance, or source-specific branch changes.

The reduced BDF/RK cases, all 429 solver tests, and all 466 core tests pass.
Formatting, whitespace checks, and all-target/all-feature Clippy for the
solver and compiler pass. The original-model gate
`multibody-initial-alias-point-gravity` reaches 5 s with all 129 compared
channels high, including all initial channels, and no missing, skipped,
excluded, or nonidentifiable traces. Body two's initial position and gravity
match the fresh OMC reference values above.

The fixed 20-model `multibody-initial-alias-canary` retains every phase,
simulation status, and comparison band from `multibody-bdf-minimum-step-canary`.
Its nine compared models and 175 initial channels remain high, with no missing,
skipped, excluded, nonidentifiable, or deviating comparisons; eleven existing
failures remain visible. Both focused runs bind HEAD
`bc71577f85df24957e5c9ab30fdaf4ed48da4311` and dirty-tree digest
`5252b059e681b90c9158b7e30359caf205e6b7edfa322983a702c31a1e172919`.

The first complete-cohort attempt aborted with `ENOSPC` while writing worker
results. It produced no aggregate or comparator result and has no parity
claim. Removing approximately 50 GB of regenerable parser and incremental
compiler caches preserved all source, traces, and verification evidence. A
fresh full-cohort run used the same source, targets, budgets, and comparator.

That complete gate, `multibody-initial-alias-full-after-disk-recovery`, passes
with 566 targets and 143 compared strict-high models. All 142 previously high
models remain high; `PointGravity` is the only new high model. There are zero
deviating or severe channels, zero missing or nonidentifiable traces, and 16
skipped models under the unchanged policy exclusions. All 14,679 initial
channels are high. This run binds HEAD
`bc71577f85df24957e5c9ab30fdaf4ed48da4311` and dirty-tree digest
`1235402343f0ecdae5a1395886d0e6a72f4f95da0dad21f7841745b60dd5367a`.

MultiBody now has 33 balanced DAE constructions, 17 successful
initializations, and 14 compared strict-high simulations out of its 42
examples. Its 5,883 compared channels have zero deviations, with no missing,
skipped, excluded, or nonidentifiable comparisons. The other 28 examples
retain their failures: two Flatten timeouts, six DAE construction failures,
ten Solve timeouts, six structural failures, three simulation timeouts, and
one Instantiate failure.

One already-failing Media model, `SolveOneNonlinearEquation.Inverse_sh_TX`,
changes from DAE parameter-binding error ED019 to Flatten record-layout error
EF015 after cache regeneration. Its simulation band remains absent; this
phase variation is recorded but its cause is not yet established. No previous
successful simulation is lost. No baseline is promoted. Complete
`verify quick` and `verify full` workflows remain required before finalization.

## Complete verification checkpoint after PointGravity

`cargo xtask verify quick` passes under the fixed four-job concurrency budget.
The checkpoint includes workspace lint, full MSL parity, all 28 pinned corpus
rows, 243 architecture tests, 17 repository tests, all 7,317 workspace tests
with none skipped, and 30 passing documentation tests with 23 existing ignored
examples. The complete log and timing report are retained in the campaign.

The MSL snapshot is `multibody-verify-quick-checkpoint-3`, at HEAD
`bc71577f85df24957e5c9ab30fdaf4ed48da4311` and dirty-tree digest
`18569abeb3ac1c904299276276a151693a099b323cd8731cc71a75f6076e24d0`.
Every one of its 566 phase/simulation outcomes and comparison bands is
unchanged from `multibody-initial-alias-full-after-disk-recovery`. It retains
143 compared strict-high models, zero deviating channels, zero missing or
nonidentifiable traces, and 16 skipped models under the existing exclusions.
MultiBody retains 14 compared strict-high examples and all 5,883 channels high,
with no skipped or missing comparisons. This is a successful complete
`verify quick` checkpoint; `verify full` and the remaining MultiBody scope are
still outstanding. No baseline is promoted and no PR is opened.

## Surfaces: forwarded replaceable function interface (in progress)

The original `MultiBody.Examples.Elementary.Surfaces` fails in Instantiate
with EI027. `PipeWithScalarField` declares a replaceable `colorMap` alias for
`Colors.ColorMaps.jet`, constrained by `Interfaces.partialColorMap`, and
forwards that alias to `Advanced.PipeWithScalarField`. `jet` explicitly extends
the interface. The retained OMC baseline completes the original 5-second
model with 502 rows and 336 channels.

The reduced source reuses the existing function-redeclaration fixture:
`PartialF` has input `x` and output `y`; `Double` and `Triple` implement it;
`Consumer` defaults to `Double`; and `Wrapper` forwards its `F = Triple` alias
into `Consumer`. A non-replaceable forwarding alias compiles and returns 6.
Adding `replaceable` and `constrainedby PartialF` reproduces EI027. Both exact
sources, extracted from the Rust fixture strings, compile and simulate in
OMC with `y = c.y = 6` at every row through 0.2 s. This is a confirmed reduced
failure, not a repaired capability.

A temporary predicate trace identifies the same first rejection in the
reduced and original models. For the reduced case, replacement declaration
DefId(110) has explicit constraining DefId(99), exactly the required
`PartialF` DefId(99). For the original, `colorMap` DefId(571) has constraining
DefId(391), exactly the required `partialColorMap` DefId(391). Both pass the
earlier subtype/class checks and fail `class_flags_compatible` when the
referenced declaration's replaceable flag is compared with the standalone
constraint declaration. The caller is
`type_overrides::component_redeclare_validation`, reached through
`resolve_component_nested_type_overrides`. The tracing has been removed.

The repair must distinguish a referenced class's effective constraining
interface from its declaration flags, under MLS §§6.3, 6.6, and 7.3. It must
also preserve validation of the default implementation, constraining
modifiers, and incompatible signatures. Merely replacing the source identity
with the named constraint could bypass a necessary default-type check. These
negative obligations still need reduced evidence before selecting the
production change. No compiler repair or additional model pass is claimed;
the newly added replaceable forwarding regression is intentionally failing
after the successful verification checkpoint above.

The candidate repair now constructs callable reference interfaces in
`inheritance/function_interfaces.rs`, keyed by resolved declaration IDs. A
replaceable reference exposes its constraint only after its actual/default
function satisfies that constraint. Short aliases follow the referenced
interface and apply their binding modifiers through the existing inheritance
helpers. Effective inherited public components feed the existing member and
ordered function-signature comparator. The ordinary declaration flag checker
is unchanged; component and extends function redeclarations share the new
reference constructor.

The expanded pre-fix regression suite records three valid failures: explicit
constraints, implicit constraints, and a bound additional input. The candidate
passes all twelve tests and the complete 473-test core suite at that checkpoint.
OMC accepts the exact implicit and bound-input variants, with four rows through
0.2 s and `y = c.y = 6` throughout. A further extends-forwarding regression
exposes the second caller of the old declaration comparison. After routing that
caller through the reference constructor, all fifteen focused tests pass,
including alias chains and rejection of an impure default under a pure
constraint. The impure fixture explicitly declares its alias impure so that
it exercises compatibility rather than Resolve's earlier purity rejection.

These are candidate-level regression results. Original Surfaces, canary,
complete-cohort, and final verification evidence for this repair are still
pending; no additional MSL parity count or completed capability is claimed.

The final core run passes all 476 tests; the combined contract and
instantiation run passes 689 and 215 tests respectively, with two existing
ignored documentation examples. All-target/all-feature instantiate Clippy,
formatting, and whitespace checks pass. The normal-budget originating run
`multibody-function-interface-surfaces` advances through Instantiate,
Typecheck, and Flatten in 1.67 seconds total, then stops in ToDae with ED019:
`pipeWithScalarField.pipe.colorMapData` evaluation expects Integer but receives
Array. No simulation or comparator runs, so this originating attempt has
unmeasured parity and exits 1. The original EI027 rejection is cleared; the
model is not yet supported.

The fixed `multibody-function-interface-canary` passes with all twenty phase,
simulation, and band outcomes unchanged from `multibody-initial-alias-canary`.
All nine compared models and 175 initial channels remain high, with zero
missing, skipped, excluded, nonidentifiable, or deviating comparisons. Its
receipt binds HEAD `bc71577f85df24957e5c9ab30fdaf4ed48da4311` and dirty-tree
digest `8ed2c9f142ab75d617e70116f2b487266349061ba8da3f53665615ec342dccc4`.
The subsequent complete `multibody-function-interface-full` cohort passes at
HEAD `bc71577f85df24957e5c9ab30fdaf4ed48da4311`, with dirty-tree digest
`7a606f2d678da165d65656988e7c7c6d2c5f1a2a79566485361b2439d5cecf46`.
All 566 targets and all model comparison bands are unchanged from
`multibody-verify-quick-checkpoint-3`: 143 compared high-agreement models,
16 existing policy exclusions, zero missing or nonidentifiable traces, and
zero deviating or severe channels. Across the full cohort, 18 channels retain
the comparator's minor band; the model bands remain high. The only phase
change is Surfaces advancing from Instantiate EI027 to ToDae ED019.
MultiBody retains 14 compared high models and 5,883 high trajectory and
initialization channels, with no minor, deviating, missing, skipped, excluded,
or nonidentifiable comparisons. Its stage counts are now 42 instantiated,
40 flattened, 33 balanced DAEs, and 17 successful solver constructions and
initializations. This completes the function-interface cohort check, not
`verify full` or support for Surfaces.

### Surfaces: constant matrix-prefix indexing

The next original failure is the constant binding of
`pipeWithScalarField.pipe.colorMapData`. The retained Flat artifact in
`.git/multibody-campaign/surfaces-slice-diagnostic/ir-flat.json` has SHA256
`275ae123228370df4599105a8b7b647004c2d5e593c45a0858d443aedf86fe19`.
Binding instance 626 has shape `[32,3]`; its call references function instance
2 and passes scalar instance 625, whose binding is Integer 32. The selected
body retains `jet`'s final assignment `colorMap := cm[1:n_colors, :]`, with
the source range and colon intact. This rejects the competing explanations
of a wrong forwarded body or an array-valued `n_colors` argument.

The first responsible evaluator is
`rumoca-eval-flat::constant::function_eval::apply_single_subscript`: it
previously required every expression subscript to yield one Integer. The
range correctly evaluates to an array of indices, which that implementation
rejects. MLS §10.5 and SPEC_0022 ARR-005/024/025 govern this indexing;
SPEC_0029 §5 assigns Flat constant evaluation to the evaluator crate.

The reduced `function_parameter_slicing` fixture initializes a 3-by-2 matrix
with distinct coordinates and returns `cm[1:n, :]`. Before the repair, that
case reproduces ED019; an explicit matrix and a scalar column-selection
control already pass. OMC accepts all three exact sources and emits the
expected constant parameter values. It eliminates their constant observables
from CSV, so the oracle comparison uses generated initialization XML and
does not claim compared trajectory channels. Source and XML receipts are in
`.git/multibody-campaign/surfaces-slicing-omc`.

The candidate shared subscript evaluator replaces the ordinary-expression
and function-body scalar-only read implementations. It evaluates selectors
in the caller's scope, preserves the order and multiplicity of vector indices,
and bounds-checks each selected coordinate. All three end-to-end regressions
now pass, as do all 140 evaluator tests, including retained versus removed
axes, repeated indices, empty selections, and invalid signed bounds. Broader
core, lint, original-model, and canary validation was pending at that point.

The full core suite subsequently passes all 479 tests; all-target/all-feature
evaluator Clippy and compiler-core Clippy pass. The original normal-budget run
`multibody-constant-slice-surfaces` clears the Integer/Array mismatch and now
fails the retained bounds check: `colorMapData` attempts index 6 in an array
of size 5. It still stops in ToDae ED019, with no simulation or comparison.
`jet` constructs its matrix from five block rows, whose scalar row counts sum
to 32 for this input. The next investigation must distinguish block matrix
concatenation from literal array nesting; bypassing the new bounds check would
hide the incorrect intermediate shape.

The fixed `multibody-constant-slice-canary` passes at HEAD
`bc71577f85df24957e5c9ab30fdaf4ed48da4311` and dirty-tree digest
`6bed78a66188b7f3de4eec331a66b386708f56dd27fa1470f8d905532a1d1e74`.
All twenty phase, simulation, and band outcomes are unchanged from
`multibody-function-interface-canary`. Its nine compared models, 175 trajectory
channels, and 175 initial channels are all high, with zero missing, skipped,
excluded, nonidentifiable, minor, or deviating comparisons. The delta receipt
is `.git/multibody-campaign/surfaces-slice-canary-delta.json`. This completes
Tier 1 for slice reads; no further MultiBody model pass is claimed.

### Surfaces: preserve and evaluate bracket concatenation

The next reduced fixture, `function_matrix_construction`, builds a 3-by-2
matrix from two block rows: two length-2 vectors followed by two length-1
vectors. Returning `cm[1:3,:]` fails with index 3 in size 2 before the repair;
an explicit scalar matrix control passes. The original retained `jet` Flat
body likewise contains five bracket block rows, and both Flat constant
evaluators previously ignored the constructor flag and merely nested each
evaluated operand. This accounts for the original five-row intermediate.

A second regression exposes an earlier representational defect. After
removing only provenance from its comparison, the parser produces identical
constructor structure for `[[11,12],[21,22]]` and `[11,12;21,22]`. OMC accepts
both and records dimensions 1-by-4 and 2-by-2 respectively. It also accepts
the exact block fixture and its explicit control, with all six expected
parameter coordinates. The pre-fix suite has one pass and two failures in
`surfaces-matrix-construction-red-1.log`; the four source and XML receipts are
under `.git/multibody-campaign/surfaces-matrix-omc`. These are parameter-value
comparisons, not complete MSL trace results.

The candidate replaces the ambiguous AST/Flat Boolean with the shared
`ArrayConstructor` enum: element construction, horizontal concatenation, or
vertical concatenation. Parse records the source separator; rewrites preserve
it; expanded comprehensions and materialized arrays remain element
constructors. Fully known operand extents use the common checked-dimension
operation named in SPEC_0041. DAE construction selects its existing checked
promoted-concatenation builtin directly, removing nested-row guessing and its
call-discovery workaround. A shared Flat value constructor promotes operands
and concatenates along the recorded axis. AST display preserves that same
operation. No old Boolean representation or fallback reader remains.

All five focused regressions pass in
`surfaces-matrix-constructor-candidate-3.log`, including nested bracket blocks
and horizontal concatenation of explicit column matrices. This is candidate
evidence only: affected library suites are running, and complete core, lint,
original Surfaces, and canary validation remain pending. The full-cohort
MultiBody count remains the preceding fourteen-model result.


The affected library/integration run then passes 1,681 tests and one doctest
(with nine existing ignored doctests), and the complete compiler-core suite
passes all 484 tests. A private borrowed dimension scope repairs the nested
comprehension shape regression exposed by the library run: lexical scalar
indices have known rank without an invented constant value, and an outer
same-named parameter cannot supply a varying local dimension.

Further review adds two reduced failures in
`surfaces-matrix-scope-red-2.log`: a resolved component with an explicit
InstanceId is incorrectly shadowed by a same-spelled comprehension index, and
an expression subscript unnecessarily requires a compile-time integer value
to determine its scalar result shape. The repair recognizes lexical indices
only when the reference has no concrete instance identity, using the same
predicate for shape inference and value-dependency inspection. Subscript
projection uses the index expression's rank: scalar removes the axis, vector
retains its length, and higher rank remains unsupported. Actual constant
index evaluation retains its bounds checks. All 142 Flat-evaluator tests pass
in `surfaces-matrix-scope-candidate-1.log`.

This remains candidate evidence. The initial lint checks exposed excessive
nesting in checked dimension construction and an oversized AST display
method; both were refactored without relaxing lint policy. Final lint/core,
original Surfaces, fixed-canary, and complete-cohort checks remain pending at
this checkpoint. No additional MultiBody model pass is claimed.


The final affected-package all-target/all-feature Clippy run passes
(`surfaces-matrix-clippy-3.log`), as does the complete compiler-core rerun
(`surfaces-matrix-core-2.log`: 484 passed). Formatting and whitespace checks
pass. The normal-budget original Surfaces attempt is next; the matrix repair
has no original-model or canary result yet.


The normal-budget `multibody-array-constructor-surfaces` attempt clears the
constant binding failure and stops later in ToDae ED019: `v1` has a colon
axis without a call-site equality during function shape proof. Compilation
takes 2.264 seconds; no simulation or comparator executes, so parity remains
unmeasured. This is progress to the next rejected owner, not a model pass.

The fixed `multibody-array-constructor-canary` passes at HEAD
`bc71577f85df24957e5c9ab30fdaf4ed48da4311`, with working-tree digest
`3a5f684d12f4fb8dac00342ac6cc64214db6fc43b641c22960879e00a787826b`.
All twenty phase, simulation, and band outcomes match
`multibody-constant-slice-canary`; all nine compared models and 175 trajectory
channels remain high, with zero minor, deviating, missing, skipped, excluded,
or nonidentifiable comparisons. The retained delta receipt is
`.git/multibody-campaign/surfaces-matrix-canary-delta.json`. Tier 1 for the
explicit constructor representation is complete. A full 566-model milestone
sweep is next; the current complete-cohort count remains the earlier result.

### Matrix-constructor cohort regression and reduced dimension proof

The subsequent `target/msl/multibody-array-constructor-full` sweep completed
all 566 targets but failed its Flatten floor: 482 passed against the required
486. It ran at HEAD `bc71577f85df24957e5c9ab30fdaf4ed48da4311`, dirty-tree digest
`22d63be38ba657693753172b6393649617582b65d83b38abfe274c0f65ce2a8d`.
The comparator ran: 143 models compared high, 16 tracked exclusions, zero
missing or nonidentifiable traces, and zero deviating channels. All 566 model
bands match the previous full run. MultiBody retains 14 high models with all
5,883 compared channels high, but OneAxis now fails Typecheck and FullRobot
times out in Flatten. This is a failed regression gate, not release evidence.
The complete phase delta is retained in
`.git/multibody-campaign/surfaces-matrix-full-status.json`.

OneAxis and four Clocked drive examples share unresolved KinematicPTP2 `nout`
dimensions. The reduced `matrix_reduction_dimensions` suite stops at the first
divergent owner, Typecheck: five literal/local-scalar controls pass, while
`q_end={Constants.pi}` fails ET004 when `pi=2*asin(1.0)` cannot be folded by
the early evaluator. Requiring every array operand's shape exposed a missing
declared-scalar fact; the previous constructor had guessed scalar for unknown
operands. OpenModelica accepts the exact literal and qualified-constant sources
and produces the expected scalar `q=time` trace in both cases
(`matrix-reduction-omc/comparison.json`). The initial test that proceeded into
DAE instead reached a separate unsupported `fill` dimension and is not used
as proof of this Typecheck regression.

The repair under validation supplies scalar-component facts by resolved DefId,
checks that every qualified prefix is a class namespace, and retains array
subscripts throughout the type-alias chain. Governing requirements are MLS
§10.1/§10.4, SPEC_0007 Stage 2, SPEC_0001 identity domains, and SPEC_0033 §2/§6a.
No floor, timeout, comparator tolerance, or target-list change is proposed.

The first declaration-based candidate passed the six original reductions and
230 evaluator/typechecker tests, including negative array-alias and
component-array-prefix controls. Strengthening the function-result control to
use `asin(u)` exposed the same missing fact for a scalar parameter: the core
run had 489 passes and that single ET004 failure (`matrix-reduction-core-1.log`).
The producer now records proven scalar component declarations independently of
variability or foldable value. The strengthened six-test suite passes in
`matrix-reduction-dimensions-candidate-2.log`. OMC also accepts its exact source
and produces `path.q[1]=time` (`matrix-reduction-function-omc/comparison.json`).
The strengthened candidate passes all 490 compiler-core tests
(`matrix-reduction-core-2.log`) and all 230 evaluator/typechecker tests
(`matrix-reduction-libraries-3.log`). All-target, all-feature Clippy passes
after extracting the alias traversal and class collection into smaller helpers
(`matrix-reduction-clippy-2.log`); the shape predicates are unchanged. Original
MSL regression attempts, canary, and cohort validation remain pending.

The nine-model originating run `multibody-declared-dimensions-origin` restores
OneAxis and all four Clocked drive examples to ToDae. It has no completed
simulation or comparator measurement. Four regressions remain in that snapshot:
PositionControlledDCPM reads a scalar record field (`driveData.wMax`), Dryden's
`a` constructor contains scalar powers (`L^2/V^2`), and SimpleLiquidWater /
TestGlycol lose `Medium.nX` during Flatten.

Each remaining case now has a reduced producer failure. The record-prefix
test returned unknown for a scalar field of a scalar record while retaining
the array-record negative control (`matrix-reduction-record-prefix-red-1.log`).
The AST dimension walk had no ordinary-power case at all
(`matrix-reduction-power-red-1.log`). The reduced Medium package passed
Typecheck and failed Flatten with `medium.X: MatrixReductionDimensions.Medium.nX`
(`matrix-reduction-medium-red-1.log`, eight other controls passed).

The candidate now gives Typecheck and Flatten the same immutable
`rumoca-eval-ast::eval::DeclaredDimensions` proof. It follows resolved type/base
identities and preserves declaration/type-alias array subscripts; every parent
reference must prove a namespace or scalar component. No constant's numerical
or string value is needed to establish its declared rank. Ordinary powers
preserve a proven scalar or square-matrix shape only with a scalar exponent;
vector bases and rectangular matrices remain unknown. These rules follow MLS
§10.1/§10.6.7–9. The strengthened nine-test suite passes in
`matrix-reduction-shared-candidate-1.log`; original-model and cohort validation
of this final shared implementation remain pending.

The final shared implementation passes 922 library/integration tests (two
existing ignored tests), all 493 compiler-core tests, and all-target/all-feature
Clippy for AST evaluation, Typecheck, and Flatten. Logs are
`matrix-reduction-shared-libraries-1.log`, `matrix-reduction-shared-core-1.log`,
and `matrix-reduction-shared-clippy-1.log`. The exact record-prefix, scalar-power,
and Medium-string fixtures all simulate successfully in OMC and produce the
expected four scalar channels (`matrix-reduction-extended-omc/comparison.json`).
Formatting and whitespace checks pass. No MSL count is promoted from these
reduced tests.

The final same-list origin run `multibody-declared-dimensions-origin-final`
restores eight of nine models to ToDae, including OneAxis, Dryden, and both
Media models. PositionControlledDCPM still fails Typecheck because `driveData`
is a replaceable record parameter; the current declaration proof deliberately
does not treat replaceable components as known scalar. Its effective-rank
obligation remains open. None of these nine models produces a completed
simulation, so their parity remains unmeasured.

The fixed `multibody-declared-dimensions-canary` passes at HEAD
`bc71577f85df24957e5c9ab30fdaf4ed48da4311`, dirty-tree digest
`f9e5eacffe7110ff80d51f704df928fd3cf4dec140af05db4104d54265ff0d4d`.
All twenty phase and band rows match `multibody-array-constructor-canary`.
Nine models compare high and all 175 trajectory channels are high, with zero
minor, deviating, missing, skipped, excluded, or nonidentifiable comparisons.
The retained receipt is `matrix-reduction-canary-delta.json`. This completes
Tier 1 for the shared declaration/power-shape repair; the remaining replaceable
record case stays visible. The full cohort sweep is next.
