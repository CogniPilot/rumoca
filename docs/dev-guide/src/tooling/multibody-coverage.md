# MultiBody coverage work

This is the working evidence ledger for `multibody-library-coverage`, based on
main commit `97eb3ab74b3e11264ab2000437eb47df1a57214d`. Work is in progress;
complete MultiBody support has not been established.

## Latest complete measurement

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

The numerical follow-up below restores eleven of the twelve lost high
models in a focused comparison, but a new complete measurement is still
required. The remaining single-phase inverter event failure is unresolved.
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
