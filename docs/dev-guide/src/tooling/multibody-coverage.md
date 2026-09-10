# MultiBody coverage work

This is the working evidence ledger for `multibody-library-coverage`, based on
main commit `97eb3ab74b3e11264ab2000437eb47df1a57214d`. Work is in progress;
complete MultiBody support has not been established.

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
