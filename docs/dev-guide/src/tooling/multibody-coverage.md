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
