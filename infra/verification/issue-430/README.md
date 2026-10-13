# Issue 430 verification ledger

This change is stacked on PR #421 at
`da102156cf4496e8a2255e905ebe66b859e339db`. Earlier observations at
`6ce369dd2f4c961704a05c9f3163cda90480fb79` are historical, not evidence for
the current stacked base. Artifact names below are relative to
`$HOME/scratch/rumoca/issue430-20261013`.

## Reproduction inputs

| Source | Immutable revision | Attribution |
| --- | --- | --- |
| [ALSETLab/rumoca-drone-library](https://github.com/ALSETLab/rumoca-drone-library) | `62d6d12f216a89f2df12b26b312d61ec411f86b2` | BSD-3-Clause reproduction, DroneHoverLoop and root adapter |
| [ALSETLab/Modelica-Drone-3D-FMI](https://github.com/ALSETLab/Modelica-Drone-3D-FMI) | `6fbfe933af0c878f9308c38eb93e588a1fb5cdd9` | BSD-3-Clause; Luigi Vanfretti, 2019 |
| [Modelica/ModelicaStandardLibrary](https://github.com/modelica/ModelicaStandardLibrary) v4.0.0 | `6626538a236a34a390655b55d5e8c906bfe31772` | Modelica Association; BSD-3-Clause |

`downloads/reporter400` combines Modelica, ModelicaServices, Complex.mo,
DroneLibrary and DroneHoverLoop.mo in one source root. It applies the exact
reporter's Math external-builtin-to-algorithm substitutions and the three
missing empty DroneLibrary package shims described in the pinned
`tools/build_rumoca_roots.py`. Official ModelicaServices 4.0.0 already lacks the
OpenModelica-specific service calls targeted by that adapter. No equations,
starts, tolerances, or solver settings were changed to repair the model.
OMC was run on this same adapted root.

The unadapted combined root has an independent external Math derivative
structural failure and does not reach initialization. It is not a substitute
for the reporter's executable input context.

## First divergent producer

On the clean latest stacked base, compilation succeeds and initialization
fails at lambda 0, continuation step 0.125:

```text
row=2178 target=droneChassis1.bodyShape.frame_a.R.T[1,3]
value=-4.166877e0 ratio=4.166877e6 norm=4.166877e0
row_scale=1.000000e0 scaled_tolerance=1.000000e-6
```

Artifact: `baseline-reporter-loop-da102.log`. The reduced FixedClockDomain
model fails at projection row 3, target q, with a NaN residual
(`reduced-before-da102.log`). The equivalent source is retained by the
`initial_relation_truth` simulation tests.

The earliest divergence is a buffered relation read during the first
simultaneous initialization iterate: cold false event memory selects the
flight-time ramp's wrong branch. The compiler previously lowered every such
read to its transient held memory. Algebraic refresh runs before the driver
can update relation memory; the wrong branch reaches a rotation projection
which cannot converge. A controlled historical probe with exactly unchanged
states, starts and tolerances succeeds when current relation truth is settled
before that projection. The fix evaluates the authored comparison at every
initialization iterate, including projected unknowns and algebraic operands,
then uses held event truth after initialization.

| Competing hypothesis | Rejection evidence |
| --- | --- |
| Zero orientation declaration start | Selected quaternion scalar seed is 1; unchanged starts succeed when only relation evaluation ordering changes. |
| Stale reset state | PR #421 baseline simpleHoverTest advances four seconds, changes 4436 of 8305 visible values, then resets all values bit-identically and advances again. |
| Math root adaptation changes the ramp failure | Latest adapted roots reproduce the exact reported rotation entry and value; OMC accepts and simulates these same roots. |
| A looser projection tolerance is necessary | The repaired initialization uses unchanged 1e-6 projection tolerance and all original starts. |

## Fixed 20-model canary (Tier 1 only)

Baseline uses MSL v4.1.0 and OMC `a96aa1a-cmake`, exact OMC reference context
`12ae478f4a9ff9e3e5e34945452e90f9e8d36650b90459b140dd3329f42dff9d`.
Target file: `infra/verification/msl-canary-20.json`.

The first clean da102 attempt completed stages but could not run the comparator
because the modelica shell lacked Node. `canary-before-da102.log` retains that
failed comparator discovery. It is not a passing parity run. A separate full
run with Node 24 available is retained as `canary-before-da102-node.log`; that
run overwrote the results JSON in `canary-before-da102`, so those JSON files
belong only to the second run. No per-model failures were selectively retried.

| Metric | Clean da102, independent Node-corrected run | Change |
| --- | --- | --- |
| Parse / flatten / DAE / Solve | 20 / 20 / 20 / 20 | pending |
| Simulation completions | 16 | pending |
| Compared / high / near / deviation | 15 / 15 / 0 / 0 | pending |
| Typed skips / missing | 1 / 0 | pending |

This partial tripwire is not full-cohort parity. Final checks and after evidence
will be appended before the draft becomes ready for review.

## Latest focused after evidence

The latest stacked-base build runs DroneHoverLoop for four seconds without the
reported projection failure. `focused-trace.json` compares 402 Rumoca output
points against 404 OMC points from the exact same reporter400 roots, using
linear interpolation on the 401-point 0.01-second grid and the final sample at
repeated exact timestamps. All 8309 runtime channels are finite. All six
inspected initial values agree exactly. Maximum altitude difference is
0.0001035544 m; the previously failing rotation entry differs by at most
1.7221e-8 and the matrix orthogonality residual stays below 1.8641e-10.
These are focused observations, not a full-cohort certification.

Raw artifact SHA256:

- `loop-after-da102.json`: `ac322f39461131ac588985dd1adbcbef95e57700980c47302ba91e9c6ad636ed`
- `omc-reporter400/DroneHoverLoop_res.csv`: `e4103db971b0f54e05d1d4eb543c49daf11eb1d06e5bb6bc3f30f4ca0f658b06`

Both real models exercised the live FMI Model Exchange session reset after four
seconds of changed flight state and warmed evaluation caches:

| Model | Visible values | Changed at 4 s | Different after reset to 0 | Advance to 4 s again |
| --- | --- | --- | --- | --- |
| DroneHoverLoop | 8309 | 4453 | 0 | success |
| DroneLibrary.Tests.simpleHoverTest | 8305 | 4436 | 0 | success |

Reset evidence covers four seconds, not the original reporter's 27-second
failure point. PR #421 already restored pristine reset state; this PR repairs
initialization relation reads shared by session creation and reset.

Latest checks:

- `cargo fmt --check`: passed.
- `cargo test --profile msl-fast -p rumoca-ir-solve -p rumoca-phase-solve`:
  900 + 267 unit tests, 3 integration tests and 9 doc tests passed.
- `cargo test --profile msl-fast -p rumoca-sim initial_relation_truth`:
  all five regressions passed; given/projected/algebraic/function-argument
  cases run both BDF and RK. The lazy-call case deliberately withholds the
  typed call table, proving held truth never executes its inactive call while
  initialization requires that call's checked owner.
- The broader sim library run passed the other 210 tests; its old fifth test
  made an incorrect assumption about low-level assertion/domain observation.
  The corrected focused suite above replaces that test and is green.
- Pinned modelica_models CI revision
  `a41f7c0c00b55c1bf54f03c9b66b901ba8e43c6f`: Tests.All compile and
  Tests.LieGroupTests.SO2 / SE2 assertion simulations at t0 passed. Exact
  downloaded archive SHA256:
  `cc10b1a8994d31ab7f20ece0d9d99aca927c56a98ba9dd323d7bac267e98c578`.

Standard workspace, coverage, full MSL and ModelicaTest gates remain pending;
the PR stays draft until required validation is accounted for.
