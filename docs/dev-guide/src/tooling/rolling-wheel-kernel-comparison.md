# RollingWheel: generated OMC C and Rumoca's integration kernel

This investigation compares the xtask-generated OMC executable with Rumoca's
requested-state/derivative-alias kernel. The latest focused changes, based on
`0dc3858bd6f0a8864f354df4560157395634c192`, reduce repeated numeric work while
preserving the previous trace byte for byte. The OMC performance gap remains
open; these focused results do not establish a new cohort count.

## Latest focused result: shared arithmetic and pure-call inputs

Two producer defects account for unnecessary geometry evaluation:

- Solve program 224 issued 48 sine/cosine operations for just six distinct
  operator/operand-register pairs. Programs 200, 201, and 213 each issued 24
  for four pairs. Scalar construction now retains each pure unary result once
  within its exact lowering context; numerical AD shares its emitted primal
  and tangent arithmetic under the same rule. Arithmetic order and compact
  tensor operations remain unchanged.
- Native pure-call storage previously lasted for one caller invocation.
  Repeated Jacobian directions therefore reran identical function inputs.
  Checked pure-call construction now issues a complete input coordinate from
  its closed input/output/local environment and earlier pure-call owners.
  The native helper compares every input cell bitwise and retains one successful
  ordered result. Directional coordinates include all tangent cells and have
  separate storage. Numerical failures invalidate the entry; assertion
  predicates remain outputs that the caller checks on every invocation.
  The previous owner-only invocation cache is removed.

These rules are grounded in SPEC_0007/SOLVE-C51/C56 and SPEC_0043 §6a.
No source function bodies are matched or merged, and no runtime schedule is
discovered. Native reuse needs a numerical input comparison, so the compiler
issues the closed-input relation and the final execution adapter implements
its exact coordinate check. The IR keeps tensor extents compact, including
input tuples larger than a native `u32` addressable range.

Both reduced regressions failed before implementation: scalar/AD construction
emitted four trig calls where two suffice (`unary-reuse-red-1.log`), and three
identical native invocations executed sine three times
(`pure-input-reuse-red-1.log`). Tests also cover changed seeds, signed zero,
whole tensor inputs, assertion predicates, repeated singular solves, different
arguments within one caller, independent compiled tables, and wire replay.

| Measurement | Previous auxiliary-primal kernel | Current kernel |
|---|---:|---:|
| Pinned focused gate, declared Sim | 2.164149 s | 1.568135 s |
| Separate actual-worker profile, declared Sim | 2.224935 s | 1.506166 s |
| Forwarding-shim sine + cosine results during Sim | 32,623,972 | 5,187,414 |

The reduction in trig work is about 84%; the measured Sim reduction is about
28–32%. Instrumented counting durations are not benchmarks. The new worker
SHA-256 is `0ce8e06306ddafc641727942e5dd79dd594304c910755fd32a53a02f6ecab541`.
Both the profiled and counted traces are byte-identical to the preceding
auxiliary-primal trace, SHA-256
`f46cbcf4bf00620007139eb17416c2ece8c3ddf5665e577a3c4e22ef2dadc47c`.
The profile collected only 42 samples, so it is insufficient to rank remaining
hotspots precisely. OMC's earlier Sim and total-process timers have different
boundaries; the table does not claim a directly comparable OMC speed ratio.

The first focused gate, `target/msl/multibody-pure-input-reuse-origin`, failed
its 60-second worker SourceRootLoad startup limit and measured no parity.
That failure is retained. A separate diagnostic allowed 180 seconds overall
for source loading and artifact emission, with the same 12-second simulation
budget. The fresh normal gate
`target/msl/multibody-pure-input-reuse-origin-2` passes: one model compared,
184 trajectory and 184 initial-condition channels high, with zero skipped,
missing, nonidentifiable, or deviating results.

The fixed `target/msl/multibody-pure-input-reuse-canary` has no phase, status,
or band changes against `target/msl/multibody-auxiliary-primal-canary`: nine
models compared high, all 175 initialization channels high, zero skipped,
missing, nonidentifiable, or deviating results, and eleven unchanged refusals.
`pure-input-reuse-canary-delta.json` records artifact digests and worktree digest
`e333300748c075ba3708f0749572e75e452536c98726a4e6d610e4603c73b8dc`.
The 515 IR/Solve/native library tests, 99 `rumoca` library tests, and 582 core
regressions pass. All-target/all-feature Clippy for the three changed crates
plus `rumoca` also passes. Combined quick/full and the next complete cohort
comparison remain pending.

A temporary counter probe on the previous kernel ruled out callback count as
the whole explanation: Rumoca made 3,267 RHS and 742 directional requests in
2,100 BDF steps, with 15 error-test failures and no nonlinear failures. OMC's
earlier run reported 1,329 ODE requests and 1,028 steps. The probe's trace was
byte-identical, and the probes were removed. Evidence is
`callback-count-origin-3/callback-counts.json`.

Rumoca still retains fourteen integrated coordinates and a 24-unknown dynamics
block with sixteen tears, versus OMC's eight coordinates and six tears.
These remain structural optimization targets requiring independent proofs.

## Measured discrepancy

The candidate's isolated `sim_run_seconds` is **2.324479 seconds**. Its `perf`
capture spans only the declared Sim phase: 447 samples, no lost samples,
11.19% in cosine and 6.49% in sine. The preceding validated candidate was
about 1.248 seconds. Correct requested-state selection has therefore exposed
a slower kernel that still needs optimization.

A process-local forwarding libm shim counts calls without changing their
arguments or returned values. Rumoca counting is enabled on observing Sim
start and disabled on completion, using 1 ms polling; it can miss boundary
calls. OMC counting covers its whole process, including initialization and
output. Neither instrumented duration is used as a benchmark.

| Calls | Rumoca Sim window | OMC whole process |
|---|---:|---:|
| `sin` | 16,935,194 | 6,270 |
| `cos` | 16,935,194 | 6,273 |
| `sincos` | 0 | 14 |
| Trigonometric results, counting `sincos` as two | 33,870,388 | 12,571 |

Both output traces are byte-identical to their uninstrumented references.
Even with the narrower Rumoca counting window, it computes approximately
2,694 times as many trigonometric results. This is evidence of duplicated
numeric work, not a claim that trigonometry explains the entire time ratio.

## OMC's generated execution path

The executable is under
`target/msl/multibody-selected-jvp-origin/omc_sim_work/`, with basename
`Modelica.Mechanics.MultiBody.Examples.Elementary.RollingWheel`.
Its `_03lsy.c`, `_09alg.c`, and `_12jac.c` are byte-identical to the earlier
private `rolling-wheel/omc/` copies. The main C differs only in resource
directories and the generated GUID; its equation and callback code agrees.

| Generated owner | Work performed |
|---|---|
| Main `.c:1954`, `functionODE_system0` | Fixed schedule of 44 equation functions |
| Equations 635, 638–640, 644, 647 | Compute three sines and three cosines of the selected angles |
| Equations 632–634 | Copy the selected angular rates into angle derivatives |
| Equations 663, 664, 677, 678 | Solve kinematic linear systems of sizes 2, 3, 2, 3 |
| Equation 731, `.c:972` | Solve the six-dimensional torn dynamic system |
| Equations 748–749 | Solve a 2×2 angular-acceleration map and assign the remaining derivative |
| `_03lsy.c:584`, `residualFunc731` | Assign six tear inputs, execute 19 causal equations, then evaluate six residuals |
| `_03lsy.c:1750` | Register system 731 as linear with a generated analytic Jacobian |
| `_12jac.c:950`, `functionJacLSJac1_column` | Execute 25 compiled tangent equations using existing primal geometry values |
| `_09alg.c:73`, `functionAlg_system0` | Separate schedule of 62 algebraic/observation equation functions |

OMC's six dynamic tear variables are the three world-frame contact forces,
vertical body acceleration, and the two second derivatives of the horizontal
contact-offset coordinates. Other acceleration/force variables are computed
by the local causal schedule. The matrices can depend on state, so this is
not evidence that numerical factors remain valid across different states.
The reusable object is the compiled computation and its dependency structure.

OMC integrates eight coordinates: x/y position, three angles, and three
angular rates. The previous 40-run measurement reports 1,028 steps, 1,329
ODE calls, 42 Jacobian evaluations, eight error-test failures, and no
convergence-test failures. Its median reported simulation time was 16.58 ms
and total runtime 50.66 ms. These timer boundaries differ from Rumoca's
Sim measurement; the call census is the stronger direct evidence here.

## Rumoca's corresponding work

The candidate now retains all eight requested states, plus six dependent
body-velocity/joint-position coordinates. Its derivative refresh closure owns
113 algebraic scalar targets, five exact-assignment stages, and seven
projection stages. The coupled dynamic block has 24 unknowns and still
retains 16 tear variables. The complete observation closure owns 888 scalar
targets; these are separate construction-issued plans.

Three hot tensor residual programs, 200, 201, and 224, each contain 17 typed
pure-call sites. Program 224 contains 431 instructions, including 22 matrix
multiplies and eight cross products. Their generated geometry is executed
again in directional programs. The native call table compiles value and
directional functions separately; the fact that code is compiled does not
establish that its intermediate values are reused across residual rows or
Jacobian seeds. The measured trigonometric count confirms that they are not
being reused sufficiently.

Relevant code owners are `rumoca-phase-solve/src/lower/typed_functions/`,
`rumoca-phase-solve/src/lower/scalar/`,
`rumoca-exec-cranelift/src/emit/typed_program.rs`, and
`rumoca-solver/src/runtime/solve_runtime/{refresh_execution,sensitivity}.rs`.
The existing seed-linearization cache already reuses algebraic
linearizations at fixed coordinates. It does not remove repeated primal
geometry embedded in separate residual and tangent programs.

## Next proof and implementation targets

The first concrete producer defect is in structural auxiliary reconstruction.
For a source-owned linear system `A*q=b`, continuous differentiation recursively
reconstructed `q=solve(A,b)` inside the tangent solve. That also materialized
the coefficient geometry through state anchors again. OMC's preceding
kinematic schedule already stores these primal values for its dynamic rows.

A reduced varying-matrix contact fixture reproduces three nested linear
solves in a second derivative, where the source primal value and two tangent
solves suffice (`auxiliary-primal-red-2.log`; the first attempt was a fixture
compilation error). The candidate retains the original primal coordinate in
`A*q'=b'-A'*q` and ordinary algebraic coefficient reads. State-only manifold
reconstruction still expands through proved anchors, and every tangent solve
retains its checked nonsingularity domain. This follows SPEC_0007's auxiliary
profile rather than introducing a runtime value cache. The seven auxiliary
and nine contact regressions pass, followed by all 962 library/core tests
(`auxiliary-primal-libraries-1.log`).

The focused `target/msl/multibody-auxiliary-primal-origin` gate compares one
model: all 184 trajectory channels and 184 initialization channels are high,
with zero skipped, missing, nonidentifiable, or deviating traces. Sim time is
2.164149 seconds; a separate actual-worker perf run measures 2.224935 seconds.
The latter keeps the earlier profile's measurement boundary. Hot programs
200/201/224 fall from 17 to three pure calls each and from 271/273/431 to
236/238/406 instructions. The derivative closure and state counts are unchanged.
The forwarding shim counts 32,623,972 trigonometric results, about 3.7% fewer;
both instrumented traces remain byte-identical to their uninstrumented
references. This removes real work but leaves most of the original discrepancy.
Evidence is in `auxiliary-primal-origin-1/`, `auxiliary-primal-counts-1/`, and
`auxiliary-primal-triage.json`. All-target/all-feature Clippy passes for the
structural, Solve, and public compiler crates (`auxiliary-primal-clippy-1.log`).
The fixed `target/msl/multibody-auxiliary-primal-canary` has no phase, status,
or band changes against `target/msl/multibody-requested-aliases-canary`: nine
models compared high, all 175 initialization channels high, and zero skipped,
missing, nonidentifiable, or deviating results. Eleven targets retain prior
refusals. `auxiliary-primal-canary-delta.json` records the artifact digests and
worktree digest `17bfd2df24a6e1fb11a938fe3cac4bff46bf304a0ec06fef80e1a919a936cb0e`.
Combined quick/full and a new complete cohort comparison remain pending.

A second forwarding-shim experiment records the immediate return address for
each trig call and resolves it against the same process's Cranelift perf map
(`auxiliary-primal-callers-2/`). The total is again 32,623,972 results; both
instrumented traces are byte-identical to their uninstrumented references.
Typed directional functions account for 19,860,192 results and row directional
kernels for another 6,367,872: about 80.4% is in derivative evaluation.
Row 224 alone accounts for 2,661,120 directional and 1,650,864 primal results.
The first caller-count run lacked the JIT symbol map and is not used for
attribution. This localizes the next optimization to sharing primal geometry
across derivative directions, while preserving the exact linearization point.

1. Construct a shared tensor computation for state/parameter-dependent
   geometry, and have residuals and tangent programs consume its results.
   Reuse must follow typed dependencies and the exact evaluation coordinate;
   effects, assertions, event changes, and parameter changes remain observable.
2. Reduce the coupled dynamics through a compiled causal schedule and smaller
   linear solve. Compare its equations and tear set directly with OMC's six
   unknowns; preserve singularity/rank checks.
3. Remove the six dependent integrated coordinates through a proved state
   reduction. Their absence from OMC is a lead, not a sufficient proof by
   itself.
4. Keep observation-only computation outside integrator callbacks, and measure
   actual callback/solve counts alongside elapsed time after each change.

The originating focused gate, `target/msl/multibody-requested-aliases-origin`,
compared one model: all 184 channels and all 184 initialization channels were
high, with zero skipped, missing, nonidentifiable, or deviating traces.
This is focused regression evidence only. The latest full-cohort result
remains the run recorded in [MultiBody coverage](multibody-coverage.md).

## Reproduction evidence

Private evidence is under `.git/multibody-campaign/rolling-wheel/`:

- `requested-aliases-origin-1/`: actual prepared DAE, Solve, trace, perf data,
  and phase-specific timings; worker SHA-256
  `d9164677ddae3067aaa8fd833c54a8e000bfb5bfdf9d41ffea69bb42fc3d70f8`.
- `omc-generated-c-walk-1/`: generated-C inventory, equation comments,
  line-preserving readable views, Rumoca kernel inventory, forwarding-shim
  source, call-count driver, logs, and `trig-counts.json`.
- `omc-perf-repeated-1/`: the earlier unmodified executable's 40-run timing,
  call-statistics, and whole-process perf evidence.

The shim is enabled only for the diagnostic subprocesses; no instrumentation
is installed in the compiler or runtime.
