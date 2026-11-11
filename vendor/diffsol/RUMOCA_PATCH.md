# Diffsol numerical corrections

This directory contains the published `diffsol` 0.13.2 crate, with source
changes confined to `src/ode_solver/bdf.rs`,
`src/ode_solver/jacobian_update.rs`, `src/nonlinear_solver/convergence.rs`, and
`src/nonlinear_solver/line_search.rs`. Original authorship and licensing are
preserved in the upstream sources and `LICENSE.txt` (copied from the same
upstream commit because the published crate omits the repository license file).

Upstream source: <https://github.com/martinjrobins/diffsol/tree/7edd02af717287394bac346d4ed74ec141ae5a86/diffsol>.
Published crate SHA-256:
`3690b3729a291ac097453742215a1884fcde8bf480a444fb6287d6357d86baaf`.
The pristine `bdf.rs` SHA-256 is
`c190d0a0fbccbb0ea98b3557283c501b0253e7e1c4767d5bd7f4b3da22e89e6a`.
The pristine `line_search.rs` SHA-256 is
`7c66486751575500d8f8ad858afcc06977b307ceba98a8e72b7a9d8db3ac5eba`.

After Newton convergence, Diffsol updates its backward differences using the
accepted correction, but copies the uncorrected predictor into `state.y`.
Consequently, the accepted state and the continuous extension disagree at the
same time. Periodic resets discard the correction when they restart from that
published state. The patch publishes column zero of the updated backward
differences, the corrected accepted solution used by interpolation.

The direct dependency regression is
`rumoca-solver-diffsol/src/me_integrator/tests.rs::native_bdf_endpoint_matches_its_continuous_extension`.
The source-level regression is
`rumoca/tests/suite_core/sampled_integral_consistency.rs`, which checks
`pre(x)` against the continuous event-entry integral before `reinit(x,0)`.
It fails with the registry dependency on BDF and passes on the RK host.

The second correction applies the configured minimum step before each new BDF
attempt, then reapplies any closer hard stop. The initial-step heuristic can
choose a smaller step for a tiny nonzero state; a hard stop can also shorten a
step below that minimum. Previously BDF accepted such steps and subsequently
raised `StepSizeTooSmall` while increasing the step after successful solves.
The error-driven reduction check is unchanged. Difference-table rescaling
uses the actual new step size, including the exact configured minimum.

Three direct dependency regressions cover tiny initial states, continuation
after a short hard stop (both forward and backward), and preservation of the
minimum-step failure for unresolved dynamics. The originating MSL execution
failure is `RectifierBridge2mPulse.HalfControlledBridge2mPulse`; it remains
subject to its existing comparator exclusion and is not a parity claim.
Upstream commit `a33f02a4952c6837979754cab92eef70763a2f41` was inspected during
triage and retains the original initial-step/minimum-step interaction.

The third correction makes backtracking Newton consume correction-norm history
through the same convergence interface as full-step Newton. The original path
never populated the history used for its contraction estimate. It reevaluated
already converged affine problems and failed to reject rates that could not
meet its iteration budget. Upstream commit
`7036380f908dbd93baa4253d2e0a34aa115cbbb5` retains that behavior in
`crates/diffsol-nl/src/line_search.rs`.

History alone is insufficient: the line search computes the next Newton
correction at its accepted trial point, but has not applied that correction.
The patch carries that pending correction to the next outer iteration, records
its norm once, and applies it before returning a convergence certificate for
the remaining error. Armijo conditions, tolerances, Jacobian/timestep estimate
resets, and the iteration budget are unchanged. Rejected line-search trials do
not advance convergence history.

Four direct dependency regressions in
`rumoca-solver-diffsol/src/me_integrator/convergence_tests.rs` cover affine solve
reuse, early rejection of inadequate contraction, retained backtracking, and
accuracy against a manufactured nonlinear root. A history-only negative control
returns `0.0098039` for an exact root of zero when its requested nonlinear bound
is `0.0002`; the corrected iteration applies the pending correction and meets
that bound with three residual evaluations, including the fresh returned-point
check below, instead of four.

The fourth correction scopes the nonlinear-failure budget to one attempted
accepted step, as the existing error-test budget already does. Lifetime
statistics remain cumulative. With measured Newton contraction enabled,
`OvervoltageProtection` stopped after 804 successful steps because 51 recovered
failures had accumulated across 50 distinct steps; no individual step had more
than two failures. The configured limit of 50 is unchanged. The direct
dependency tests in `me_integrator/failure_budget_tests.rs` require accurate
continued integration after earlier recoveries and retain rejection of a step
with persistent failures. Upstream commit
`7036380f908dbd93baa4253d2e0a34aa115cbbb5` retains the cumulative-limit check in
`crates/diffsol/src/ode_solver/bdf.rs`. A per-step convergence-failure budget is
also the documented [CVODE recovery contract](https://sundials.readthedocs.io/en/v7.5.0/cvode/Usage/).

The fifth correction keeps the continuous extension of the accepted step.
After accepting a step, `step` may change the order and rescales the backward
differences to the next step size, and its closing `handle_tstop` rescales them
again so that the next step lands on a nearby hard stop. `interpolate` evaluated
that next-step table. In exact arithmetic every rescale re-represents the same
polynomial, but evaluating the previous accepted point from a table rescaled to a
much smaller step is an extrapolation far outside its nodes: at order k the
interpolation weights grow like (h_old / h_new)^k / k!, which amplifies the
table's rounding error. A stage-instrumented run of `CanyonRun.Run` under BDF
showed the accepted update, the order change and the
step-size rescale all exact to 1e-8; only the closing stop clamp, which shrank
the step from about 0.015 to 5.93e-6 at order 5 (about 1e14 amplification),
moved the previous-point value, by 2.0e-4 on a state of 63.6. The Rumoca host
requires both endpoints of every accepted interval to agree with the accepted
states, so the run failed its left-endpoint check at t = 20.085 with a relative
disagreement of about 3e-6 against a bound of 1e-6. The patch copies the
differences, step size and order immediately after the accepted difference
update. While the state remains at that step, `interpolate` and
`interpolate_dy` use the copy, which reproduces the previous accepted point
exactly (`D0 - D1` is the previous column zero, and every higher term vanishes
there). Any state change or replacement discards the copy. Output and
sensitivity interpolation, and `state_mut_back`, are unchanged.

The direct dependency regression is
`rumoca-solver-diffsol/src/me_integrator/tests.rs::native_bdf_previous_point_matches_its_continuous_extension`.
It requires at least one order change, and also checks the previous point after
a hard stop just beyond the accepted time shrinks the step at order four or
more. The source-level regressions are
`rumoca/tests/suite_core/bdf_episode_reset_intervals.rs`: periodic episode
resets under a sampled controller, and the same run split into differently
sized advance calls.

The workspace patch makes local, CI, native, and Wasm builds use the same
corrected source. A registry-published Rumoca crate cannot rely on a workspace
`[patch]`: registry publication still requires an upstream release containing
these fixes, or publication of an explicitly maintained patched dependency.
These patches have not been submitted upstream. Replace this directory with a
released dependency only after all direct numerical and source-level
regressions pass against that release and the complete MSL comparison passes.

The accepted state error test now uses the NDF error coefficient for the
current order, matching the constructor's one-based coefficient table, the
predicted-order estimates, and the output/sensitivity error tests. Previously
it used the preceding index (the order-zero sentinel at order one). This was
stricter at orders one through three and looser at orders four and five; the
correction is not a uniform tolerance tightening. The direct public-API
regression is `me_integrator/ndf_error_tests.rs` in
`rumoca-solver-diffsol`, which runs in ordinary workspace library tests. It
checks an analytically determined first-order correction inside the NDF error
budget that the sentinel incorrectly rejects. No configured tolerance or
trace-comparison policy changes, and no DCPM parity claim follows from it.

The corrected order coefficient exposes a separate hard-stop cache defect:
a shortened final step updates c=h*alpha but could retain the factorization
of I-c_old*J under the ordinary reuse threshold. A distinct truncation event
now rebuilds I-c*J while preserving the existing RHS-Jacobian refresh
schedule and reusing that Jacobian when it is not due for refresh. The existing
`shortened_bdf_step_preserves_the_stiff_voltage_solution` regression is
unchanged: it fails with only the order correction and passes when the
shortened-step matrix is rebuilt. Tolerances and the analytic bound stay fixed.

The additional accuracy corrections use the solved candidate, rather than the
previous accepted state, for the relative state-error weights. For
`y'=-1000+0.001*t`, `y(0)=1`, the trial at `t=0.001` has exact value `5e-10`.
The old weights accepted `8.4388e-10`, whose error exceeds the new coordinate's
`1e-10 + 1e-6*abs(y)` budget. The corrected check rejects that trial. This
matches the candidate-state scaling in
[SciPy's BDF implementation](https://github.com/scipy/scipy/blob/v1.16.0/scipy/integrate/_ivp/bdf.py).

Newton convergence now requires current-solve contraction evidence before a
large first correction can rely on a retained rate. After applying a correction
whose estimated remaining error permits success, backtracking evaluates a fresh
linearized correction at the returned point. A failed check continues the same
bounded solve. An exact affine solve followed by a nonlinear root, and a
continuous piecewise-affine root whose slope changes under a cached Jacobian,
previously returned points outside the unchanged correction budget. The direct
regressions keep those inaccurate successes visible and permit the typed
nonlinear failure that allows the caller to refresh its Jacobian. This check
adds one residual evaluation to a successful returned-point certificate.
Replacing a BDF state also resets the equal-step counter associated with its
discarded difference history.

The accepted interval's native interpolating polynomial is additionally checked
against the ODE at its midpoint before committing the step. The residual
`P'(t)-f(t,P(t))`, multiplied by the actual step duration, is in state units and
uses the same requested tolerances and nominal scaling as the state error test.
The larger of this sampled defect and the ordinary NDF estimate controls
acceptance. A stiff quadratic tracking solution exposes amplified derivative
error that an endpoint state estimate misses. An exact stiff ramp is the
negative control: it retains its large step rather than imposing a stiffness-
based step cap. This is a sampled local accuracy guard, not a uniform residual
certificate or an arbitrary-model global error proof. The direct tests are in
`me_integrator/ndf_error_tests.rs` and `me_integrator/convergence_tests.rs` in
`rumoca-solver-diffsol`; SPEC_0044 ME-INT-004 through ME-INT-008 state the
corresponding numerical obligations.

Current upstream commit `1ba9192337f0c39d822b378b970b036fe36c7553` retains
previous-state relative weighting in `crates/diffsol/src/ode_solver/bdf.rs`,
retained first-iteration convergence estimates in
`crates/diffsol-nl/src/convergence.rs`, and the backtracking path without the
fresh returned-point correction check in `crates/diffsol-nl/src/line_search.rs`.
The changes here leave configured tolerances and vector RMS norms intact.

The sampled `P'-f` guard is restricted to identity-mass ODEs. General
mass-matrix/DAE equations retain their existing NDF error test: subtracting
`f` from `P'` there would test a different equation. The exact control
`2*y'=2`, `y(0)=1`, retains its full 0.01 step with zero error-test failures.

A nonfinite midpoint defect remains a rejected trial. Its rejection factor
uses the configured minimum shrink before rescaling retained difference
history, rather than propagating NaN through the power/comparison into the
step size. A direct stiff-quadratic control has valid endpoints but a coarse
midpoint outside its RHS domain; both NaN and infinity refusals must recover
at a finite smaller step and continue with finite dense output.
