# Diffsol numerical corrections

This directory contains the published `diffsol` 0.13.2 crate, with source
changes confined to `src/ode_solver/bdf.rs`. Original authorship and licensing are
preserved in the upstream sources and `LICENSE.txt` (copied from the same
upstream commit because the published crate omits the repository license file).

Upstream source: <https://github.com/martinjrobins/diffsol/tree/7edd02af717287394bac346d4ed74ec141ae5a86/diffsol>.
Published crate SHA-256:
`3690b3729a291ac097453742215a1884fcde8bf480a444fb6287d6357d86baaf`.
The pristine `bdf.rs` SHA-256 is
`c190d0a0fbccbb0ea98b3557283c501b0253e7e1c4767d5bd7f4b3da22e89e6a`.

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

The workspace patch makes local, CI, native, and Wasm builds use the same
corrected source. A registry-published Rumoca crate cannot rely on a workspace
`[patch]`: registry publication still requires an upstream release containing
these fixes, or publication of an explicitly maintained patched dependency.
These patches have not been submitted upstream. Replace this directory with a
released dependency only after all direct numerical and source-level
regressions pass against that release and the complete MSL comparison passes.
