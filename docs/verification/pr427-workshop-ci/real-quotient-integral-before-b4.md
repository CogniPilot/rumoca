# Real quotient state accuracy: source-only scalar control

The measured structured BEFORE is pinned to
`b4e38503723816d0529168c46ac1b027ca1ea73d`. Its original model, eleven live
sample times, Interpreter/Auto loop and tolerance remain unchanged. Interpreter
first fails at `x[2](0.7)=2.83499971322463`, against `2.835`.
The actual source sets `atol=rtol=1e-9`, so the unchanged bound there is
`20*(1e-9+1e-9*2.835)=7.67e-8`; the observed deficit is about `2.86775e-7`.
The preceding check of `y[2](0.7)` passed. Auto was not reached by that test.

Raw attempt under the owned scratch cycle:
`pr427-real-structured-quotient-trace-1-b4-20261011`.

| Actual retained file | SHA-256 |
|---|---|
| `real-structured-quotient-trace-1.log` | `eb1dc54864edd163e78c04fa34301fabd3764e4431722c096b38bd35ef727753` |
| `real-structured-quotient-trace-1.guardian.json` | `54c15c633eefb8e56b57fab9735cbe7e09f3297aa4b5056aca0dd14ea9d5eb0d` |

The new ordinary scalar control uses `y=mod(8*time+2,7); der(x)=y`,
with the same options and eleven exact live observations. Its independent
integral is `4*t*t+2*t-7*max(t-5/8,0)`, with event state `x(5/8)=2.8125`.
It checks the scalar quotient/root owner, retains the original error bound,
collects every numerical failure across both policies, and prints actual
execution receipts. A separate public batch simulation prints actual retained
trace rows within `1e-6` of the event, including their states and derivatives.
That neighborhood only selects evidence; it does not set root accuracy or
relax a comparison. The public trace does not expose integrator stage intervals.

```sh
cargo test -p rumoca --test suite_core \
  reported_bug_runtime::scalar_modulo_integral::scalar_real_quotient_preserves_integrated_state_across_its_boundary \
  -- --exact --nocapture
# exactly one ordinary test; qualification UNRUN
```

Source evidence separates three hypotheses. The component's standard root
location tolerance is time-coordinate roundoff from `RootLocationPlan`,
independent of state units (SPEC_0044 §6); it is not an `atol`-sized window.
The generated indicator is `sin(pi*lhs/rhs)`, while scalar quotient evaluation
computes Floor/Trunc at each trial coordinate. RK45's accepted dense polynomial
is formed from its trial derivatives; the host applies a root using a sampled
state from that polynomial and then resets history. Endpoint consistency checks
prove sampler agreement with accepted endpoints, not one-sided physical
accuracy across a discontinuous RHS. Mixed-side stage contamination is thus a
concrete competing hypothesis, not a measured diagnosis. The scalar control
distinguishes a general numerical/event path from compact-domain ownership.
If it also fails, compare its actual event-left state to the analytic value
before adding any internal test-only stage observation or repairing a producer.
No numerical tolerance, root policy or production evaluator changes here.

Mandatory workshop CI selection remains the existing three-OS examples job,
workspace tests, strict backend jobs and playground command, mapped in
[the source selection record](ci-reachability-a64.md). B4 still contains the
four issuer metadata environment reads; the separate `712c18605` CLI repair
must be integrated and qualified. No new environment knob, path attribute,
ignored test or unsupported-model filtering is introduced by this control.
Fresh instrumented coverage and exact-head CI remain pending.
