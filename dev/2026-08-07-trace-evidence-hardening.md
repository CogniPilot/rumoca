# Tier 1 ledger — exact-start and strict-high trace evidence

Branch `msl-trace-parity-50`, uncommitted working tree based on `a166e83d`.

## Capability and root cause

Trace recording used a tolerance-based time comparison to deduplicate samples.
An event immediately after `t = 0` could therefore replace the exact-start row,
so the comparator measured trajectory behavior as initialization. Separately, a
trace with zero initialization channels was reported as accurate, and the
strict-high band permitted up to one percent deviation channels. Those are three
ways for missing or adverse evidence to be presented as affirmative evidence.

The recorder now replaces only an observation at the identical time coordinate.
The comparison summary reports zero-channel initialization evidence as
`unmeasured`, the outer parity gate rejects any unmeasured initialization model,
and strict-high requires zero deviation channels.

## Focused evidence

The focused recorder, initialization-summary, outer-comparator, and complete
`sim_trace_compare` suites pass. Regressions cover a distinct near-start event,
replacement at an identical superdense coordinate, zero-channel initialization,
and one deviation hidden among one thousand high-agreement channels.

`Modelica.Electrical.PowerConverters.Examples.ACDC.RectifierCenterTap2mPulse.ThyristorCenterTap2mPulse_RLV`
now retains both `0.0` and `8.08247864527423e-22`; all 426 exact-start channels
compare high. Its four trajectory deviations are aliases of the sampled RMS
output. OMC at the model tolerance ends at `17103.33799700161`, OMC at `1e-10`
ends at `17103.26688626457`, and Rumoca ends at `17103.258673966167`. The model
is therefore a tracked oracle-resolution exclusion, visible and non-strict-high,
not an actionable compiler counterexample and not a claimed parity success.

## Tier 1 canary delta

The fixed `dev/msl-canary-20.json` set received one attempt per phase under the
accepted 10-second default. The retained before artifact is
`target/msl/canary-20-wave3`; the after artifact is
`target/msl/canary-trace-evidence-10s`. This is a partial tripwire, not a cohort
parity claim. Intervening compiler work means the breadth increase is observed
but is not attributed solely to this evidence-hardening change.

| Metric | Before (`836cec9a`, dirty) | After (`a166e83d`, dirty) |
|---|---:|---:|
| parsed / flattened | 20 / 20 | 20 / 20 |
| reached DAE / solve | 8 / 8 | 11 / 11 |
| `sim_ok` completion | 6 / 20 | 8 / 20 |
| OMC traces compared | unmeasured | 8 |
| strict-high | unmeasured | 8 |
| near / deviation | unmeasured | 0 / 0 |
| missing / skipped | unmeasured | 0 / 0 |
| unmeasured initialization | unmeasured | 0; 129 channels measured |

The current canary has no actionable trace counterexample. Nine models stop at
typed DAE construction failures and three stop at typed solve-IR
structural/computability failures; none is represented as a successful compile
and simulation.

## Process defect closed

The first diagnostic rerun exposed that `MSL_SIM_TIMEOUT_SECS` had been raised
from 10 to 12 seconds while accepted SPEC_0033 still required the 10-second
canary budget. That 12-second artifact is not Tier 1 evidence. The implementation,
CLI documentation, harness documentation, and quality-baseline budget are
restored to 10 seconds before the recorded rerun above.
