# Scalar modulo root-adoption regression

This test-only candidate follows source `6aedc673a50f7a45bb4ed3244cd73a6f6d2af49d`
and leaves that public scalar control and the original structured failing test
unchanged. Neither scalar control has an executed result at preparation time.
The retained structured failure is described in
[the original evidence note](real-quotient-integral-before-b4.md).

The ordinary `rumoca-sim` unit test compiles the same `ScalarRealQuotient`
source through the existing compiler and runtime-artifact owner. It instantiates
the canonical FMI host and delegates to the existing RK45 numerical plugin.
The delegate forwards the original host-issued derivative handle unchanged.
It records numerical candidates, the host's own dense samples for intervals
overlapping the root neighborhood, and the state passed to `truncate_reset`.
It adds no derivative evaluation, sampling call, event, configuration, or
production API. A candidate is explicitly unproved until the existing host
validates it; its diagnostic label does not imply it was committed.

The source has one continuous state with root at `5/8`. For each Interpreter and
Auto run, the test retains the original eleven times `0, 0.1, ..., 1`,
`atol = rtol = 1e-9`, initial step selection, and independent integral
`4*t*t + 2*t - 7*max(t - 5/8, 0)`. Both reported samples and the actual
root-adoption state must satisfy the unchanged bound
`20*(atol + rtol*abs(expected))`. The `0.01` neighborhood and `1e-6` root-row
identification window select diagnostics; neither changes numerical oracles or
root policy. Shape, finiteness, a real root adoption, and a containing recorded
candidate are mandatory. Execution receipts and backend declines are printed;
Auto is not asserted to be native. Capture is test-local and bounded to 4096
observations, with no global logger or parallel-test ownership race.

SPEC_0038 and SPEC_0044 §6 keep RK stages and dense coefficients private to the
plugin. The opaque derivative handle cannot be cloned or replaced by this test.
Therefore this capture exposes endpoints, actual dense samples, and adoption,
**not RK stage states or derivatives**. It does not reconstruct those stages
from a continuous extension or substitute a handwritten numerical evaluator.

[MLS §3.7.2](https://specification.modelica.org/maint/3.6/operators-and-expressions.html#event-triggering-mathematical-functions)
identifies `floor(x/y)` as the event-generating expression for `mod`.
[MLS §8.5](https://specification.modelica.org/maint/3.6/equations.html#events-and-synchronization)
requires the event-expression buffer to stay constant during continuous
integration. These primary stable-version references cross-check the same
requirements routed by SPEC_0022 EXPR-040. Recomputed trial quotients and dense
state adoption remain competing source hypotheses until the real capture runs.
No production change or tolerance widening is included.

Focused command, expected **one executed test**, with the parent's ordinary
scratch outputs and unchanged resource guardian:

```sh
cargo test -p rumoca-sim --lib \
  solve_lowering::tests::quotient_root_capture::scalar_modulo_root_adoption_preserves_the_integral \
  -- --exact --nocapture
```

`rumoca-sim` defaults enable `native-solvers`, including `solver-rk45`, so this
unignored normal module is reachable by the existing mandatory
`cargo xtask verify workspace` owner and workspace coverage tests. No workflow,
selector exemption, additional dependency, or new runner is required.
Source-only rustfmt and diff checks passed; compilation, execution, and fresh
instrumented coverage remain unrun.
