# Switched RLC CI precision mismatch

PR source `5a0ca1470` (integration production `f7819dc1e`) fails the unchanged
`switched_rlc_msl_retains_storage_states_through_step` on Linux, macOS and
locally: voltage trace difference `1.3800029002197789e-5`, versus the test's
`1e-9` equivalence assertion. Its default simulation request uses relative
and absolute tolerances of `1e-6`. The different continuous observable
inventories need not select identical adaptive steps under shared ME error
control; the test cannot rely on identical numerical meshes.

An independent circuit oracle separates integration error from storage-state
or lowering errors. For the examples' R=100, L=1, C=0.001 and 24 V step at
0.5 s, the zero-initial-state response obeys
`V'' + 10 V' + 1000 V = 24000` after the step. With
`tau=max(t-0.5,0)` and `w=sqrt(975)`,
`V=24*(1-exp(-5*tau)*(cos(w*tau)+5/w*sin(w*tau)))` and
`i=24/w*exp(-5*tau)*sin(w*tau)+V/100`.

| Requested absolute/relative tolerance | Simple max V error | MSL max V error | Pair max V difference |
|---|---:|---:|---:|
| 1e-6 / 1e-6 | 2.5398650166152947e-5 | 1.3920199917194509e-5 | 1.3800029002197789e-5 |
| 1e-12 / 1e-12 | 2.5554669491612003e-11 | 1.3589129821411916e-11 | 1.4146905868983595e-11 |

All 503 samples were compared against the oracle. Maximum current errors fall
from `8.764887366452534e-7` / `4.6188259927859576e-7` to
`8.512079929801075e-13` / `4.3903769508801815e-13` for simple/MSL respectively.
This observed six-order convergence and independent oracle agreement classify
the failure as a test precision mismatch, not a closed-by-tolerance semantic
counterexample. These observations are not a general global-error guarantee.

The test now explicitly requests `1e-12` precision, retains both state-retention
and original `1e-9` pair assertions, verifies timestamp alignment, and adds
independent `1e-9` analytic assertions for both representations. It also checks
trace completion and column lengths. No production tolerance, solver behavior,
comparison threshold or model exception changes. Temporary diagnostic prints
are removed. Independent review agrees with this classification and test scope.

Original CI logs and both diagnostic runs remain under
`target/fluid-campaign/pr-shared-me-parity-fourbar/` as
`test-107420112546.log`, `test-107420112485.log`,
`switched-rlc-repro.log`, `switched-rlc-analytic-default.log`, and
`switched-rlc-analytic-precise.log`. Final ordinary verification passes all four `suite_msl_sim` tests, all 244
architecture tests, and formatting on integration production `0aa7a5380` plus
the test-only fix recorded with this note. Logs are `rlc-recovery-suite.log`,
`rlc-recovery-architecture.log` and `rlc-recovery-fmt.log` in that directory.
