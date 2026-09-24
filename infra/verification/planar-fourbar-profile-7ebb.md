# PlanarFourbar bounded profile at 7ebb651bb

One diagnostic only, using the existing optimized `msl-fast` profile and a
12-second solver budget. No canonical run, reference rerun, tolerance change,
production optimization, or projection-file edit. Source:
`7ebb651bb463e8c73dd2f5e7f544ae9e75763e0b`.

Artifacts and exact commands:
`/tmp/rumoca-zero-rhs-test/fluid-diagnostic/planar-fourbar-7ebb651bb/`.
`probe.patch` contains the two-file temporary instrumentation;
`source-executable-hashes.txt` binds it to the executable, source, and request;
`profile-12.log` retains all raw counter snapshots; `summary.json` and
`extract-summary.pl` retain the reproducible calculation. Worker request,
result, and Flat/DAE/Solve IR are retained. Probes are removed from the worktree.

| Measurement | Result |
|---|---:|
| Previous-good completion, saved main-f477 evidence | 2.927116837 s; high, 872 channels |
| Existing uninstrumented 7ebb focused canonical result | timeout after 12 s; run 12.083658515 s |
| This instrumented diagnostic | timeout after 12 s; run 12.035914950 s |
| Last retained numerical accepted time / source stop time | 4.853100643231493 / 5 s |
| Native accepted steps | 1,602 |
| LTE rejected trials | 153 |
| Nonlinear failures / iterations | 0 / 1,958 |
| Native advance time | 11.471182852 s; 7.161 ms per accepted step |

The following callback totals isolate the advancing component lifetime by
subtracting the second pre-advance snapshot from the final cumulative snapshot.
Both earlier lifetimes report zero advances. Counter categories come directly
from Diffsol's native statistics; nonlinear failures are separate from LTE
rejections, not inferred from callback counts.

| Shared callback scope | Calls | Seconds |
|---|---:|---:|
| RHS, including its projection work | 3,716 | 2.191337148 |
| Directional derivative, inclusive | 48 | 0.410771685 |
| Observable error pair, inclusive | 4,417 | 8.858085182 |
| Per-member observable value transaction, nested in pair | 8,834 | 8.677993498 |
| `kernel.observe()` coordinate reconstruction, nested in transaction | 8,834 | 8.554096004 |
| Published output evaluation, nested in transaction | 8,834 | 0.049861675 |

Only the first three rows are disjoint. The observation-coordinate measurement
is a **lower bound on total projection time**: derivative, JVP, root, and other
host projection work is not separately instrumented. `kernel.observe()` calls
`observation_coordinate`, which clones the candidate coordinate/parameters and
calls `refresh_public_observation_coordinate`; the measurement includes that
boundary work and does not isolate individual projection algorithms.

The measured dominant cost is observable-coordinate reconstruction: **71.07%**
of runtime, approximately **0.968 ms per projection**, with the complete
observable-pair path taking **73.60%**. It exceeds the entire previous-good
completion time. The model makes progress almost to its 5-second endpoint;
there is no nonlinear retry storm, and LTE rejections are 8.72% of accepted
plus LTE-rejected trials. This directs the next investigation to expensive
projection work per step, owned by Galileo. Historical accepted/rejected step
counts are unavailable here, so step-count inflation relative to the old run
is not ruled out. No uninstrumented overhead ablation or completion-time
extrapolation is claimed.

The old runtime comes from the retained census's exact PlanarFourbar row in
`main-f477d0b69-evidence/msl_results.json`, under
`/tmp/rumoca-fluid/target/fluid-campaign/`; that historical snapshot reports
unknown commit metadata and a dirty worktree. The current canonical pointer is
`recovery-progress-projection-7ebb651bb/model_worker/Modelica.Mechanics.MultiBody.Examples.Loops.PlanarFourbar/result.json`
under the same campaign root. Both references are preserved in `summary.json`.

Frozen worker SHA256:
`2690bac11952a101d12487a35f3943f56f63ca2967925719f9eecaa7fa638aa4`.
Frozen probe SHA256:
`bdd35246e59121b0b75d2f960924dc46bc1b1d2339c439d1d395e233441f64d5`.

Governing workflow: SPEC0033 first-producer evidence and bounded verification,
SPEC0029 shared execution ownership. No semantic changes were made. No commands
remain running; other model work, including ED008, remains paused.

## OMC comparison point

Main ran the retained pinned OMC executable with its unchanged initialization
XML, `-lv=LOG_STATS`, and a separate output CSV. It completes the 5-second
horizon in 798 steps, with 1,179 ODE calls, 57 Jacobian evaluations, 38 error
test failures, and no convergence failures. OMC reports 0.0238675 seconds for
simulation and 0.161721 seconds total, mostly output-file work. Logs, the
original experiment settings, and executable/XML/CSV hashes are retained in
`target/fluid-campaign/planar-omc-reference/`.

Rumoca's instrumented count above covers only 4.8531 seconds, and timer scopes
differ, so no precise cross-tool speedup is claimed. OMC provides an independent
step-count reference; Rumoca's measured projection cost remains the stronger
optimization target. This diagnostic changes no canonical OMC trace or policy.
