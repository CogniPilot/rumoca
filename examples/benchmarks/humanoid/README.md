# Humanoid native / MuJoCo comparison

CI runs this comparison for every pull request, after the Linux Nix-check job
builds its release package. It reuses that package; it adds no Rust build. Speed
is informational. An incomplete run, invalid result or failed task gate fails
the job. Raw attempts are retained even when a later step fails.

The frozen model stands for one second, then walks. Timing therefore covers two
simulated seconds, with output interval 0.01 s. Rumoca uses `velimplicit`, one
warmup and three hot runs in one prepared session. The MuJoCo controller uses
`native_act`, `implicitfast` at 1 ms, one discarded run and three measured runs;
its internal timer excludes XML loading and state reset. Both run sequentially
on the same pinned logical CPU, with thread budgets set to one. The reported
ratio is best Rumoca hot time / best MuJoCo hot time, matching the existing
chart protocol. Values above one mean Rumoca is slower. No ratio fails CI.
Rumoca compilation and preparation are reported separately.

This is the same robot geometry, inertias, controller and walking task, with
different contact and actuator physics: MuJoCo uses soft contact and position
actuators; the Modelica model uses its explicit contact law and actuator
saturation. Neither equation parity nor sample-by-sample trajectory parity is
claimed.

## Fidelity gate

Each engine also produces a separate four-second trajectory. Both must pass
the established task rule against the frozen Modelica BDF reference:

- Reach at least 3.998 s.
- Keep minimum pelvis height at least half the reference minimum.
- Finish with pelvis height within 0.05 m of the reference at 4 s.
- Keep forward position at 3.5 s within 15% of the reference.
- Keep nonzero gait-phase transitions within two of the reference count.

The last sample at a duplicate event time is used. Missing columns, non-finite
task values, invalid phase values, backwards time and truncated trajectories
are refused. These validity checks make malformed input fail closed; they do
not relax the established physical thresholds. `reference.csv` is a projection
of the original BDF trace onto exactly the four columns consumed by this gate;
all rows and numerical text are preserved. `provenance.json` records the
original compressed-trace digest. Fixtures and generator inputs are checked
against `SHA256SUMS` before execution.

## Reproduce

Use the release package under review, MSL 4.1.0 and MuJoCo 3.8.1's Linux C
distribution. The CI shell script downloads both immutable releases and checks
their archive SHA256 digests before extracting them.

```bash
python3 -m unittest discover -s examples/benchmarks/humanoid -p test_benchmark.py -v
python3 examples/benchmarks/humanoid/run.py \
  --rumoca /path/to/rumoca --msl /path/to/MSL-4.1.0 \
  --mujoco /path/to/mujoco-3.8.1 --output target/humanoid-comparison
```

The output directory must be new. `result.json`, `summary.md`, command records,
binary/input hashes, runner image identity, generated MJCF, raw logs and both
traces are retained. The job summary also records added wall minutes including
downloads/setup/runs, excluding the existing package build. Each child command
has a 180 s timeout; the CI comparison step has a 15 minute timeout. The first
hosted run establishes the actual added CI time; historical laptop timings
cannot establish that number.

CI binds the reused Nix executable to the package derivation's realized output,
its filtered source path and the immutable flake's exact checkout revision and
source NAR hash. Both raw Nix records are retained. This package does not embed
a commit, so CI does not require `build-info`. Direct non-Nix invocations still
require that command; the optional `--nix-derivation` and `--nix-metadata` inputs
must be supplied together with `CI_SOURCE_SHA` and pass the same binding checks.

The fixture's Modelica model, contact model, compact MultiBodyLie library and
joint-coordinate C controller are frozen comparison inputs, not production
library implementations or a new model generator used by the compiler.
