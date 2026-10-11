# Actual workshop qualification at c758abd70

Root executed frozen source `c758abd7060dc5246502b15583e409cbb103062a`,
bound by 3,758 tracked source digests. The raw attempt is
`$HOME/scratch/rumoca/slam-integration-cycle-20261010/pr427-workshop-authored-backprop-and-record-columns-20261011/`.
Its recipe and logs retain the exact source checks, command arguments and limits.

Both gates passed. Node ran three tests with no failures, skips or cancellations.
The strict Modelica gate ran both declared workshop tests: two passed, zero failed
or ignored, 33 filtered out, 54.89 seconds of test execution. The raw log contains
exactly 28 completed OMC control markers: six actual originals and eighteen
exports, plus the separate derivative fixture's original and three exports.

The actual roster is Circuit.Test, SympyDecay, FmiTensorDecay, NeuralODETensor,
NeuralPredatorPrey and NeuralODEBackprop. Each tracked original was compiled by
Rumoca and simulated by OMC; each base/flat/DAE Modelica export was rendered,
reloaded by Rumoca and independently checked and simulated by OMC. Controls
compare OMC equation/variable counts, required known physical initial values at
the last common time-zero sample, and every declared observable at the same exact
short sample grid. All sample times/values must be finite, times nondecreasing,
and CSV columns unique and rectangular. Missing observables fail. Circuit's
measured capacitor/resistor drops independently sum to the authored 5 V source.
Backprop retains its actual defaults nHidden=12, nBatch=8 and 218 trainable states.
The extra vector/matrix/record fixture uses explicit checked record-column mapping.

OMC simulated through 0.002 seconds with two intervals and solver tolerance
1e-10. The unchanged comparison is `abs(actual-expected) <= 1e-8 * (1+abs(expected))`.
Each OMC invocation has a 60-second GNU timeout and five-second kill delay.
These are focused export/initialization/short-trace controls, not a claim of
long-horizon training convergence, all tracked examples or the PR's manual matrix.

Both commands ran inside `nix develop .#full`. Node used the pinned Nix Node
24.18.0 executable with `--test packages/playground/tests/default_workspace_smoke.mjs`.
The Rust command was:

```sh
cargo test --locked --offline --jobs 4 -p rumoca --no-default-features \
  --features template-runtime-tests --test suite_template_runtime \
  modelica_interchange_runtime::workshop_examples:: -- --nocapture
```

The wrapper created `target/template-runtimes/strict` before the Rust command and
removed it with an EXIT trap; absent OMC/timeout prerequisites therefore fail.
The normal backend-Modelica CI selector `modelica_interchange_runtime::` includes
both tests. The Node test reads all five actual configured codegen scenario files.
It checks workspace asset loading; the earlier LSP receipt separately establishes
rendering, so this Node pass is not another numerical execution claim.

The recipe fixed Cargo/Rust/Rayon workers to four, disabled incremental/debug
outputs, and put target/cache/TMPDIR under task-owned `$HOME/scratch` paths.
Guardian limits remained 600 seconds, 4 GiB process-group RSS, 16 GiB available
host memory, 8 GiB target cache, 8 GiB free scratch and no owned swap. Node guardian
exit zero: 4,455.413 ms, peak RSS 507,412 KiB. OMC gate guardian exit zero:
205,989.857 ms including compilation, peak RSS 2,887,016 KiB, peak cache
7,353,720,832 bytes, minimum free scratch 10,113,105,920 bytes. No guard fired.
Root's terminal source audit at 07:04:26 UTC found zero workshop-tree references;
subsequent native tasks belonged to root's scalar qualification lease.

The raw recipe, guardian and test logs are retained locally. Temporary per-model
Modelica/CSV/OMC products are removed by the test's temporary-directory owner;
this receipt does not claim they were retained. Optional future evidence capture
could copy those exact products into an explicitly requested test-owned evidence
root before deletion, with source and result hashes; it must retain the same
oracles, required columns, sample grid and tolerances.

| File relative to raw attempt | SHA-256 |
|---|---|
| recipe.json | `41fc74d3b79689aaa80833690d5d90b93166f528069a8a970c89f71cc5a33218` |
| actual-playground-codegen-scenario-files.log | `27ee3f746aeed92a1130864a012d05238fbc242c9a669add35357b5c03697998` |
| actual-playground-codegen-scenario-files.guardian.json | `1fa09cd69fecd22ceaccd939a45ec9a93c3328c426cdd33ec409caf13c24b8df` |
| actual-workshop-modelica-strict-roundtrip.log | `7b112bce984673d51b25d7bd29b6248e5cead26b02265a9f546fc0a798e67bf4` |
| actual-workshop-modelica-strict-roundtrip.guardian.json | `744eeddb500d50007158e6020db94ad5a172635383119eec942e3b4a36999e10` |
