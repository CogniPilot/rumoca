# Workshop qualification at 26f474b15

Frozen source: `26f474b15ed6b63419d6cc0ef7f12a6878c14368`, clean before and
after both gates. The wrapper checked all 3,754 tracked source digests.
Raw attempt: `$HOME/scratch/rumoca/slam-integration-cycle-20261010/pr427-workshop-qualification-physical-and-seeded-scenarios-20261011/`.

Both commands ran inside `nix develop .#full`, with four Cargo/test/Rayon
workers, incremental compilation disabled, dev/test debug information disabled,
and Cargo target, Cargo home and TMPDIR beneath `$HOME/scratch/rumoca/`.
Unchanged guards: 600 seconds per gate, 4 GiB process-group RSS, 16 GiB available
host memory, 8 GiB shared target capacity, 8 GiB free scratch floor, no owned swap.
The strict marker was created exclusively and removed on exit; missing OMC or
GNU timeout could not silently skip the Rust test.

```bash
node --test packages/playground/tests/default_workspace_smoke.mjs
cargo test --locked --offline --jobs 4 -p rumoca --no-default-features \
  --features template-runtime-tests --test suite_template_runtime \
  modelica_interchange_runtime::workshop_examples:: -- --nocapture
```

The Node gate passed all three tests, with no skipped/cancelled cases. Its
actual-file loader test enumerated all five tracked codegen scenarios. Guardian
exit 0, elapsed 4,411.929 ms, peak RSS 509,556 KiB.

The strict OMC gate failed: zero Rust tests passed, one failed, zero ignored,
33 filtered out. Guardian exit 101, elapsed 91,537.743 ms, peak RSS 2,553,452 KiB,
peak target 8,348,844,032 bytes, minimum free scratch 9,626,546,176 bytes. No
resource guard fired.

| Actual model | Original OMC and controls | Base export | Flat export | DAE export |
|---|---|---|---|---|
| Circuit.Test | passed | passed | passed | passed |
| SympyDecay | passed | passed | passed | passed |
| FmiTensorDecay | passed | passed | passed | passed |
| NeuralODETensor | passed | passed | passed | passed |
| NeuralPredatorPrey | passed | passed | passed | rejected by OMC |
| NeuralODEBackprop | not reached | not reached | not reached | not reached |

Each passed export includes Rumoca reload, OMC structural counts, settled
initialization and the unchanged physical-channel trace comparison. This is
five original controls and fourteen export controls, not a six-model pass.
The fifteenth export was attempted and rejected: `der(population)[1]` and
`der(population)[2]` put subscripts after a call, outside the Modelica primary
grammar. OMC reported `No viable alternative near token: [` at model line 95.
Rumoca self-reload accepted this text, so independent OMC caught a genuine
exporter defect. No model source, parameter, channel or tolerance changed.

The terminal target audit at 06:37:00 UTC found no observable target references
or native tasks; 2,264 denied reads were explicitly unobserved.

## SHA-256 bindings

Files below are relative to the raw attempt directory unless marked audit.

| File | SHA-256 |
|---|---|
| recipe.json | `0e36108f6d3ea41c2939ac44c38935e48d3b8fca6f5ca3fd327062a295db66ef` |
| actual-playground-codegen-scenario-files.log | `4a268b71daac2a1a2ad326377530a2c21ac4d94d46c1d55a4e2756a5864051c6` |
| actual-playground-codegen-scenario-files.guardian.json | `c1093fb511ddc2244cb3a9e7df3eb54aa6542caeb312941d3c13b6895cb84b61` |
| actual-workshop-modelica-strict-roundtrip.log | `68676605c9c000abfec0753fe20584bcf299d4ca496fe7c8d83a08ae08209029` |
| actual-workshop-modelica-strict-roundtrip.guardian.json | `4c1f24e0a5081f7253a07a06305bfb2987d5e79a8e3ea9da6f08624fd7c787e9` |
| audit: fluid-assertion-observation-20261009/pr427-workshop-terminal-26f-clean-20261011.json | `00de78c6647356d37e7e311b60d545bf401f3fb153c4d2db6254724839195ecd` |

The earlier LSP request gate passed four tests at `169ed612c`; it was not rerun
in this attempt. Full workspace, lint, canary and cohort gates remain unclaimed.
