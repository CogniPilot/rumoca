# Derivative repair qualification at 4659ef1d5

Frozen source `4659ef1d5e8d118a1e46b79e5512b6c0cba0029d`, 3,756 tracked
digests. Root executed the prepared Node and strict OMC commands from the
[previous receipt](qualification-26f-20261011.md), with unchanged resource
guards and four workers. Raw attempt:
`$HOME/scratch/rumoca/slam-integration-cycle-20261010/pr427-workshop-derivative-projection-corrected-markers-20261011/`.

Node passed all three tests again. Strict OMC selected two Rust tests and
failed both: zero passed/ignored, 33 filtered out. Guardian exit 101,
174,587.075 ms, peak RSS 2,874,444 KiB, peak target 6,886,785,024 bytes,
minimum free scratch 10,671,599,616 bytes. No guard fired.

The actual-model test completed original/base/flat/DAE controls for Circuit,
SympyDecay, FmiTensorDecay, NeuralODETensor and NeuralPredatorPrey: five originals
and fifteen exports. The former DAE NeuralPredatorPrey parser counterexample
now passes the unchanged structure, settled-start and trace controls.

The sixth original, NeuralODEBackprop, was attempted and rejected by OMC before
any of its exports: `Non-array modification 'true' for array component 'fixed',
possibly due to missing 'each'` at authored source lines 20–25. Six array state
declarations use scalar `fixed = true` without `each`. This is a genuine example
source error, not an exporter error. The complete invalid original is retained
in the raw log and frozen commit. Correcting the authored source will retain its
intended fixed starts, dimensions, equations and default parameters.

The extra vector/matrix/record-member fixture passed original OMC controls,
then its base export executed but the required trace-column lookup failed:
original `pair.position[1]` maps to exported `'pair.position'[1]`. The entire
flattened declaration identifier is quoted; its index is outside those quotes.
The real CSV columns are listed in the failure. No missing value is supplied.
The flat/DAE fixture variants were not reached.

Root's terminal audit at 06:55:55 UTC found no references to this source tree;
global native tasks belonged to the subsequent root-owned browser gate. The
shared heavy lease stayed with root. The six-model suite remains unqualified.

| File relative to raw attempt | SHA-256 |
|---|---|
| recipe.json | `6567525f6e697a1896883c06120afc8bc3a3674de44268b43962a4818be73135` |
| actual-playground-codegen-scenario-files.log | `c5683c1225ff41a9187ed979eae87d4a9f02912c8252697f18f8dd80adb7ca32` |
| actual-playground-codegen-scenario-files.guardian.json | `94a138e09361ca42a8b0579c2a353204f64c1dace7400e8230a26103a740951c` |
| actual-workshop-modelica-strict-roundtrip.log | `7c418e9f00e2a179a64e032c42a1e3a7ea025f916fca32684b662fcd445f93ac` |
| actual-workshop-modelica-strict-roundtrip.guardian.json | `e8bf4c99e86bd966a6e0ca00f0896634daea1ab71353ec753f95a0af00ddf06d` |
