# Actual shared model coverage in ordinary example CI

Test-only followup based on `673ce6ec74418fb0043d3ad7354e0ceb2c7543da`.
**Execution is pending**: authoring ran rustfmt and diff checks, no Cargo/OMC.
The original PR427 `cfe53b91d` diff against `0b249f0f7` changes no files under
`examples/`; this expanded source roster follows the user's workshop request,
not a reconstructed manual 32-model matrix.

The existing mandatory `test` CI jobs already call `cargo xtask verify examples`
on Linux, macOS and Windows. That command stages the libraries declared by
`examples/modelica_dependencies.toml`, then runs `suite_examples_smoke` with
`examples-smoke-tests`. Its previous sole test uses an inline Decay model.
This followup adds `tracked_models` to that same existing test target, without
new CI jobs, selectors, dependencies, feature changes or optional skips.

The explicit roster covers all **13 tracked `.mo` files / 15 demo entrypoints**:
Ball, Circuit.Test, FmiTensorDecay, GalecCounter, KalmanFilterStepTest,
NeuralLatentOscillator, NeuralODEBackprop, NeuralODEBackprop1k, NeuralODETensor,
NeuralODETensor100k, NeuralPredatorPrey, PIDMSL, SwitchedRLC, SwitchedRLC_MSL,
and SympyDecay. Circuit component helpers are exercised in Circuit.Test rather
than asserted to be standalone connected circuits. New model files fail the
file-roster equality check until their entrypoints are declared explicitly.
Compilation visits the whole roster and reports every compiler/balance failure.

Sources are loaded unchanged through the public Compiler API. Defaults remain
authored: NeuralODETensor uses nHidden=32; Backprop uses nHidden=12/nBatch=8
(218 trainable states). The separately declared 100k/1k variants are compiled
under their original declarations; compiling a default is not counted as
compiling its larger variant. Missing source or required MSL files fail.
The test derives MSL cache location and required files from the same tracked
dependency manifest that the existing staging command consumes.

Five numeric tests additionally simulate six unchanged source models through
the explicit Interpreter/RkLike profile. Their independent controls are scalar
and tensor exponential decay, pre-impact Ball free fall, the Circuit RC
charging curve, and the local RLC's voltage response before/after its 0.5-second
source step. KalmanFilterStepTest executes its own unchanged estimate/covariance
initialization assertions at authored tolerance 1e-10, then its x'=1 trace.
Acceptance uses rtol/atol=1e-9 and error bound 20*(atol+rtol*abs(expected));
required channels, finite values, rectangular samples and the full requested
time interval are checked. No model rewrite or tolerance retry is allowed.

PIDMSL and SwitchedRLC_MSL receive compilation coverage here, not a numerical
trace claim. Neural defaults retain independent numeric tests in existing C,
JAX and CasADi template lanes. Ball's source event remains intact, but this
particular analytic free-fall test stops before impact and proves no bounce.
Interpreter evidence must not be described as native execution or FMI proof.

Focused qualification command (6 new tests; full existing target has 7):

```sh
cargo test -p rumoca --features examples-smoke-tests --test suite_examples_smoke tracked_models:: -- --nocapture
```

The Modelica lane separately selects `modelica_interchange_runtime::` with
`--require-external-tools` (six original models/eighteen exports plus the
independent derivative fixture: 28 OMC cells). Workspace tests select actual
LSP scenarios; Playground CI discovers `default_workspace.test.mjs` and runs
the source-built FullWeb target API smoke. These established lanes stay intact.
The new roster is ordinary example CI coverage, not native CoverageGate
coverage: that job does not enable `examples-smoke-tests`.

Active SPEC_0007/0029/0032 keep compilation and execution with their existing
owners; SPEC_0033/0025 require actual focused qualification before final claims.
MLS defaults, array dimensions, connectors and initialization come from the
original models; this followup changes no Modelica semantics or production API.
