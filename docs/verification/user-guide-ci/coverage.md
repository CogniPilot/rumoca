# User-guide example coverage

The published21d guide contained44 Modelica fences:15 complete inline models,
21 scenario references to16 distinct configurations, and8 teaching fragments.
The connector explanation now supplies the previously undefined Tank definition,
adding one complete fence. The inventory therefore requires45 fences,
16 standalone models and8 fragments. Seventeen fences contain a top-level
model declaration because the TwoTanks composition is one of those8 fragments. Every fence must be classified; additions
fail the inventory until their context and expectations are reviewed.

The single fixture owner is `rumoca-bind-wasm/src/tests/user_guide_sources.rs`.
The existing mandatory Playground gate, `cargo xtask playground test`, first
stages the pinned MSL and CMM dependencies, then runs bind-wasm with all features.
The focused native command is:

```sh
cargo xtask repo modelica-deps ensure
cargo test --locked -p rumoca-bind-wasm --features full-web user_guide_ -- --nocapture
```

The original expanded filter selected13 tests. The current source additionally
selects the corrected thermostat cycle test and the full Latent256/512 prose
variants, full PredatorPrey313, plus TOML-context, two other-language inventory tests, and the numerical
observer negative control: 20 tests,
with no ignored tests. Completed and resource-stopped receipts
remain distinct from the complete current gate. The same source fixtures enter the actual
binding compilation/simulation front door rather than a separate snippet parser.

| Sources | Mandatory checks | Numerical scope |
|---|---|---|
| All16 complete inline fences | Inventory and balanced checked compilation | CPU samples at0,.01,.02 for non-GPU models; three GPU examples also exercise CPU execution and live-source preparation |
| All21 scenario references /16 configurations | Actual scenario parser, model/file/settings and balanced compilation with pinned source-root binary loader; mandatory browser runtime roster checked by the same guide parser | Rover supplied-input bicycle curve; browser roster requires14 simulation cells and2 complete GALEC production export cells, with runtime receipts pending |
| Coffee and Plant | Original inline model and host input initialization/update | Full Coffee1800s exponential curve; Plant1s step curve |
| Neural equations, population, decoder and dynamics fragments | Exact unchanged excerpt lines in complete Tensor, PredatorPrey and LatentOscillator source; compile and actual stepping | Finite state samples at0,.01,.02 |
| Hidden-width314 fragment | Exact documented declaration replaces only the width declaration in full NeuralODETensor | Full100482-parameter model compile and finite state samples; no smaller dimensions |
| State declaration and experiment annotation fragments | Exact snippets in the guide-linked SympyDecay model | Authored start1 and all3001 points through30s against exp(-.5t), with annotation-selected interval |
| TwoTanks composition | Exact fragment with the newly documented Tank/TankPort definition | Two heads remain1 through1s; actual area modifications2 and.5 |

These tests distinguish short runtime checks from full authored experiment
horizons. They do not claim all long experiments, actual browser GPU execution,
or all generated backend variants from a native finite-sample test. Existing
browser smoke scripts exercise actual FullWeb artifacts; their receipts are
recorded separately. The neural generated-backend gates use the original tracked
models and are owned by the export validation group.

Baseline actual receipts remain distinct from this source-derived inventory.
The earlier consolidated10-test binding gate ran9PASS/1FAIL: the unchanged
ReusableBoosterLanding scenario failed checked clock ownership. Its compiler
repair and this expanded13-test gate require replay on their exact combined
source. Historical pre-consolidation9PASS receipts do not validate the rejected
cross-crate module topology.


The sole Rust excerpt is included from
`rumoca-opt::tests::user_guide_rhs_fit_uses_the_authored_neural_model`, which is
selected by ordinary workspace unit tests. Its focused command is
`cargo test --locked -p rumoca-opt user_guide_rhs_fit_ -- --nocapture` (one test).
The exact unchanged32-wide tensor model provides the context and actual
`b1[1]` / `bmid[1]` trainables, with a retained `W1[1,1]` refusal control;
this is separate from generated JAX/CasADi proof.


All other fenced examples are hash-bound in
[non-modelica-inventory.json](non-modelica-inventory.json):117 entries, comprising
101 execution/setup/template contexts and16 explanatory outputs, counted from
the current finite profiles. The executable notebook cell has the same required
Python owner as the ten Python fences, even though its Markdown language is `text`. The inventory records each exact page/line/language,
exact body line count, classification, finite execution owner and proof scope.
The inventory rejects unknown owners, language/classification mismatches, missing
source spans, and absent required test or CI registrations. Source registration
is distinct from actual completion:
a registered owner or syntax check is not an execution receipt. The required
routes are now integrated; their current runtime qualification remains separate.
New/changed snippets
fail the mandatory FullWeb inventory test until the inventory is reviewed.
Sixteen TOML excerpts enter the actual scenario/workspace configuration parser;
application paths and external-process placeholders are not claimed as runnable
models. Four visualization snippets require actual browser runtime context.

The frozen14-test gate at656013a6fa0f25aea9d507b14e929daed03a6a81
actually passed all14 tests (93.15s test execution), including all16 scenario
compilations with the corrected periodic equation scheduler. Its raw log SHA256
is50da8178c8158bf89530b7f9ac98b9b6daa7c9dc07aaa60e918a420328a175dc.
That receipt includes the original thermostat counterexample, which proves
heat 0.02 never switches. The corrected heat 0.05 model subsequently passed the
independent repeated hysteresis-cycle test at
`1b3e1139b41dde6113383af03b2d455deceee1a9`: all 241 samples through 7200 s
agree with the piecewise analytic temperature and relay state, including more
than 20 switches. The raw log SHA256 is
`75ec1effbba2078605eab2c76da543dc640058d8c1cdaafb41a13ea38f824995`.
The current complete 19-test attempt at
`c85128c50cd6e65198a712578c80679afc01aeaf` failed with SIGABRT during
Latent512 initialization. Four tests reported pass before the abort (TOML
contexts, Airfoil slider reseeding, GPU preparation and GPU CPU execution).
Inside the width test, the full Latent256 model compiled, initialized and
produced three samples in 27.17 seconds; that test did not complete because
Latent512 subsequently aborted. The retained original core identifies a
2,253,424-byte native JIT stack tape during FMI initialization, after successful
compilation. The full 19-test gate, the remaining test outcomes and current
neural widths are not qualified. The bounded attempt took 161.43 seconds,
peaked at 3,723,736 KiB RSS and recorded no resource stop. Raw evidence is in
`pr427-guide-and-scope-after-integrated-20261011/guide-all-native.log` and its
matching guardian receipt. The earlier isolated TOML resource refusal remains
historical and is not a completed gate.

At `c01db27355d9be5836c605acf80ed84325b7168f`, the three CLI context tests
passed with 78 original CLI argument vectors, all 66 Bash source spans and 30
actual disposable commands, including the exact ten-step stdio controller.
The Rust fitting test also passed with two actual optimizer updates and
independent finite-difference checks in 113.48 seconds. These earlier focused
receipts do not establish a complete current source or installed-wheel gate.

## Required execution routes

The registry requires these concrete owners. This table describes source
reachability, not a completed combined CI run. Missing implementations or
registrations make the inventory test fail.

| Owner | Required command or selector | Scope |
|---|---|---|
| Modelica and configuration contexts | `cargo xtask playground test`; focused `cargo test --locked -p rumoca-bind-wasm --features full-web user_guide_ -- --nocapture` | 20 native tests on the current source, including both inventory checks and the numerical observer negative control |
| Rust fitting excerpt | `cargo test --locked -p rumoca-opt user_guide_rhs_fit_ -- --nocapture` | One test, using the exact anchored snippet and unchanged Tensor32 model |
| CLI and shell contexts | `cargo xtask verify examples`; focused `cargo test --locked -p rumoca --test suite_examples_smoke --features examples-smoke-tests guide_cli_contexts:: -- --nocapture` | Three tests: all 66 Bash snippets, declared command contexts, and the exact sampled stdio controller |
| Installed-wheel Python and CMake | `cargo xtask python test --require-guide` (also selected by `cargo xtask verify full`) | The shared wheel roster requires all 10 Python fences, the notebook cell, both CMake fences and four authored codegen configurations on Linux x86_64 |
| Browser visualizations and controls | `node packages/playground/tests/user_guide_browser.mjs` in the mandatory PR `build-wasm` job | The shipped FullWeb artifact, actual local guide, Balloon/Turkey visualizers, hardware-admitted Wave2D/Airfoil compute, FixedWing keyboard controls and all16 linked scenario runtime/export cells |

The linked scenario roster is `linked-scenarios.json`. The sole Rust fence
parser checks its exact set against all21 authored references, so a new linked
scenario cannot silently remain outside the browser runtime gate. The browser
uses the public checked scenario configuration unchanged. Interactive cases
initialize and advance through `createInteractiveSimulation`, including its
actual configured input routing, parameters, tolerances, scene and solver.
Result-only cases use `simulateModelWithRuntime`: an explicit authored output
interval receives a short three-interval check, while an absent interval stays
zero so the canonical runtime selects the annotation/default interval and
retains the authored horizon. Each receipt records both authored and sampled
horizons, with nonempty finite ordered rectangular batch traces or initialized
finite session states followed by actual updates. These source-registered cells
are unrun until a named shipped-artifact browser receipt is retained; compilation
receipts do not qualify them.

The schema-2 `contexts` table also identifies the exact authored Plant and
HangingMass source spans for the CLI owner. It supplies locations and hashes,
not copied models. The CLI loader reads those spans from the actual guide.
Display-only text, XML and JSON examples have explicit output classifications;
they cannot be used to exempt executable-language snippets.

The browser launcher is `packages/playground/tests/browser_launcher.mjs`.
The Linux docs shell includes Chromium explicitly. Every required browser
context uses Chromium's [SwANGLE CPU WebGL path](https://chromium.googlesource.com/chromium/src/+/main/docs/gpu/swiftshader.md),
so Balloon, Turkey, FixedWing and the scenario roster do not require a graphics
card. GPU execution of Wave2D and AirfoilFlow is conditional on an actual adapter
with [`GPUAdapter.info.isFallbackAdapter`](https://gpuweb.github.io/gpuweb/#dom-gpuadapterinfo-isfallbackadapter)
equal to `false`. Missing WebGPU, no adapter, a fallback adapter or unavailable
adapter identity prints a per-model `SKIP` receipt. Admission exceptions and
failures after hardware admission fail the test; timeouts do not skip GPU work.
An admitted run must submit command buffers, dispatch positive compute groups
and read finite computed state samples. These are correctness observations, not
hardware performance measurements. Admission observes an independently requested
adapter; the canonical driver may request a different adapter. Compute receipts
therefore establish hardware-admitted WebGPU execution, without asserting the
actual device is hardware. The native
`user_guide_gpu_examples_prepare_for_webgpu` and
`user_guide_gpu_examples_simulate_on_the_cpu_path` tests remain mandatory even
when browser GPU execution is skipped. Browser source registration and syntax
checks do not establish an executed browser receipt.

The direct launcher probe on source `2a4a56219820490e402d70736dca91c8464008bb`
actually passed on Chromium 154.0.8037.92 in 2.40 seconds: CPU SwiftShader WebGL2
cleared and read the independently expected pixel `[64,128,191,255]` with no GL
error. The real WebGPU adapter reported `info.isFallbackAdapter=true`, so its
hardware compute scope was explicitly skipped. Exact source/probe/raw hashes
are retained in `cpu-browser-2a4-receipt.json`; this probe did not load the
shipped WASM or execute guide models, and does not qualify those pending gates.
