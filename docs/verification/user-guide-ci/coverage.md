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
variants, full PredatorPrey313, plus TOML-context and two other-language inventory tests:19 tests,
with no ignored tests. Completed and resource-stopped receipts
remain distinct from the complete current gate. The same source fixtures enter the actual
binding compilation/simulation front door rather than a separate snippet parser.

| Sources | Mandatory checks | Numerical scope |
|---|---|---|
| All16 complete inline fences | Inventory and balanced checked compilation | CPU samples at0,.01,.02 for non-GPU models; three GPU examples also exercise CPU execution and live-source preparation |
| All21 scenario references /16 configurations | Actual scenario parser, model/file/settings and balanced compilation with pinned source-root binary loader | Rover supplied-input bicycle curve; other configurations have compilation coverage here |
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
`W1[1,1]` / `b1[1]` trainables; this is separate from generated JAX/CasADi proof.


All other fenced examples are hash-bound in
[non-modelica-inventory.json](non-modelica-inventory.json):102 code fences and15
explanatory outputs. The inventory records each exact page/line/language,
exact body line count, classification, finite execution owner and proof scope.
The inventory rejects unknown owners, language/classification mismatches, missing
source spans, and absent required test or CI registrations. Source registration
is distinct from actual completion:
a registered owner or syntax check is not an execution receipt. Some required
owners are not yet integrated into this source, so its inventory gate must fail
until those concrete routes are present. New/changed snippets
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
The complete 19-test suite, Latent256/512, PredatorPrey313 and stronger execution
owner registry remain unmeasured. The isolated TOML-context test produced a
passing raw result but its guardian recorded a scratch free-space refusal;
it needs a clean bounded retry.

## Required execution routes

The registry requires these concrete owners. This table describes source
reachability, not a completed combined CI run. Missing implementations or
registrations make the inventory test fail.

| Owner | Required command or selector | Scope |
|---|---|---|
| Modelica and configuration contexts | `cargo xtask playground test`; focused `cargo test --locked -p rumoca-bind-wasm --features full-web user_guide_ -- --nocapture` | 19 native tests on the current source, including both inventory checks |
| Rust fitting excerpt | `cargo test --locked -p rumoca-opt user_guide_rhs_fit_ -- --nocapture` | One test, using the exact anchored snippet and unchanged Tensor32 model |
| CLI and shell contexts | `cargo xtask verify examples`; focused `cargo test --locked -p rumoca --test suite_examples_smoke --features examples-smoke-tests guide_cli_contexts:: -- --nocapture` | Three tests: all 66 Bash snippets, declared command contexts, and the exact sampled stdio controller |
| Installed-wheel Python and CMake | `cargo xtask python test --require-guide` (also selected by `cargo xtask verify full`) | The shared wheel roster requires all 10 Python fences, the notebook cell, both CMake fences and four authored codegen configurations on Linux x86_64 |
| Browser visualizations and controls | `node packages/playground/tests/user_guide_browser.mjs` in the mandatory PR `build-wasm` job | The shipped FullWeb artifact, actual local guide, four authored visualizers and FixedWing keyboard controls |

The schema-2 `contexts` table also identifies the exact authored Plant and
HangingMass source spans for the CLI owner. It supplies locations and hashes,
not copied models. The CLI loader reads those spans from the actual guide.
Display-only text, XML and JSON examples have explicit output classifications;
they cannot be used to exempt executable-language snippets.
