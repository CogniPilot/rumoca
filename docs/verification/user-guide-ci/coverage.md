# User-guide example coverage

The published21d guide contained44 Modelica fences:15 complete inline models,
21 scenario references to16 distinct configurations, and8 teaching fragments.
The connector explanation now supplies the previously undefined Tank definition,
adding one complete fence. The inventory therefore requires45 fences,
16 standalone models and8 fragments. Every fence must be classified; additions
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
selects the original thermostat counterexample and the full Latent256/512 prose
variants plus TOML-context and other-language inventory tests:17 tests, with no ignored tests. Completed and resource-stopped receipts
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
[non-modelica-inventory.json](non-modelica-inventory.json):100 code fences and15
explanatory outputs. The inventory records each exact page/line/language,
classification, owner and proof scope. It deliberately records pending gates:
a named owner or syntax check is not an execution receipt. New/changed snippets
fail the mandatory FullWeb inventory test until the inventory is reviewed.
Sixteen TOML excerpts enter the actual scenario/workspace configuration parser;
application paths and external-process placeholders are not claimed as runnable
models. Four visualization snippets require actual browser runtime context.
