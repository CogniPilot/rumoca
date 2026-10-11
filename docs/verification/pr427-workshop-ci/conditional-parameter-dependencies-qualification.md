# Conditional initialization parameter dependencies

At source `86c3bc1a6128e3d3174fefa169ae7bd5e03243e9`, seven previously
certified MSL tank models compiled but failed Solve construction with EL001:
“cyclic initialization parameter definitions require a coupled owner.”
The sealed `1dfdf57af2e45edbf5adc50a73fcf462020cefd8` CLI independently
reproduced that failure on the reduced opposite-arm parameter model.
One preceding attempt performed no model execution because the scratch reserve
preflight refused; the qualified reproduction completed normally with exit 1.

The actual issued `EmptyTanks` DAE identifies the first false dependency edges.
Parameter 130, `pipe.flowModel.m_flow_nominal`, reads parameter 131,
`pipe.flowModel.m_flow_small`, only in its absolute-flow arm. Parameter 131
reads parameter 130 only in its relative-flow arm. Both conditions read
`system.use_eps_Re`, whose actual declaration is non-tunable and evaluable
with a false binding. `Fluid/Pipes.mo` supplies the opposite conditionals;
`Fluid/System.mo` declares the `Evaluate=true` guard. The raw expression walk
collected both arms and manufactured a cycle before any runtime was built.

The initialization owner now uses the existing `ScalarSelector` immutable
guard proof with the existing pruned DAE traversal. Tested conditions remain
dependencies; known false values are excluded and a known true value ends
branch selection. Unknown and tunable guards retain the conservative graph.
An unbound compact-domain binder also remains unknown, without enumerating
domain points. Genuine selected cycles keep the existing coupled-owner
refusal. There is no wire change or public API addition. This follows MLS
§8.6 initialization and §18.6 evaluable parameters, SPEC_0007's initialization
ownership, SPEC_0032's compact domains, SPEC_0029's shared semantic owners,
and SPEC_0033's first-divergence validation.

At clean source `00e801e8550644ce0366cedeb6ebae99f4506d50`, the four
CI-selected `suite_core::initialization_parameter_conditionals` tests passed:
both immutable guard values have independent 11-point analytic traces under
Interpreter and Auto, a tunable guard cannot choose a default-based ordering,
a selected genuine cycle refuses, and a compact binder guard remains valid.
All seven unchanged original tank models then completed their full authored
horizons through the ordinary CLI Auto benchmark:

| Model | Final time | Output points |
|---|---:|---:|
| `Tanks.EmptyTanks` | 50 | 505 |
| `Tanks.ThreeTanks` | 200 | 501 |
| `AST_BatchPlant.Test.OneTank` | 100 | 1161 |
| `AST_BatchPlant.Test.TankWithEmptyingPipe1` | 35 | 505 |
| `AST_BatchPlant.Test.TankWithEmptyingPipe2` | 120 | 509 |
| `AST_BatchPlant.Test.TanksWithEmptyingPipe1` | 120 | 523 |
| `AST_BatchPlant.Test.TanksWithEmptyingPipe2` | 120 | 513 |

All names above have prefix `Modelica.Fluid.Examples.`. Every attempt used
the unchanged 600-second, 4-GiB aggregate resident-plus-swap, 8-GiB cache,
16-GiB host reserve, and 8-GiB scratch reserve guards. None stopped on a
resource limit. The adjacent JSON binds exact source, commands, source-file
hashes, sealed ELF hashes, raw logs, and guardian receipts. Raw evidence and
issued binaries are retained in
`${HOME}/scratch/rumoca/workshop427-cyclic-init-20261012/evidence`.

These are source-bound completion and focused semantic proofs. OMC agreement,
certified model wall budgets, final integrated-source checks, and full CI
remain separate qualifications.

Focused command:

```bash
cargo test --locked --offline --jobs 4 -p rumoca --test suite_core \
  initialization_parameter_conditionals:: -- --nocapture
```

The ordinary `suite_core` CI owner selects all four tests without optional
features, ignores, or workflow exceptions.
