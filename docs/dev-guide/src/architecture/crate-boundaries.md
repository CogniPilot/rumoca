# Crate Map

The workspace is deliberately granular — phases, IRs, tools, backends, and
bindings are separate crates with enforced dependency edges. The normative
boundary rules are
[SPEC_0029](https://github.com/CogniPilot/rumoca/blob/main/spec/SPEC_0029_CRATE_BOUNDARIES.md);
this page is the orientation map.

## Families

| Family | Crates | Role |
|---|---|---|
| Foundation | `rumoca-core`, `rumoca-contracts`, `rumoca-codec`, `rumoca-codec-flatbuffers` | Shared low-level types, contracts, serialization |
| IRs | `rumoca-ir-ast`, `rumoca-ir-flat`, `rumoca-ir-dae`, `rumoca-ir-solve` | The four stage data structures |
| Phases | `rumoca-phase-parse`, `-resolve`, `-typecheck`, `-instantiate`, `-flatten`, `-dae`, `-structural`, `-solve`, `-codegen` | One transformation each |
| Facade | `rumoca-compile` | Session/compilation API the tools use |
| Evaluators | `rumoca-eval-ast`, `-eval-flat`, `-eval-dae`, `-eval-solve` | Stage-appropriate evaluation |
| Runtime | `rumoca-sim`, `rumoca-solver`, `rumoca-solver-rk45`, `rumoca-solver-diffsol`, `rumoca-worker` | Simulation orchestration and solver backends |
| Optimization | `rumoca-opt` | Training and optimization over differentiable Solve runtime |
| Execution adapters | `rumoca-exec-cranelift`, `rumoca-exec-mlir`, `rumoca-exec-wasm` | JIT/compiled execution over Solve |
| Interactive I/O | `rumoca-input`, `rumoca-input-keyboard`, `rumoca-input-gamepad`, `rumoca-signal-frame`, `rumoca-transport-udp`, `rumoca-transport-websocket`, `rumoca-web` | Devices, signals, transports, viewer |
| Tools | `rumoca-tool-fmt`, `rumoca-tool-lint`, `rumoca-tool-lsp` | Formatter, linter, language server logic |
| Bindings | `rumoca` (CLI), `rumoca-bind-wasm`, `rumoca-bind-python` | User-facing entry points |
| Testing/dev | `rumoca-test-msl`, `xtask` | MSL gates, developer CLI |

## The Rules That Matter Daily

- Foundation types live in low-level crates; phases depend *forward*
  through explicit IR contracts.
- Tools and bindings use the `rumoca-compile` facade instead of reaching
  into phase internals.
- Backend/runtime crates stay thin around the shared solver and execution
  APIs; shared policy lives in the shared layer.
- Target-language specifics live in `target.toml` + templates, never in
  phase logic.

Architecture tests enforce the important edges. If a dependency you want
violates a spec, update the spec first — see
[Where the Rules Live](../contributing/specs-process.md).
