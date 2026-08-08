# eFMI Algorithm Code Export (GALEC)

Rumoca can project a compiled model into [eFMI](https://efmi-standard.org/)
Algorithm Code — the GALEC (Guarded Algorithmic Language for Embedded
Control) `.alg` representation — and package it as a schema-valid eFMU
container.

## The four targets

All GALEC targets consume the `dae` IR and accept **fixed-sample
discrete models only** — models with no continuous states and no `der()`.

| Target | Output | eFMI container? |
|---|---|---|
| `galec` | eFMI Algorithm Code eFMU: `AlgorithmCode/Model.alg` + `manifest.xml`, plus `__content.xml` and `schemas/` | Yes |
| `galec-production` | eFMI Production Code eFMU: adds `ProductionCode/` C99 + LogicalData manifest, co-emits the `AlgorithmCode/` representation | Yes |
| `embedded-c-galec` | GALEC-derived embedded C (`.h` + `.c`): block-state struct with `startup`/`recalibrate`/`dostep` | No — not an eFMI container |
| `embedded-rust-galec` | GALEC-derived embedded Rust (`.rs`): self-contained `#![no_std]` crate root with `startup`/`recalibrate`/`do_step` returning `Result<(), Signals>` when the block declares signal escapes | No — not an eFMI container (Rust is outside the Beta-1 ProductionCode schema) |

The **eFMI container?** column describes the CLI packaging step. The GUI's
Generate Code (below) renders the inspectable `.alg`/`.h`/`.c` sources for any
target but does not itself build the container.

## Exporting from the CLI

```bash
rumoca compile Model.mo --target galec -o out/
rumoca compile Model.mo --target galec-production -o out/
rumoca compile Model.mo --target embedded-c-galec -o out/
rumoca compile Model.mo --target embedded-rust-galec -o out/
```

The `galec` and `galec-production` targets write the eFMU container in two
forms — a directory and the equivalent `.efmu` zip:

```text
out/
  Model/                eFMU container, directory form
    __content.xml
    schemas/
    AlgorithmCode/      Model.alg + manifest.xml
    ProductionCode/     galec-production only: Model.c/.h + manifest.xml
  Model.efmu            eFMU container, zip form (same content)
```

The `embedded-c-galec` target instead writes plain `out/Model.h` and
`out/Model.c`, and `embedded-rust-galec` writes `out/Model.rs` — no manifest
and no container. Models whose GALEC block declares signal escapes (e.g. a
Kalman filter using `Modelica.Math.Matrices.solve`, which maps to the
`solveLinearEquations` builtin) get the status ABI: the C methods return the
32-bit ErrorSignalStatus word and the Rust methods return
`Result<(), Signals>`. See `examples/models/QuadrotorAltitudeKF.mo` for a
full estimator exercising matrix algebra, computed initialization, and the
solve builtin across every target.

## Code generation in the GUI

The scenario config editor (the VS Code custom editor and its web build)
exposes a **Code Generation** task. Pick a GALEC target from the codegen
target dropdown (`galec`, `galec-production`, or `embedded-c-galec`), choose
an output location, and select **Generate Code**.

The GUI renders the **inspectable GALEC sources** in stages: first the `.alg`
Algorithm Code text, then generated C header/source from the current `.alg`
editor contents. This browser render is *identity-free* — it does not mint the
eFMU **container** (the `__content.xml` registry, per-representation
`manifest.xml`, and checksum web). For `galec`/`galec-production`, those
container artifacts are produced by the CLI packaging step
(`rumoca compile … --target galec-production`). Use the GUI to inspect and edit
the target-language code, and use the CLI when you need the full eFMU package.

## Try GALEC Production Code in the guide

The example below is a fixed-sample discrete counter, which is the subset the
current GALEC projection accepts. Select **Generate .alg** to project the
Modelica source into GALEC Algorithm Code. The `.alg` artifact opens in the
same Monaco editor surface as the Modelica input, with GALEC syntax
highlighting and the GALEC language service diagnostics/hover/definition hooks
active. Then select **Generate C/H** in the `.alg` panel to generate C header
and source from the current GALEC editor text.

```modelica,codegen
// rumoca-live-scenario: ../repo-examples/codegen/rumoca-scenario.galec_counter_production.toml
```

Native run:

```bash
cargo run -p rumoca -- \
  compile examples/models/GalecCounter.mo \
  --model GalecCounter \
  --target galec-production \
  --output examples/codegen/gen/galec_counter_production
```

## See also

- [Targets and Templates](./targets.md) — the full target list and the
  live `rumoca targets` readiness table.
- The authoritative contract for what each target emits and which
  conformance rung it claims is
  [SPEC_0034](https://github.com/cognipilot/rumoca/blob/main/spec/SPEC_0034_GALEC_EFMI_EXPORT.md);
  this page does not restate its rules.
