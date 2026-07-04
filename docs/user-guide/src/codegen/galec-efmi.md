# eFMI Algorithm Code Export (GALEC)

Rumoca can project a compiled model into [eFMI](https://efmi-standard.org/)
Algorithm Code — the GALEC (Guarded Algorithmic Language for Embedded
Control) `.alg` representation — and package it as a schema-valid eFMU
container.

## The three targets

All three GALEC targets consume the `dae` IR and accept **fixed-sample
discrete models only** — models with no continuous states and no `der()`.

| Target | Output | eFMI container? |
|---|---|---|
| `galec` | eFMI Algorithm Code eFMU: `AlgorithmCode/Model.alg` + `manifest.xml`, plus `__content.xml` and `schemas/` | Yes |
| `galec-production` | eFMI Production Code eFMU: adds `ProductionCode/` C99 + LogicalData manifest, co-emits the `AlgorithmCode/` representation | Yes |
| `embedded-c-galec` | GALEC-derived embedded C (`.h` + `.c`): block-state struct with `startup`/`recalibrate`/`dostep` | No — not an eFMI container |

## Exporting from the CLI

```bash
rumoca compile Model.mo --target galec -o out/
rumoca compile Model.mo --target galec-production -o out/
rumoca compile Model.mo --target embedded-c-galec -o out/
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
`out/Model.c` — no manifest and no container.

## Code generation in the GUI

The scenario config editor (the VS Code custom editor and its web build)
exposes a **Code Generation** task. Pick a GALEC target from the codegen
target dropdown (`galec`, `galec-production`, or `embedded-c-galec`), choose
an output location, and select **Generate Code** to produce and inspect the
GALEC `.alg` text and generated C source. (This GUI support is being added
alongside this page.)

## See also

- [Targets and Templates](./targets.md) — the full target list and the
  live `rumoca targets` readiness table.
- The authoritative contract for what each target emits and which
  conformance rung it claims is
  [SPEC_0034](https://github.com/cognipilot/rumoca/blob/main/spec/SPEC_0034_GALEC_EFMI_EXPORT.md);
  this page does not restate its rules.
