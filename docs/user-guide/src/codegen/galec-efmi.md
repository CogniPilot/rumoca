# eFMI Algorithm Code Export (GALEC)

Rumoca can project a compiled model into [eFMI](https://efmi-standard.org/)
Algorithm Code, the GALEC (Guarded Algorithmic Language for Embedded
Control) `.alg` representation, and package it as a schema-valid eFMU
container.

## The target

The `galec` target consumes the `dae` IR and accepts **fixed-sample discrete
models only** (models with no continuous states and no `der()`). It writes an
eFMI Algorithm Code eFMU: `AlgorithmCode/model.alg` + `manifest.xml`, plus
`__content.xml` and `schemas/`.

GALEC never emits C. The deleted `embedded-c-galec` and `galec-production`
spellings have no registry entries, aliases, or tailored compatibility
behavior. The eventual deployable eFMI product is
the `efmu` target: one `SolveAlgorithmProduct` retains its checked
`AlgorithmCodePackage` and correlated `SolveAlgorithmBlock`, and each output
file borrows the appropriate view. Its Production Code C leaf renders only
from the Solve block. Until that target lands, use `galec` for Algorithm Code. For a general
C export today, use the `fmi3` target (FMI 3.0 ME+CS): Model Exchange serves
host-owned integration and Co-Simulation serves the built-in solver.

## Exporting from the CLI

```bash
rumoca compile Model.mo --target galec -o out/
```

The `galec` target writes the eFMU container in two forms, a directory and
the equivalent `.efmu` zip:

```text
out/
  Model/                eFMU container, directory form
    __content.xml
    schemas/
    AlgorithmCode/      model.alg + manifest.xml
  Model.efmu            eFMU container, zip form (same content)
```

## Algorithm Code in the GUI

Select **Generate .alg** to project the Modelica source into GALEC Algorithm
Code. The `.alg` artifact opens in the same Monaco editor surface as the
Modelica input, with GALEC syntax highlighting and the GALEC language service
diagnostics/hover/definition hooks active. This browser render is
*identity-free*: it does not mint the eFMU **container** (the
`__content.xml` registry, the representation `manifest.xml`, and the checksum
web); those container artifacts are produced by the CLI packaging step
(`rumoca compile … --target galec`).

## Try GALEC in the guide

The example below is a fixed-sample discrete counter, which is the subset the
current GALEC projection accepts.

```modelica,codegen
// rumoca-live-scenario: ../repo-examples/codegen/rumoca-scenario.galec_counter.toml
```

Native run:

```bash
cargo run -p rumoca -- \
  compile examples/models/GalecCounter.mo \
  --model GalecCounter \
  --target galec \
  --output examples/codegen/gen/galec_counter
```

## See also

- [Targets and Templates](./targets.md): the full target list and the
  live `rumoca targets` readiness table.
- The authoritative contract for what each target emits and which
  conformance rung it claims is
  [SPEC_0034](https://github.com/cognipilot/rumoca/blob/main/spec/SPEC_0034_GALEC_EFMI_EXPORT.md);
  this page does not restate its rules.
