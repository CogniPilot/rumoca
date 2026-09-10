# `galec-production`

## Use case

Use this target to deploy a fixed-sample controller as an eFMI Production Code
eFMU while retaining its Algorithm Code representation for traceability and
tool interchange.

## Contract

- Readiness 2: the packaged production-code web and generated C have executable
  conformance coverage.
- Input: one checked Algorithm Code aggregate.
- Output: directory and `.efmu` archive containing both AlgorithmCode and
  ProductionCode representations.
- Generated C99 functions and storage have complete LogicalData and BlockMethod
  mappings.
- GALEC Real values use C99 `float`; the Production Code manifest declares
  `efmiFloat32` and `32-bit` precision for the same storage.
- GALEC error signals are accumulated in a 32-bit status field using the
  standard bit encoding and mapped to Algorithm Code `ErrorSignalStatus`.
- The checksum graph binds both representations and every generated code file
  to the exact packaged bytes.

## Safety-assurance status

Production Code uses the same fixed-storage, bounded, non-recursive C subset as
`embedded-c-galec` and is designed for analysis against the MISRA C:2023
assurance profile in SPEC_0034 GAL-029/030. This is **not a MISRA compliance
claim**: MISRA Compliance:2020 planning, guideline classification, analyzer
evidence, deviations, and review records belong to the deploying project.
The shared template's documented C99 multidimensional-input const boundary
applies here as well; no cast is used to conceal it.

The checksummed eFMU is useful configuration evidence for a DO-178C lifecycle,
but it is not itself a DO-178C compliance showing. Model-to-generated-code
statements retain source-id/byte-range anchors, but a packaged file/line trace
map, project-level structural coverage, target/compiler assumptions, and a
reviewed choice between DO-330 tool qualification and independent output
verification remain required.

## Unsupported

A Production-Code-only container is never emitted. Continuous dynamics,
unsupported GALEC/C constructs, external calls, random operations, and runtime
event iteration fail before any package is committed.

## Verification

- `cli_target_galec_production` validates schemas, checksum/reference graphs,
  LogicalData coverage, C compilation, and runtime method behavior.
- Negative controls corrupt representation checksums and required mappings.
- The co-emitted Algorithm Code receives the same parser round-trip checks as
  the `galec` target.

## Example

```sh
rumoca compile Controller.mo --model Controller --target galec-production --output generated
```
