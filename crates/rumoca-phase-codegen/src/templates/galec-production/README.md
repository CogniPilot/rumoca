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
- GALEC error signals are accumulated in a 32-bit status field using the
  standard bit encoding and mapped to Algorithm Code `ErrorSignalStatus`.
- The checksum graph binds both representations and every generated code file
  to the exact packaged bytes.

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
