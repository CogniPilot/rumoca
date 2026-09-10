# `galec`

## Use case

Use this target to exchange or inspect a fixed-sample controller as an eFMI
Algorithm Code eFMU. It is the portable algorithm representation consumed by
eFMI-aware tools before platform production code is selected.

## Contract

- Readiness 2: the Algorithm Code eFMU container has schema, checksum, and
  parser round-trip evidence with corruption controls.
- Input: a checked Algorithm Code aggregate constructed from an admissible DAE.
- Output: directory and `.efmu` archive containing `__content.xml`, the pinned
  schema set, Algorithm Code manifest, and GALEC source.
- UUIDs, timestamps, representation references, and SHA-1 edges are generated
  transactionally from the exact written bytes.
- The GALEC source round-trips through Rumoca's independent parser.

## Unsupported

This target does not claim eFMI Production Code. Continuous-time dynamics,
unsupported GALEC forms, external functions/tables, random operations, and
runtime event iteration are rejected before packaging.

## Verification

- `cli_target_galec` validates the complete container against the vendored
  schema revision and recomputes every checksum.
- `rumoca-phase-parse-galec` round-trip tests validate the language surface.
- Deliberately corrupted archives prove the schema and checksum gates reject.

## Example

```sh
rumoca compile Controller.mo --model Controller --target galec --output generated
```
