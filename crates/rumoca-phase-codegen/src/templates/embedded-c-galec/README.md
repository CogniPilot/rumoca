# `embedded-c-galec`

## Use case

Use this target to embed a fixed-sample discrete controller generated from the
same checked Algorithm Code block used by Rumoca's eFMI exports, when the
application needs plain C rather than an eFMU container.

## Contract

- Readiness 2: the embedded C product is built and executed by its target gate.
- Input: the checked Algorithm Code projection of an admissible DAE.
- Output: C99 header/source with a block-state structure and
  startup/recalibrate/do-step functions.
- Tensor assignments preserve checked extents and deterministic row-major
  storage.
- The standard `ErrorSignalStatus` is reset at each method boundary, explicit
  signals set their assigned bits, and Real comparisons set `NAN` and return
  false when either operand is NaN.
- It is deliberately a non-eFMI deployment track.

## Unsupported

This is not an eFMI Production Code container and emits no LogicalData or
ManifestReference metadata. Continuous-time dynamics, unsupported array forms,
external calls, random operations, and runtime event iteration fail closed.

## Verification

- `cli_target_embedded_c_galec` compiles the generated C and checks the target's
  explicit non-eFMI self-description.
- `galec_c_arrays` executes recursive tensor expressions and covers malformed
  shapes and unresolved values.
- `galec_equivalence` compares generated C execution with the checked GALEC
  evaluator.

## Example

```sh
rumoca compile Controller.mo --model Controller --target embedded-c-galec --output generated
cc -std=c99 -Wall -Werror -c generated/Controller.c
```
