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
- GALEC Real values use C99 `float` storage and arithmetic for embedded
  deployment.
- Tensor assignments preserve checked extents and deterministic row-major
  storage.
- Each source-derived statement retains its stable Modelica source id and exact
  byte range in an adjacent generated-C trace comment.
- The standard `ErrorSignalStatus` is reset at each method boundary, explicit
  signals set their assigned bits, and Real comparisons set `NAN` and return
  false when either operand is NaN.
- It is deliberately a non-eFMI deployment track.

## Safety-assurance status

The emitted subset is designed for analysis against the MISRA C:2023
assurance profile in SPEC_0034 GAL-029/030: fixed storage, statically bounded
control flow, fixed-width integers, no recursion or dynamic allocation, and no
function-like macros in generated implementation or API code. This is **not a
MISRA compliance claim**. Such a claim additionally requires the project's
MISRA Compliance:2020 plan, guideline classification, pinned qualified-capable
analyzer results, reviewed deviations, and review records.

C99 has one deliberate const-correctness boundary: a writable multidimensional
array cannot be passed implicitly as a pointer to an array of const elements
before C23. Rank-one input arrays are const-qualified; higher-rank function
inputs remain unqualified, while checked Algorithm Code construction proves
they are never written. The eventual MISRA profile must classify this against
guideline 8.13 and record a deviation if the selected analyzer requires one;
the generator will not insert a qualifier-dropping cast.

The target can contribute source and verification evidence to a DO-178C
project, but neither Rumoca nor its output is DO-178C compliant or a qualified
code generator. A packaged source-id/file and generated-line trace map,
project-level structural coverage, and a reviewed DO-330 qualification or
independent-output-verification strategy remain required certification-project
evidence.

## Unsupported

This is not an eFMI Production Code container and emits no LogicalData or
ManifestReference metadata. Continuous-time dynamics, unsupported array forms,
external calls, random operations, and runtime event iteration fail closed.

## Verification

- `cli_target_embedded_c_galec` compiles the generated C under strict
  conversion warnings and checks both the target's explicit non-eFMI
  self-description and its no-compliance-claim banner.
- `galec_c_arrays` executes recursive tensor expressions and covers malformed
  shapes and unresolved values.
- `galec_equivalence` compares generated C execution with the checked GALEC
  evaluator.

## Example

```sh
rumoca compile Controller.mo --model Controller --target embedded-c-galec --output generated
cc -std=c99 -pedantic -Wall -Wextra -Wconversion -Wsign-conversion \
  -Wshadow -Wundef -Wcast-qual -Wstrict-prototypes -Wmissing-prototypes \
  -Werror -c generated/Controller.c
```
