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
- **One block instance per program.** The array intermediates the generated
  bodies work in, and every generated function's outputs, are working memory,
  not interface: they live in one file-scope object private to the `.c`, so two
  block-state objects in one program would share one set of slots. The
  generated header states this; it is not left for a reader to discover. The
  three methods must likewise not run concurrently or reentrantly on one state,
  which is already true of a block whose methods are its only entry points.
- **A local read before it is written observes another owner's data.** GALEC
  does not require definite assignment of locals, so a body may read one; the
  value it gets was always arbitrary, but with the overlay it is now whatever
  the owner sharing that storage last left there rather than what the same owner
  left on its own previous activation. The three methods share one group, so a
  `DoStep` local read before assignment can carry a value `Startup` wrote. It
  takes a source defect to reach, and neither the old value nor the new one is
  one the source asked for — but the arbitrary value CHANGED, so a test that
  passed by accident on the old one may fail on this one.
- Working memory is **overlaid**: the regions of two owners share storage only
  where the checked projection has established that the owners can never be
  active at the same time — a caller's call depth is strictly below its
  callee's, so equal-depth owners lie on no common call chain. The decision is
  taken in `views/algorithm_code_typed.rs` (`ScratchLayoutView`) and this
  target prints it; the generated source reports the achieved total and the
  call chain that determines it.
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
- `symbols.jinja` is a **support partial**, declared by this target's
  `[[partials]]` entry and published to the shared render environment as
  `galec-c-symbols.jinja`. A support partial renders no product file — it has
  no `[[files]]` entry by construction — and exists only to be imported by the
  templates that print C identifiers. It is the single declaration site of the
  C symbol policy (reserved spellings, generated namespace) and of the two
  allocated symbol tables, so this target's header/source and the
  `galec-production` Production Code manifest all read one allocation.
- The two artifact templates are additionally published under
  `[[files]].shared_as` names (`galec-model.{c,h}.jinja`): the GALEC-derived C
  body is target-agnostic, and `galec-production` `{% extends %}` these files
  to override only its conformance banner.
- Shared names live in ONE global namespace owned by the built-in target
  manifests. `build.rs` rejects a duplicate name and rejects any bundled
  `.jinja` that no `[[files]]`/`[[partials]]` entry declares. A copied target
  *directory* cannot register or shadow a shared name; the loader rejects such
  a manifest rather than letting the copy's edited partial silently no-op.

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
- `galec_c_working_memory` executes a block whose value depends on three pieces
  of working memory surviving a call, so an overlay that put a caller and its
  callee on one piece of storage fails with a wrong *number*, not a wrong line
  of text. The layout rule itself is tested as a property of the call graph in
  `views::algorithm_code_typed::layout_tests`.

## Example

```sh
rumoca compile Controller.mo --model Controller --target embedded-c-galec --output generated
cc -std=c99 -pedantic -Wall -Wextra -Wconversion -Wsign-conversion \
  -Wshadow -Wundef -Wcast-qual -Wstrict-prototypes -Wmissing-prototypes \
  -Werror -c generated/Controller.c
```
