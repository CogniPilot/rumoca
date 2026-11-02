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
- **Any number of block instances per program (GAL-039).** The array
  intermediates the generated bodies work in, and every generated function's
  outputs, are working memory, not interface, and that working memory is a
  member of the block-state structure: no mutable file-scope scratch arena
  exists. Distinct state objects therefore have disjoint working storage and may
  step concurrently. The generated header states this; it is not left for a
  reader to discover, and it reports the per-instance cost as
  `RUMOCA_<MODEL>_SCRATCH_BYTES` next to an `AUXILIARY_SCRATCH_BYTES` of zero.
  Concurrent calls on *one* state object remain forbidden: the three methods
  share that instance's slots, so they require ordinary exclusive mutable
  ownership, which is already true of a block whose methods are its only entry
  points.
- **A local read before it is written observes another owner's data.** GALEC
  does not require definite assignment of locals, so a body may read one; the
  value it gets was always arbitrary, but with the overlay it is now whatever
  the frame sharing that storage last left there rather than what the same owner
  left on its own previous activation. The three method frames share one union,
  so a `DoStep` local read before assignment can carry a value `Startup` wrote.
  It takes a source defect to reach, and neither the old value nor the new one
  is one the source asked for, but the arbitrary value CHANGED, so a test that
  passed by accident on the old one may fail on this one.
- Working memory is **overlaid** by frame. The three method frames are one
  union, because the methods never run together on one state object. Each user
  function gets its call level, one more than its deepest caller's, and the
  frames of one level are one union: calls do not recurse, so a call chain
  strictly climbs levels and two frames of one level are never live together.
  The levels are assigned in `codegen/solve_block_renderer.rs`
  (`assign_call_levels`) and `model.h.jinja` prints the unions.
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
- `galec_solve_digest_routing` reads the Solve program digest both model units
  print after their banner and recomputes it from the checked package, so the
  C is tied to the refined block it came from, not to a target name.
- `galec_c_arrays` executes recursive tensor expressions and covers malformed
  shapes and unresolved values.
- `galec_equivalence` compares generated C execution with the checked GALEC
  evaluator.
- `galec_c_working_memory` executes a block whose value depends on three pieces
  of working memory surviving a call, so an overlay that put a caller and its
  callee on one piece of storage fails with a wrong *number*, not a wrong line
  of text, and checks that the header states one union per call level.
- The same file executes a block whose `DoStep` is a three-armed correction
  chain with one buffer per arm and one buffer read in every arm, so storage
  shared with the spanning buffer produces a wrong number on every path.

## Example

```sh
rumoca compile Controller.mo --model Controller --target embedded-c-galec --output generated
cc -O2 -std=c99 -pedantic -Wall -Wextra -Wconversion -Wsign-conversion \
  -Wshadow -Wundef -Wcast-qual -Wstrict-prototypes -Wmissing-prototypes \
  -Werror -c generated/Controller.c
```
