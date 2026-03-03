# SPEC_0020: Model Algorithm Lowering and Function Algorithm Preservation

## Status
ACCEPTED

## Summary
ToDae uses a split policy:

1. **Model algorithms** (`Model.algorithms`, `Model.initial_algorithms`) are lowered to DAE equations when they match a supported declarative subset.
2. **Function algorithms** (`Function.body`) are **never** lowered in ToDae and remain structured for downstream code generation readability (especially embedded targets).

## Motivation
One policy for all algorithm sections is not sufficient:

1. Model algorithms that are declarative in practice should contribute directly to the DAE equation set.
2. Function bodies must remain readable and structured for embedded codegen and certification-oriented review.
3. Preserving function algorithm structure avoids lossy reconstruction in backends that emit human-readable code.

## Normative Rules

### 1. Function Algorithm Preservation (Required)

- ToDae MUST preserve all reachable function bodies in `Dae.functions` without lowering them to DAE equations.
- ToDae MUST NOT convert `Function.body` statements into `f_x`, `f_z`, `f_m`, `f_c`, or `initial_equations`.

### 2. Model Algorithm Lowering (Required)

- ToDae SHOULD lower model-level algorithm sections to equations when statements are in the supported declarative subset.
- Current supported subset:
  - Direct assignment statements.
  - If-assignment statements where all branches assign the same target and each branch lowers cleanly.
- Lowered runtime model algorithm equations go to `f_x`.
- Lowered initial model algorithm equations go to `initial_equations`.
- Model `when` statements that lower to assignment-form event updates MUST be routed into `f_z` or `f_m` according to MLS Appendix B variable categories.
- Unsupported model algorithm statements MUST fail ToDae with explicit `ED013`; they MUST NOT be silently dropped or retained in solver-facing DAE compatibility storage.

### 3. Safety Constraints (Required)

- If a model algorithm section cannot be fully lowered without changing semantics, ToDae MUST fail with `ED013` rather than partially lower or retain imperative runtime sections in DAE.
- Lowering MUST be deterministic and preserve scalar equation counts.

## DAE Shape Requirements

- DAE remains centered on MLS B.1 equation buckets (`f_x`, `f_z`, `f_m`, `f_c`).
- Function definitions remain available in `Dae.functions` for runtime evaluation and backend code generation.
- Solver-facing DAE MUST NOT include model-level `algorithms` or `initial_algorithms` fields.

## Validation Requirements

Implementations MUST include regression tests for:

1. Function algorithm preservation:
   - Reachable function with non-empty algorithm body remains non-empty in `Dae.functions`.
2. Model algorithm lowering:
   - Supported model-level algorithm statements lower into equation buckets.
   - Unsupported model-level statements fail ToDae with `ED013`.

## Relationship to Other Specs

- **SPEC_0003 (Hybrid DAE)**: DAE equation buckets remain canonical.
- **SPEC_0019 (Array Preservation)**: Same philosophy of preserving semantic structure where expansion would be lossy.
- **SPEC_0022 (MLS Compliance)**: Aligns algorithm handling with MLS semantics and current implementation boundaries.

## References

- MLS Chapter 11: Algorithm Sections
- MLS Chapter 12: Functions
