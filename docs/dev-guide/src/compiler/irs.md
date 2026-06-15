# The Four IRs

Each IR is a serializable data structure crate (`rumoca-ir-*`) with a
schema version. This chapter says what each one *is for*; the binding
contracts live in
[SPEC_0007](https://github.com/CogniPilot/rumoca/blob/main/spec/SPEC_0007_IR_PIPELINE.md).

## AST (`rumoca-ir-ast`)

The parser's output: concrete syntax with comments and spans. It represents
*text*, not semantics — no name resolution, no types. Formatters,
pretty-printers, and doc generators work here because they need the original
syntax. Every node carries a `Span`, and that provenance must survive any
AST merging.

Dump it: `rumoca compile Model.mo --emit ast-mo` (or `ast-json`).

## Flat (`rumoca-ir-flat`)

The instantiated, modified, flattened model: variables and equations with
fully qualified names, no unresolved references, no pending modification
chains. Two things are deliberately *not* done yet:

- **Arrays stay symbolic.** Scalarization happens later, with shape
  metadata, only for backends that need it.
- **Modelica operators survive.** `der()`, `pre()`, `sample()`,
  `initial()` are still expression nodes; their semantic lowering is the
  DAE phase's job.

Flat is the right level for flat-Modelica export and for structural
transformations that preserve Modelica expression form.

Dump it: `--emit flat-mo` / `flat-json`.

## DAE (`rumoca-ir-dae`)

The stable contract of the whole project: the MLS Appendix B canonical DAE.
Pure functions over the variable vector `v := [p; t; ẋ; x; y; z; m;
pre(z); pre(m)]`, partitioned by kind:

| ID | Function | Role |
|---|---|---|
| B.1a | `fx(v, c) = 0` | Continuous DAE residual |
| B.1b | `fz(v, c) = 0` | Discrete real update |
| B.1c | `fm(v, c) = 0` | Discrete-valued update |
| B.1d | `fc(relation(v))` | Event conditions |

By the time a model is DAE, **no source temporal operator survives** — not
in any partition. `pre` has become explicit `__pre__.*` parameter slots;
`reinit` has become guarded update equations; `sample` has become event and
clock metadata; `assert`/`terminate` have become event actions. A validation
pass (`appendix_b_validation`) enforces this positively rather than relying
on downstream code to cope.

The DAE is also deliberately *lean*: it represents Modelica semantics and
source identity, never solver work products. If you are tempted to cache a
Jacobian, a BLT ordering, or a scalarized variant inside the DAE — that
belongs in structural analysis results or Solve artifacts.

Dump it: `--emit dae-mo` / `dae-json`. The `dae-mo` form is what the
**Show DAE** buttons in both guides render.

## Solve (`rumoca-ir-solve`)

A register-machine representation of the DAE functions: `ComputeBlock`
graphs mixing `ScalarProgramBlock`s (flat register programs, one scalar
output each) with tensor program nodes (`MatMul`, `LinSolve`, …) that carry
explicit shape/layout metadata and a scalar fallback. Solve adds no new
mathematics — it changes *format* so execution backends (interpreter,
Cranelift JIT, MLIR, CUDA, generated C/Rust) can consume the system
directly, choosing scalar expansion or native tensor kernels.

Dump it: `--emit solve-json` (there is no Modelica rendering of Solve).

## Schema Versions

Serialized DAE and Solve payloads carry a mandatory root `schema_version`;
deserializers reject unsupported versions. The policy is in
[IR Schema Versioning](../schema-versioning.md).
