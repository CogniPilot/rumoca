# `flat-modelica`

## Use case

Use this target to inspect the compiler immediately after instantiation and
flattening, or to replay that exact flattened equation system in OMC during
compiler-parity investigations. It is especially useful for locating whether
a discrepancy first appears before or after DAE lowering.

## Contract

- Readiness 1: the emitted model is independently recompiled by Rumoca and OMC.
- Input: checked Flat IR.
- Output: one self-contained Modelica class containing the flattened variables,
  equations, initial equations, and assertions.
- Expression structure and tensor dimensions are retained; class hierarchy and
  connection syntax have already been eliminated.

## Unsupported

This is not a source formatter and cannot recover original component names,
layout, comments, or annotations. It does not claim that every Modelica tool
will accept compiler-internal names; unsupported syntax must fail closed.

## Verification

- `cli_tests::compile_target_flat_modelica_uses_flat_template_context` checks
  the target boundary.
- `balance_pipeline_render_sim` recompiles and simulates Flat renderings.
- `template_target_ci::modelica_interchange_targets_round_trip_through_the_compiler`
  covers deterministic re-entry through Rumoca.

## Example

```sh
rumoca compile Vehicle.mo --model Vehicle --target flat-modelica --output generated
```
