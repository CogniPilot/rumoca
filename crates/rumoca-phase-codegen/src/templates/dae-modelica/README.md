# `dae-modelica`

## Use case

Use this target to inspect the canonical checked DAE that numerical and export
lowering consume. It provides a readable counterexample artifact when source,
Flat, DAE, and Solve behavior must be compared independently.

## Contract

- Readiness 1: the emitted model is independently recompiled by Rumoca and OMC.
- Input: checked DAE IR.
- Output: one Modelica class representing DAE variables, tensor domains,
  continuous and initialization owners, functions, and supported event owners.
- Every reference is derived from typed DAE identity and provenance.

## Unsupported

It is not original Modelica and does not reverse structural transformations.
Unsupported coordinates, built-ins, function statements, or owners invoke a
span-bearing `unsupported-feature:dae-modelica-*` failure; they are never
printed as placeholders.

## Verification

- `checked_dae_tests` covers supported tensor/function/event forms and focused
  fail-closed cases.
- `balance_pipeline_render_sim` recompiles and simulates emitted DAE source.
- `template_target_ci::modelica_interchange_targets_round_trip_through_the_compiler`
  exercises the full target manifest.

## Example

```sh
rumoca compile Vehicle.mo --model Vehicle --target dae-modelica --output generated
```
