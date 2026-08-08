# `base-modelica`

## Use case

Use this target to hand a flattened Rumoca model to a tool that consumes the
emerging Base Modelica interchange form, or to inspect whether hierarchy and
connections were eliminated without scalarizing arrays. It is an interchange
and debugging artifact, not a simulation runtime.

## Contract

- Input: checked Flat IR.
- Output: one Modelica-family source file with declarations and flattened
  equations kept in deterministic source order.
- Arrays and structured equations remain expressed at the Flat boundary.
- The current readiness is experimental; the target does not claim conformance
  to a finalized Base Modelica standard revision.

## Unsupported

The output is not original source, does not preserve class hierarchy or
annotations, and must not be presented as a formatter. A Flat construct that
cannot be represented must fail rendering instead of being omitted.

## Verification

- `template_target_ci::builtin_template_targets_render_or_are_explicit_readiness_zero_manifests`
  exercises the real manifest render path.
- `template_target_ci::modelica_interchange_targets_round_trip_through_the_compiler`
  reparses and recompiles the generated source.
- The Modelica interchange runtime lane also checks OMC acceptance.

## Example

```sh
rumoca compile Plant.mo --model Plant --target base-modelica --output generated
```
