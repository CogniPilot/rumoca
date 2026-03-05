# rumoca-ir-flat

Flat equation-system IR with globally unique names.

## Role in Rumoca
Intermediate handoff format between flattening and DAE lowering. Represents flattened variables/equations/algorithms before Appendix-B variable classification.

## Public Surface
- Core model types: `Model`, `Variable`, `Equation`, `Expression`, `Statement`, `VarName`.
- Connection/clock surfaces: `ConnectionSet` family and `ClockPartition` family.
- Helper APIs used by other crates: `component_base_name`, visitor traits/utilities.

## Inputs
- Flattened instance content from `rumoca-phase-flatten`.

## Outputs
- Flat IR consumed by DAE lowering, tooling, and diagnostics.

## Design Constraints
- Keep IR data-centric and serialization-friendly.
- Preserve array and algorithm structure for downstream phases.
- Keep naming globally unique and deterministic.
