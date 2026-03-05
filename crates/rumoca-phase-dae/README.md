# rumoca-phase-dae

Lowering phase from flat equations to hybrid DAE representation.

## Role in Rumoca
Converts `rumoca-ir-flat::Model` to `rumoca-ir-dae::Dae`, including variable classification, equation partitioning, and runtime-precompute metadata.

## Public Surface
- Main entry points: `to_dae`, `to_dae_with_options`.
- Public options/error types: `ToDaeOptions`, `ToDaeError`, `ToDaeResult`.

## Inputs
- Flattened model IR from `rumoca-phase-flatten`.

## Outputs
- Canonical DAE IR consumed by simulation and code generation.

## Design Constraints
- Enforce DAE shape and consistency contracts at this boundary.
- Keep solver-specific behavior out of this phase.
- Preserve algorithm/function information needed downstream.
