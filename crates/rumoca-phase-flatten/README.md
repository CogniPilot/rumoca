# rumoca-phase-flatten

Flattening phase from instantiated class hierarchy to flat equation system.

## Role in Rumoca
Transforms `InstancedTree` into `flat::Model`, expanding hierarchy and connections into globally named equations while preserving algorithm/array semantics.

## Public Surface
- Main entry points: `flatten`, `flatten_ref`, `flatten_ref_with_options`.
- Public options/error types: `FlattenOptions`, `FlattenError`, `FlattenResult`.
- Timing/stat APIs used by tooling: `flatten_phase_timing_stats`, `reset_flatten_phase_timing_stats`.

## Inputs
- Instanced class tree and overlay data from `rumoca-phase-instantiate`.

## Outputs
- Flat model IR with variables, equations, algorithms, and connection-derived equations.

## Design Constraints
- Preserve array semantics and algorithm structure through flattening.
- Keep connection/equation expansion deterministic and diagnosable.
- Keep flatten-specific transforms in this crate.
