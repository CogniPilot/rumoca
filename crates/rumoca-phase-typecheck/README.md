# rumoca-phase-typecheck

Type-checking and dimension/variability analysis phase.

## Role in Rumoca
Resolves type IDs, evaluates dimension expressions, marks structural parameters, and validates variability/causality constraints before instantiation.

## Public Surface
- Main entry points: `typecheck`, `typecheck_instanced`.
- Public error/result types: `TypeCheckError`, `TypeCheckResult`.
- Public evaluation support module: `eval`.

## Inputs
- Name-resolved class tree (`ResolvedTree`).

## Outputs
- Typed class tree (`TypedTree`) with type and structural metadata.

## Design Constraints
- Evaluate dimensions and structural parameters in this phase.
- Keep type rules explicit and diagnosable.
- Separate typing from instantiation/flattening transforms.
