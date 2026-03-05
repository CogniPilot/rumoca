# rumoca-phase-resolve

Name-resolution phase for class-tree symbols, definitions, and scopes.

## Role in Rumoca
Assigns stable definition IDs, builds scope relationships, resolves references, and validates unresolved symbols before type checking.

## Public Surface
- Main entry points: `resolve`, `resolve_with_options`, `resolve_with_stats`, `resolve_parsed`.
- Public options/stats types: `ResolveOptions`, `ResolutionStats`, `ResolveWithStatsResult`.
- Public diagnostics types: `ResolveError`, `ResolveResult`, `ValidationResult`, `UnresolvedSymbol`, `UnresolvedKind`.

## Inputs
- Parsed class tree (`ParsedTree`).

## Outputs
- `ResolvedTree` with populated IDs/scopes and resolution diagnostics.

## Design Constraints
- Deterministic ID/scope assignment for traceability.
- Keep resolution separate from type inference/flattening.
- Preserve full source context for diagnostics.
