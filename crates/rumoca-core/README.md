# rumoca-core

Foundation crate with shared IDs, spans, diagnostics, and core utilities.

## Role in Rumoca
Tier-1 base crate used across the workspace for common primitives and helper APIs that must not create dependency cycles.

## Public Surface
- Core IDs/types used by other crates: `DefId`, `TypeId`, `ScopeId`, `Span`, `SourceMap`, `Diagnostics`, `Diagnostic`.
- Shared utility APIs: builtin predicates (`is_builtin_type`, `is_builtin_function`, `is_builtin_variable`) and workspace/MSL cache path helpers.
- Shared traits/aliases: `PhaseError`, `BoxedResult`.

## Inputs
- Calls from IR/phase/simulation/tool crates needing shared primitives.

## Outputs
- Stable cross-crate foundational types and utility behavior.

## Design Constraints
- Keep dependency surface minimal and stable.
- No phase-specific transforms or solver logic.
- Changes must be conservative due to wide transitive usage.
