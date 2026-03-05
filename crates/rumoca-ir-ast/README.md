# rumoca-ir-ast

Class-tree and phase-typed AST IR for Rumoca compilation.

## Role in Rumoca
Canonical data representation for parsed/resolved/typed/instanced class structures consumed by early compiler phases.

## Public Surface
- Phase wrappers: `ParsedTree`, `ResolvedTree`, `TypedTree`.
- Core structures: `ClassTree`, `StoredDefinition`, `ClassDef`, `Component`, `Expression`, `Equation`, `Statement`.
- Shared ID re-exports used across phases: `DefId`, `ScopeId`, `TypeId`.

## Inputs
- Parser-produced syntax structures plus phase annotations.

## Outputs
- Serializable class-tree IR passed across parse/resolve/typecheck/instantiate.

## Design Constraints
- Keep IR crate behavior-light and data-focused.
- Preserve source and identity traceability needed for diagnostics.
- Maintain stable handoff structures across phases.
