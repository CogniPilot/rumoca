# SPEC_0002: Scope Tree for Name Lookup

## Status
ACCEPTED

## Summary
Name resolution uses a scope tree for managing nested scopes, with hierarchical lookup that walks from the current scope toward the root (MLS §5.3).

## Motivation
Modelica has complex scoping rules:
- Nested classes create new scopes
- For-loops introduce iterator variables
- Functions have input/output/protected sections
- Extends clauses import names from base classes
- Encapsulated classes block upward lookup (MLS §5.3.1)

A scope tree (not just a stack) is needed because:
- Multiple scopes can be active simultaneously (extends)
- Lookups may need to search multiple branches
- Encapsulation boundaries must be respected

## Specification

### Data Structures

Located in `rumoca-ir-ast/src/scope.rs`:

```rust
pub struct ScopeTree {
    scopes: Vec<Scope>,
}

pub struct Scope {
    pub kind: ScopeKind,
    pub parent: Option<ScopeId>,
    pub members: IndexMap<ComponentPath, DefId>,
    pub imports: Vec<Import>,
}

pub enum ScopeKind {
    Global,
    Package,
    Class,
    Encapsulated,
    Function,
    ForLoop,
}
```

### Operations

**REQUIRED pattern — creating scopes:**
```rust
// Create child scope
let child = scope_tree.create_scope(parent_id, ScopeKind::Class);

// Add a member to a scope
scope_tree.add_member(scope_id, name, def_id);

// Add an import
scope_tree.add_import(scope_id, import);
```

**REQUIRED pattern — name lookup:**
```rust
// Hierarchical lookup: walks from scope toward root
let def_id = scope_tree.lookup(scope_id, name);

// Local-only lookup: checks only the given scope
let def_id = scope_tree.lookup_local(scope_id, name);

// Lookup with exclusion (for extends resolution)
let def_id = scope_tree.lookup_excluding(scope_id, name, Some(self_def_id));
```

**PROHIBITED:**
- Bypassing the ScopeTree to do manual name lookup
- Assuming scope IDs are sequential (use `ScopeId` opaque type)
- Keying scope members by raw strings, `VarName`, or rendered flat names
- Hashing display text, `String`, `&str`, `VarName`, rendered
  `ComponentPath`, or rendered `ComponentReference` to stand in for
  declaration identity

Scope lookup keys are structured component-reference paths while resolution is
in progress. Once a declaration has been resolved, downstream semantic maps use
`DefId` as specified by SPEC_0001. Post-resolution string hashing is
prohibited for compiler identity. Rendered names may be cached for diagnostics,
serialization, and display, but they are not scope identity.

If a downstream phase needs scope-like lookup, the scope entries must carry and
return `DefId` values. A phase must not rebuild semantic identity by formatting,
splitting, or hashing names that were already resolved earlier in the pipeline.

### Lookup Semantics (MLS §5.3)

1. Check direct members of the current scope
2. Check imports in the current scope
3. Move to parent scope and repeat
4. Stop at global scope (return None if not found)

Encapsulated scopes (`ScopeKind::Encapsulated`) block upward lookup — names must be found locally or via imports.

## Rationale
- Tree structure handles Modelica's extends semantics
- IndexMap for members preserves insertion order (SPEC_0021)
- ScopeId is an opaque index into the tree, not a raw u32

## References
- Modelica Specification §5.2–§5.3: Scope and Name Lookup
