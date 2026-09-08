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
    predefined_members: IndexMap<ComponentPath, DefId>,
    lookup_schema: LookupSchema,
}

pub struct Scope {
    pub kind: ScopeKind,
    pub parent: Option<ScopeId>,
    pub members: IndexMap<ComponentPath, DefId>,
    pub imports: Vec<ScopeImport>,
    pub inherited_members: IndexMap<ComponentPath, InheritedMember>,
}

pub enum ScopeImport {
    SingleDefinition {
        name: ComponentPath,
        path: Vec<String>,
        def_id: DefId,
    },
    Wildcard {
        path: Vec<String>,
        names: IndexMap<ComponentPath, WildcardMember>,
    },
}

pub enum LookupOutcome {
    Found(DefId),
    Absent,
    AmbiguousInherited,
    AmbiguousUnqualifiedImport,
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

// Publish the complete resolved import set atomically
scope_tree.set_imports(scope_id, imports);

// Publish the effective inherited-member view atomically
scope_tree.set_inherited_members(scope_id, inherited_members);
```

**REQUIRED pattern — name lookup:**
```rust
// Hierarchical lookup: exhaustively handle the closed result
match scope_tree.lookup(scope_id, name) {
    LookupOutcome::Found(def_id) => use_definition(def_id),
    LookupOutcome::Absent => report_absence(),
    LookupOutcome::AmbiguousInherited => report_inherited_ambiguity(),
    LookupOutcome::AmbiguousUnqualifiedImport => report_import_ambiguity(),
}

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
  `ComponentReference`, or any compatibility path wrapper to stand in for
  declaration identity

Scope lookup keys are structured component references while resolution is in
progress. Once a declaration has been resolved, downstream semantic maps use
`DefId` as specified by SPEC_0001, or a purpose-built structured key whose
identity is derived from already-resolved IR. Post-resolution string hashing is
prohibited for compiler identity. Rendered names may be cached for diagnostics,
serialization, and display, but they are not scope identity.

If a downstream phase needs scope-like lookup, the scope entries must carry and
return `DefId` values. A phase must not rebuild semantic identity by formatting,
splitting, or hashing names that were already resolved earlier in the pipeline.

### Lookup Semantics (MLS §5.3.1)

1. Check declared elements of the current scope: direct members, then
   effective inherited members of the current class scope. A direct member
   hides an inherited member of the same name. Semantically equivalent
   declarations inherited from multiple bases share the first deterministic
   identity, while conflicting inherited names stop lookup without selecting
   an arbitrary declaration and without falling through to imports or
   enclosing scopes.
2. Check qualified import names declared in the current scope, including
   renamed aliases and selective imports. A selective import
   (`import A.{C,D}`) is equivalent to multiple single-definition imports and
   ranks with them, never with unqualified imports.
3. Check names supplied by unqualified imports declared in the
   current scope. A name supplied by more than one unqualified import is an
   error at use, never a first-clause selection; the error does not apply when
   a declared, inherited, or qualified-import match already resolved the name.
4. Move to parent scope and repeat.
5. Stop at global scope (return the closed absent outcome if not found).

Lookup publishes a closed outcome that distinguishes a found declaration,
absence, conflicting inherited declarations, and ambiguous unqualified
imports. Ambiguity must not be encoded as absence or repaired by a later
source-tree scan: the Resolve use that consumes the lookup outcome emits the
diagnostic and cannot mint a resolved identity from an ambiguous result.

This lookup-order contract does not itself establish the MLS §13.2.1.2
public/protected export filter. Until wildcard import construction carries that
visibility proof, this scope view is not evidence that protected package
members were excluded.

Encapsulated scopes (`ScopeKind::Encapsulated`) block upward lookup — names must be found locally or via imports.

Imports are not inherited. Effective inherited-member entries are populated
from resolved `extends` edges before class contents are resolved. Direct
and inherited elements rank above current-scope imports, while an unrelated
declaration in an enclosing scope cannot shadow an inherited member.

## Rationale
- Tree structure handles Modelica's extends semantics
- IndexMap for members preserves insertion order (SPEC_0021)
- ScopeId is an opaque index into the tree, not a raw u32

## References
- Modelica Specification §5.2–§5.3: Scope and Name Lookup
