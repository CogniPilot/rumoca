# SPEC_0017: Ordered Collection Pattern

## Status
ACCEPTED

## Summary
IR and DAE data structures use `IndexMap` for deterministic iteration order. `HashMap`/`HashSet` are permitted only in phase-internal computation where iteration order does not affect output.

## Motivation
HashMap iteration order is non-deterministic. If IR data structures use HashMap, you get:
1. **Non-deterministic codegen output** — different generated code between runs
2. **Flaky tests** — golden tests fail intermittently
3. **Non-reproducible hashes** — DAE hashing depends on insertion order

## Specification

### Where IndexMap Is REQUIRED

All public fields on IR and DAE types that will be iterated over or serialized:

```rust
// REQUIRED: deterministic iteration for IR/DAE data
pub states: IndexMap<VarName, Variable>,
pub parameters: IndexMap<VarName, Variable>,
pub functions: IndexMap<VarName, Function>,
```

### Where HashMap/HashSet Is PERMITTED

Phase-internal computation where:
- The map is used only for lookup, never iterated in output
- The result is collected into an ordered structure before leaving the phase
- Performance matters more than order (e.g., structural analysis, matching)

```rust
// PERMITTED: phase-internal lookup, not iterated in output
let visited: HashSet<VarName> = HashSet::new();
let lookup: HashMap<&str, DefId> = HashMap::new();
```

### Rules

**REQUIRED:**
- All `IndexMap` fields in IR crates (`rumoca-ir-ast`, `rumoca-ir-flat`, `rumoca-ir-dae`) use `IndexMap`
- All `IndexMap` fields in the `Dae` struct use `IndexMap`
- Serialized output must be deterministic given the same input

**PROHIBITED:**
- `HashMap` or `HashSet` as a public field on any IR or DAE type
- Iterating a `HashMap` to produce output that affects codegen, hashing, or serialization

### Exceptions

Direct `IndexMap::new()` with manual insertion is acceptable when the insertion order is inherently deterministic (e.g., iterating a source that is already ordered). Document with a comment:

```rust
// Insertion order matches source order (already deterministic)
let mut map = IndexMap::new();
for item in already_ordered_items {
    map.insert(item.name.clone(), item);
}
```

## Rationale
- IndexMap preserves insertion order — deterministic iteration
- HashMap is faster for pure lookup — acceptable internally
- The boundary is clear: public IR/DAE fields use IndexMap, internal computation may use HashMap

## References
- indexmap crate: https://docs.rs/indexmap
