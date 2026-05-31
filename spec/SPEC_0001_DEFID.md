# SPEC_0001: DefId for Stable References

## Status
ACCEPTED

## Summary
Every declaration in the Rumoca compiler receives a unique `DefId` that remains stable across compilation phases.

## Specification

### DefId Structure
```rust
/// Located in rumoca-core/src/lib.rs
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Serialize, Deserialize)]
pub struct DefId(pub u32);

impl DefId {
    pub fn new(index: u32) -> Self;
    pub fn index(&self) -> u32;
}
```

### Assignment Rules
1. DefIds are assigned during the resolve phase
2. Each syntactic declaration gets exactly one DefId
3. DefIds are local to a compilation unit (not globally unique)
4. DefId(0) is reserved for the root/global scope

### What Gets a DefId
- Class definitions (model, block, connector, record, function, etc.)
- Component declarations
- Parameter declarations
- For-loop iterators
- Function formal parameters
- Nested class definitions

### What Does NOT Get a DefId
- Expressions
- Equations
- Statements
- Annotations

### Semantic Identity Keys

**Hard rule:** compiler semantic identity is `DefId` based. Compiler data
structures that identify declarations MUST key by `DefId`, or by a small
structured key whose semantic identity fields are all `DefId` values. After name
resolution, string hashing is not permitted for compiler semantic identity.
Rendered strings, `VarName`, flat names, component-reference display text, and
other textual names are display/source/protocol data, not semantic identity.

Hashing rendered text for compiler identity is prohibited. This includes
hashing a `String`, `&str`, `VarName`, cached display name, rendered
`ComponentPath`, or rendered `ComponentReference` in any semantic map. Hashing a
wrapper around those textual values is also prohibited. If code wants such a key
after resolution, that is a phase-boundary bug: move the `DefId` to that point
and key by `DefId`.

Allowed semantic keys after resolution:
- `DefId`
- Tuples or structs whose identity fields are `DefId` values
- Opaque local indices that are allocated from `DefId`-keyed tables and cannot
  be reconstructed from rendered text

Allowed exceptions:
- Name-resolution scopes before a declaration has been resolved. These must use
  structured component-reference keys, not raw string keys, and must produce
  `DefId` results. They must not escape the resolution phase or be reused as
  downstream semantic identity.
- Serialized JSON, diagnostics, user-facing output, and source text boundaries.
- Builtin registries or protocol/config maps where the external contract is a
  textual name.

If a phase needs to look up metadata for a resolved class, component, iterator,
or function parameter, the lookup key is the declaration's `DefId`. Re-parsing
or hashing a rendered name to recover identity is prohibited. If the code only
has a textual name at that point, carry the `DefId` forward instead of
recreating identity from text.

## Rationale
- Inspired by Rust compiler's `DefId` which provides stable cross-crate references
- u32 is sufficient for any realistic model (4B declarations)
- Copy semantics enable efficient passing without cloning

## References
- Rust compiler DefId: https://rustc-dev-guide.rust-lang.org/ty.html
