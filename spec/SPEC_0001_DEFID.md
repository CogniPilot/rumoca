# SPEC_0001: DefId for Stable References

## Status
ACCEPTED

## Summary
Rumoca uses source declaration `DefId`s for resolved declarations and
instance-unique identity for post-instantiation Flat/DAE runtime components.

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

### Identity Domains

| Identity | Owner/Where | Rule |
|---|---|---|
| Source declaration `DefId` | Resolve / class tree | One id per syntactic declaration |
| Instance identity | Flat, DAE, Solve, Sim | One id/key per concrete runtime component instance |
| Rendered name | Diagnostics, serialization, display | Never compiler semantic identity |

Source declaration identity and instance identity are different domains. A
component declaration inside a reusable class has one source declaration
`DefId`, but each instantiation of that declaration is a different runtime
component and MUST have unique post-instantiation identity.

### Assignment Rules

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| DefIds are assigned during resolve for source/class-tree declarations | Resolve | Declaration lookup needs stable ids |
| Each syntactic declaration gets exactly one source declaration DefId | Resolve | Source declarations are not duplicated |
| Flat/DAE runtime components use instance-unique identity | Flatten and later | Reused classes create distinct unknowns |
| `DefId(0)` is reserved for root/global scope | Core ids | Stable sentinel |
| DefIds are local to a compilation unit | Whole compiler | No cross-unit global registry |

Instance-unique identity may be represented by a dedicated instance `DefId`, or
by a small structured key whose identity fields are all `DefId` /
`InstanceId`-like opaque ids. Rendered names, flat paths, and
component-reference display text are not valid identity fields.

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

**Hard rule:** compiler semantic identity is resolved-id based. Compiler data
structures that identify declarations MUST key by `DefId`; compiler data
structures that identify concrete post-instantiation components MUST key by an
instance-unique resolved id/key. After name resolution, string hashing is not
permitted for compiler semantic identity.
Rendered strings, `VarName`, flat names, component-reference display text, and
other textual names are display/source/protocol data, not semantic identity.

Hashing rendered text for compiler identity is prohibited. This includes
hashing a `String`, `&str`, `VarName`, cached display name, rendered
`ComponentPath`, or rendered `ComponentReference` in any semantic map. Hashing a
wrapper around those textual values is also prohibited. If code wants such a key
after resolution, that is a phase-boundary bug: move the `DefId` to that point
and key by `DefId`.

Allowed semantic keys after resolution:
- `DefId` for source declarations
- Instance `DefId` for concrete Flat/DAE runtime components
- Tuples or structs whose identity fields are `DefId` / `InstanceId` values
- Opaque local indices allocated from resolved-id-keyed tables and not
  reconstructable from rendered text

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

For flattened component instances, source declaration DefId and instance
identity are distinct concepts. A variable such as `a.y` and a sibling variable
`b.y` may originate from the same source declaration `y`, but they are different
runtime unknowns and MUST have different post-instantiation identity. Downstream
DAE/Solve/Sim code must key or compare such variables by the instance identity,
not by the reused source declaration DefId.

If downstream code needs to answer "does this instance originate from this
source declaration?", it must use explicit ancestry metadata such as
`symbol_ancestry`, not string prefix checks or equality on reused source
declaration ids.

## Rationale
- Inspired by Rust compiler's `DefId` which provides stable cross-crate references
- u32 is sufficient for any realistic model (4B declarations)
- Copy semantics enable efficient passing without cloning

## References
- Rust compiler DefId: https://rustc-dev-guide.rust-lang.org/ty.html
