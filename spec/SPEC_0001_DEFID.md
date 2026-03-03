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

## Rationale
- Inspired by Rust compiler's `DefId` which provides stable cross-crate references
- u32 is sufficient for any realistic model (4B declarations)
- Copy semantics enable efficient passing without cloning

## References
- Rust compiler DefId: https://rustc-dev-guide.rust-lang.org/ty.html
