# SPEC_0006: N-ary Add/Mul with Sorting for Canonicalization

## Status
ACCEPTED

## Summary
Addition and multiplication are represented as n-ary operations with operands sorted by their full semantic digest (not a partial comparator). This ensures a total order for deterministic hashing.

## Motivation
For caching and diffing to work, expressions must have a canonical form:
- `a + b` and `b + a` should hash identically
- `(a + b) + c` and `a + (b + c)` should hash identically
- Nested additions should flatten to a single n-ary Add

An ad-hoc comparator that returns `Equal` for unhandled cases breaks canonicalization silently.

## Specification

### N-ary Representation

```rust
pub enum Expr {
    // N-ary commutative operations
    Add(Vec<Expr>),  // a + b + c + ...
    Mul(Vec<Expr>),  // a * b * c * ...

    // Binary non-commutative operations (NOT n-ary)
    Sub(Box<Expr>, Box<Expr>),  // a - b
    Div(Box<Expr>, Box<Expr>),  // a / b
    Pow(Box<Expr>, Box<Expr>),  // a ^ b

    // ...
}
```

### Canonicalization Algorithm

For each `Add` or `Mul` node:
1. Recursively canonicalize all children first
2. Flatten nested same-type nodes (e.g., `Add(Add(a, b), c)` → `Add(a, b, c)`)
3. Sort operands by their full 256-bit semantic digest

**CRITICAL:** Sorting MUST use full semantic digest, not a partial comparator.

**REQUIRED approach:**
```rust
// GOOD: total order via digest comparison
flat.sort_by(|a, b| {
    a.semantic_digest().cmp(&b.semantic_digest())
});
```

**PROHIBITED approach:**
```rust
// BAD: returns Equal for unhandled variants — NOT a total order
fn expr_cmp(a: &Expr, b: &Expr) -> Ordering {
    a.tag().cmp(&b.tag()).then_with(|| {
        match (a, b) {
            (Expr::Real(x), Expr::Real(y)) => float_cmp(*x, *y),
            // ... some cases
            _ => Ordering::Equal  // WRONG!
        }
    })
}
```

### Semantic Digest

Each expression variant has a unique tag byte. The `hash_into` method writes the tag followed by variant-specific content (length-prefixed for variable-length fields). The full implementation lives in the source code — the key properties are:

| Property | Requirement |
|----------|-------------|
| Unique tag per variant | Every `Expr` variant gets a distinct `u8` tag byte |
| Length-prefixed collections | `Add`/`Mul` args, `Call` args, `String` values use length prefix |
| Canonical NaN/zero | NaN normalizes to `0x7FF8...`, `-0.0` normalizes to `0.0` |
| Recursive hashing | Compound expressions hash their children recursively |

```rust
impl Expr {
    /// Compute 256-bit semantic digest for total ordering.
    /// Must be called on already-canonicalized expressions.
    pub fn semantic_digest(&self) -> [u8; 32] {
        let mut hasher = blake3::Hasher::new();
        self.hash_into(&mut hasher);
        *hasher.finalize().as_bytes()
    }
}
```

### What IS Canonicalized

- Operand order in `Add` and `Mul` (sorted by digest)
- Nested `Add`/`Mul` flattened (associativity)
- NaN and `-0.0` normalized in literals

### What Is NOT Canonicalized

- `x + x` is NOT simplified to `2*x` (no algebraic simplification)
- `x * 1` is NOT simplified to `x`
- `0 + x` is NOT simplified to `x`

Canonicalization is structural, not algebraic.

## Rationale
- Full digest ensures total order without ad-hoc comparator maintenance
- Sorting by `[u8; 32]` is robust to expression tree depth/shape
- Adding new `Expr` variants doesn't break existing canonicalization

## References
- SPEC_0005: Two-Hash Strategy
- SPEC_0013: Unified stable_id Contract
- BLAKE3 specification: https://github.com/BLAKE3-team/BLAKE3
