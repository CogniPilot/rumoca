# SPEC_0013: Unified stable_id Contract

## Status
ACCEPTED

## Summary
All stable IDs (VarId, EqId, EventId, FuncId) are generated through a single `stable_id(role_tag, canonical_bytes)` function. No ad-hoc string formatting.

## Motivation
Early designs used string formatting:

```rust
// REJECTED
let input = format!("{}:{}:{}", kind_str, origin, hex::encode(hash));
let hash = blake3::hash(input.as_bytes());
```

Problems:
1. **Ambiguity:** String separators can appear in content
2. **Allocation:** Format strings allocate
3. **Inconsistency:** Each ID type had different formatting
4. **Hard to test:** No single spec to verify

## Specification

### Core Function

```rust
/// Generate stable ID from role tag and content.
/// 
/// # Contract
/// - Same inputs → same output (deterministic)
/// - Different roles → different IDs (namespaced)
/// - Length-prefixed encoding (unambiguous)
pub fn stable_id(role_tag: &[u8], content: &[u8]) -> u64 {
    let mut hasher = blake3::Hasher::new();
    
    // Length-prefix the role tag
    hasher.update(&(role_tag.len() as u32).to_le_bytes());
    hasher.update(role_tag);
    
    // Length-prefix the content
    hasher.update(&(content.len() as u64).to_le_bytes());
    hasher.update(content);
    
    // Take first 8 bytes as u64
    let hash = hasher.finalize();
    u64::from_le_bytes(hash.as_bytes()[0..8].try_into().unwrap())
}
```

### IdBuilder Helper

```rust
pub struct IdBuilder {
    bytes: Vec<u8>,
}

impl IdBuilder {
    pub fn new() -> Self {
        IdBuilder { bytes: Vec::new() }
    }

    pub fn push_str(&mut self, s: &str) -> &mut Self {
        // Length-prefixed string
        self.bytes.extend_from_slice(&(s.len() as u32).to_le_bytes());
        self.bytes.extend_from_slice(s.as_bytes());
        self
    }

    pub fn push_u64(&mut self, v: u64) -> &mut Self {
        self.bytes.extend_from_slice(&v.to_le_bytes());
        self
    }

    pub fn push_bytes(&mut self, b: &[u8]) -> &mut Self {
        // Length-prefixed bytes
        self.bytes.extend_from_slice(&(b.len() as u32).to_le_bytes());
        self.bytes.extend_from_slice(b);
        self
    }

    pub fn finish(self, role_tag: &[u8]) -> u64 {
        stable_id(role_tag, &self.bytes)
    }
}
```

### Role Tags

```rust
const ROLE_VAR: &[u8] = b"var";
const ROLE_EQ: &[u8] = b"eq";
const ROLE_EVENT: &[u8] = b"event";
const ROLE_FUNC: &[u8] = b"func";
```

### ID Generation Pattern

All ID types follow the same pattern — build content with `IdBuilder`, finish with a role tag:

```rust
impl VarId {
    pub fn new(qualified_name: &str, kind: VarKind) -> Self {
        let normalized = Self::normalize_name(qualified_name);
        let id = IdBuilder::new()
            .push_u64(kind as u64)
            .push_str(&normalized)
            .finish(ROLE_VAR);
        VarId(id)
    }
}
```

`EqId`, `EventId`, and `FuncId` follow the same pattern with their respective role tags and content fields. The key invariant: every ID is `IdBuilder::new().push_*(...).finish(ROLE_TAG)`.

### Why Length-Prefixing?

Without length prefixes, concatenation is ambiguous:

```
push_str("ab") + push_str("cd") = "abcd"
push_str("abc") + push_str("d") = "abcd"  // COLLISION!
```

With length prefixes:

```
push_str("ab") + push_str("cd") = [2,0,0,0,'a','b',2,0,0,0,'c','d']
push_str("abc") + push_str("d") = [3,0,0,0,'a','b','c',1,0,0,0,'d']
// Different bytes → different hash
```

### Properties

1. **Deterministic:** Same inputs always produce same ID
2. **Role-namespaced:** Different role tags can't collide
3. **Unambiguous:** Length-prefixing prevents content collisions
4. **Efficient:** No string allocation, direct byte operations
5. **Testable:** Single function to verify

### Test Suite

```rust
#[test]
fn different_roles_dont_collide() {
    let var = IdBuilder::new().push_str("test").finish(ROLE_VAR);
    let eq = IdBuilder::new().push_str("test").finish(ROLE_EQ);
    assert_ne!(var, eq);
}

#[test]
fn length_prefix_prevents_collision() {
    let a = IdBuilder::new()
        .push_str("ab")
        .push_str("cd")
        .finish(ROLE_VAR);
    let b = IdBuilder::new()
        .push_str("abc")
        .push_str("d")
        .finish(ROLE_VAR);
    assert_ne!(a, b);
}

#[test]
fn deterministic() {
    let a = IdBuilder::new().push_str("test").push_u64(42).finish(ROLE_VAR);
    let b = IdBuilder::new().push_str("test").push_u64(42).finish(ROLE_VAR);
    assert_eq!(a, b);
}
```

## Rationale
- Single source of truth for ID generation
- Binary encoding is unambiguous
- No string formatting overhead
- Easy to audit and test
- Matches architectural invariant: "stable_id(role_tag, semantic_content)"

## References
- BLAKE3 specification: https://github.com/BLAKE3-team/BLAKE3
- SPEC_0010: Case-Sensitive Identifiers
- SPEC_0011: Order-Independent EqId
