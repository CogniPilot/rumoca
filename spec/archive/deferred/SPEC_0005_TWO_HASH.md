# SPEC_0005: Two-Hash Strategy (Semantic vs Debug)

## Status
ACCEPTED

## Summary
The DAE representation includes two hashes: a semantic hash for caching (ignores origins) and a debug hash for diffing (includes origins).

## Motivation
Different use cases require different hash granularity:
- **Caching:** "Has the math changed?" → Only care about semantic content
- **Debugging:** "Did the compilation path change?" → Care about origins too

A single hash can't serve both purposes well.

## Specification

### Semantic Hash

**Purpose:** Cache invalidation, "has the math changed?"

**Includes:**
- Model name
- Variable names, types, attributes (start, min, max, nominal)
- Equation residual structures (canonicalized)
- Event conditions and actions
- Function signatures and bodies

**Excludes:**
- Source file names and locations
- Comments
- Instance paths (where equations came from)
- Variable declaration order (sorted by VarId)

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SemanticHash(pub [u8; 32]);

fn compute_semantic_hash(&self) -> SemanticHash {
    let mut h = StableHasher::new();
    h.update_str(&self.name);
    
    // Hash vars in ID order (sorted)
    for (id, var) in &self.vars.x {
        h.update_u64(id.0);
        h.update_str(&var.name);
        // ... attributes
    }
    
    // Hash equations by structure
    for (id, eq) in &self.eqs.ode {
        h.update_u64(id.0);
        h.update(&eq.residual.semantic_digest());
    }
    
    h.finalize_semantic()
}
```

### Debug Hash

**Purpose:** Golden test diffs, "did the compilation path change?"

**Includes:**
- Everything in semantic hash
- Origin information encoded in IDs (instance_path in EqId)
- Unit strings (may differ based on import path)

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DebugHash(pub [u8; 32]);

fn compute_debug_hash(&self) -> DebugHash {
    let mut h = StableHasher::new();
    
    // Start with semantic content
    h.update(&self.semantic_hash.0);
    
    // Add origin info (encoded in EqId via instance_path)
    for (id, _) in &self.eqs.ode {
        h.update_u64(id.0);
    }
    
    h.finalize_debug()
}
```

### Usage Patterns

```rust
// Caching: skip regeneration if math unchanged
if cached_dae.semantic_hash == new_dae.semantic_hash {
    return cached_codegen_output;
}

// CI diffing: detect any change including origin
if golden.debug_hash != actual.debug_hash {
    report_diff(golden, actual);
}
```

### Hash Computation Order

1. `finalize()` sorts all collections by ID
2. Compute `semantic_hash` from sorted content
3. Compute `debug_hash` from semantic_hash + origins

## Rationale
- Two hashes serve distinct purposes without compromise
- Debug hash builds on semantic hash (additive, not separate)
- Semantic hash enables aggressive caching without false positives

## References
- SPEC_0013: Unified stable_id Contract
- SPEC_0014: Eval Memoization
