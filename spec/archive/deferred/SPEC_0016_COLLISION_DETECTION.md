# SPEC_0016: Collision Detection Posture

## Status
DEFERRED

> **Note:** 256-bit digests are used for expression hashing. Debug collision detection
> is not implemented as collision probability is negligible for typical model sizes.

## Summary
u64 IDs (VarId, EqId, etc.) are acceptable for storage, but cache keys use wide digests (256-bit). Debug/test builds detect collisions by verifying same u64 implies same fingerprint.

## Motivation
u64 from BLAKE3 has ~2^64 possible values. Birthday paradox gives ~50% collision chance at ~2^32 items. For most models this is fine, but:

1. **Silent corruption:** A collision means two different things get the same ID
2. **Hard to debug:** Symptoms appear far from cause
3. **No detection:** Without checks, you'd never know

## Specification

### ID Width Policy

| Use Case | Width | Rationale |
|----------|-------|-----------|
| VarId, EqId, EventId, FuncId | u64 | Compact storage, fast comparison |
| Cache keys (EvalCache) | 256-bit | Low collision risk for correctness-critical path |
| Semantic/Debug hash | 256-bit | Integrity verification |

### Cache Key Type

```rust
/// Cache key uses full 256-bit digest to minimize collision risk.
pub type CacheKey = ([u8; 32], [u8; 32]);  // (expr_digest, env_digest)

pub struct EvalCache {
    cache: HashMap<CacheKey, Value>,
}

impl EvalCache {
    pub fn eval(&mut self, expr: &Expr, env: &Env) -> Value {
        // Use full 256-bit digests, not truncated u64
        let key = (expr.semantic_digest(), env.semantic_digest());
        
        if let Some(v) = self.cache.get(&key) {
            return v.clone();
        }
        
        let v = self.eval_uncached(expr, env);
        self.cache.insert(key, v.clone());
        v
    }
}
```

### Debug Collision Detection

In debug/test builds, maintain a reverse map to detect collisions:

```rust
#[cfg(debug_assertions)]
pub struct CollisionDetector {
    /// Maps truncated ID → full fingerprint
    seen: HashMap<u64, [u8; 32]>,
}

#[cfg(debug_assertions)]
impl CollisionDetector {
    pub fn new() -> Self {
        CollisionDetector { seen: HashMap::new() }
    }
    
    /// Check for collision when creating an ID.
    /// Panics if same u64 maps to different fingerprint.
    pub fn check(&mut self, id: u64, fingerprint: &[u8; 32]) {
        if let Some(existing) = self.seen.get(&id) {
            assert_eq!(
                existing, fingerprint,
                "ID collision detected! u64={:016x} maps to different fingerprints:\n\
                 existing: {}\n\
                 new:      {}",
                id,
                hex::encode(existing),
                hex::encode(fingerprint)
            );
        } else {
            self.seen.insert(id, *fingerprint);
        }
    }
}

#[cfg(not(debug_assertions))]
pub struct CollisionDetector;

#[cfg(not(debug_assertions))]
impl CollisionDetector {
    pub fn new() -> Self { CollisionDetector }
    pub fn check(&mut self, _id: u64, _fingerprint: &[u8; 32]) {}
}
```

### Integration with ID Generation

```rust
impl VarId {
    pub fn new(qualified_name: &str, kind: VarKind, detector: &mut CollisionDetector) -> Self {
        let normalized = Self::normalize_name(qualified_name);
        
        // Compute full fingerprint
        let fingerprint = IdBuilder::new()
            .push_u64(kind as u64)
            .push_str(&normalized)
            .fingerprint(ROLE_VAR);  // Returns [u8; 32]
        
        // Truncate to u64
        let id = u64::from_le_bytes(fingerprint[0..8].try_into().unwrap());
        
        // Check for collision in debug builds
        detector.check(id, &fingerprint);
        
        VarId(id)
    }
}

impl IdBuilder {
    /// Compute full 256-bit fingerprint.
    pub fn fingerprint(self, role_tag: &[u8]) -> [u8; 32] {
        let mut hasher = blake3::Hasher::new();
        hasher.update(&(role_tag.len() as u32).to_le_bytes());
        hasher.update(role_tag);
        hasher.update(&(self.bytes.len() as u64).to_le_bytes());
        hasher.update(&self.bytes);
        *hasher.finalize().as_bytes()
    }
    
    /// Compute truncated u64 ID (for storage).
    pub fn finish(self, role_tag: &[u8]) -> u64 {
        let fp = self.fingerprint(role_tag);
        u64::from_le_bytes(fp[0..8].try_into().unwrap())
    }
}
```

### Thread-Local Detector

For parallel compilation:

```rust
thread_local! {
    static COLLISION_DETECTOR: RefCell<CollisionDetector> = 
        RefCell::new(CollisionDetector::new());
}

impl VarId {
    pub fn new(qualified_name: &str, kind: VarKind) -> Self {
        COLLISION_DETECTOR.with(|detector| {
            Self::new_with_detector(qualified_name, kind, &mut detector.borrow_mut())
        })
    }
}
```

### Test for Collision Detection

```rust
#[test]
#[should_panic(expected = "ID collision detected")]
fn test_collision_detection() {
    let mut detector = CollisionDetector::new();
    
    // Simulate a collision by forcing same u64 with different fingerprints
    // (In practice, you'd need to find an actual collision)
    detector.check(0x1234, &[0u8; 32]);
    detector.check(0x1234, &[1u8; 32]);  // Should panic
}
```

### CI Integration

```yaml
# .github/workflows/ci.yml
- name: Run tests with collision detection
  run: cargo test
  env:
    RUSTFLAGS: "-C debug-assertions"
```

### What Happens on Collision?

| Build | Collision Behavior |
|-------|-------------------|
| Debug | Panic with diagnostic message |
| Release | Silent (no detection) |
| Test | Panic, test fails |

If a collision is detected in CI:
1. Investigate the two inputs that collided
2. Consider using 128-bit IDs if model size warrants
3. File a bug report with reproduction case

### Collision Probability

For u64 IDs with uniform BLAKE3 distribution:

| Items | P(collision) |
|-------|--------------|
| 1,000 | ~10^-13 |
| 10,000 | ~10^-11 |
| 100,000 | ~10^-9 |
| 1,000,000 | ~10^-7 |
| 10,000,000 | ~10^-5 |

For typical models (<100k items), collision is astronomically unlikely but not impossible.

## Rationale
- u64 IDs are compact and fast for normal operation
- 256-bit cache keys prevent cache corruption
- Debug detection catches the rare collision before it causes damage
- Zero runtime cost in release builds

## References
- SPEC_0013: Unified stable_id Contract
- SPEC_0014: Eval Memoization
- Birthday problem: https://en.wikipedia.org/wiki/Birthday_problem
