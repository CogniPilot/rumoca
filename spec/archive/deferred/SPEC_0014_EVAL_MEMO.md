# SPEC_0014: Eval Memoization at Phase Boundaries

## Status
DEFERRED

> **Note:** Expression semantic_digest() exists but memoization framework is not implemented.
> This is an optimization that can be added when compilation performance becomes a concern.

## Summary
Phases that evaluate expressions (flatten, todae) MUST use memoization with key `(eval_version, expr_semantic_hash, env_semantic_hash)` to enable caching and avoid redundant computation.

## Motivation
Expression evaluation during flattening can be expensive:
- Array size computations: `n * m * p` evaluated repeatedly
- Parameter expressions: `Modelica.Constants.pi` resolved many times
- Start values: Same expression evaluated for each instance

Without memoization, compilation time grows unnecessarily.

## Specification

### Memoization Contract

```rust
memo_key = (eval_version, expr_semantic_digest, env_semantic_digest)
```

- **eval_version:** Evaluator semantics version (bumped when evaluation rules change)
- **expr_semantic_digest:** Full 256-bit structural digest of the canonicalized expression
- **env_semantic_digest:** Full 256-bit digest of all in-scope bindings

### Why eval_version?

If evaluation semantics change (e.g., how `smooth()` is handled, or constant folding rules), old cache entries must not be reused. The version acts as a cache invalidation mechanism:

```rust
/// Bump this when evaluation semantics change.
/// This prevents stale cache entries from persisting across compiler updates.
pub const EVAL_VERSION: u32 = 1;
```

### EvalCache Implementation

```rust
pub struct EvalCache {
    cache: HashMap<CacheKey, Value>,
}

/// Cache key uses full 256-bit digests to minimize collision risk.
/// See SPEC_0016 for collision posture.
pub type CacheKey = (u32, [u8; 32], [u8; 32]);  // (version, expr_digest, env_digest)

impl EvalCache {
    pub fn new() -> Self {
        EvalCache { cache: HashMap::new() }
    }
    
    /// Evaluate expression with memoization.
    pub fn eval(&mut self, expr: &Expr, env: &Env) -> Value {
        let expr_digest = expr.semantic_digest();
        let env_digest = env.semantic_digest();
        let key = (EVAL_VERSION, expr_digest, env_digest);
        
        if let Some(v) = self.cache.get(&key) {
            return v.clone();
        }
        
        let v = self.eval_uncached(expr, env);
        self.cache.insert(key, v.clone());
        v
    }
    
    fn eval_uncached(&self, expr: &Expr, env: &Env) -> Value {
        // Actual evaluation logic
        todo!()
    }
}
```

### Environment Digest

The environment digest includes all bindings that could affect evaluation:

```rust
pub struct Env {
    /// Parameter bindings: name → value
    params: HashMap<String, Value>,
    /// Structural parameters (affect array sizes)
    structural: HashMap<String, i64>,
}

impl Env {
    pub fn semantic_digest(&self) -> [u8; 32] {
        let mut h = blake3::Hasher::new();
        
        // Sort keys for determinism
        let mut params: Vec<_> = self.params.iter().collect();
        params.sort_by_key(|(k, _)| *k);
        
        for (name, value) in params {
            h.update(&(name.len() as u32).to_le_bytes());
            h.update(name.as_bytes());
            value.hash_into(&mut h);
        }
        
        let mut structural: Vec<_> = self.structural.iter().collect();
        structural.sort_by_key(|(k, _)| *k);
        
        for (name, value) in structural {
            h.update(&(name.len() as u32).to_le_bytes());
            h.update(name.as_bytes());
            h.update(&value.to_le_bytes());
        }
        
        *h.finalize().as_bytes()
    }
}
```

### Phase Requirements

| Phase | Caching Requirement |
|-------|---------------------|
| `flatten` | MUST use EvalCache for parameter expressions, array sizes |
| `todae` | MUST use EvalCache for start/nominal/min/max values |
| `codegen` | MAY cache template output by DAE semantic_hash |

### What Environment Digest Includes

- All in-scope parameter values
- Structural parameter bindings (array sizes)
- Import bindings (resolved paths)

### What Environment Digest Excludes

- Source locations
- Comments
- Declaration order
- Variable names not referenced in expression

### Cache Lifetime

**Phase 1 (current):** Cache lifetime is single compilation unit. Caches are created fresh for each `compile()` call and discarded after.

**Future phases may add:**
- Persistent disk cache keyed by `(eval_version, file_content_hash)`
- Cross-compilation-unit caching for libraries

When persisting caches: `eval_version` ensures old entries are invalidated when evaluation rules change.

### Example: Array Size Caching

```modelica
model Test
  parameter Integer n = 100;
  parameter Integer m = 200;
  Real x[n, m];    // Needs to evaluate n*m
  Real y[n, m];    // Same n*m, should hit cache
  Real z[n, m];    // Same n*m, should hit cache
end Test;
```

```rust
// During flatten
let size_expr = parse_expr("n * m");
let env = Env::from_params(&[("n", 100), ("m", 200)]);

// First call: computes and caches
let size1 = cache.eval(&size_expr, &env);  // → 20000

// Second call: cache hit
let size2 = cache.eval(&size_expr, &env);  // → 20000 (cached)

// Third call: cache hit
let size3 = cache.eval(&size_expr, &env);  // → 20000 (cached)
```

### Thread Safety

For parallel compilation, use concurrent cache:

```rust
pub struct ConcurrentEvalCache {
    cache: dashmap::DashMap<CacheKey, Value>,
}
```

## Rationale
- Avoids redundant computation in common cases
- Full 256-bit digests prevent cache corruption (see SPEC_0016)
- eval_version prevents stale entries after compiler updates
- Environment digest ensures correctness (no stale values)
- Single compilation unit lifetime avoids staleness bugs (Phase 1)

## References
- SPEC_0005: Two-Hash Strategy
- SPEC_0006: N-ary Add/Mul with Sorting (for expr digest)
- SPEC_0013: Unified stable_id Contract
- SPEC_0016: Collision Detection Posture
