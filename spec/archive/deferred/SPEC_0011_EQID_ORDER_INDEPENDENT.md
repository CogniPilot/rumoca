# SPEC_0011: Order-Independent EqId (Expression-Based)

## Status
ACCEPTED

## Summary
EqId is derived from equation kind, instance path, and structural expression hash — NOT from equation index. This makes EqId order-independent and stable under reordering.

## Motivation
An early design used equation index:

```rust
// REJECTED
EqId::new(kind, origin: "{instance_path}:{equation_index}", ...)
```

Problems:
1. **Reordering sensitivity:** Moving an equation changes its index
2. **Inheritance fragility:** Base class changes renumber equations
3. **Violation of architectural invariant:** IDs shouldn't change due to harmless edits

## Specification

### EqId Construction

```rust
impl EqId {
    /// Create EqId from equation metadata.
    ///
    /// # Arguments
    /// * `kind` - Equation category (ODE, algebraic, initial)
    /// * `instance_path` - Stable path in model hierarchy, e.g. "body.spring"
    /// * `expr_hash` - Structural hash of canonicalized residual
    pub fn new(kind: EqKind, instance_path: &str, expr_hash: &[u8; 32]) -> Self {
        let id = IdBuilder::new()
            .push_u64(kind as u64)
            .push_str(instance_path)
            .push_bytes(expr_hash)
            .finish(ROLE_EQ);
        EqId(id)
    }
    
    pub fn from_expr(kind: EqKind, instance_path: &str, expr: &Expr) -> Self {
        let canonical = expr.canonicalize();
        let expr_hash = canonical.semantic_digest();
        Self::new(kind, instance_path, &expr_hash)
    }
}
```

### What Determines EqId

| Component | Example | Changes When |
|-----------|---------|--------------|
| kind | `EqKind::Ode` | Equation reclassified |
| instance_path | `"body.spring"` | Model hierarchy changes |
| expr_hash | `[0x3a, 0xf2, ...]` | Equation math changes |

### What Does NOT Affect EqId

- Position in source file
- Order among equations in same scope
- Comments
- Whitespace/formatting

### Handling Duplicate Equations

Identical equations at the same path produce the same EqId. This is valid:

```modelica
model Test
  extends Base;  // Adds: der(x) = v
  // ... later, same equation appears again
equation
  der(x) = v;    // Same EqId as inherited one!
end Test;
```

Storage uses `Vec<(EqId, Eq)>` to handle duplicates:

```rust
pub struct Eqs {
    pub ode: Vec<(EqId, Eq)>,   // Not IndexMap!
    pub alg: Vec<(EqId, Eq)>,
    pub init: Vec<(EqId, Eq)>,
}
```

### Finalization Sorts by EqId

```rust
impl Dae {
    pub fn finalize(&mut self) {
        // Sort equations by EqId for determinism
        self.eqs.ode.sort_by_key(|(id, _)| id.0);
        self.eqs.alg.sort_by_key(|(id, _)| id.0);
        self.eqs.init.sort_by_key(|(id, _)| id.0);
        // ...
    }
}
```

### Instance Path Format

Instance path is the dot-separated component path:

```
Root model              → ""
component body          → "body"
component body.spring   → "body.spring"
array element body[1]   → "body.1"
nested body[1].pos.x    → "body.1.pos.x"
```

### Example: Stability Under Reordering

```modelica
// Version A
model M
equation
  der(x) = v;      // EqId based on "der(x) - v"
  der(v) = -9.81;  // EqId based on "der(v) - (-9.81)"
end M;

// Version B (reordered)
model M
equation
  der(v) = -9.81;  // Same EqId!
  der(x) = v;      // Same EqId!
end M;
```

Both versions produce identical DAE (after sorting).

## Rationale
- Order-independence is a core architectural invariant
- Expression hash captures semantic identity
- Duplicates are rare but valid (inheritance)
- Vec storage handles duplicates without special cases

## References
- SPEC_0006: N-ary Add/Mul with Sorting
- SPEC_0013: Unified stable_id Contract
- Architectural invariant: "IDs stable under harmless edits"
