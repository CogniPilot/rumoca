# SPEC_0010: Case-Sensitive Identifiers

## Status
ACCEPTED

## Summary
VarId, FuncId, and all stable IDs preserve identifier case. Modelica is case-sensitive, so `myVar` and `MyVar` are distinct.

## Motivation
An early design lowercased all names for ID generation:

```rust
// REJECTED
fn normalize_name(name: &str) -> String {
    name.to_ascii_lowercase()  // WRONG!
}
```

This causes collisions:
```modelica
model Test
  Real myVar;   // VarId("myvar", State)
  Real MyVar;   // VarId("myvar", State) — COLLISION!
end Test;
```

## Specification

### Modelica Case Sensitivity

From Modelica Specification §2.3.1:
> "Modelica is a case-sensitive language."

Examples of distinct identifiers:
- `position` ≠ `Position` ≠ `POSITION`
- `der` (built-in) vs `Der` (user identifier)
- `MyModel` vs `myModel`

### VarId Generation

```rust
impl VarId {
    pub fn new(qualified_name: &str, kind: VarKind) -> Self {
        let normalized = Self::normalize_name(qualified_name);
        let id = IdBuilder::new()
            .push_u64(kind as u64)
            .push_str(&normalized)  // Case preserved!
            .finish(ROLE_VAR);
        VarId(id)
    }

    fn normalize_name(name: &str) -> String {
        let mut result = String::with_capacity(name.len());
        let mut in_bracket = false;
        for c in name.chars() {
            match c {
                '[' => { in_bracket = true; result.push('.'); }
                ']' => in_bracket = false,
                ',' if in_bracket => result.push('.'),
                c if c.is_whitespace() => {}
                c => result.push(c),  // NO LOWERCASING
            }
        }
        result
    }
}
```

### FuncId Generation

```rust
impl FuncId {
    pub fn new(name: &str, num_inputs: usize, num_outputs: usize) -> Self {
        let id = IdBuilder::new()
            .push_str(name)  // Case preserved!
            .push_u64(num_inputs as u64)
            .push_u64(num_outputs as u64)
            .finish(ROLE_FUNC);
        FuncId(id)
    }
}
```

### Test Cases

```rust
#[test]
fn var_id_case_sensitive() {
    let a = VarId::new("myVar", VarKind::State);
    let b = VarId::new("MyVar", VarKind::State);
    let c = VarId::new("MYVAR", VarKind::State);
    
    assert_ne!(a, b);
    assert_ne!(b, c);
    assert_ne!(a, c);
}

#[test]
fn func_id_case_sensitive() {
    let a = FuncId::new("myFunction", 1, 1);
    let b = FuncId::new("MyFunction", 1, 1);
    
    assert_ne!(a, b);
}

#[test]
fn var_id_deterministic() {
    // Same case = same ID
    let a = VarId::new("body.position.x", VarKind::State);
    let b = VarId::new("body.position.x", VarKind::State);
    
    assert_eq!(a, b);
}
```

### What IS Normalized

Only structural variations, not case:
- `x[1,2]` → `x.1.2` (subscript syntax)
- `x[ 1 ]` → `x.1` (whitespace removed)
- `  x  ` → `x` (leading/trailing whitespace)

### What Is NOT Normalized

- Case: `MyVar` stays `MyVar`
- Unicode: `naïve` stays `naïve`

## Rationale
- Matches Modelica language semantics
- Prevents silent collisions
- Downstream tools expect case-sensitive names
- Generated code (CasADi, Julia) preserves case

## References
- Modelica Specification §2.3.1: Identifiers
