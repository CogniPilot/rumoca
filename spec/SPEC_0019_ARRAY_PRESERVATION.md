# SPEC_0019: Array Preservation Through Flatten Phase

## Status
ACCEPTED

## Summary
Array variables remain as single variables with dimension metadata through the flatten and DAE phases. Scalarization is deferred to code generation.

Array-expression structure is also preserved: array comprehensions (e.g., `{f(i) for i in 1:n}`) remain structured expression nodes through flatten and DAE. They are not eagerly expanded into explicit element lists in core IR.

## Motivation
Early scalarization in the flatten phase causes several problems:

1. **Loss of array identity** - Array functions like `transpose(A)` can't find variable `A` when it's been expanded to `A[1,1]`, `A[1,2]`, etc.
2. **Explosion of variable count** - A declaration like `Real A[100,100]` would become 10,000 separate variables.
3. **Inefficient representation** - Array-aware solvers can handle arrays natively; premature scalarization removes this optimization opportunity.
4. **Lost semantic information** - The relationship between array elements is lost after expansion.

## Design

### Array Dimension Storage

Array dimensions are stored in a `dims` field on variable structures:

```rust
// In Variable (flat.rs)
pub struct Variable {
    pub name: String,
    pub type_id: TypeId,
    pub dims: Vec<i64>,  // Array dimensions, empty for scalars
    // ...
}

// In Var, DiscreteVar, Const (dae/lib.rs)
pub struct Var {
    pub name: String,
    pub typ: VarType,
    pub dims: Vec<i64>,  // Array dimensions, empty for scalars
    // ...
}
```

### Pipeline Flow

```
Typed IR:     Type::Array { element, dims: [2,3] }
     |
Instantiate:  InstVariable.array_dims = [2,3]
     |
Flatten:      Variable { name: "A", dims: [2,3] }  <- PRESERVED
     |
ToDae:        Var { name: "A", dims: [2,3] }      <- PRESERVED
     |
Codegen:      Scalarize if needed for solver
```

### Array Comprehension Preservation

Array comprehensions are expression-level array constructors and must remain symbolic in compiler IR:

```modelica
z = sum({cellData.rcData[k].R for k in 1:cellData.nRC});
```

Preserved as structured expression form (Flat/DAE):

```text
BuiltinCall(sum, [ArrayComprehension(expr=..., indices=[k in 1:n], ...)])
```

#### Required Behavior

1. Flatten MUST preserve `ArrayComprehension` expression nodes in equation residuals and bindings.
2. DAE lowering MUST preserve `ArrayComprehension` expression nodes (or an equivalent structured representation), not force eager element expansion.
3. Balance/dimension inference MAY evaluate comprehension index ranges for scalar-count and shape inference, but this analysis MUST NOT rewrite the residual expression into an eagerly expanded literal array.
4. Backend/runtime/codegen layers MAY lower comprehensions to loops or explicit element operations as target-specific implementation details.

#### Non-Goal of This Rule

- This rule does not change statement-level handling of `for`-equations. `for`-equations are equation-set constructs; array comprehensions are expression constructs. They are intentionally treated separately.

### Balance Checking

Variable and equation counting must account for array dimensions:

```rust
/// Count scalar elements in Var map, accounting for array dimensions.
fn count_var_elements(vars: &IndexMap<VarId, Var>) -> usize {
    vars.values()
        .map(|v| {
            if v.dims.is_empty() {
                1
            } else {
                v.dims.iter().product::<i64>() as usize
            }
        })
        .sum()
}

/// Get total number of variables accounting for array dimensions.
pub fn effective_variable_count(&self) -> usize {
    count_var_elements(&self.vars.x)
        + count_var_elements(&self.vars.z)
        + count_var_elements(&self.vars.u)
        + count_var_elements_const(&self.vars.c)
        + count_var_elements_discrete(&self.vars.q)
}
```

Similarly, `effective_equation_count()` counts array equation elements by examining `Expr::Array` nodes in residuals.

### Future: Scalarization for Solvers

For solvers that require scalar form, an optional `scalarize()` method can be added to `Dae`:

```rust
impl Dae {
    /// Scalarize arrays for solver backends that require scalar form.
    pub fn scalarize(&mut self) {
        // Expand array variables to scalars: A[2,3] -> A[1,1], A[1,2], ...
        // Expand array equations to scalar equations
    }
}
```

This allows the choice of representation to be made at code generation time based on the target solver's capabilities.

## Affected Files

| File | Changes |
|------|---------|
| `crates/rumoca-ir/src/flat.rs` | Added `dims: Vec<i64>` to `Variable` |
| `crates/rumoca-dae/src/lib.rs` | Added `dims` to `Var`, `DiscreteVar`, `Const`; added `effective_variable_count()` |
| `crates/rumoca-phase-flatten/src/flatten.rs` | Removed `expand_array_variable()`, preserved dimensions |
| `crates/rumoca-phase-dae/src/todae.rs` | Copy dimensions from `Variable` to DAE variables |

## Backward Compatibility

- Scalar variables have `dims = []` (empty vector)
- Existing code paths are unchanged for scalars
- Array-unaware code can call `scalarize()` before processing

## Limitations

This specification covers variable and array-expression preservation. Additional work is needed for:
- Array subscript expressions (slicing like `A[1,:]`)
- Array equation expansion during balance checking
- Full array expression semantics

## References
- MLS Chapter 10: Array Expressions
- [SPEC_0004](SPEC_0004_INSTANTIATE_FLATTEN.md): Instantiate/Flatten Phases
- [SPEC_0003](SPEC_0003_HYBRID_DAE.md): Hybrid DAE Formulation
