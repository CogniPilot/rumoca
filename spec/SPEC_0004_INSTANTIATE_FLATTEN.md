# SPEC_0004: Separate Instantiation and Flattening Phases

## Status
ACCEPTED

## Summary
Model instantiation (applying modifications, building instance tree) and flattening (generating qualified names, expanding connections) are separate compiler phases.

## Motivation
Separating instantiation from flattening follows MLS §5.6 which defines them as
logically distinct operations. The benefits of phase separation are well-established
in compiler design (Cooper & Torczon, *Engineering a Compiler*):
- Instantiation can be cached per class + modification set
- Flattening is a straightforward tree traversal
- Error messages are clearer when phases are distinct

Combining them creates a "big ball of mud" that's hard to optimize and debug.

## Specification

### Phase 1: Instantiate (Typed → InstTree)

**Input:** Typed IR, model name to instantiate

**Output:** InstTree (instance hierarchy with modifications applied)

**Responsibilities:**
1. Find the root model class
2. Apply modifications recursively (top-down)
3. Evaluate structural parameters (array sizes, conditional components)
4. Build instance tree preserving hierarchy
5. Collect connections (not yet expanded)

```rust
pub struct InstTree {
    pub root: InstNode,
    pub functions: Vec<InstFunction>,
}

pub struct InstNode {
    pub name: String,
    pub def_id: DefId,
    pub type_id: TypeId,
    pub kind: InstNodeKind,
    pub span: Span,
}

pub enum InstNodeKind {
    Class {
        components: Vec<InstNode>,
        equations: Vec<InstEquation>,
        algorithms: Vec<InstAlgorithm>,
        connections: Vec<InstConnection>,  // Not yet expanded
    },
    Variable { ... },
    Record { ... },
}
```

### Phase 2: Flatten (InstTree → Flat)

**Input:** InstTree

**Output:** Flat model with qualified names

**Responsibilities:**
1. Traverse instance tree, collecting variables with qualified names
2. Expand connections into equations (flow balance, stream inStream)
3. Expand for-loops into individual equations
4. Scalarize arrays (if configured)
5. Generate qualified names: `body.position.x`

```rust
pub struct Flat {
    pub name: String,
    pub variables: Vec<Variable>,
    pub equations: Vec<Equation>,
    pub initial_equations: Vec<Equation>,
    pub when_clauses: Vec<WhenClause>,
    pub functions: Vec<Function>,
}

pub struct Variable {
    pub name: String,  // Qualified: "body.pos.x"
    // ... attributes
}
```

### Connection Expansion

Connections are stored in InstTree but expanded in Flatten:

```modelica
connect(a.p, b.p)  // In InstTree as InstConnection

// Expanded in Flat to:
// For potential variables:
a.p.v = b.p.v

// For flow variables:
a.p.i + b.p.i = 0
```

### Caching Opportunities

```
InstTree = instantiate(Typed, model_name, modifications)
         = cached by (class_def_id, modification_hash)

Flat = flatten(InstTree)
     = pure function, no caching needed (fast)
```

## Rationale
- Follows the logical separation defined in MLS §5.6
- Clear phase boundary enables targeted optimization
- Modification handling is isolated in instantiate phase
- Flattening becomes a simple tree walk

## References
- Modelica Language Specification §5.6: Scoping, Name Lookup, and Flattening
- Modelica Specification §7: Inheritance, Modification
