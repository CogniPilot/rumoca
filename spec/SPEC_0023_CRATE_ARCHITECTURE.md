# SPEC_0023: Crate Architecture for MLS Compliance

## Status
DRAFT

## Summary
Maps SPEC_0022 (MLS Compiler Compliance) data structures and phases to rumoca crate organization.

## Design Rationale

### MLS Flexibility

The MLS explicitly allows implementation flexibility:

> "An implementation may delay and/or omit building parts of these trees, which means that the different steps can be interleaved." — MLS §5.6

The spec defines **logical** phases (Instantiation → Flattening) but does **not** mandate how to organize code.

### Industry Practice

Research into existing Modelica compilers reveals a preference for **explicit phase separation**:

- Separating instantiation, type checking, and flattening into distinct phases is standard practice in multi-pass compilers (see *Engineering a Compiler*, Cooper & Torczon).
- The Modelica Specification (MLS §5.6) defines these as logically distinct operations.
- JModelica/OPTIMICA uses JastAdd attribute grammars with declarative phase specifications where name analysis, type analysis, and flattening are separate concerns.

### Recommendation

**Keep phases in separate crates.** The evidence shows that:

1. **Performance**: Clean boundaries enable independent optimization
2. **Debugging**: Clear responsibility per phase simplifies diagnosis
3. **Incremental compilation**: Change isolation enables selective recompilation
4. **Testing**: Each phase can be tested in isolation

The MLS says you *may* interleave, not that you *should*. Successful compilers tend toward explicit phases with well-defined IR boundaries.

## Pipeline Mapping

```
MLS Phase          | rumoca Phase Crate           | rumoca IR Crate
-------------------|------------------------------|------------------
Parsing (§2,§13)   | rumoca-phase-parse           | rumoca-ir-ast
Name Resolution    | rumoca-phase-resolve         | rumoca-ir-resolved
Type Checking      | rumoca-phase-typecheck       | rumoca-ir-typed
Instantiation(§5.6)| rumoca-phase-instantiate     | rumoca-ir-inst
Flattening (§5.6)  | rumoca-phase-flatten         | rumoca-ir-flat
DAE Formation(App B)| rumoca-phase-dae          | rumoca-ir-dae
Code Generation    | rumoca-phase-codegen         | (solver-specific)
```

### Phase Separation Rationale

The MLS groups "Parsing" conceptually, but rumoca separates it into three phases following standard multi-pass compiler design:

| Phase | Responsibility | Why Separate |
|-------|----------------|--------------|
| **Parse** | Syntax → AST | Lexer/parser are mechanically different from semantic analysis |
| **Resolve** | Name lookup, DefId assignment | Complex scope rules (§5) warrant isolation |
| **Typecheck** | Type inference, variability | Type checking after instantiation ensures full modifier context is available (MLS §10.1) |

Keeping instantiation and typing as distinct phases enables better error messages and incremental compilation.

## Data Structure Mapping

### SPEC_0022 §3.1: Class Tree → rumoca-ir-ast + rumoca-ir-resolved + rumoca-ir-typed

MLS: "represents the syntactic information from the class definitions"

| MLS Concept | rumoca Type | Crate |
|-------------|-------------|-------|
| Class definition | `ClassDef` | rumoca-ir-ast |
| Component clause | `ComponentDecl` | rumoca-ir-ast |
| Extends clause | `ExtendsClause` | rumoca-ir-ast |
| Modification | `Modification` | rumoca-ir-ast |
| DefId (stable reference) | `DefId` | rumoca-ir-resolved |
| TypeId | `TypeId` | rumoca-ir-typed |

### SPEC_0022 §3.2: Instance Tree → rumoca-ir-inst

MLS: "contains the instantiated elements with redeclarations taken into account and merged modifications applied"

| MLS Concept | rumoca Type | Location |
|-------------|-------------|----------|
| Instance node | `InstNode` | rumoca-ir-inst |
| Instance variable | `InstVariable` | rumoca-ir-inst |
| Instance equation | `InstEquation` | rumoca-ir-inst |
| Instance algorithm | `InstAlgorithm` | rumoca-ir-inst |
| Instance connection | `InstConnection` | rumoca-ir-inst |
| Qualified name | `QualifiedName` | rumoca-ir-inst |
| Outer-to-inner mapping | `outer_to_inner` | InstTree |

### SPEC_0022 §3.3: Modification Environment → rumoca-phase-instantiate

MLS: "determines the values of modifiers"

| MLS Concept | rumoca Implementation |
|-------------|----------------------|
| Modification merging | `merge_modifications()` in instantiate/conflicts.rs |
| Outer overrides inner | Modification application order in `active_modifications` |
| Each modifier | `each` flag on `Modification`/`TypedModification` |
| Final modifier | `final_` flag prevents further modification |

### SPEC_0022 §3.5-3.6: Connection Set/Graph → rumoca-phase-flatten

MLS: "set of connectors that are directly or indirectly connected"

| MLS Concept | rumoca Implementation |
|-------------|----------------------|
| Connection set | `ConnectionSet` in flatten/connections.rs |
| Flow sum equations | Generated in `process_connections()` |
| Potential equality | Generated in `process_connections()` |
| Stream equations | Not yet implemented (see Missing/Future Work) |

### SPEC_0022 §3.7: Flat Equation System → rumoca-ir-flat

MLS: "flat equation system with globally unique variable names"

| MLS Concept | rumoca Type | Notes |
|-------------|-------------|-------|
| Flat variable | `Variable` | Qualified name, dims preserved (SPEC_0019) |
| Flat equation | `Equation` | Residual form |
| Flat algorithm | `Algorithm` | Structure preserved (SPEC_0020) |
| When clause | `WhenClause` | Discrete events |
| Function | `Function` | User-defined functions |

### SPEC_0022 §3.7: DAE System → rumoca-dae

MLS Appendix B variable classification:

| MLS Symbol | rumoca Field | Description |
|------------|--------------|-------------|
| p | `Dae.vars.p` | Parameters |
| x(t) | `Dae.vars.x` | Differential states |
| y(t) | `Dae.vars.z` | Algebraic variables |
| z(tₑ) | `Dae.vars.q` | Discrete Real |
| m(tₑ) | `Dae.vars.q` | Discrete-valued (Boolean, Integer) |
| c(tₑ) | (conditions) | Event conditions |
| u(t) | `Dae.vars.u` | Inputs |

MLS equation forms:

| MLS Form | rumoca Field | Description |
|----------|--------------|-------------|
| B.1a | `Dae.eqs.ode` + `Dae.eqs.alg` | Continuous equations |
| B.1b | `Dae.events` | Discrete Real updates |
| B.1c | `Dae.events` | Discrete-valued assignments |
| B.1d | (event conditions) | Condition evaluation |

### SPEC_0022 §3.16: Type Attributes → rumoca-ir-flat, rumoca-dae

| MLS Attribute | Variable Field | Var Field |
|---------------|---------------|-----------|
| start | `start` | `start` |
| fixed | `fixed` | `fixed` |
| min | `min` | `min` |
| max | `max` | `max` |
| nominal | `nominal` | `nominal` |
| unit | `unit` | `unit` |
| stateSelect | `state_select` | `state_select` |

### SPEC_0022 §3.17: Variability Classification → rumoca-ir-typed

| MLS Variability | rumoca Type |
|-----------------|-------------|
| constant | `ComputedVariability::Constant` |
| parameter | `ComputedVariability::Parameter` |
| discrete | `ComputedVariability::Discrete` |
| continuous | `ComputedVariability::Continuous` |

### SPEC_0022 §3.18: Specialized Class Types → rumoca-ir-ast

| MLS Kind | rumoca Type |
|----------|-------------|
| class | `ClassKind::Class` |
| model | `ClassKind::Model` |
| block | `ClassKind::Block` |
| connector | `ClassKind::Connector` |
| record | `ClassKind::Record` |
| type | `ClassKind::Type` |
| function | `ClassKind::Function` |
| package | `ClassKind::Package` |
| operator | `ClassKind::Operator` |
| operator record | `ClassKind::OperatorRecord` |

### SPEC_0022 §3.19-3.20: Prefixes → rumoca-ir-ast

| MLS Prefix | rumoca Field |
|------------|--------------|
| flow | `ComponentDecl.flow` |
| stream | `ComponentDecl.stream` |
| input | `Causality::Input` |
| output | `Causality::Output` |
| parameter | `Variability::Parameter` |
| constant | `Variability::Constant` |
| discrete | `Variability::Discrete` |
| inner | `ComponentDecl.inner` |
| outer | `ComponentDecl.outer` |
| partial | `ClassDef.partial` |
| encapsulated | `ClassDef.encapsulated` |
| final | `Modification.final` |
| each | `Modification.each` |
| replaceable | `ComponentDecl.replaceable` |

## Crate Dependencies

```
rumoca-core (spans, errors, config)
    |
    v
rumoca-ir-ast (syntax tree, class definitions)
    |
    v
rumoca-ir-resolved (DefIds, scope tree)
    |
    v
rumoca-ir-typed (type information, variability)
    |
    v
rumoca-ir-inst (instance tree, merged modifications)
    |
    v
rumoca-ir-flat (flat equations, qualified names)
    |
    v
rumoca-dae (DAE representation, stable API)
```

## Compile DAG Protection

To preserve strict phase boundaries and prevent accidental architectural drift:

1. Dependency direction MUST remain acyclic and phase-ordered (AST -> resolved -> typed -> inst -> flat -> dae -> simulator/codegen).
2. Phase/runtime crates MUST depend on the IR crate that defines their contract, not earlier IR crates bypassing that contract.
3. Wholesale cross-layer crate re-exports are forbidden.
: Do not add patterns like `pub use some_lower_layer_crate as alias;` to tunnel around dependency boundaries.
4. Wildcard cross-layer API forwarding is forbidden.
: Do not expose `pub use some_lower_layer_crate::*;` from a higher-level IR crate as a replacement for explicit interfaces.
5. Shared symbols crossing a layer boundary MUST be exposed explicitly from the owning IR crate (named types, aliases, or wrapper structs), with intentional review.

Rationale:
- Keeps compile-time dependency graph meaningful and reviewable.
- Prevents “shortcut imports” that hide layer violations.
- Reduces AI/new-contributor drift into convenience-based architecture erosion.

## Runtime And Simulation Layering

The compiler pipeline ends at the DAE contract. Runtime and simulation consumers must remain
layered below that contract rather than being folded back into `rumoca-session`.

Target direction:

```
rumoca-session / compiler entry points
    -> runtime contracts
    -> concrete solver backend
    -> stepper APIs
    -> report/payload contracts
    -> visualization/assets
```

Recommended crate mapping:

| Responsibility | Preferred Crate Role |
|----------------|----------------------|
| compile/session orchestration | `rumoca-session` |
| backend-neutral runtime contracts | `rumoca-sim` |
| concrete diffsol backend | `rumoca-sim-diffsol` |
| simple pure-Rust RK backend | `rumoca-sim-rk45` |
| shared stepping/runtime loop surface | `rumoca-sim` |
| report/payload shaping | `rumoca-sim-report` |
| web/HTML/assets | `rumoca-viz-web` |

Migration rule:

- Until the split is complete, do not add new APIs that further couple these responsibilities.
- New CI checks should remove legacy assumptions first, then ratchet toward the target layering as
  each split lands.

## Compliance Notes

### Array Preservation (SPEC_0019)
- `Variable.dims: Vec<i64>` - preserved through flatten
- `Var.dims: Vec<i64>` - preserved in DAE
- Scalarization deferred to codegen

### Model/Function Algorithm Handling (SPEC_0020)
- `Algorithm` - structured statements preserved at flat IR
- ToDae lowers supported model algorithms into DAE equations
- Unsupported model algorithm forms fail ToDae with `ED013` (no runtime compatibility storage in solver-facing DAE)
- `Function.body` is preserved in `Dae.functions` for codegen readability

### Solver-Facing DAE Boundary (SPEC_0007 + SPEC_0020)
- `rumoca-ir-dae::Dae` is the only solver-facing hybrid IR contract.
- Solver-facing DAE must carry MLS Appendix B canonical equation buckets (`f_x`, `f_z`, `f_m`, `f_c`, `relation`) plus runtime metadata (`synthetic_root_conditions`, `scheduled_time_events`, `clock_constructor_exprs`, `clock_schedules`).
- Solver-facing DAE must not include model-level `when_clauses`, `algorithms`, or `initial_algorithms`.
- Solver-facing equation partitions consumed by continuous solvers (`f_x`, `initial_equations`) must not contain unlowered synchronous operators (`sample/hold/Clock/subSample/superSample/shiftSample/backSample/noClock/firstTick/previous`); ToDae rejects these with `ED014`.
- Backend adapters (e.g. `rumoca-sim`) must consume this pre-lowered DAE contract and must not perform fallback semantic lowering of model algorithms/when clauses.

### Balance Checking (MLS §4.8)
- `Dae.effective_equation_count()` - counts array elements
- `Dae.effective_unknown_count()` - counts array elements
- Algorithm equations counted via `Algorithm.effective_equation_count()`

## Missing/Future Work

| MLS Feature | Status | Notes |
|-------------|--------|-------|
| Stream connectors (§15) | Partial | Basic support in flatten |
| Clocks (§16) | Not implemented | Requires clock partitioning |
| State machines (§17) | Not implemented | Requires state tracking |
| Overconstrained connectors (§9.4) | Not implemented | Requires spanning tree |
| External functions (§12.9) | Partial | Basic C interface |

## References

### Internal Specifications
- [SPEC_0022](SPEC_0022_MLS_COMPILER_COMPLIANCE.md): MLS Compiler Compliance
- [SPEC_0004](SPEC_0004_INSTANTIATE_FLATTEN.md): Instantiate/Flatten Separation
- [SPEC_0019](SPEC_0019_ARRAY_PRESERVATION.md): Array Preservation
- [SPEC_0020](SPEC_0020_ALGORITHM_PRESERVATION.md): Algorithm Preservation

### External References
- [MLS §5.6](https://specification.modelica.org/maint/3.6/scoping-name-lookup-and-flattening.html): Scoping, Name Lookup, and Flattening
- [JModelica.org User Guide](https://jmodelica.org/downloads/UsersGuide.pdf): JastAdd-based architecture
- Cooper & Torczon, *Engineering a Compiler*: Multi-pass IR design
