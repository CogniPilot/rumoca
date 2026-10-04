# SPEC_0022: MLS Compiler Compliance

**Status:** REFERENCE
**Created:** 2026-01-23
**Source:** [Modelica Language Specification 3.7](https://specification.modelica.org/maint/3.7/MLS.html)

## Abstract

This document catalogs the implicit and explicit contracts from the Modelica Language Specification (MLS) for use in contract-based compiler development. Each contract is assigned a unique ID for traceability, enabling systematic testing and assertion-based verification.

**Purpose:** Pre-extracted contracts to guide implementation of a spec-compliant Modelica compiler.

**Note:** This is a 1010-line reference document. Do not load it in full as AI context. Use the section index below to find the relevant section.

### Section Index (for selective loading)

| Section | Lines | Content |
|---------|-------|---------|
| §1. Key Definitions | 46–57 | MLS terminology (component, element, flattening, etc.) |
| §2. Compilation Pipeline | 58–73 | Source → Class Tree → Instance Tree → Flat → DAE → Simulation |
| §3. Data Structures | 74–331 | Class tree, instance tree, modification env, connection set, DAE, type attributes, variability, class types, prefixes, arrays, state machines |
| §4.1 LEX contracts | 332–351 | Contract catalog heading + lexical rules (13 contracts) |
| §4.2 DECL contracts | 352–393 | Declaration rules (37 contracts) |
| §4.3 INST contracts | 394–457 | Instantiation rules (59 contracts) |
| §4.4 EXPR contracts | 458–503 | Expression/operator rules (41 contracts) |
| §4.5 EQN contracts | 504–549 | Equation rules (41 contracts) |
| §4.6 ALG contracts | 550–571 | Algorithm rules (18 contracts) |
| §4.7 CONN contracts | 572–606 | Connection rules (30 contracts) |
| §4.8 FUNC contracts | 607–654 | Function rules (43 contracts) |
| §4.9 TYPE contracts | 655–695 | Type/interface rules (36 contracts) |
| §4.10 ARR contracts | 696–746 | Array rules (46 contracts) |
| §4.11 PKG contracts | 747–764 | Package/import rules (13 contracts) |
| §4.12 OPREC contracts | 765–781 | Operator record rules (12 contracts) |
| §4.13 SIM contracts | 782–796 | Simulation rules (10 contracts) |
| §4.14 CLK contracts | 797–821 | Clock/synchronous rules (20 contracts) |
| §4.15 STRM contracts | 822–839 | Stream connector rules (13 contracts) |
| §4.16 SM contracts | 840–938 | State machine rules (8 contracts) + §4.16.1 Rumoca Phase 5 scope note |
| §4.17 ANN contracts | 939–960 | Annotation rules (17 contracts) |
| §4.18 UNIT contracts | 961–976 | Unit expression rules (9 contracts) |
| §5. Contract Summary | 977–1002 | Category counts and totals |
| §6. Compiler Phases | 1003–1052 | Phase input/output mapping |
| §7. MLS Chapter Index | 1053–1080 | MLS chapter → contract category mapping |

---

## 1. Key Definitions ([MLS §1.3](https://specification.modelica.org/maint/3.7/introduction1.html))

| Term | Definition (MLS) |
|------|------------------|
| **Component** | "An element defined by the production component-clause in the Modelica grammar (basically a variable or an instance of a class)" |
| **Element** | "Class definition, extends-clause, or component-clause declared in a class (basically a class reference or a component in a declaration)" |
| **Flattening** | "The translation process mapping the hierarchical structure of a model into a set of differential, algebraic and discrete equations together with the corresponding variable declarations and function definitions" |
| **Translation** | "The process of preparing a Modelica simulation model for simulation, starting with flattening but not including the simulation itself" |
| **Simulation** | "The combination of initialization followed by transient analysis" |

---

## 2. Compilation Pipeline ([MLS §5.6](https://specification.modelica.org/maint/3.7/scoping-name-lookup-and-flattening.html))

The MLS defines flattening as two major steps:

```
Source → Class Tree → Instance Tree → Flat Equation System → DAE System → Simulation
         (Parsing)    (Instantiation)  (Flattening)          (Appendix B)  (§8.6)
```

1. **Instantiation**: Build class tree (syntactic) and instance tree (with modifications applied)
2. **Flattening**: Generate flat equation system with globally unique variable names

> "An implementation may delay and/or omit building parts of these trees, which means that the different steps can be interleaved." — MLS §5.6

---

## 3. Data Structures

### 3.1 Class Tree ([MLS §5.6](https://specification.modelica.org/maint/3.7/scoping-name-lookup-and-flattening.html))

The class tree "represents the syntactic information from the class definitions" with "all modifications at their original locations in syntactic form."

### 3.2 Instance Tree ([MLS §5.6](https://specification.modelica.org/maint/3.7/scoping-name-lookup-and-flattening.html))

"It contains the instantiated elements of the class definitions, with redeclarations taken into account and merged modifications applied."

### 3.3 Modification Environment ([MLS §7.2](https://specification.modelica.org/maint/3.7/inheritance-modification-and-redeclaration.html))

"The modification environment is built by merging class modifications, where outer modifications override inner modifications."

### 3.4 Scope Chain ([MLS §5.3](https://specification.modelica.org/maint/3.7/scoping-name-lookup-and-flattening.html))

Ordered set of instance scopes for sequential name lookup until match or encapsulation boundary.

### 3.5 Connection Set ([MLS §9.2](https://specification.modelica.org/maint/3.7/connectors-and-connections.html))

Variables connected by connect-equations; generates equality or sum-to-zero equations.

### 3.6 Connection Graph ([MLS §9.4](https://specification.modelica.org/maint/3.7/connectors-and-connections.html))

Virtual graph for overconstrained connectors using spanning trees.

### 3.7 DAE System ([MLS Appendix B](https://specification.modelica.org/maint/3.7/modelica-dae-representation.html))

Hybrid DAE with the following variable classifications:
- **p**: Parameters (constant/parameter, no time dependency)
- **x(t)**: Differential states (Real variables appearing differentiated)
- **y(t)**: Algebraic variables (continuous Real, not differentiated)
- **z(tₑ)**: Discrete Real variables (change only at events)
- **m(tₑ)**: Discrete-valued variables (Boolean, Integer, enum)
- **c(tₑ)**: Condition variables (if-expression/when-clause conditions)

Equation forms:
- **B.1a**: Continuous equations: 0 = f(v, c)
- **B.1b**: Discrete Real: z = {f(v,c) at events; pre(z) otherwise}
- **B.1c**: Discrete-valued assignments: m := f(v, c)
- **B.1d**: Condition evaluation: c := f(relation(v))

### 3.8 Flattened Equation System ([MLS §5.6](https://specification.modelica.org/maint/3.7/scoping-name-lookup-and-flattening.html))

"The flat equation system consists of a list of variables with dimensions, flattened equations and algorithms, and a list of called functions." All name references replaced with globally unique identifiers.

### 3.9 Partially Instantiated Element ([MLS §5.6](https://specification.modelica.org/maint/3.7/scoping-name-lookup-and-flattening.html))

"A partially instantiated class or component is an element that is ready to be instantiated; comprised of a reference to the original element (from the class tree) and the modifiers for that element."

### 3.10 Instance Scope ([MLS §5.6](https://specification.modelica.org/maint/3.7/scoping-name-lookup-and-flattening.html))

"A node in the instance tree is the instance scope for the modifiers and elements syntactically defined in the class it is instantiated from." Starting point for name lookup during flattening.

### 3.11 Ordered Set of Enclosing Classes ([MLS §5.3](https://specification.modelica.org/maint/3.7/scoping-name-lookup-and-flattening.html))

"The classes lexically enclosing an element form an ordered set of enclosing classes." Nested classes precede their enclosing class; unnamed root contains top-level definitions.

### 3.12 Spanning Tree ([MLS §9.4](https://specification.modelica.org/maint/3.7/connectors-and-connections.html))

Constructed from the virtual connection graph by "removing optional spanning tree edges." Contains all nodes with selected root nodes and required spanning-tree edges.

MLS §9.4 lets the tool choose among the valid spanning trees. Rumoca's choice:

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Select one spanning forest per graph. Required (`Connections.branch`) edges are admitted first; optional edges are then admitted in ascending order of (smaller, larger) endpoint distance from the nearest root of their connected component, ties broken by the lexicographically ordered endpoint names; an edge is broken when it would close a cycle or join two definite-root trees. Distances are graph distances over all required and optional edges from the component's definite roots, or from the root that `isRoot` selects when the component has none. `Connections.rooted`/`isRoot` evaluation and connection-equation emission consume this one forest: every selected optional edge keeps its ordinary equalities, every broken one is replaced by `equalityConstraint` (omitted when zero-width). | `rumoca-phase-flatten` `vcg::build_vcg`, `vcg::spanning_forest` | The tree depends only on the graph and node names, never on `connect` statement order; a graph without overconstrained loops breaks no edge and emits the same equations as before, and the branch orientation `rooted` reports is exactly the tree whose loops the emitted equations cut; two independently chosen forests leave `if Connections.rooted(...)` branches inconsistent with the cut |

### 3.13 Root Nodes ([MLS §9.4](https://specification.modelica.org/maint/3.7/connectors-and-connections.html))

**Definite Root**: "The overdetermined type or record instance R in connector instance a is a (definite) root node." Represents consistently initialized overdetermined records.

**Potential Root**: Has priority value p≥0. "When subgraphs lack definite roots, one potential root node with the lowest priority number is selected."

### 3.14 Clock Partitions ([MLS §16.7](https://specification.modelica.org/maint/3.7/synchronous-language-elements.html))

**Base-Clock Partition**: "A set of equations and variables which must be executed together in one task." Different base-partitions can execute asynchronously.

**Sub-Clock Partition**: "A subset of equations and variables of a base-partition which are partially synchronized with other sub-partitions of the same base-partition."

**Discrete-Time vs Discretized**: Discrete-time partitions contain difference equations only; discretized partitions contain der(), delay(), or Boolean when-clauses requiring integration methods.

---

## 3.15 Key Algorithmic Processes

### Event Iteration ([MLS Appendix B](https://specification.modelica.org/maint/3.7/modelica-dae-representation.html))

```
loop
  solve equations for unknowns, with pre(z) and pre(m) fixed
  if z == pre(z) and m == pre(m) then break
  pre(z) := z
  pre(m) := m
end loop
```

### Clock Partitioning ([MLS §16.7](https://specification.modelica.org/maint/3.7/synchronous-language-elements.html))

Three-phase process after flattening:
1. **Base-Partitioning**: Group by base-clock inference, excluding sample/hold/Clock first arguments from incidence
2. **Sub-Partitioning**: Group within base-partitions, excluding subSample/superSample/shiftSample/backSample/noClock first arguments
3. **Sub-Clock Inferencing**: Solve constraint sets for base intervals and sub-sampling factors

### Perfect Matching ([MLS §8.4](https://specification.modelica.org/maint/3.7/equations.html))

"There must exist a perfect matching of variables to equations after flattening, where a variable can only be matched to equations that can contribute to solving for the variable."

### Initialization ([MLS §8.6](https://specification.modelica.org/maint/3.7/equations.html))

Derivatives (der) and pre-variables (pre) treated as algebraic unknowns during initialization, requiring 2n+m equations for n state variables and m output variables. Start values follow priority hierarchy based on model component hierarchy level.

---

### 3.16 Type Attributes ([MLS §4.4](https://specification.modelica.org/maint/3.7/class-predefined-types-and-declarations.html))

Predefined type attributes controlling simulation behavior:

| Attribute | Type | Description |
|-----------|------|-------------|
| `start` | Same as base | Initial value for simulation/initialization problem |
| `fixed` | Boolean | Whether initial value is constrained (true) or free (false) |
| `min` | Same as base | Lower bound constraint |
| `max` | Same as base | Upper bound constraint |
| `nominal` | Real | Scaling factor for numerical solvers |
| `unit` | String | Physical unit (e.g., "V", "N.m") |
| `quantity` | String | Physical meaning beyond units |
| `stateSelect` | StateSelect | Priority hint for state variable selection |
| `unbounded` | Boolean | No upper/lower bounds (Real only) |
| `displayUnit` | String | Preferred display unit |

### 3.17 Variability Classification ([MLS §4.5](https://specification.modelica.org/maint/3.7/class-predefined-types-and-declarations.html))

Ordered variability hierarchy (lower ≤ higher):

```
constant < evaluable parameter < non-evaluable parameter < discrete-time < continuous-time
```

| Level | Description |
|-------|-------------|
| **constant** | Value fixed during translation, unaffected by initialization |
| **evaluable parameter** | fixed=true, no Evaluate=false, evaluable declaration equation |
| **non-evaluable parameter** | Determined by initialization problem only |
| **discrete-time** | Changes only at event instants (when-clause or non-Real type) |
| **continuous-time** | May change continuously during simulation |

### 3.18 Specialized Class Types ([MLS §4.7](https://specification.modelica.org/maint/3.7/class-predefined-types-and-declarations.html))

| Kind | Restrictions | Purpose |
|------|--------------|---------|
| `record` | Public only, no equations/algorithms, implicit constructor | Data structures |
| `type` | Predefined types, enums, arrays, or type extensions only | Type aliases |
| `model` | None beyond general rules | Physical modeling |
| `block` | Public connectors must have input/output prefixes | Block diagrams |
| `connector` | Public only, may contain connector/record/type | Interface points |
| `function` | Algorithm/external only, input/output causality | Computation |
| `package` | Classes and constants only | Namespace organization |
| `operator record` | Operator overloading enabled | Custom arithmetic |
| `operator` | Functions only, inside operator record | Overload definitions |

### 3.19 Component Prefixes ([MLS §4.4.2](https://specification.modelica.org/maint/3.7/class-predefined-types-and-declarations.html))

| Prefix | Context | Effect |
|--------|---------|--------|
| `flow` | Connector primitives | Generates sum-to-zero equations |
| `stream` | Connector Real variables | Stream semantics with inStream/actualStream |
| `discrete` | Real variables | Clarifies discrete-time variability |
| `parameter` | Variables | Constant during transient, set by initialization |
| `constant` | Variables | Fixed during translation |
| `input` | Functions/models/connectors | Causality or binding requirement |
| `output` | Functions/models/connectors | Causality or environmental access |
| `inner` | Components | Target for outer references |
| `outer` | Components | References inner declaration in enclosing scope |

### 3.20 Class Prefixes ([MLS §4.6](https://specification.modelica.org/maint/3.7/class-predefined-types-and-declarations.html))

| Prefix | Effect |
|--------|--------|
| `partial` | Cannot instantiate in simulation models |
| `encapsulated` | Restricts lookup to explicit imports |
| `expandable` | Connector accepts undeclared variables |
| `pure` | Function has no side effects |
| `impure` | Function may have side effects |
| `operator` | Operator overloading context |

### 3.21 Interface Structure ([MLS §6.4](https://specification.modelica.org/maint/3.7/interface-or-type-relationships.html))

The interface (type) of a class comprises:
- **Replaceability**: Whether transitively non-replaceable
- **Element classification**: Component vs. class
- **Prefixes**: flow/stream, variability, input/output, inner/outer, final
- **Structural info**: Array dimensions, conditional status, specialized class kind
- **Named elements**: Public elements with their interfaces (recursive)
- **Operator record base**: If applicable, the base class identity

### 3.22 Virtual Connection Graph ([MLS §9.4](https://specification.modelica.org/maint/3.7/connectors-and-connections.html))

For overconstrained connectors:

| Element | Description |
|---------|-------------|
| **Nodes** | Overdetermined type/record instances in connectors |
| **Optional edges** | From connect(), can be removed to break cycles |
| **Required edges** | From Connections.branch(), must remain |
| **Definite root** | Connections.root(), always selected as root |
| **Potential root** | Connections.potentialRoot(), selected by priority if needed |

### 3.23 Array Type Structure ([MLS §10](https://specification.modelica.org/maint/3.7/arrays.html))

Arrays have fixed dimensionality but runtime-determinable sizes:

| Component | Description |
|-----------|-------------|
| **Base type** | Underlying scalar type (Real, Integer, Boolean, enum, record) |
| **Dimensions** | Fixed count, each with size (literal, parameter, or `:` for unknown) |
| **Index type** | Integer (1-based), Boolean (false/true), or enumeration |

### 3.24 State Machine State ([MLS §17.2](https://specification.modelica.org/maint/3.7/state-machines.html))

Discrete-time state tracking variables:

| Variable | Description |
|----------|-------------|
| `activeState` | Currently active state index |
| `nextState` | Determined next state |
| `activeReset` | Active reset status |
| `activeResetStates[:]` | Per-state reset flags |
| `stateMachineInFinalState` | No outgoing transitions can fire |

### 3.25 Transition Record ([MLS §17.1](https://specification.modelica.org/maint/3.7/state-machines.html))

Defines state-to-state transitions with priority and timing control.

### 3.26 Equation Classification ([MLS §8](https://specification.modelica.org/maint/3.7/equations.html))

| Category | Context | Purpose |
|----------|---------|---------|
| **Normal equality** | Equation section | Constraint relationship |
| **Declaration equation** | Variable declaration | Initial/binding value |
| **Modification equation** | Class modification | Override attribute |
| **Binding equation** | Component binding | Constraint value (non-function) |
| **Initial equation** | Initial equation section | Initialization problem |

### 3.27 Annotation Categories ([MLS §18](https://specification.modelica.org/maint/3.7/annotations.html))

| Category | Content |
|----------|---------|
| **Documentation** | info, revisions, styleSheets, figures |
| **Experiment** | StartTime, StopTime, Interval, Tolerance |
| **Graphical** | Icon, Diagram layers with primitives |
| **Placement** | Component positioning in diagrams |
| **Usage** | singleInstance, mustBeConnected, mayOnlyConnectOnce |
| **Evaluation** | Evaluate, HideResult |
| **External** | Library, Include, IncludeDirectory |

---

## 4. Contract Catalog

### 4.1 Lexical Contracts (LEX)

| ID | Contract | MLS | Requirement |
|----|----------|-----|-------------|
| LEX-001 | ASCII identifiers | §2.3 | "Restricted to Unicode characters corresponding to 7-bit ASCII for identifiers" |
| LEX-002 | No token whitespace | §2.2 | "Whitespace cannot occur inside tokens" |
| LEX-003 | No nested comments | §2.2 | "Delimited Modelica comments do not nest" |
| LEX-004 | Case sensitivity | §2.3 | "Case is significant, i.e., Inductor and inductor are different" |
| LEX-005 | Reserved keywords | §2.3.3 | "Keywords are reserved words that cannot be used where IDENT is expected" |
| LEX-006 | Reserved type names | §2.3.3 | "Not allowed to declare element or enumeration literal with reserved names" |
| LEX-007 | Quoted distinct | §2.3.1 | "Single quotes are part of identifier: 'x' and x are distinct identifiers" |
| LEX-008 | String concat explicit | App A | "Concatenation of string literals requires binary expression (+), no C-style adjacent literal concat" |
| LEX-009 | Semantic parser check | App A | "Parsers must implement semantic validation for equation-or-procedure beyond syntax" |
| LEX-010 | Float range minimum | §2.4.1 | "At least IEEE double precision range (1.797×10³⁰⁸ to 2.225×10⁻³⁰⁸)" |
| LEX-011 | Integer range minimum | §2.4.2 | "Range at least  enough to represent largest positive IntegerType value" |
| LEX-012 | Boolean literals only | §2.4.3 | "Only true and false permitted as Boolean literals" |
| LEX-013 | String escapes required | §2.4.4 | "Backslash escape sequences required for quote, backslash, newline, tab, etc."

### 4.2 Declaration Contracts (DECL)

| ID | Contract | MLS | Requirement |
|----|----------|-----|-------------|
| DECL-001 | Name uniqueness | §4.4 | "Name shall not have the same name as any other element in its partially flattened enclosing class" |
| DECL-002 | Block connector prefixes | §4.6 | "Each public connector component of a block must have prefixes input and/or output" |
| DECL-003 | Record public only | §4.6 | "Only public sections are allowed in record definition" |
| DECL-004 | Record no prefixes | §4.6 | "Elements of a record shall not have prefixes input, output, inner, outer, stream, or flow" |
| DECL-005 | Record component types | §4.6 | "Components in a record may only be of specialized class record or type" |
| DECL-006 | Connector public only | §4.6 | "Only public sections are allowed in connector definition" |
| DECL-007 | Connector no inner/outer | §4.6 | "Elements of a connector shall not have prefixes inner or outer" |
| DECL-008 | Operator record extends | §4.6 | "Not legal to extend from an operator record except as short class definition" |
| DECL-009 | Protected access | §4.4 | "A protected element shall not be accessed via dot notation" |
| DECL-010 | Type prefix scope | §4.4 | "Type prefixes (flow, stream, etc.) only for type, record, operator record, connector" |
| DECL-011 | Stream subtype | §4.4 | "Variables with stream prefix shall be a subtype of Real" |
| DECL-012 | Input not parameter | §4.4 | "Variables with input prefix must not also have prefix parameter or constant" |
| DECL-013 | Declaration equation scope | §4.4 | "Only type, record, operator record, connector, ExternalObject may have declaration equations" |
| DECL-014 | Partial class error | §4.6 | "Error if the type is partial in a simulation model" |
| DECL-015 | Component/class namespace | §5.3 | "Component cannot have the same name as its class" |
| DECL-016 | Flow primitive types | §4.4.2.2 | "Primitive elements with flow prefix shall be subtype of Real, Integer, or operator record defining additive group" |
| DECL-017 | Nested prefix restriction | §4.4.2.2 | "When flow/input/output applied to structured component, no element may have these or stream prefix" |
| DECL-018 | Array dim evaluable | §4.4.2 | "Array dimensions shall be scalar non-negative evaluable expressions of type Integer or enumeration/Boolean" |
| DECL-019 | Constant declaration eq | §4.5.1 | "Constant variables shall have declaration equation with constant expression if used in simulation" |
| DECL-020 | Discrete der forbidden | §4.5.3 | "It is not allowed to apply der to discrete-time variables" |
| DECL-021 | Discrete Real when-assign | §4.5.3 | "Discrete Real variable must be assigned in when-clause, by assignment or equation" |
| DECL-022 | Non-Real always discrete | §4.5.3 | "Default variability for Integer/String/Boolean/enum is discrete-time; continuous prohibited" |
| DECL-023 | Short partial inherit | §4.6.1 | "Short class definition inheriting from partial class will be partial regardless of prefix" |
| DECL-024 | Package contents | §4.7 | "Package may only contain declarations of classes and constants" |
| DECL-025 | Operator contents | §4.7 | "Operator class may only contain declarations of functions" |
| DECL-026 | Operator placement | §4.7 | "Operator may only be placed directly in an operator record" |
| DECL-027 | Connector component types | §4.7 | "Connector may only contain components of specialized class connector, record and type" |
| DECL-028 | Operator record base | §4.7 | "An operator record can only extend from an operator record" |
| DECL-029 | No enclosing scope extends | §4.7 | "Operator record cannot extend from any of its enclosing scopes" |
| DECL-030 | Stream record members | §4.4.2.2 | "Members of record with stream prefix may not have stream type prefix" |
| DECL-031 | ExternalObject prefixes | §4.4.2.2 | "ExternalObject instances may have type prefixes parameter and constant, in functions also input and output" |
| DECL-032 | Globally balanced | §4.8 | "Simulation models must be globally balanced, matching total unknowns to total equations across all components" |
| DECL-033 | When-clause subcomponent | §4.5.3 | "Variable assigned in when-clause shall not be defined in sub-component of model or block" |
| DECL-034 | Array class extends | §4.6.2 | "Not legal to combine equations/algorithms/components with extends from array class or simple type" |
| DECL-035 | Local class flattenable | §4.6.3 | "Local class should be statically flattenable with partially flattened enclosing class" |
| DECL-036 | Type class contents | §4.7 | "type – May only be predefined types, enumerations, array of type, or classes extending from type"
| DECL-037 | Structural parameters | §10.1, §8.3.3 | "The number of dimensions and the dimension sizes are fixed at translation"; a for-equation range is evaluated at translation. Rumoca: an ordinary parameter read by a declared array dimension or a for-equation range, directly or through a `final` or `Evaluate = true` parameter whose binding reads it (a `size`/`ndims` operand contributes only its fixed shape, so `size` of an input array is constant), is structural: flatten records the use (`flat::ParameterBranchSelection` with `ArrayDimension`/`ForRange`), DAE construction marks it evaluable with every parameter its binding reads (SPEC_0040 DAE-C22), WD001 names it at the use, and it exports as `calculatedParameter`; an ordinary parameter no structure reads stays settable. A dimension may read an element of an Integer array parameter or a String parameter, and a colon dimension takes the shape of its binding, whose function result shape reads the call's String and array arguments: flatten evaluates String and array parameter bindings into its structural inventory, a String parameter being its declared value (SPEC_0040 FLAT-C01), and a cataloged foreign file reader in such a binding executes at translation (FLAT-C06, `Modelica.Utilities.Examples.ReadRealMatrixFromFile`). Tested in `suite_core/structural_parameters.rs`, `suite_core/structural_string_and_array_values.rs`, and `suite_core/translation_file_reads.rs` |

### 4.3 Instantiation Contracts (INST)

| ID | Contract | MLS | Requirement |
|----|----------|-----|-------------|
| INST-001 | Modification context | §7.2 | "Modifier value found in the context in which the modifier occurs". Rumoca: flatten qualifies a modification binding from the modifier's scope, and translation-time evaluation (branch selection, ranges, sizes) reads those names from the root before the modified component's scope, so `c(p = p)` reads the enclosing `p`, never `c.p` or its declaration default. Tested in `suite_core/modifier_value_context.rs` |
| INST-002 | Modification merging | §7.2.3 | "Outer modifiers override inner modifiers" |
| INST-003 | Single modification | §7.2.4 | "Two arguments of a modification shall not modify the same element, attribute, or description-string" |
| INST-004 | Unnamed extends nodes | §5.6 | Preserve declaration order in inheritance |
| INST-005 | Extends lookup stability | §5.6 | "Extends classes looked up before and after handling; error if different" |
| INST-006 | Name collision | §7.1 | "Declaration elements of flattened base class shall either not exist or match exactly" |
| INST-007 | Evaluable expressions | §4.5 | Structural parameters must be compile-time evaluable |
| INST-008 | Acyclic binding | §4.4.4 | "Expression must not depend on the variable itself, directly or indirectly" |
| INST-009 | Final inheritance | §7.2.6 | "All elements of a final element are also final" |
| INST-010 | Final immutability | §7.2.6 | "Element defined as final cannot be modified by modification or redeclaration" |
| INST-011 | Inner/outer subtype | §5.4 | "Inner component must be subtype of corresponding outer" |
| INST-012 | Outer no modifications | §5.4 | "Outer component declarations shall not have modifications" |
| INST-013 | Constant-only references | §5.3 | "Enclosing class variables accessible only if declared constant" |
| INST-014 | Redeclaration constraint | §7.3.3 | "Only classes and components declared as replaceable can be redeclared with a new type" |
| INST-015 | Transitively non-replaceable | §7.1.4 | "Class name after extends for base classes must use a class reference considered transitively non-replaceable" |
| INST-016 | Conditional evaluable | §4.5 | "Condition expression must be evaluable Boolean scalar" |
| INST-017 | Conditional no redeclare | §4.5 | "Redeclaration shall not include a condition attribute" |
| INST-018 | Outer same class | §5.4 | "Two outer declarations with same name but different classes is error" |
| INST-019 | Outer partial error | §5.4 | "Outer of partial class is error" |
| INST-020 | Inner/outer connector | §4.8 | "Inner/outer component shall not have top-level public connectors containing inputs" |
| INST-021 | Lookup non-partial | §5.3 | "Class looked inside shall not be partial in simulation model" |
| INST-022 | Constant not redeclared | §7.3.3 | "An element declared as constant cannot be redeclared" |
| INST-023 | Protection not changed | §7.3.3 | "Protected element cannot be redeclared as public, or public as protected" |
| INST-024 | Extends kind compatibility | §7.1.3 | "Only specialized classes in some sense compatible can inherit from each other" |
| INST-025 | Equation syntactic equiv | §7.1 | "Equations syntactically equivalent to equations in enclosing class are discarded" |
| INST-026 | Class extends replaceable | §7.3.1 | "Original class B should be replaceable in class extends" |
| INST-027 | Constraining type auto-apply | §7.3.2 | "Modifications following constraining type applied both for constraint and declaration" |
| INST-028 | Break deselection order | §7.4 | "Deselection break D is applied before any other non-selective modifications" |
| INST-029 | Break must match | §7.4 | "Deselection break D must match at least one element of B" |
| INST-030 | Override final error | §7.2.3 | "Error if value for entire non-simple component overrides a final prefix" |
| INST-031 | Outer class short | §5.4 | "Outer class declarations should be defined using short-class definitions without modifications" |
| INST-032 | Outer disabled conditional | §5.4 | "If outer component declaration is disabled conditional, also ignored for automatic inner creation" |
| INST-033 | Global scope error | §5.3.3 | "If there does not exist a class A in global scope this is an error" |
| INST-034 | Encapsulated lookup stop | §5.3.1 | "Lookup stops if enclosing class is encapsulated (except predefined types/operators)" |
| INST-035 | Record/enum lookup ignore | §5.3 | "Names of record classes and enumeration types are ignored during function name lookup" |
| INST-036 | Constructor lookup ignore | §5.3 | "Implicitly defined names of record constructors and enum conversions ignored during type lookup" |
| INST-037 | Identical children first kept | §5.6.1.4 | "Children with same name must be identical; only first one kept, error if not identical" |
| INST-038 | Inner/outer both modifies | §5.5 | "Modifications of elements declared with both inner and outer only applied to inner declaration" |
| INST-039 | Protected extends | §7.1.2 | "If extends under protected heading, all elements of base class become protected" |
| INST-040 | Each array required | §7.2.5 | "Each keyword on modifier requires it is applied in array declaration/modification" |
| INST-041 | Each size match | §7.2.5 | "Sizes must match without each prefix or it is an error" |
| INST-042 | Break removes expression | §7.2.7 | "Remaining break modifications treated as if expression was missing" |
| INST-043 | Implicit constraining type | §7.3.2 | "If constraining-clause not present, type of declaration used as constraining type" |
| INST-044 | Constraining subtype | §7.3.2 | "Constraining type of replaceable redeclaration must be subtype of original constraining type" |
| INST-045 | Dimension correspondence | §7.3.2 | "Number of dimensions in constraining type should correspond to type-part" |
| INST-046 | Array dim redeclaration | §7.3.3 | "Array dimensions may be redeclared if sub-typing rules are satisfied" |
| INST-047 | Component deselection types | §7.4 | "Matched components for deselection must be models, blocks, or connectors" |
| INST-048 | Conditional deselection | §7.4 | "Conditionally declared components of B assumed declared for matching purposes" |
| INST-049 | Partial deselection allowed | §7.4 | "Deselected component may be of partial class even in simulation model" |
| INST-050 | Unqualified import conflict | §5.3.1 | "Error if multiple unqualified imports match same name" |
| INST-051 | Constraining annotation conflict | §7.3.2.1 | "Error if annotations appear on both definition and constraining clause" |
| INST-052 | Redeclaration dimension match | §7.3.2 | "Redeclaration must have same number of dimensions as original element" |
| INST-053 | Conditional component removal | §5.6.2 | "Conditional components with false condition are removed and not part of simulation model"
| INST-054 | Automatic inner creation | §5.4 | "If a unique non-partial class is used for all outer declarations of the same name lacking a matching inner declaration, an inner declaration of that class is automatically added at the top of the model and a diagnostic is given" (rumoca: WI013; conflicting classes EI015, partial class EI012) |
| INST-055 | Extends redeclaration replaces element | §7.3 | "A redeclaration in the modification of an extends-clause replaces the inherited element; the derived class and its descendants see the replacing class under the element name" |
| INST-056 | Extends-modified package constant in sibling bindings | §7.2 | Modifications of an extends clause apply to the inherited elements, so a constant an extends modification binds (`extends TableBased(tableDensity = [...])`) has that value wherever the package is read, including inside the binding of a sibling constant (`poly_rho = fitting(tableDensity[:, 1], ...)`) and through an element selection. Rumoca: constant substitution reads a constant's binding in the package it is exposed through and resolves modified constants, whole or indexed, from that scope. Tested in `suite_core/fluid_function_data.rs` |
| INST-057 | Occurrence-selected member class | §7.3 | A redeclaration modifies one occurrence, so a component declared through a replaceable alias (`Medium.BaseProperties medium`) therefore has its class selected per occurrence, and a reference `tank.medium.T` written outside `tank` (in an equation or a modifier binding) names the member of the class that occurrence of `tank.medium` selected; occurrences of one declaration that select different classes have no single member tail and are rejected |
| INST-058 | Redeclare value through the enclosing occurrence | §7.3 | A redeclare value that names a replaceable element of the enclosing class (`Inner a(redeclare package Medium = MA)`) denotes the class that occurrence of the enclosing class selected for `MA`, which an outer redeclaration may have replaced, never the default of the lexical declaration |
| INST-059 | Predefined function names | §5.3.1, §5.3.3 | Lookup that reaches no enclosing declaration continues in the global scope, and the first identifier of a name starting with a dot (`.A`) is looked up in the global scope alone. The predefined functions (`exp`, `sqrt`, ...) are members of the global scope, so an import or an enclosing declaration of the same name (`import Modelica.ComplexMath.exp`, `Modelica.Math.exp` inside `Modelica.Math`) shadows one, and `.exp` reaches the predefined function past any enclosing declaration. Rumoca: Resolve starts the lookup of a leading-dot name at the global scope, and flatten lowers a call spelled as a predefined function to that intrinsic only when its resolved declaration is the predefined one; any other declaration is called as the function it is. Tested in `suite_core/predefined_function_lookup.rs` |

### 4.4 Expression/Operator Contracts (EXPR)

| ID | Contract | MLS | Requirement |
|----|----------|-----|-------------|
| EXPR-001 | Relational scalar only | §3.5 | "Relational operators only defined for scalar operands of simple types" |
| EXPR-002 | No Real equality | §3.5 | "v1 == v2 or v1 <> v2: v1 or v2 shall not be subtype of Real (except in functions)" |
| EXPR-003 | der continuity | §3.7 | "Expression must be continuous and semi-differentiable" |
| EXPR-004 | der not in functions | §3.7 | "der operator not allowed inside function classes" |
| EXPR-005 | Overflow undefined | §3.3 | "If a numeric operation overflows the result is undefined" |
| EXPR-006 | delay parameter | §3.7 | "delayMax shall be a parameter expression" |
| EXPR-007 | delay bounds | §3.7 | "0 < delayTime ≤ delayMax must hold" |
| EXPR-008 | delay not in functions | §3.7 | "delay operator not allowed inside function classes" |
| EXPR-009 | cardinality restrictions | §3.7 | "Shall not be applied to expandable connectors or arrays of connectors" |
| EXPR-010 | spatialDistribution range | §3.7 | "initialPoints array shall span entire range from 0 to 1" |
| EXPR-011 | spatialDistribution sorted | §3.7 | "initialPoints must be sorted in non-descending order" |
| EXPR-012 | Variability assignment | §3.8 | "Expression must not have higher variability than assigned component". Rumoca: a Boolean, Integer, String, or enumeration variable defined outside a when-clause by an expression that reads `time` or a continuous variable outside an event-generating relation (§3.8.5: relations and `ceil`/`floor`/`div`/`integer` outside `noEvent`/`smooth`; `mod` and `rem` generate events but are not discrete-time; `pre`, `edge`, `change`, `sample`, `initial`, `terminal`, and calls whose arguments are all discrete-time) is refused (ED023). Rumoca (documented deviation, accepted extension that MSL 4.1 relies on: `Media.Water` binds the Integer `ThermodynamicState.phase` field from `setState_phX(p, h)` of continuous port values, e.g. `flowModel.states` in `Fluid.Pipes.StaticPipe`): such a definition is accepted when nothing reads the variable, proven at DAE construction. A read is any occurrence of the variable, of a component that contains it, or of an element of it in an equation, binding, attribute, assertion, algorithm, when-clause, or `pre`, except a plain reference passed to a function input the callee never reads (MSL `density(state)` reads only `state.d`; an input of an external, impure, or derivative-annotated function is always read). The variable cannot influence the simulation, so it is an observation: the DAE marks its owner observed and the solver evaluates it at every output point instead of holding it between events, as OpenModelica reports it. A definition whose variable is read stays refused (ED023). Tested in `suite_core/discrete_time_definitions.rs` and `expr_contracts.rs` |
| EXPR-013 | end only in subscripts | §10.5 | "Expression 'end' may only appear inside array subscripts" |
| EXPR-014 | Non-associative chaining | §3.2 | "Non-associative operators (^, :, relational, ?:) cannot be chained: 1 < 2 < 3 is invalid" |
| EXPR-015 | Unary additive position | §3.2 | "Additive unary expressions only allowed in first term of sum: 2*-2 is illegal" |
| EXPR-016 | If-expr Boolean condition | §3.6.5 | "First expression of if-expression must be Boolean expression" |
| EXPR-017 | If-expr type compatible | §3.6.5 | "The two branch expressions must be type compatible expressions (§6.7)" |
| EXPR-018 | abs argument type | §3.7.1 | "Argument v of abs(v) needs to be Integer or Real expression" |
| EXPR-019 | sign argument type | §3.7.1 | "Argument v of sign(v) needs to be Integer or Real expression". MLS 3.7 also gives the result: `sign(v)` "expands into noEvent(if v > 0 then 1 else if v < 0 then -1 else 0)", an Integer for a Real operand too, so typecheck, the DAE, and typed Solve programs type it Integer and an Integer may receive it. Tested in `suite_core/sign_integer_result.rs` |
| EXPR-020 | nthRoot constraints | §3.7.1 | "v shall be Real expression, n>0 shall be Integer; if n even, v must be non-negative" |
| EXPR-021 | EnumTypeName error | §3.7.1 | "Error to attempt to convert values of i that do not correspond to enumeration values" |
| EXPR-022 | String no coercion | §3.7.1 | "Standard type coercion shall not be applied for first argument of String()" |
| EXPR-023 | String significantDigits | §3.7.1 | "Specifying significantDigits is error when first argument of String is Integer" |
| EXPR-024 | div/mod/rem types | §3.7.2 | "Result and arguments shall have type Real or Integer" |
| EXPR-025 | ceil/floor argument | §3.7.2 | "Result and argument shall have type Real" |
| EXPR-026 | integer conversion | §3.7.2 | "Argument shall have type Real; result is the largest Integer not greater than the argument". Rumoca spells `integer(x)` and the enumeration conversion `Integer(e)` (§4.9.5, the ordinal of `e`) with one builtin; only a Real argument is an event-generating step, so `Integer(e)` of an enumeration owns no root. Tested in `suite_core/enumeration_ordinal_conversion.rs` |
| EXPR-027 | delay expr type | §3.7.2 | "Expression shall be subtype of Real, Integer, Boolean, or enumeration". Rumoca: `delay(u, d)` "evaluates to" `u(time.start)` when `time - delayTime <= time.start` and `u(time - delayTime)` otherwise (Operator 3.10). `u(time.start)` is the value of `u` in the solution of the initialization problem: §8.6 "After the initialization problem has been solved, the transient analysis begins by performing an event iteration (see operator 3.24). Note that the event iteration belongs to the transient analysis, and does not solve the initial equations again.", and Operator 3.23 "At the start of the simulation the first event iteration occurs after the initialization even if the start argument is identical to the start time of the simulation." While the initialization problem settles, every delayed value follows its source (the identity `delay(u, d) = u`), so a discrete row reading the delay reads the settled initialization value; the delay history is committed from that solution before the first event iteration. A change of `u` in that event iteration (a phase-zero `sample` or clock tick, or `pre(v) <> v`) is a source jump at the event coordinate `time.start` like any later one: the delay holds `u(time.start)` through `time.start + d` and takes the right limit there, at a transported delay event. The operator text leaves open which superdense value at `time.start` is meant; the initialization solution is the only one the event iteration cannot change, and OpenModelica (2026-07) gives the same trace for a phase-zero `sample` source. A source jump at a later event is a jump of the delayed value exactly `d` later, so `integer` of it switches there: Event Mode entry holds the accepted delay history to the event coordinate as the left limit, and the settled values committed at that coordinate complete the left/right pair, also when the host adopted the event coordinate from a roundoff-coincident step end (a pure-discrete model stepped at the delay time, `Modelica.Electrical.Digital.Examples.Counter`). Tested in `suite_core/delay_initial_values.rs` and `suite_core/delay_event_jumps.rs` |
| EXPR-028 | delay time type | §3.7.2 | "Time arguments shall be subtypes of Real" |
| EXPR-029 | delay delayTime param | §3.7.2 | "When delayMax not provided, delayTime > 0 shall be parameter expression" |
| EXPR-030 | spatialDistribution params | §3.7.2.2 | "initialPoints and initialValues shall be parameter expressions of equal size" |
| EXPR-031 | spatialDistribution no vectorize | §3.7.2.2 | "Operator cannot be vectorized according to §12.4.6" |
| EXPR-032 | cardinality scope | §3.7.4.1 | "Should only be used in condition of assert and if-statements without connect" |
| EXPR-033 | cardinality not in function | §3.7.4.1 | "cardinality operator not allowed inside function classes" |
| EXPR-034 | homotopy types | §3.7.4.2 | "Scalar expressions actual and simplified are subtypes of Real". An Integer operand is a Real expression by the implicit Integer-to-Real conversion (§10.6.13), so `homotopy(if s > 0 then s/rho else 0, 0)` is a Real homotopy; both operands share one shape and the result is Real. |
| EXPR-035 | inStream not in function | §3.7.4 | "inStream operator not allowed inside function classes" |
| EXPR-036 | actualStream not in function | §3.7.4 | "actualStream operator not allowed inside function classes" |
| EXPR-037 | pre not in function | §3.7.5 | "pre operator is not allowed inside function classes" |
| EXPR-038 | smooth differentiability | §3.7.5 | "smooth(p, expr) treats expression as p times continuously differentiable" |
| EXPR-039 | noEvent event suppression | §3.3 | "noEvent suppresses event generation for relational operators within its scope" |
| EXPR-040 | Event triggering operators | §3.7.2 | "div, ceil, floor, integer can only change values at events and will trigger events as needed". Rumoca: a model-level `floor`, `ceil`, or `integer` of a varying argument `x` outside `noEvent`/`smooth` owns one state-event root on `sin(pi*x) >= 0`, so a discrete target defined from it changes at each integer crossing; function bodies generate no events. Tested in `suite_core/integer_step_events.rs` |
| EXPR-041 | smooth event freedom | §3.7.5 | "A tool is free to not generate events for expressions inside smooth. However, smooth does not guarantee that no events will be generated." Rumoca takes the freedom except for a relation inside `smooth(0, ..)` whose operands are unknowns of its own algebraic block, which owns an MLS §8.5 event (SPEC_0044 ME-EVENT-008) |

### 4.5 Equation Contracts (EQN)

| ID | Contract | MLS | Requirement |
|----|----------|-----|-------------|
| EQN-001 | Local balance | §4.8 | "Local number of unknowns identical to local equation size" |
| EQN-002 | Input binding | §4.8 | "All non-connector inputs of model/block must have binding equations" |
| EQN-003 | Algorithm atomicity | §11 | "Algorithm section with N left-hand side variables = atomic N-dimensional vector-equation" |
| EQN-004 | If-equation balance | §8.3.3 | "Same number of equations in each branch" |
| EQN-005 | When nesting | §8.3.5 | "When-equations cannot be nested" |
| EQN-006 | When location | §8.3.5 | "When-equations shall not occur inside initial equations" |
| EQN-007 | Discrete-valued solved form | App B | "Discrete-valued equations must be in almost solved form" |
| EQN-008 | For-equation vector | §8.3.2 | "Expression of a for-equation shall be a vector expression" |
| EQN-009 | For-equation evaluable | §8.3.2 | "Expression of a for-equation shall be evaluable" |
| EQN-010 | For-loop variable readonly | §8.3.2 | "Loop-variable shall not be assigned to" |
| EQN-011 | If-equation scalar Boolean | §8.3.3 | "Expression of if-clause must be a scalar Boolean expression" |
| EQN-012 | If-equation variable sets | §8.3.3 | "All branches shall have same set of variables in non-Real equations" |
| EQN-013 | When branch variables | §8.3.5 | "Different branches of when/elsewhen must have same set of left-hand side component references". §8.3.5.1 writes a `when`/`elsewhen` chain as one if-expression per assigned variable over the edges of every branch, so the chain belongs to a periodic clock partition only when every branch is activated by that same clock; a chain that also reacts to `initial()` or a relation is an ordinary event chain, and its `sample(start, interval)` branches are activations that own no partition (MLS §3.7.5: a Boolean event operator). Tested in `suite_core/sample_tick_pulses.rs` |
| EQN-014 | When LHS evaluable indices | §8.3.5 | "Any left hand side indices must be evaluable expressions" |
| EQN-015 | reinit in when only | §8.3.5 | "reinit can only be used in the body of a when-equation" |
| EQN-016 | reinit state variable | §8.3.5 | "reinit x: x must be selected as a state" |
| EQN-017 | reinit single when | §8.3.5 | "reinit for a variable can only be applied in one when-equation" |
| EQN-018 | reinit if branches | §8.3.5 | "Multiple reinit in same when-clause must appear in different if branches" |
| EQN-019 | connect evaluable | §8.3.6 | "connect in for/if only if indices/conditions are evaluable expressions" |
| EQN-020 | When single-assign | §8.3.5 | "Two when-equations shall not define the same variable" |
| EQN-021 | Constants by declaration | §8.6 | "Constants shall be determined by declaration equations" |
| EQN-022 | Constant fixed | §8.6 | "fixed = false is not allowed for constants" |
| EQN-023 | When initial activation | §8.6 | "When-clause equations active during initialization only if enabled with initial()". Rumoca: `initial()` is true for the initialization pass only; the event iteration that §8.6 says must follow at the start time ("pre(vi) := vi must be set and an event iteration at the initial time must follow") runs with `initial()` false, so `when {initial(), change(x) and not initial()}` also fires on a `change` the advanced `pre` values cause there (`Modelica.Electrical.Digital.Delay.InertialDelaySensitive`). Event actions (assertions, terminations, terminal prints) act per instant: those of the initialization instant are evaluated on the initialization pass with `initial()` true, and those of the event iteration at convergence with `initial()` false, on the edges since the initialization pass. A pass that settles only unowned rows ahead of a phase-zero clock tick reads every clock lane as not ticking, so the tick acts once, after initialization. Tested in `suite_core/when_activation_settled_reads.rs` and `suite_core/terminal_print.rs` |
| EQN-024 | No statements in equations | §8.3 | "No statements allowed in equation sections, including := operator" |
| EQN-025 | Equation type compatible | §8.3.1 | "Types of left-hand-side and right-hand-side must be compatible" |
| EQN-026 | Connect evaluable conditions | §8.3.3 | "Indices/conditions of for/if containing connect must be evaluable, not depend on cardinality/rooted" |
| EQN-027 | If-equation LHS component | §8.3.4 | "Non-Real equations in non-evaluable if-equation shall have component-references as LHS" |
| EQN-028 | If-equation nested evaluable | §8.3.4 | "Any for- and if-equations in if-equation branches shall have evaluable controlling conditions" |
| EQN-029 | When expr type | §8.3.5 | "Expression shall be discrete-time Boolean scalar or vector expression". A condition that reads a `sample(start, interval)` tick is true only at its tick instants (§3.7.5), so its edge buffer is a Solve `PulseConditionMemory` row: it follows the current value inside the tick's event iteration and, once the event has converged, takes the condition with every tick cleared, which is the left limit the next instant reads; a tick at the start instant is the first event after initialization (§8.6), so `{initial(), sample(0, T)}` activates twice at the start instant. The FMI C profile refuses pulse buffers. Tested in `suite_core/sample_tick_pulses.rs` |
| EQN-030 | When conditional scope | §8.3.5.2 | "When-equations can only occur in if/for-equations if controlling expressions are evaluable" |
| EQN-031 | reinit type compatible | §8.3.6 | "Expr needs to be type-compatible with x" |
| EQN-032 | reinit implies stateSelect | §8.3.6 | "Reinit on x implies stateSelect = StateSelect.always on x" |
| EQN-033 | Perfect matching | §8.4 | "There must exist a perfect matching of variables to equations after flattening" |
| EQN-034 | Discrete persistence | §8.4 | "Discrete-time variables keep their values until explicitly changed" |
| EQN-035 | Init pre equality | §8.6 | "Before start of integration, for all variables v, v = pre(v) must be guaranteed" |
| EQN-036 | Assert evaluable level | §8.3.7 | "assertionLevel is an optional evaluable expression". "If the level is AssertionLevel.warning, the current evaluation is not aborted" and "the assert(..) statement shall have no influence on the behavior of the model". For example, "by evaluating the condition to report the message an event is not triggered". Implemented: flattening settles a level that names a predefined literal by its declaration identity; `AssertionLevel.error` is the default level and any other level expression is a typed construction rejection (`UnsupportedAssertionLevel`). A warning-level condition is evaluated as `noEvent(condition)`: it owns no relation, condition memory, root, or step-control influence, never aborts a call, event iteration, step, or transaction commit, and leaves the trace bit-identical. The run observes it at every settled event and every accepted output point and reports each site once, at its first observed violation, as SPEC_0008 `WX001` on the simulation result; generated FMI 2 and FMI 3 components log it once through the importer's logger at the warning status, at initialization, events, completed integrator steps, and co-simulation step ends. GALEC and eFMI export omit warnings, which have no behavior to represent; that target has no reporting channel. |
| EQN-037 | When not in initial eq | §8.6 | "It is not allowed to use when-clauses in initial equation/algorithm sections" |
| EQN-038 | Connections.branch scope | §8.3.3 | "Connections.branch/root/potentialRoot same restrictions as connect in for/if-equations"
| EQN-039 | If-equation evaluable conditions | §8.3.4 | "The if-equations which do not have exclusively evaluable expressions as switching conditions shall satisfy the following: [...] Have the same number of equations in each branch, where the number of equations is defined as in definition 4.4." An ordinary parameter (MLS 3.7 §4.5: evaluable, and neither `final` nor `Evaluate = true`) is determined by the initialization problem, which §4.5 leaves to the tool, so a guard reading one over branches with the same scalar equation count and the same unknown incidence is kept as a run-time branch; otherwise the tool determines those parameters during translation instead (structural selection), publishes them non-settable, and warns at the equation (WD001). A guard reading a non-evaluable parameter (§4.5: `fixed = false`, `Evaluate = false`, or a binding that is not an evaluable expression) is never selected at translation, so an if-equation whose branches differ in equation count under such a guard is rejected |
| EQN-040 | Initial discrete definitions | §8.6 | "During this phase, also the derivatives `der(...)` and the pre-variables `pre(...)` are interpreted as unknown algebraic variables. The initialization uses all equations and algorithms that are utilized in the intended operation." An initial equation `pre(m) = e` or `m = e` therefore determines the initialization value of a discrete `m` from the initialization solution, not only from parameters. Rumoca: `e` may read parameters, constants, `time`, inputs, states, and algebraics (directly or through a binding or connection); Solve applies the definition as an initialization update after the projection, and construction proves that its reads, expanded through matched algebraic definitions, derivative rows, and substituted bindings, reach no discrete or `pre` value other than one settled before the projection, so the projection settles them independently of `m` (SPEC_0043 §4). A discrete whose own definition reads only `time`, parameters, constants, inputs, given states, and settled discretes (`count = integer((time - startTime)/period)` in `Modelica.Blocks.Sources.SawTooth`) is settled: the projection and other definitions read it as a known value. A read that leads back to an unsettled discrete value is rejected rather than iterated; a definition reading `pre`, a derivative, or another discrete coordinate stays an initialization residual. Tested in `suite_core/initial_discrete_continuous_reads.rs`. A whole-array definition (`pre(reset) = fill(false, nReset)` in `Modelica.Blocks.Sources.RadioButtonSource`) owns the one row its row-major structured family materializes. An initial equation relating two whole discrete coordinates (`pre(newActive) = pre(localActive)` in `Modelica.StateGraph` steps) is settled through the equation section (SPEC_0040 DAE-C28): the coordinates that initial relations and plain aliases (`a = b`, `a = pre(b)`) join take the one value determined among them (`InitialStep`: `active = true`); none or two determined values are refused (ED013), never settled by a start value. Tested in `suite_core/initial_discrete_relations.rs`. Tested in `suite_core/initial_discrete_row_major_family.rs` |
| EQN-041 | Mixed initialization system | §8.6 | "During this phase, also the derivatives `der(...)` and the pre-variables `pre(...)` are interpreted as unknown algebraic variables. The initialization uses all equations and algorithms that are utilized in the intended operation." An initialization row that reads a discrete coordinate determined from continuous values (`quenched = off and (abs(i) <= abs(v)*Goff or pre(quenched))` in `Modelica.Electrical.Analog.Interfaces.IdealSwitchWithArc`) is part of one mixed system. Rumoca: the projection holds each such discrete at its current value, the Solve plan records that it does (`InitializationProjectionPlan::iterates_discretes`), and the runtime alternates the projection with the discrete assignments until the discrete values stop changing; that fixed point satisfies every equation at once, and an iteration that does not settle is refused. A discrete settled before the projection (EQN-040) is read as a known value without iteration. The event memory a relation holds between events is, at the initialization instant, the relation evaluated at the initialization solution: the settle loop refreshes it after every projection until it no longer changes (`Modelica.Blocks.Sources.Pulse` reads `time < T_start + T_width` with `T_start` from its initial algorithm). Tested in `suite_core/initial_held_discretes.rs` |

### 4.6 Algorithm Contracts (ALG)

| ID | Contract | MLS | Requirement |
|----|----------|-----|-------------|
| ALG-001 | No equation syntax | §11 | "Equation equality = shall not be used in an algorithm section" |
| ALG-002 | LHS component types | §11.1 | "Only type, record, operator record, connector may appear as left-hand-side" |
| ALG-003 | For event evaluable | §11.2 | "If for-statement contains event-generating expressions, index shall be evaluable" |
| ALG-004 | For no array overwrite | §11.2 | "No assignments to entire arrays subscripted with loop variable inside for" |
| ALG-005 | While scalar Boolean | §11.2 | "Expression of while-statement shall be a scalar Boolean expression" |
| ALG-006 | While no events | §11.2 | "Event-generating expressions not allowed in while condition or body" |
| ALG-007 | When not in function | §11.2 | "When-statement shall not be used inside a function class" |
| ALG-008 | When not in initial | §11.2 | "When-statement shall not occur inside an initial algorithm" |
| ALG-009 | When no nesting | §11.2 | "When-statement cannot be nested inside another when-statement" |
| ALG-010 | When not in control | §11.2 | "When-statements shall not occur inside while/for/if in algorithms" |
| ALG-011 | When discrete Boolean | §11.2 | "Expression of when-statement shall be discrete-time Boolean" |
| ALG-012 | break scope | §11.2 | "break can only be used in while or for loop" |
| ALG-013 | return scope | §11.2 | "return can only be used inside functions". A `return` ends the function's algorithm (§11.2.6): statements after a top-level `return` never run, a conditional all of whose branches return guards the statements after it, and a conditional only some of whose branches return (the else part of `Modelica.Fluid.Utilities.regRoot2_utility`) continues each non-returning branch with the statements after it. A further `return` after such a conditional, or one inside a loop, is a typed refusal. |
| ALG-014 | terminate not in function | §11.2 | "terminate-statement shall not be used in functions" |
| ALG-015 | Assert execution halt | §11.2.8.1 | "A failed assert stops the execution of the current algorithm". An `assert` or `terminate` message is a String expression (§8.3.7, §8.3.8): literals, concatenation, `String(...)` conversions, and String parameters, which read as their declared value because the runtime owns no String storage (`Modelica.Blocks.Logical.TerminateSimulation.terminationText`); any other String-valued message is refused at its own span. |
| ALG-016 | For range fixed | §11.2.2 | "For-statement range expressions are evaluated once before entering loop". Rumoca: an `initial algorithm` `for` whose range bounds read only literals, constants, and evaluable parameters unrolls into its iterations, each replayed with the index bound, so `y[i]` names the element coordinate `y[k]`; a range that reads a settable parameter is refused (ED013). Tested in `suite_core/initial_algorithm_test.rs`. A model algorithm `for` whose body assigns only whole variables (or nothing) carries one value through its iterations, which no compact element domain holds; with a range fixed the same way it is unrolled before DAE analysis, so each relation of each iteration owns its own event, and a top-level `if` of checks reading no algorithm target becomes its own assertion section. Tested in `suite_core/algorithm_carried_loops.rs` |
| ALG-017 | LHS initialization | §11.1.2 | "Variables on the left-hand side of := must be initialized when algorithm is invoked". Rumoca: an event algorithm seeds every discrete target with its `pre` value (§11.1.2), so a read before the target's own definition is a history read and no current-value self-dependency in the Appendix B solved-form proof. A statement after a `when` statement reads a value the `when` body writes as that variable's current value when no later statement writes it again, since that is its value after the algorithm; a read of a value a later statement writes again is refused (ED013). Both rules match storage, not names: a whole record write releases, and conflicts with a later write of, each field it writes, and a field write conflicts with a later write of its whole record. Tested in `suite_core/when_written_reads.rs` |
| ALG-018 | While loop execution | §11.2.3 | "The body of a while-statement is executed as long as the condition is true". Rumoca: a function `while` loop with a proven iteration bound (a counter conjunct `k < N`, `k <= N` with `N` settled at translation, one top-level `k := k + d` with `d > 0` and otherwise only `k := N` before it, no `break`/`return`, and `k >= 1` proven by a subscript it indexes or a dominating literal assignment or declaration default) lowers to `for w in 1:N loop if c then S end if; end for`, exact because a false condition changes no value; a loop inside a conditional branch runs as statements guarded by the branch's immutable guard, and the loop-free remainder stays one conditional. Any other `while` keeps its typed rejection. Tested in `suite_core/function_while_loops.rs` |
### 4.7 Connection Contracts (CONN)

| ID | Contract | MLS | Requirement |
|----|----------|-----|-------------|
| CONN-001 | Homogeneity | §9.2 | "Connection set shall contain either only flow or only non-flow variables" |
| CONN-002 | Type matching | §9.2 | "Matched primitive components must have the same primitive types" |
| CONN-003 | Flow-to-flow | §9.2 | "Flow variables may only connect to other flow variables" |
| CONN-004 | Single source | §9.2 | "At most one inside output connector or one public outside input connector". Rumoca: the equalities of a discrete-valued connection set are oriented away from its one producer (a defined coordinate), each defining the member farther from it; a set whose output member is the right side of a plain alias `a = b` whose `a` already has a definition (`Modelica.StateGraph.Interfaces.CompositeStepState.suspend = subgraphStatePort.suspend` under the composite step's binding) is produced by that member through the reversed alias (MLS Appendix B). Tested in `suite_core/discrete_alias_fed_connections.rs`. A connection set is a set of scalar coordinates, so the distance from the producer is kept per array element: `connect(b, and.x[1])` and `connect(a, and.x[2])` orient each element from its own set's producer, and an element connection reads that producer directly, so two arrays whose elements feed each other in opposite directions form no whole-coordinate cycle. Tested in `suite_core/element_connection_sets.rs` |
| CONN-005 | Quantity matching | §9.2 | "Variables with non-empty quantity attribute must match" |
| CONN-006 | No outer-to-outer | §9.2 | "Cannot connect two connectors of outer elements" |
| CONN-007 | Connector not parameter | §9.1 | "Connector component shall not be declared with parameter or constant" |
| CONN-008 | Same dimensions | §9.2 | "Two connectors must have same named elements with same dimensions" |
| CONN-009 | Expandable no flow | §9.1.3 | "Expandable connector shall not contain flow component" |
| CONN-010 | Expandable to expandable | §9.1.3 | "Expandable connectors can only connect to other expandable connectors" |
| CONN-011 | Expandable declared | §9.1.3 | "At least one connector must reference a declared component" |
| CONN-012 | Expandable input deduction | §9.1.3 | "Multiple inputs in expandable connectors deduced as input is error" |
| CONN-013 | Overconstrained root | §9.4 | "Every subgraph shall have at least one definite or potential root node" |
| CONN-014 | No spanning tree cycle | §9.4 | "Cycle among required spanning-tree-edges is error" |
| CONN-015 | Two definite roots | §9.4 | "Error if two definite root nodes connected through required spanning tree edges" |
| CONN-016 | Protected connector | §9.2 | "Protected outside connector must connect to inside or public outside connector" |
| CONN-017 | Balance flow = potential | §9.3.1 | "For non-partial non-simple non-expandable connector: number of flow = number of potential variables" |
| CONN-018 | Simple connector prefix | §9.3.1 | "Simple connector components must be declared as input, output, or protected" |
| CONN-019 | Connect subscripts evaluable | §9.3 | "Subscripts shall be evaluable expressions or special operator :" |
| CONN-020 | Expandable sizeless no use | §9.1.3 | "Sizeless array component shall not be used without subscripts" |
| CONN-021 | Expandable input source | §9.1.3 | "If variable appears as input in expandable, should appear as non-input in at least one other" |
| CONN-022 | Overdetermined no flow | §9.4.1 | "Overdetermined type/record may not have flow components nor be used as type of flow components" |
| CONN-023 | Overconstrained not in function | §9.4.1 | "None of these operators allowed inside function classes" |
| CONN-024 | equalityConstraint prototype | §9.4.1 | "equalityConstraint function shall have specified prototype" |
| CONN-025 | equalityConstraint n constant | §9.4.1 | "Array dimension n shall be constant Integer expression evaluable during translation, n ≥ 0" |
| CONN-026 | Flow sign convention | §9.2 | "Flow sign is +1 for inside connectors and -1 for outside connectors" |
| CONN-027 | potentialRoot priority | §9.4.1 | "Priority p for potentialRoot must be p ≥ 0" |
| CONN-028 | Parameter/constant variability | §9.3 | "Primitive components may only connect parameter to parameter and constant to constant" |
| CONN-029 | Connect arguments are connectors | §9.3 | "Both arguments of connect must be connector references" |
| CONN-030 | Stream-to-stream | §9.3 | "Stream variables may only connect to other stream variables" |

### 4.8 Function Contracts (FUNC)

| ID | Contract | MLS | Requirement |
|----|----------|-----|-------------|
| FUNC-001 | Input/output only | §12.2 | "Each public component must have prefix input or output" |
| FUNC-002 | Input read-only | §12.2 | "Input formal parameters are read-only after being bound" |
| FUNC-003 | Output assignment | §12.2 | "Output variables must be assigned inside the function or have external function interface" |
| FUNC-004 | Impure restrictions | §12.3 | "Error if impure function call is part of a system of equations" |
| FUNC-005 | Pure guarantee | §12.3 | "Pure functions always give same output for same input" |
| FUNC-006 | No equations | §12.2 | "Function shall not have equations, shall not have initial algorithms" |
| FUNC-007 | No when-statements | §12.2 | "Function body shall not contain when-statements" |
| FUNC-008 | Variability constraint | §3.8 | "Higher variability cannot assign to lower variability" |
| FUNC-009 | Array dimension | §12.2 | "Array dimension sizes must be given by inputs, constants, or parameter expressions". Rumoca: a function-local range or slice `a:s:b` (MLS §10.4.1) whose bounds are not translation-time values still has an exact extent when `b - a` is: both bounds are read as affine Integer forms over the scalars the scope does not settle, and the extent is taken from their difference when every such term cancels, so `state[i - 2:i - 1]` in a loop over `i` has extent 2. A dimension of a function local is proven per call-site specialization from the input values that call proves, through the same translation-time evaluator as parameter bindings, including a call of another function of those inputs (`SI.Angle oBase[numberOfSymmetricBaseSystems(m)]` in `Modelica.Electrical.Polyphase.Functions.symmetricOrientationMatrix`). Tested in `suite_core/function_loop_offset_slices.rs` and `suite_core/function_call_extents.rs` |
| FUNC-010 | Forbidden operators | §12.2 | "der, initial, terminal, sample, pre, edge, change, reinit, delay, cardinality, inStream, actualStream" |
| FUNC-011 | No Clock components | §12.2 | "Function may not contain components of type Clock" |
| FUNC-012 | No inner/outer | §12.2 | "Function elements shall not have prefixes inner or outer" |
| FUNC-013 | Non-partial for simulation | §12 | "For simulation, function shall not be partial" |
| FUNC-014 | Single algorithm/external | §12.2 | "Function can have at most one algorithm section or one external function interface". MLS 3.7 wording: "A function can have at most one algorithm section or one external function interface (not both), which, if present, is the body of the function." Rumoca: the algorithm sections a function declares and inherits are counted together, so a `function extends` (including `redeclare function extends`) that adds a section to a base with one is refused (EF035); a function that only modifies an inherited body is accepted. Tested in `suite_core/function_single_body.rs`. A function that declares no body and extends several function bases (the `Modelica.Utilities.Files.loadResource` pattern: a partial interface and an implementation of it) has as its body the one body its bases' inheritance chains declare; bodyless bases contribute only the interface. Tested in `suite_core/function_interface_and_body_bases.rs` |
| FUNC-015 | Component types | §12.2 | "Function must not contain model, block, operator, or connector components" |
| FUNC-016 | Not in connections | §12.2 | "Functions shall not be used in connections" |
| FUNC-017 | Return in algorithm only | §12.1.2 | "Return statement can only be used in an algorithm section of a function" |
| FUNC-018 | Input ordering significant | §12.1.1 | "Relative ordering between input formal parameter declarations is significant" |
| FUNC-019 | Named arg slot error | §12.4.1 | "Error if named argument slot is already filled" |
| FUNC-020 | Unfilled slots error | §12.4.1 | "Error if any unfilled slots remain after argument processing" |
| FUNC-021 | Impure inheritance | §12.3 | "If function declared impure, any extending function shall be declared impure" |
| FUNC-022 | Impure call scope | §12.3 | Stated of the written prefix: "With the prefix keyword impure it is stated that a Modelica function is impure and it is only allowed to call such a function from within: Another function marked with the prefix impure. A when-equation. A when-statement. pure(impureFunction(…)). Initial equations and initial algorithms. Binding equations for components declared as parameter. Binding equations for external objects." Rumoca enforces the first three plus initial sections and parameter bindings. `pure(…)` is recognized and erased during lowering, but it does not yet suppress the callee purity check (that needs a Flat-visible call-site marker; task #57), so `pure(impureFunction(…))` is still rejected by Resolve. External-object bindings are not yet constructible (ED019). Neither is advertised in the diagnostics as an accepting context while that holds. The restriction binds a body whose declaration wrote `pure`; a function without explicit purity is the case MLS 3.7 §12.3 calls deprecated ("For a function without explicit purity, it is deprecated to call any function declared impure, except when wrapped in pure(…)"), and such a function is then treated as impure for transformations (FUNC-032). Rumoca accepts that call and rejects one from a body declared `pure` (ED019). A call of the terminal printer `ModelicaInternal_print` in a when-statement of a model algorithm is a print event action (SPEC_0040 DAE-C31), and the same call outside a when-statement is refused (`IllegalImpureCallContext`). A call reaching stateful foreign library state is threaded through a pure specialization (SPEC_0040 FLAT-C05), so its context is proven before threading with the recursive impurity of FUNC-032: it is refused outside a when-statement of a model algorithm and in a body declared `pure` (EF036). Tested in `suite_core/function_purity_contexts.rs`, `suite_core/terminal_print.rs`, and `suite_core/foreign_library_state.rs` |
| FUNC-023 | Binding no cycles | §12.4.4 | "Binding execution order must not have cycles" |
| FUNC-024 | Uninitialized error | §12.4.4 | "It is an error to use or return an un-initialized variable in a function." The error belongs to the executed path. Rumoca: a value that a top-level `if` of the function body defines totally on some paths and leaves unwritten on others is joined with a dead operand on the unwritten paths; each top-level use of it, and its return as an output, is preceded by a call-scoped assertion that the executed path assigned it (the call fails there, never returns the dead operand). A use inside a nested statement, or a use after a loop that may write the value, stays a typed construction rejection. Rumoca also (partial): a record result or protected record field is initialized by its type's field default when the algorithm never writes it; a field with neither is rejected as ED022. Tested in `suite_core/function_record_defaults.rs`. Element reads are proven per unrolled loop point: a slice whose bounds read the loop variable names exact indices there, including the one-iterator comprehension form slice normalization writes, so a loop may read the elements its earlier iterations defined. Tested in `suite_core/function_loop_offset_slices.rs` |
| FUNC-025 | LHS list output | §12.4.3 | "Left-hand side references must agree with type of corresponding output component" |
| FUNC-026 | Vectorization non-replaceable | §12.4.6 | "Only function classes that are transitively non-replaceable (see section 6.3.1 and section 7.1.4) may be called vectorized." §6.3.1: "A class reference is transitively non-replaceable if and only if all parts of the name satisfy the following: If the class definition is long it is transitively non-replaceable if not declared replaceable. If the class definition is short (i.e., class A = P.B) it is transitively non-replaceable if it is non-replaceable and equal to class reference (P.B) that is transitively non-replaceable." Rumoca (documented deviation, accepted extension that MSL 4.1 relies on, e.g. `Medium.prandtlNumber(states)` in `Fluid.Pipes.BaseClasses.HeatTransfer.LocalPipeFlowHeatTransfer`): a vectorized call is also accepted when, after replaceable-package selection (MLS §7.3), the call's exposure path names the function through enclosing classes that are all transitively non-replaceable, so the selected package fixes the function at translation even though the function itself is declared `replaceable` there (the restriction exists so the callee is known at translation, and it then is). A call spelled through a replaceable package alias (`replaceable package Medium = A` in the model) is accepted on the same ground when the classes enclosing the alias and its right-hand side are transitively non-replaceable: the declared selection, or the redeclaration flattening resolves to one exact function instance, fixes the package. A call whose callee is still unresolved after selection (an alias to a replaceable package, or a partial function) is refused. Tested in `suite_core/selected_package_vectorization.rs` and `func_contracts.rs` |
| FUNC-027 | Vectorization size match | §12.4.6 | "Array arguments have to be the same size". Rumoca: the translation-time evaluator applies a scalar function to an array argument element-wise over the extra leading dimension and rejects arguments of different sizes; element-wise operators broadcast a scalar operand (§10.6). Tested in `suite_core/vectorized_constant_calls.rs` |
| FUNC-028 | Record constructor scope | §12.6 | "Record constructor can only reference records found in global scope" |
| FUNC-029 | Record cast conditional error | §12.6.1 | "A record cast is erroneous if a corresponding source model/block/connector component is conditional" |
| FUNC-030 | Derivative outputs non-empty | §12.7.1 | "Derivative output list shall not be empty" |
| FUNC-031 | zeroDerivative condition | §12.7.1 | "zeroDerivative applies only if inputVar is independent of differentiation variables" |
| FUNC-032 | External purity deprecated | §12.3 | "External functions not explicitly declared with pure or impure is deprecated." Such a function is normative-impure for transformations: "a function shall be treated as impure in the following cases (applied recursively): It is declared impure. It is an external function without explicit purity. It calls another function treated as impure, except when wrapped in pure(…)." The deprecation is a report, not a call restriction — MLS 3.6 §12.3 stated the report as a requirement ("a diagnostic must be given if called in a simulation model") and made callability explicit ("without any restriction on calling them"), which is the historical basis for accepting the call. Rumoca: WR001 on every such declaration; body recorded impure. The recursive third case is closed over the DAE call graph in `rumoca-phase-structural/src/time_invariant.rs` (greatest fixpoint starting from all-pure; a bare external is impure; the `pure(...)` exception is not honored, conservatively) and feeds the structural time-invariance proof (SPEC_0040 STRUCT-T03). |
| FUNC-033 | Functional param type | §12.4.2 | "Function type parameter cannot be type-specifier of record or enumeration" |
| FUNC-034 | Input default independence | §12.4.1 | "Default values for inputs shall not depend on non-input variables in the function" |
| FUNC-035 | Derivative ordering | §12.7.1 | "Most restrictive derivative annotations should be written first"
| FUNC-036 | ExternalObject lifecycle shape | §12.9.7 | "ExternalObject owner uses the specialized class `class`, directly extends ExternalObject, owns exactly non-replaceable constructor and destructor functions, and owns no other elements" |
| FUNC-037 | ExternalObject lifecycle signatures | §12.9.7 | "Constructor has exactly one output of the owning ExternalObject type; destructor has exactly one input of that type and no outputs" |
| FUNC-038 | ExternalObject lifecycle calls | §12.9.7 | "Constructor and destructor cannot be called explicitly; each constructed object is constructed and destroyed exactly once" |
| FUNC-039 | Component bindings read inputs | §12.4.4 | Output and protected component declaration bindings are evaluated in the function, with its inputs available, including fields of a record input (`Real k1 = f(if data.zeta1_at_a then data.diameter_a else data.diameter_b)`). Rumoca: record inputs are decomposed into field inputs, and the declaration expressions of outputs and locals are rewritten to those fields like the algorithm section. Tested in `suite_core/fluid_function_data.rs` |
| FUNC-040 | Function arguments | §12.4.2.1 | "A function partial application is specified by the function keyword followed by a function call to func_name giving named formal parameter associations for the formal parameters to be bound". Rumoca: flattening specializes the callee once per distinct function argument; calls of the formal function become direct calls with the bound formals as extra inputs, so no function value reaches DAE or Solve. A bare function name is collected and specialized like a partial application with no bindings. An argument that is neither (a Real value at a function input, for example) is refused (EF016). Tested in `suite_core/function_valued_arguments.rs` |
| FUNC-041 | Element definedness through comprehensions | §12.4.4, §10.4.2 | A comprehension iterator takes each value of its range in turn, so an element-written function local is readable inside `{e(i) for i in r}` exactly when every element the settled points of `r` name is defined. Rumoca: a comprehension whose ranges settle to static integers is checked per point (Vandermonde column fill of `Math.Polynomials.fitting`); a range that does not settle keeps the whole-body check. Tested in `suite_core/function_comprehension_definedness.rs` |
| FUNC-042 | Text in pure-call interfaces | §4.9.4, §12.4 | A `String` value carries no numeric value. Rumoca: a `String` scalar, array, or record field occupies no leaf of a Solve pure-call interface, so neither a call inside a function nor a model-level call lowers a text argument, and a record constructor does not lower its text fields; any numeric use of a text value is refused at construction. Tested in `suite_core/text_record_fields.rs` |
| FUNC-043 | Recursive functions | §12.2 | "A function can be recursive." The MLS states no termination requirement. Rumoca: a recursive call SCC of pure Modelica functions executes as one recursive owner group of the Solve pure-call table (SPEC_0040 SOLVE-C62). The declared recursion depth limit of the execution profile bounds every chain of active group invocations; a call beyond it fails with a typed recursion-depth error at its span (never truncated or replaced). DAE incidence projection reads every argument scalar of a call to a function already being projected, a bound that holds for every pure function and needs no unrolling. A call whose arguments carry no tangent differentiates to zero without a directional relation. A recursive group whose functions carry call-scoped assertions, a nonzero derivative through a recursive call, and the FMI C profile are refused with typed diagnostics. Tested in `suite_core/recursive_functions.rs` and `func_contracts.rs` |

### 4.9 Type/Interface Contracts (TYPE)

| ID | Contract | MLS | Requirement |
|----|----------|-----|-------------|
| TYPE-001 | Subtype redeclaration | §7.3.2 | "Class or type must be subtype of constraining type" |
| TYPE-002 | Plug-compatible modifier | §6.5 | "Redeclarations must be plug-compatible with constraining interface" |
| TYPE-003 | Default-connectable | §6.5 | "Additional public components must be default-connectable" |
| TYPE-004 | Function compatibility | §6.6 | "Inputs same order, outputs match, purity matches" |
| TYPE-005 | Class/component match | §6.4 | "A is a class if and only if B is a class" |
| TYPE-006 | Operator record base | §6.4 | "If A has operator record base class, B must have same one" |
| TYPE-007 | ExternalObject identity | §6.4 | "If A derived from ExternalObject, B must also be and have same full name" |
| TYPE-008 | Replaceable constraint | §6.4 | "If B is not replaceable then A shall not be replaceable" |
| TYPE-009 | Variability ordering | §6.4 | "A compatible with B only if declared variability in A ≤ variability in B" |
| TYPE-010 | Input/output match | §6.4 | "Input and output prefixes must be matched" |
| TYPE-011 | Dimension count match | §6.4 | "Number of array dimensions in A and B must be matched" |
| TYPE-012 | Conditional match | §6.4 | "Conditional components are only compatible with conditional components" |
| TYPE-013 | Enumeration match | §6.4 | "If B is enumeration type, A must also be and vice versa" |
| TYPE-014 | Built-in type match | §6.4 | "If B is built-in type, A must be same built-in type" |
| TYPE-015 | Connector not input | §6.5 | "Connector component must not be an input" |
| TYPE-016 | Not expandable | §6.5 | "Connector component must not be of expandable connector class" |
| TYPE-017 | Binding required | §6.5 | "Parameter/constant/non-connector input must have binding equations" |
| TYPE-018 | Func input order | §6.6 | "All public input components of B have named inputs in A in same order" |
| TYPE-019 | Func output order | §6.6 | "All public output components of B have named outputs in A in same order" |
| TYPE-020 | Func input binding | §6.6 | "Public input component of A not present in B must have binding assignment" |
| TYPE-021 | Func impure propagate | §6.6 | "If A is impure, then B must also be impure" |
| TYPE-022 | Transitively non-replaceable | §6.4 | "If B is transitively non-replaceable then A must be transitively non-replaceable" |
| TYPE-023 | No other elements | §6.4 | "Interface of A shall not contain any other elements when B is transitively non-replaceable" |
| TYPE-024 | Flow/stream match | §6.4 | "The flow or stream prefix should be matched for compatibility" |
| TYPE-025 | Inner/outer match | §6.4 | "The inner and/or outer prefixes should be matched" |
| TYPE-026 | Final semantic match | §6.4 | "If B is final, A must also be final and have same semantic contents" |
| TYPE-027 | Conditional contents | §6.4 | "Conditional components: conditions must have equivalent contents" |
| TYPE-028 | Class kind match | §6.4 | "Function only compatible with function, package with package, connector with connector, etc." |
| TYPE-029 | Enum literals order | §6.4 | "If B is enumeration not defined as (:), A must have same literals in same order" |
| TYPE-030 | Modifier element exists | §6.4 | "Modified element should exist in element being modified" |
| TYPE-031 | Top-level redecl subtype | §6.5 | "Redeclaration of inherited top-level component must be subtype of constraining interface" |
| TYPE-032 | Record expr compatible | §6.7 | "If A is record expression, B must also be record expression with same named elements" |
| TYPE-033 | Real/Integer coercion | §6.7 | "If A is Real expression, B must be Real or Integer; result is Real" |
| TYPE-034 | Integer division result | §6.7 | "For Integer exponentiation and division, result type is Real even if both operands Integer" |
| TYPE-035 | Operator record consistency | §6.7 | "For array/if-expressions: if A has operator record base, B must have same one"
| TYPE-036 | Predefined-type attribute inheritance | §4.6.2, §4.9, §7.2.3 | A component of a type derived from a predefined type (short class definition or `extends` of a type, through any chain) carries the attribute modifications of that chain; the component's own modifiers and outer modifications win, then the innermost type, then its bases in order. Rumoca: every §3.16 attribute except `unbounded`, which the IR does not represent; values are evaluated in the scope of the type that wrote them; a non-`each` value of an array type repeats over the component's own dimensions; a modification of an attribute the chain made `final` is EI028 (INST-010). |

### 4.10 Array Contracts (ARR)

| ID | Contract | MLS | Requirement |
|----|----------|-----|-------------|
| ARR-001 | Fixed dimensionality | §10 | "Number of dimensions is fixed and cannot be changed at run-time" |
| ARR-002 | Rectangular arrays | §10 | "All dimensions must be uniform (no ragged arrays)" |
| ARR-003 | Size matching | §10 | "Binary operations require matching sizes or scalar operands" |
| ARR-004 | Slice uniformity | §10 | "Member access slices valid only if component sizes match across elements" |
| ARR-005 | Index bounds | §10.5 | "Array dimension indexed by Integer has lower bound 1, upper bound = size" |
| ARR-006 | Constructor non-empty | §10.4 | "array() or {} is not defined; there must be at least one argument" |
| ARR-007 | Concatenation dimensions | §10.4 | "Arrays must have same number of dimensions for concatenation" |
| ARR-008 | Concatenation size match | §10.4 | "Arrays must have identical sizes except for concatenation dimension" |
| ARR-009 | Integer to Real coercion | §10.6.13 | "Integer expression automatically converted to Real in Real context" |
| ARR-010 | promote n >= ndims | §10.3 | "promote(A, n): n ≥ ndims(A) is required" |
| ARR-011 | promote n constant | §10.3 | "Argument n must be constant that can be evaluated during translation" |
| ARR-012 | size i bounds | §10.3.1 | "size(A, i): required that 1 ≤ i ≤ ndims(A)". Rumoca: `size(A, i)` is the extent of `A`, not a read of its value; inside a function body whose shape proof settles that extent the query lowers to the Integer, so an output may ask its own extent before the algorithm defines it (`state := f(size(state, 1))`). Tested in `suite_core/function_loop_offset_slices.rs` |
| ARR-013 | size expandable decl | §10.3.1 | "size(A) with expandable connector: component must be declared, must not use colon" |
| ARR-014 | scalar all size 1 | §10.3.2 | "scalar(A): size(A, i) = 1 required for 1 ≤ i ≤ ndims(A)" |
| ARR-015 | vector at most one > 1 | §10.3.2 | "vector(A): at most one dimension size > 1" |
| ARR-016 | matrix trailing size 1 | §10.3.2 | "matrix(A): size(A, i) = 1 required for 2 < i ≤ ndims(A)" |
| ARR-017 | zeros/ones non-empty | §10.3.3 | "zeros/ones needs one or more arguments; zeros() is not legal" |
| ARR-018 | fill at least two args | §10.3.3 | "fill(s, n1, n2, ...) needs two or more arguments; fill(s) is not legal" |
| ARR-019 | linspace n >= 2 | §10.3.3 | "linspace(x1, x2, n): required that n ≥ 2" |
| ARR-020 | array type compatible | §10.4 | "All arguments of array() must be type compatible expressions" |
| ARR-021 | cat k existing dim | §10.4.2 | "cat(k, A, B, C): k must characterize existing dimension, 1 ≤ k ≤ ndims(A)" |
| ARR-022 | cat k parameter | §10.4.2 | "k shall be a parameter expression of Integer type" |
| ARR-023 | concat non-empty | §10.4.2.1 | "Concatenation syntax: there must be at least one argument ([] is not defined)" |
| ARR-024 | Index bounds assertion | §10.5 | "assert(1 <= i and i <= size(A, di), ...) is generated for index access" |
| ARR-025 | Index type match | §10.5.1 | "Type of index should correspond to type used for declaring dimension" |
| ARR-026 | Slice subscript limit | §10.6.9 | "Number of subscripts on m must not be greater than array dimension for m" |
| ARR-027 | Equality same dimensions | §10.6.1 | "Equality/assignment require both objects to have same number of dimensions and sizes" |
| ARR-028 | Equality type equivalent | §10.6.1 | "The operands need to be type equivalent" |
| ARR-029 | Add/sub same size | §10.6.2 | "Addition/subtraction require size(a) = size(b) and numeric type" |
| ARR-030 | Division by scalar | §10.6.5 | "Division a / s: a is numeric scalar/vector/matrix/array, s is numeric scalar" |
| ARR-031 | Power scalar Real | §10.6.7 | "a .^ b: required that a and b are scalar Real or Integer expressions" |
| ARR-032 | Power exceptional illegal | §10.6.7 | "Other exceptional situations are illegal" |
| ARR-033 | Matrix power square | §10.6.8 | "a ^ s: a is square numeric matrix, s is scalar Integer with s ≥ 0" |
| ARR-034 | Reduction iterator vector | §10.3.4.1 | "Expressions in iterators shall be vector expressions" |
| ARR-035 | Reduction event evaluable | §10.3.4 | "If event-generating, expressions inside iterators shall be evaluable" |
| ARR-036 | Empty array size req | §10.7 | "Size-requirements of operations must be fulfilled even if dimension is zero" |
| ARR-037 | cross/skew 3-vector | §10.3.5 | "cross(x,y) and skew(x) only defined for Real 3-vectors" |
| ARR-038 | transpose 2D minimum | §10.3.5 | "transpose(A): error if A does not have at least 2 dimensions" |
| ARR-039 | Reduction empty defaults | §10.3.4 | "Empty array: sum returns zeros, product returns 1, min/max return type extrema" |
| ARR-040 | min/max type restriction | §10.3.4 | "min/max require scalar enumeration, Boolean, Integer, or Real types" |
| ARR-041 | diagonal vector shape | §10.3.5 | "diagonal(v) requires a vector and returns a square matrix with both extents equal to size(v, 1)" |
| ARR-042 | outer product shape | §10.3.5 | "outerProduct(v1, v2) requires two vectors and returns a matrix with extents size(v1, 1) and size(v2, 1)" |
| ARR-043 | Record array equality element-wise | §10.6.1 | "Equality a=b ... of scalars, vectors, matrices, and arrays is defined element-wise". Each element pair of an equation between arrays of records is one whole-record equality, so a slice `s[1:n] = m[1:n].r`, a whole array `s = m.r` (Flat spells the member of a component array as the array of its element records), and a for-equation over elements state the same field equations. Rumoca: DAE record-equation analysis resolves each operand to its element record occurrences, proving the indexed declaration, the element subscript, and every projected field declaration, and owns the leaf equalities of every pair; a structured family whose rows are all such equalities is not a separate owner. A discrete-valued equation `{a1, ..., an} = e` over unsubscripted scalar discrete-valued variables (the component-array slice `split.set = fill(inPort.set, nBranches)` of `Modelica.StateGraph.Parallel`) is its element equations `ai = e[i]`, each defining its own variable. Likewise a for-equation over the fields of a component array (`inPort[i].occupied = ...` in `Modelica.StateGraph.Step`) is its materialized element-field assignments, each defining its own field. Tested in `suite_core/record_array_equation.rs` and `suite_core/discrete_array_element_equations.rs` |
| ARR-044 | der and pre element-wise shape | §3.7.4, §3.7.5 | "der(expr) ... If expr is an array, the operator is applied to all elements of the array" (§3.7.4; likewise `pre(y)`, §3.7.5), so the result has the operand shape and an equation between zero-sized operands has no scalar equations. Tested in `suite_core/record_array_equation.rs` |
| ARR-045 | zeros/ones are Integer arrays | §10.3.3 | "zeros(n1, n2, n3, ...) Returns the n1 x n2 x n3 x ... Integer array with all elements equal to zero"; `ones` likewise with one. Rumoca: the DAE types `zeros` and `ones` as Integer arrays, so `Integer jpvt[n] = zeros(n)` (the pivot vector of `Matrices.LAPACK.dgelsy_vec`) is well typed and a Real context converts them as any Integer expression. Tested in `arr_contracts.rs` |
| ARR-046 | size of an array expression | §10.3.1 | "size(A, i) Returns the size of dimension i of array expression A". Rumoca: with a translation-time dimension index, `size(e, i)` of any array expression is the Integer extent the checked type of `e` fixes, so `e` (a function call result, for example) is not evaluated for it. Tested in `suite_core/size_of_call_results.rs` |

### 4.11 Package/Import Contracts (PKG)

| ID | Contract | MLS | Requirement |
|----|----------|-----|-------------|
| PKG-001 | Unique import names | §13.2.2 | "Multiple qualified import-clauses shall not have the same import name" |
| PKG-002 | Import not inherited | §13.2.2 | "Import clauses are not inherited" |
| PKG-003 | Package-only imports | §13.2.2 | "One can only import from packages, not from other kinds of classes" |
| PKG-004 | MODELICAPATH search | §13.2.1 | "Top-level names lookup starts lexically before searching MODELICAPATH" |
| PKG-005 | Qualified import target | §13.2.1 | "Qualified import-clauses may only refer to packages or elements of packages" |
| PKG-006 | Directory package.mo | §13.4.1 | "Each directory shall contain a node, the file package.mo" |
| PKG-007 | No duplicate class names | §13.4.1 | "Two sub-entities shall not define classes with identical names" |
| PKG-008 | No dir and file conflict | §13.4.1 | "A directory shall not contain both sub-directory A and file A.mo" |
| PKG-009 | Within required | §13.4.3 | "A non-top-level entity shall begin with a within-clause" |
| PKG-010 | Within designates enclosing | §13.4.3 | "The within-clause shall designate the class of the enclosing entity" |
| PKG-011 | Import fully qualified | §13.2.2 | "An imported package or definition should always be referred to by its fully qualified name" |
| PKG-012 | Import not modifiable | §13.2.2 | "Import-clauses are not named elements and cannot be modified or redeclared"
| PKG-013 | Record constant members through packages | §7.1, §7.3, §12.6 | `Medium.data.R_s` reads field `R_s` of the record constant `data` the package selected by `Medium` exposes; the field declaration has no value of its own, the record constant's binding gives it one. Rumoca folds the member from the extracted field value or by projecting the constructor binding, following a binding that names another record constant (`extends Base(data = Data.H2O)`), both at model level and inside functions of the exposing package. Tested in `suite_core/record_constant_members.rs` |

### 4.12 Operator Record Contracts (OPREC)

| ID | Contract | MLS | Requirement |
|----|----------|-----|-------------|
| OPREC-001 | Encapsulated | §14 | "Operator or operator function must be encapsulated" |
| OPREC-002 | Single output | §14 | "All operator functions shall return exactly one output" |
| OPREC-003 | Record input | §14 | "Must have at least one component of record class as input (except constructor)" |
| OPREC-004 | Constructor output | §14 | "Constructor shall return one component of the operator record class" |
| OPREC-005 | No multiple matches | §14 | "For potential call, shall not exist multiple matches" |
| OPREC-006 | Binary defaults | §14 | "First two inputs no default values, rest must have defaults" |
| OPREC-007 | Binary no ambiguity | §14 | "Error if multiple functions match a binary operation" |
| OPREC-008 | Zero operator single | §14 | "'0' operator can only contain one function with zero inputs" |
| OPREC-009 | Constructor mutual exclusion | §14.3 | "For pair of operator record classes C and D, at most one of C.'constructor'(d) and D.'constructor'(c) shall be legal" |
| OPREC-010 | String operator output | §14.4 | "operator A.'String' shall only contain functions declaring one output of String type" |
| OPREC-011 | Zero inner dimension | §14.5 | "If inner dimension is zero for matrix*vector/matrix, uses '0' operator; error if '0' not defined"
| OPREC-012 | Operator resolution | §14.2–14.5 | An operator on an operator-record operand denotes a call of the matching operator function (MLS §14.5). Rumoca: flatten replaces each operator on an operator-record operand (`+`, binary and unary `-`, `*`, `/`, `^`, relations, `and`, `or`) by a call of the record's operator function whose first inputs accept the operands (a record input the same operator record, a Real input any numeric operand, an Integer input only an Integer; further inputs defaulted), searching both operands' records; two matches are an error (OPREC-007). With no match, a numeric operand is converted by the unique matching `'constructor'` function of the other record and matching repeats; a record vector operand no function accepts applies element by element for `+`, `-`, the element-wise operators, and a scalar factor or divisor, `v * w` of two record vectors is their scalar product (the chained `'+'` of the element products, or `'0'` for empty vectors; §14.4 with §10.6.4), and any other operator between two record vectors is left unresolved and refused; `sum` of an empty record vector is the record's `'0'` element. A record equation whose sides no record owner reads directly (`a + b = Complex(0, 0)`) is one scalar equation per field. A record-typed function output or input declared with field modifiers (`output Complex result(re = re, im = im)`) has the constructor call as its value, to which a nested field modification contributes only its binding (`re(min = -10) = a` passes `a`, and an attribute-only `im(start = 0)` passes nothing; MLS §7.2). An operand is typed from its declarations through every part of its reference (MLS §10.1): a record field read through an array of components (`plug.pin.v` of `Pin pin[m]`) is a record vector of the elements `plug.pin[k].v`, an element `phi[k]` of a Real or Integer array (a type such as `SI.Angle` taken to its predefined root) is a numeric operand, and an equation side `fill(c, n)` pairs `c` with every element of the other side. A record array comprehension passed to a record input (`real({v[k]*v[k] for k in 1:n})`) passes, per field, the comprehension of that field of its element (MLS §10.4.1), and a call inside a comprehension body is shaped with the comprehension indices bound. Resolution and function collection alternate until no new operator function is collected, at any depth. Tested in `suite_core/operator_record_overloads.rs` |

### 4.13 Simulation Contracts (SIM)

| ID | Contract | MLS | Requirement |
|----|----------|-----|-------------|
| SIM-001 | Event iteration | §8.6/App B | "Iterate solving equations until z == pre(z) and m == pre(m)". The activation buffer of an unclocked `when c` (§8.3.5.1: `b = c`, `edge(b) = b and not pre(b)`) is a `pre` lane: it holds while one pass settles and advances from the settled pass before the next one, and a changed buffer continues the iteration, so a body active at an event reads the values settled at that event and runs once. Tested in `suite_core/when_activation_settled_reads.rs`. Known difference from OpenModelica, not yet resolved: coupled `Modelica.Blocks.Sources.RadioButtonSource` instances whose `reset` inputs read each other settle to different outputs when two of them fire at the same instant; the equivalent equation form shows the same difference, so it belongs to event iteration rather than to algorithm `when` lowering (DAE-C25). |
| SIM-002 | Initialization fixed | §8.6 | "Continuous Real with fixed=true adds equation vc = startExpression" |
| SIM-003 | Parameter fixed default | §8.6 | "For parameters: fixed defaults to true" |
| SIM-004 | Variable fixed default | §8.6 | "For other variables: fixed defaults to false" |
| SIM-005 | Discrete-valued solved form | App B | "Discrete-valued variables must be solvable through sequence of assignments with no cyclic dependencies" |
| SIM-006 | Integer solved form | App B | "Solved variable must appear uniquely as term (no multiplicative factor) on either side" |
| SIM-007 | Non-Integer flip form | App B | "Non-Integer equations require at most flipping sides to obtain assignment form" |
| SIM-008 | Discrete variable stability | App B | "Values of conditions c, z, and m only changed at event instant, constant during continuous integration". Rumoca: a relation that reads `time` or an unclocked continuous-time coordinate (state, algebraic, input) owns a located crossing even when it also reads a value held between clock ticks, such as `t_i` written by `when sample(..)` (`Modelica.Electrical.Digital.Sources.DigitalClock`: `time >= t_i + t_width`); only a relation reading its clocked partition alone is evaluated on the ticks without a root (§16.1). Tested in `suite_core/held_sample_relations.rs` |
| SIM-009 | DAE structure | App B | "System shall consist of differential equations, discrete equations, discrete-valued assignments, and condition equations" |
| SIM-010 | Clocked event-iteration participation | App B | "Clocked variables use previous values, and a clock partition is solved once per tick, in the first event iteration of that tick"; that restriction bounds fixed-point RE-ITERATION of the partition, not value exchange between producers inside that single solution, and clocked lanes do not participate in ordinary z == pre(z), m == pre(m) convergence. Implemented: the first-iteration restriction and the excluded clocked lanes, plus equation-shaped same-tick exchange inside the partition — the SPEC_0040 SOLVE-C57 owner (SPEC_0046), implemented toward the SPEC_0046 SDO-001/SDO-002 read semantics, with credit gated on the SPEC_0046 gates and acceptance vote: construction issues the ordered producer list and the runtime replays it, so an ordinary same-instant read consumes this tick's value while only pre/previous/sample(u) consumes a history lane, and an inactive guarded producer's target still holds (SOLVE-C07/C10) because its held entry value *is* its `next`. A coupled simultaneous discrete residual SCC remains a typed rejection with the preregistered SPEC_0046 SDO-021/SDO-023 ResidualSccOwner successor |

### 4.14 Clock/Synchronous Contracts (CLK)

| ID | Contract | MLS | Requirement |
|----|----------|-----|-------------|
| CLK-001 | Clock not in function | §16.3 | "Clock constructors not allowed inside function classes" |
| CLK-002 | Clock prefix restrictions | §16.3 | "Clock variables cannot have flow, stream, discrete, parameter, or constant prefix" |
| CLK-003 | Clock switch evaluable | §16.3 | "Subscripts and conditions switching between clock variables must be evaluable expressions" |
| CLK-004 | No cross-subclock equations | §16.7.4 | "Systems of equations cannot involve different sub-clocks simultaneously" |
| CLK-005 | Unique clock association | §16.2.1 | "Every clocked variable associates uniquely with exactly one clock" |
| CLK-006 | Clocked access restriction | §16.2.1 | "Clocked variables can only be directly accessed when their associated clock is active" |
| CLK-007 | sample() continuous input | §16.5.1 | "sample() input must be continuous-time with no variability restrictions" |
| CLK-008 | hold() component input | §16.5.1 | "hold() input must be a component expression or parameter expression" |
| CLK-009 | previous() component input | §16.4 | "previous() input must be a component expression; parameter inputs return their value" |
| CLK-010 | superSample event clock | §16.5.2 | "superSample() cannot be applied to event clocks to create faster clocks" |
| CLK-011 | shiftSample resolution | §16.5.2 | "shiftSample() on event clocks cannot have resolution other than 1" |
| CLK-012 | backSample pre-start | §16.5.2 | "backSample() cannot create clock ticks before the base-clock starts" |
| CLK-013 | No derivative on clock ops | §16.5.2 | "Derivative on sample(), subSample(), superSample(), shiftSample(), backSample(), noClock() is forbidden" |
| CLK-014 | Clocked when no nest | §16.6 | "Clocked when-clauses cannot nest and have no elsewhen parts" |
| CLK-015 | Clocked when not in alg | §16.6 | "Clocked when-clauses cannot appear inside algorithms" |
| CLK-016 | Discretized partition | §16.8.1 | "Partitions containing der(), delay(), or when-clauses with Boolean conditions are discretized" |
| CLK-017 | Sampling factor range | §16.7.5 | "Accumulated sub- and supersampling factors in range 1 to 2⁶³ must be supported" |
| CLK-018 | Clock interval positive | §16.3 | "Clock(interval): interval must be strictly positive (interval > 0)" |
| CLK-019 | intervalCounter positive | §16.3 | "Clock(intervalCounter, resolution): clocked component expression intervalCounter must be > 0" |
| CLK-020 | Clock variable restrictions | §16.3 | "Clock variables cannot have prefixes flow, stream, discrete, parameter, or constant"

### 4.15 Stream Connector Contracts (STRM)

| ID | Contract | MLS | Requirement |
|----|----------|-----|-------------|
| STRM-001 | Stream prefix scope | §15.1 | "The stream prefix can only be used in a connector declaration" |
| STRM-002 | Exactly one flow | §15.1 | "A stream connector must have exactly one variable with the flow prefix" |
| STRM-003 | Flow scalar Real | §15.1 | "Flow variable shall be a scalar that is a subtype of Real" |
| STRM-004 | Outside connector equations | §15.1 | "For every outside connector, one equation is generated for every variable with stream prefix" |
| STRM-005 | Inside connector no equations | §15.1 | "For inside connectors, variables with stream prefix do not lead to connection equations" |
| STRM-006 | inStream stream only | §15.2 | "inStream(v) is only allowed on stream variables v" |
| STRM-007 | inStream vectorizable | §15.2 | "If argument of inStream is array, implicit equation system holds elementwise" |
| STRM-008 | inStream continuity | §15.2 | "Implementation must be continuous and differentiable given continuous inputs" |
| STRM-009 | No division by zero | §15.2 | "Division by zero can no longer occur; result is always well-defined" |
| STRM-010 | actualStream argument | §15.3 | "Only argument of actualStream needs to be a reference to a stream variable" |
| STRM-011 | Flow/stream same level | §15.1 | "Flow variable must exist at same level as stream variable in connector hierarchy" |
| STRM-012 | actualStream flow product | §15.3 | "The product of a flow variable and actualStream of a stream variable of the same connector is continuous, so a tool may treat it as smooth(0, ...)" (rumoca: the product owns no event; a standalone actualStream keeps its flow-reversal event) |
| STRM-013 | One-direction flow | §15.2 | A connector whose flow can only enter its own component (`m_flow.min >= 0` on an inside connector, `max <= 0` on an outside connector) never supplies its connection set, so its term vanishes from every peer's `inStream` mix; with no supplying peer left, `inStream(c.h_outflow)` is `c.h_outflow`, as for an unconnected connector. Rumoca reads literal `min`/`max` attributes of the flow variable. Tested in `suite_core/stream_one_direction_flow.rs` |

### 4.16 State Machine Contracts (SM)

| ID | Contract | MLS | Requirement |
|----|----------|-----|-------------|
| SM-001 | Same clock | §17.1 | "All state machine components must have the same clock" |
| SM-002 | Different priorities | §17.1 | "All transitions leaving one state must have different priorities" |
| SM-003 | Single initial state | §17.1 | "One and only one instance in each state machine must be marked as initial" |
| SM-004 | Priority minimum | §17.1 | "Priority ≥ 1 required" |
| SM-005 | Not in functions | §17.1 | "None of these operators allowed inside function classes (transition, initialState, activeState, etc.)" |
| SM-006 | Equation context only | §17.1 | "transition/initialState can only be used in equations, not in non-parameter if-equations or when-equations" |
| SM-007 | activeState instance check | §17.3.1 | "Error if the instance is not a state of a state machine" |
| SM-008 | Parallel assignment error | §17 | "Error if parallel state machines assign to same variable at same clock tick" |

### 4.16.1 Rumoca Phase 5 Clock, StateGraph, Stream, Media, and MultiBody Scope

This subsection records the current release-validation scope for the Phase 5
semantic representatives. It is not a claim of full MLS coverage for these
areas.

- Clocked/synchronous support currently includes the MSL subset exercised by
  `Modelica.Clocked.Examples.Elementary.RealSignals.Sample1`: scalar/vector
  `Clock`, `sample`, `hold`, `previous`, and basic sub/super/shift/back-sample
  lowering paths that reach Solve simulation. Full base-clock partition
  inference, cross-subclock validation, and all CLK contracts above remain the
  broader compliance target.
- Static clock schedule inference currently supports `Clock(period)`,
  `Clock(intervalCounter, resolution)`, aliases that resolve to those forms,
  and `shiftSample`/`backSample` compositions over statically scheduled base
  clocks. `Clock(condition)` is preserved as a dynamic event clock; unresolved
  or unsupported constructor forms must report `ED009` before simulation.
  A Clock defined by a parameter-selected `if` (equation or expression form)
  takes the branch the parameter values select (§16.7).
- Clock inference (§16.5.1, §16.5.2): `subSample(u)`/`superSample(u)` without
  `factor` take the exact integer ratio of the source and target partitions'
  clocks, proven per component instance; a `sample(u)` partition is owned
  through the conversions it takes part in before any fallback to a unique
  model clock; declaration bindings and clocked `when` bodies contribute
  conversions like equations, and a `when`-body `if` whose arms state a
  conversion is decided by its parameter values.
- Clocked values: `sample(u)` of a discrete `u` reads its left limit (the pre
  value); a vector `sample` defines clocked discrete Real elements; an MLS
  §12.4.3 multi-result call equation may define discrete receivers (one pure
  call per discrete owner); the condition of an `if` inside a clocked `when`
  body is a clocked value of the partition; `firstTick()` (§16.10) reads the
  `previous` of a generated clocked indicator of its partition clock; the
  scalar elements of a discrete Real array may each be defined by their own
  row (`y[1] = u[1]`, a connection `d.u = y[1]`), oriented like whole
  coordinates and issued as one producer per array coordinate.
- Functions (§3.7.2, §12): `div`/`mod`/`rem` of Integer operands in a
  function body are exact Integer quotients (truncating `div`, flooring `mod`)
  in the typed Solve program, and a function-body quotient needs no event owner
  whatever its operands' variability.
- Event clocks (§16.3 Operator 16.4): `Clock(condition, startInterval)` whose
  condition names a scalar Boolean coordinate ticks when `edge(pre(condition))`
  becomes true, once per rise: a condition that stays true through later event
  iterations or events does not tick it again. Its partition's rows are guarded
  updates on the tick, ordered over the whole partition so a row observes the
  tick's values of the rows it reads while `sample(u)` reads `u` at its left
  limit (§16.5.1), before the tick's own `hold` updates; the left limit is the
  event-entry value, so every pass of the event iteration that settles the
  tick reads the same sample;
  `previous` reads the value of the previous tick, and `interval()` (§16.10)
  is `startInterval` at the first tick and the time since the previous tick
  afterwards. `shiftSample(u, k)` of an event clock (§16.5.2, resolution 1) is
  the clock that skips its first `k` ticks and then ticks with it, for a
  `Clock` and for a clocked value alike. Any other clock conversion of an
  event clock, and a condition that is not a Boolean coordinate, are refused
  at construction.
- Clock arrays (a `ClockVectorInput`): each element of a clock array is a
  connection hub that carries the scalar clock connected to it; an element
  with no connected clock, or a clock array used other than through element
  connections, has no resolved schedule and is refused.
- Not yet supported: `Clock(c, solverMethod)` discretized
  partitions (§16.8.1), refused at construction when a clock-owned equation
  contains `der()` so a partition is never integrated continuously; external C noise generators with Integer state
  arrays (`Xorshift64star`).
- State-machine support currently covers library-style `Modelica.StateGraph`
  models that lower as ordinary discrete/event equations, with
  `Modelica.StateGraph.Examples.ExecutionPaths` as the OMC-backed
  representative. Full MLS section 17 state-machine operator semantics
  (`transition`, `initialState`, `activeState`, same-clock checks, parallel
  assignment checks) remain scoped by the SM contracts above.
- Stream/Fluid support is not claimed for the Phase 5 representative. The
  current `Modelica.Fluid.Examples.IncompressibleFluidNetwork` blocker is
  classified as `unsupported-feature:replaceable_media_package_lookup`, which
  fails before stream-equation parity can be validated.
- Media property/function support is partial. The representative
  `Modelica.Media.Examples.IdealGasH2O` reaches DAE/Solve lowering but currently
  fails simulation with `unsupported-feature:media_property_initialization_singularity`.
  Other explored Media examples expose constrained redeclare, partial-medium
  function-body, nonlinear-solver callback, and vector-slice gaps.
- Higher-index MultiBody support is not claimed for the Phase 5
  representatives. `Modelica.Mechanics.MultiBody.Examples.Elementary.Pendulum`
  and `DoublePendulum` are tracked as OMC-backed structural representatives but
  currently fail because an `outer` component reference inside a nested
  component is not bound/qualified through the matching enclosing `inner`
  component consistently enough for DAE function lowering. The stable
  classifier is `unsupported-feature:inner_outer_qualified_lookup`.

### 4.17 Annotation Contracts (ANN)

| ID | Contract | MLS | Requirement |
|----|----------|-----|-------------|
| ANN-001 | Forbidden constructs | §18.2 | "final, each, element-redeclaration, element-replaceable shall not be used in annotations" |
| ANN-002 | No empty modification | §18.2 | "element-modification without modification is deprecated" |
| ANN-003 | Unit expression valid | §18.5.2.2 | "Non-empty unit shall match unit-expression in chapter 19" |
| ANN-004 | Plot result scalar | §18.5.2.3 | "Error if x or y does not designate a scalar result" |
| ANN-005 | Derivative limit | §18.5.2.3 | "If x or y is der(v,n), then n must not exceed maximum differentiation applied to v" |
| ANN-006 | Unit compatibility | §18.5.2.3 | "Error if unit of x or y expression is incompatible with axis unit" |
| ANN-007 | Figure identifier unique | §18.5.2.1 | "Figure must be uniquely identified by its identifier" |
| ANN-008 | Evaluate scope | §18.6 | "Annotation Evaluate is only allowed for parameters and constants" |
| ANN-009 | Evaluate non-evaluable warn | §18.6 | "For non-evaluable parameter, Evaluate has no impact; issue warning" |
| ANN-010 | StopTime simulation model | §18.7 | "If StopTime is set in non-partial model, it is required to be a simulation model" |
| ANN-011 | mustBeConnected error | §18.8 | "Error if connector does not appear as inside connector in any connect-equation" |
| ANN-012 | mayOnlyConnectOnce error | §18.8 | "Error if connection set has more than two elements" |
| ANN-013 | Annotation placement | §18.1 | "Standard annotations shall only be used where their semantics is defined" |
| ANN-014 | TestCase restriction | §18.7 | "Class with TestCase annotation shall not be used in other models unless those also have TestCase" |
| ANN-015 | Extent coordinate order | §18.9.1.1 | "Coordinates of first point shall be less than coordinates of second point" |
| ANN-016 | Inline expansion | §18.3 | `Inline = true` and `LateInline = true` propose including the function body at each call; `InlineAfterIndexReduction = true` proposes it after the function is differentiated for index reduction and before other symbolic transformations. Rumoca (SPEC_0040 STRUCT-T10(b)): a pure, non-recursive Modelica body that is one straight-line result assignment is inlined with substituted arguments (a body with an assertion, relation, or event-generating operator is refused), `Inline`/`LateInline` before index reduction and `InlineAfterIndexReduction` after formal-derivative construction; a callee with a derivative annotation is never inlined before index reduction, and external, impure, recursive, or loop-bearing callees are refused |
| ANN-017 | Evaluate folding | §18.6 | `Evaluate = true` on an evaluable parameter proposes using its value during symbolic processing, after which the value cannot change. Rumoca (SPEC_0040 STRUCT-T10(a)): evaluable parameters declared `final` or `Evaluate = true`, and dependent parameters whose bindings read only such parameters, literals, and constants, are replaced by their values in one checked reconstruction and exported as constants; `Evaluate = false` outranks `final`, and ordinary independent parameters and unevaluable bindings are never folded (ANN-009). An assert condition that evaluates to `true` from its `final` and `Evaluate = true` parameters alone, whatever its time-varying operands and ordinary parameters are, is the literal `true` and owns no relation, so it raises no event (`m_flow > -m_flow_small or allowFlowReversal` under `allowFlowReversal = true`); a condition whose value depends on an ordinary parameter keeps its relation. Tested in `suite_core/evaluate_assert_fold.rs` |

### 4.18 Unit Expression Contracts (UNIT)

| ID | Contract | MLS | Requirement |
|----|----------|-----|-------------|
| UNIT-001 | Syntax match | §19.1 | "Unit string shall match the unit-expression rule" |
| UNIT-002 | No whitespace | §19.1 | "Modelica unit string syntax allows neither comments nor white-space" |
| UNIT-003 | No multiple divisors | §19.1 | "SI standard does not allow multiple units right of division-symbol (ambiguous)" |
| UNIT-004 | No exponent spacing | §19.1 | "There must be no spacing between unit-operand and possible unit-exponent" |
| UNIT-005 | No prefix spacing | §19.1 | "There must be no spacing between unit-symbol and possible unit-prefix" |
| UNIT-006 | Symbol first interpretation | §19.1 | "Unit-operands should first be interpreted as unit-symbol, only then as prefixed operand" |
| UNIT-007 | SI unit recognition | §19.1 | "Tools shall recognize basic and derived units of the SI system" |
| UNIT-008 | Non-SI unit recognition | §19.1 | "Tools shall recognize: minute, hour, day, liter, electronvolt, degree, debye" |
| UNIT-009 | Dot notation required | §19.1 | "Multiplication uses dot notation: 'N.m' for newton-meter, not 'Nm'"

---

## 5. Contract Summary by Category

| Category | Prefix | Count |
|----------|--------|-------|
| Lexical | LEX | 13 |
| Declarations | DECL | 37 |
| Instantiation | INST | 59 |
| Expressions | EXPR | 41 |
| Equations | EQN | 41 |
| Algorithms | ALG | 18 |
| Connections | CONN | 30 |
| Functions | FUNC | 43 |
| Types/Interfaces | TYPE | 36 |
| Arrays | ARR | 46 |
| Packages | PKG | 13 |
| Operator Records | OPREC | 12 |
| Simulation | SIM | 10 |
| Clocks/Synchronous | CLK | 20 |
| Stream Connectors | STRM | 13 |
| State Machines | SM | 8 |
| Annotations | ANN | 17 |
| Unit Expressions | UNIT | 9 |
| **Total** | | **466** |

---

## 6. Compiler Phases with Input/Output

| Phase | MLS Reference | Input | Output |
|-------|---------------|-------|--------|
| **Parsing** | §2, §13, App A | Source text, package structure | Class Tree (syntactic, modifications in-place) |
| **Instantiation** | §5.6, §7.2, §7.3 | Class Tree + root class name | Instance Tree (modifications merged, inheritance resolved) |
| **Flattening** | §5.3, §5.6, §8.3, §9 | Instance Tree | Flat Equation System (global names, connection equations, streams) |
| **DAE Formation** | §4.8, §11, App B | Flat Equation System | Hybrid DAE (variable classification, balance verified) |
| **Simulation** | §8.6, App B | Hybrid DAE + experiment settings | Time-series results |

### Phase Details

**Parsing** (§2, §13):
- Tokenizes source according to lexical rules
- Builds syntax tree per grammar (Appendix A)
- Loads packages from file system (§13.4)

**Instantiation** (§5.6):
- Expands class hierarchy via extends clauses
- Merges modifications (outer overrides inner)
- Resolves conditional components
- Creates instance tree nodes with applied modifications

**Flattening** (§5.6, §9):
- Resolves all name references to global paths
- Expands for-equations/if-equations to individual equations (note: array variables are preserved per SPEC_0007)
- Processes connect() to generate connection equations
- Handles stream connectors (§15), clocks (§16), state machines (§17)

**DAE Formation** (§4.8, App B):
- Classifies variables: p, x(t), y(t), z(tₑ), m(tₑ), c(tₑ)
- Verifies local and global balance (counting algorithm contributions)
- Lowers supported model algorithms to equations while preserving function
  algorithm bodies for codegen readability (per SPEC_0007)

**Simulation** (§8.6, App B):
- Solves initialization problem
- Integrates continuous equations
- Handles event iteration until convergence

### Rumoca Design Notes

The following design decisions extend MLS requirements for implementation:

- **SPEC_0007**: Array variables preserved with dimension metadata; scalarization deferred to backend-specific layers
- **SPEC_0007**: Model algorithms lower to equations when declarative; function
  algorithms are preserved for codegen readability

---

## 7. MLS Chapter Index

| Chapter | Topic | URL |
|---------|-------|-----|
| 1 | Introduction | [Link](https://specification.modelica.org/maint/3.7/introduction1.html) |
| 2 | Lexical Structure | [Link](https://specification.modelica.org/maint/3.7/lexical-structure.html) |
| 3 | Operators/Expressions | [Link](https://specification.modelica.org/maint/3.7/operators-and-expressions.html) |
| 4 | Classes/Declarations | [Link](https://specification.modelica.org/maint/3.7/class-predefined-types-and-declarations.html) |
| 5 | Scoping/Flattening | [Link](https://specification.modelica.org/maint/3.7/scoping-name-lookup-and-flattening.html) |
| 6 | Interfaces/Types | [Link](https://specification.modelica.org/maint/3.7/interface-or-type-relationships.html) |
| 7 | Inheritance/Modification | [Link](https://specification.modelica.org/maint/3.7/inheritance-modification-and-redeclaration.html) |
| 8 | Equations | [Link](https://specification.modelica.org/maint/3.7/equations.html) |
| 9 | Connectors/Connections | [Link](https://specification.modelica.org/maint/3.7/connectors-and-connections.html) |
| 10 | Arrays | [Link](https://specification.modelica.org/maint/3.7/arrays.html) |
| 11 | Algorithms | [Link](https://specification.modelica.org/maint/3.7/statements-and-algorithm-sections.html) |
| 12 | Functions | [Link](https://specification.modelica.org/maint/3.7/functions.html) |
| 13 | Packages | [Link](https://specification.modelica.org/maint/3.7/packages.html) |
| 14 | Operator Overloading | [Link](https://specification.modelica.org/maint/3.7/overloaded-operators.html) |
| 15 | Stream Connectors | [Link](https://specification.modelica.org/maint/3.7/stream-connectors.html) |
| 16 | Synchronous Elements | [Link](https://specification.modelica.org/maint/3.7/synchronous-language-elements.html) |
| 17 | State Machines | [Link](https://specification.modelica.org/maint/3.7/state-machines.html) |
| 18 | Annotations | [Link](https://specification.modelica.org/maint/3.7/annotations.html) |
| A | Concrete Syntax | [Link](https://specification.modelica.org/maint/3.7/modelica-concrete-syntax.html) |
| B | DAE Representation | [Link](https://specification.modelica.org/maint/3.7/modelica-dae-representation.html) |
| C | Stream Equations | [Link](https://specification.modelica.org/maint/3.7/derivation-of-stream-equations.html) |

---

## Document Summary

| Category | Count |
|----------|-------|
| Data Structures | 26 |
| Algorithmic Processes | 4 |
| Contract Categories | 18 |
| Total Contracts | 466 |
| MLS Chapters Referenced | 21 |
