# SPEC_0027: Compilation Phase Responsibilities

> Archived legacy document. Kept for historical context only.
> This specification is not part of the active compliance set.

**Status:** ARCHIVED (legacy; originally ACTIVE)
**Created:** 2026-02-02
**Updated:** 2026-02-03
**Related:** SPEC_0004 (Instantiate/Flatten), SPEC_0023 (Crate Architecture)

## 1. Overview

This specification defines the responsibilities of each compilation phase in Rumoca. The compiler transforms Modelica source code into a Differential-Algebraic Equation (DAE) system through six sequential phases.

### 1.1 Pipeline

```
Source → Parse → Resolve → Instantiate → Typecheck → Flatten → ToDae → DAE
```

### 1.2 Design Rationale

Rumoca's phase order follows from the Modelica Language Specification and standard multi-pass compiler design:

| Rumoca Phase | MLS Concept |
|--------------|-------------|
| Parse | Lexical/syntactic analysis (MLS §2, §13) |
| Resolve | Name lookup, scoping (MLS §5) |
| Instantiate | Class instantiation (MLS §5.6) |
| Typecheck | Type checking, dimension evaluation (MLS §10.1) |
| Flatten | Flattening to equations (MLS §5.6) |
| ToDae | DAE formation (MLS Appendix B) |

**Key architectural decision:** Type checking runs AFTER instantiation. This ensures type checking has access to the full modification context, enabling proper evaluation of dimension expressions that depend on modifier values (MLS §10.1).

---

## 2. Phase Specifications

### 2.1 Parse Phase

**Crate:** `rumoca-phase-parse`
**Input:** Modelica source text (`&str`)
**Output:** `StoredDefinition` (AST)

#### 2.1.1 Responsibilities

The parse phase SHALL:

1. Perform lexical analysis according to MLS Appendix B grammar
2. Perform syntactic analysis to build an Abstract Syntax Tree
3. Preserve source locations on all AST nodes for error reporting
4. Report syntax errors with location information
5. Handle Unicode identifiers per MLS §2.2.5

#### 2.1.2 MLS Compliance

- MLS §2: Lexical structure
- MLS Appendix B: Modelica grammar

---

### 2.2 Resolve Phase

**Crate:** `rumoca-phase-resolve`
**Input:** `ParsedTree`
**Output:** `ResolvedTree`

#### 2.2.1 Responsibilities

The resolve phase SHALL:

1. Build the scope tree for name lookup
2. Resolve all imports (qualified, unqualified, and renaming)
3. Populate `def_id` on all name references
4. Populate `type_def_id` on component declarations
5. Handle `within` clauses for package scoping
6. Report undefined name errors

#### 2.2.2 MLS Compliance

- MLS §5.1: Name lookup rules (lexically enclosing scope first)
- MLS §5.3: Import handling
- MLS §5.6: Global name lookup with `.<name>`

---

### 2.3 Instantiate Phase

**Crate:** `rumoca-phase-instantiate`
**Input:** `ResolvedTree`, model name
**Output:** `InstancedTree` (contains `InstanceOverlay`)

#### 2.3.1 Responsibilities

The instantiate phase SHALL:

1. Create the instance tree by recursive class instantiation
2. Process `extends` clauses to merge inherited components
3. Apply all modifications (binding, element, redeclaration)
4. Build qualified names for all component instances
5. Handle `inner`/`outer` component references
6. Detect missing `inner` declarations and report `NeedsInner`
7. Merge equations from all extended classes

#### 2.3.2 Output Structure

The `InstanceOverlay` SHALL contain:
- `InstanceData`: Per-component data (qualified name, binding, dimensions expression)
- `ClassInstanceData`: Equations from each instantiated class
- All modifications applied with final binding values

#### 2.3.3 MLS Compliance

- MLS §5.4: Inner/outer semantics
- MLS §5.6: Instance hierarchy construction
- MLS §7.2: Modification semantics

---

### 2.4 Typecheck Phase

**Crate:** `rumoca-phase-typecheck`
**Input:** `InstancedTree` (tree reference + mutable overlay)
**Output:** `InstancedTree` (with dimensions evaluated)

#### 2.4.1 Responsibilities

The typecheck phase SHALL:

1. Resolve type specifiers to `TypeId` values
2. Populate `type_id` on component declarations
3. Evaluate dimension expressions to concrete integer values
4. Infer colon (`:`) dimensions from binding expressions
5. Mark structural parameters (those affecting array sizes)
6. Validate variability constraints per MLS §4.5
7. Validate causality constraints per MLS §4.6
8. Build evaluation context from overlay parameter values

#### 2.4.2 Dimension Evaluation

For each component with unevaluated dimensions, the typecheck phase SHALL:

1. Build an evaluation context from known parameter values in the overlay
2. Attempt to evaluate each dimension expression to an integer
3. For colon dimensions, infer size from binding expression if available
4. Store evaluated dimensions in `InstanceData.dims`
5. Update evaluation context with newly computed array dimensions

#### 2.4.3 MLS Compliance

- MLS §4.4: Component declarations (dimension evaluation)
- MLS §4.5: Variability (constant < parameter < discrete < continuous)
- MLS §4.6: Causality (input/output validation)
- MLS §10.1: Array dimension requirements
- MLS §18.3: Structural parameter identification

---

### 2.5 Flatten Phase

**Crate:** `rumoca-phase-flatten`
**Input:** `InstancedTree`
**Output:** `Model`

#### 2.5.1 Responsibilities

The flatten phase SHALL:

1. Generate globally unique flat variable names (`a.b.c` format)
2. Create `Variable` instances with dimensions and bindings
3. Process connections per MLS §9
4. Expand for-equations to concrete equation sets
5. Handle if-equations (select branch if condition evaluable)
6. Apply bindings as equations where appropriate
7. Collect function definitions for constant evaluation
8. Validate connector compatibility for connections

#### 2.5.2 For-Equation Expansion

For each for-equation with range `start:step:end`, the flatten phase SHALL:

1. Evaluate `start`, `step`, and `end` to integer values
2. Generate one equation instance for each index value
3. Substitute the loop variable in each generated equation
4. Report error if range bounds cannot be evaluated

#### 2.5.3 Connection Processing

For each connection statement `connect(a, b)`, the flatten phase SHALL:

1. Expand connector references to primitive components
2. Validate flow prefix consistency (both flow or both non-flow)
3. Validate type compatibility
4. Validate array dimension compatibility
5. Generate appropriate equations (equality for potential, sum for flow)

#### 2.5.4 MLS Compliance

- MLS §5.6: Flat name generation
- MLS §8.3: For-equation semantics
- MLS §8.4: If-equation semantics
- MLS §9: Connection semantics

---

### 2.6 ToDae Phase

**Crate:** `rumoca-phase-dae`
**Input:** `Model`
**Output:** `Dae`

#### 2.6.1 Responsibilities

The ToDae phase SHALL:

1. Classify variables (states, algebraics, parameters, constants)
2. Classify equations (ODE, algebraic, output)
3. Identify `der()` relationships between variables
4. Compute model balance (equation count vs unknown count)
5. Extract algorithm sections
6. Extract when-clause event handlers

#### 2.6.2 Variable Classification

Variables SHALL be classified as:
- **State**: Variables appearing in `der(x)` expressions
- **Algebraic**: Variables solved by algebraic equations
- **Parameter**: Variables with `parameter` prefix
- **Constant**: Variables with `constant` prefix
- **Input**: Variables with `input` prefix
- **Output**: Variables with `output` prefix

#### 2.6.3 MLS Compliance

- MLS §4.7: Balanced model requirements

---

## 3. Data Flow

### 3.1 Tree Wrapper Types

| Type | Description | Phase Output |
|------|-------------|--------------|
| `ParsedTree` | Raw AST from parser | Parse |
| `ResolvedTree` | AST with name resolution complete | Resolve |
| `InstancedTree` | Tree + InstanceOverlay | Instantiate |
| `Model` | Flat variables and equations | Flatten |
| `Dae` | Classified DAE system | ToDae |

### 3.2 Key Data Structures

**`InstanceOverlay`** contains:
- `components: HashMap<DefId, InstanceData>` - Per-component instance data
- `classes: HashMap<DefId, ClassInstanceData>` - Per-class equation data

**`InstanceData`** contains:
- `qualified_name: QualifiedName` - Full hierarchical path
- `binding: Option<Expression>` - Binding expression
- `dims: Vec<i64>` - Evaluated dimensions
- `dims_expr: Vec<Subscript>` - Unevaluated dimension expressions
- `start: Option<Expression>` - Start value

---

## 4. Error Handling

### 4.1 Error Codes by Phase

| Phase | Code Prefix | Example |
|-------|------------|---------|
| Parse | EP | EP001: Syntax error |
| Resolve | ER | ER001: Undefined name |
| Instantiate | EI | EI001: Missing inner |
| Typecheck | ET | ET001: Type mismatch |
| Flatten | EF | EF001: Unsupported equation |
| ToDae | ED | ED001: Unbalanced model |

### 4.2 Phase Failure Modes

Each phase SHALL report failures with:
- Error code
- Human-readable message
- Source location (span)
- Suggested remediation where applicable

---

## 5. References

- Modelica Language Specification 3.7 Development (MLS)
- Cooper & Torczon, *Engineering a Compiler*: Multi-pass IR design
