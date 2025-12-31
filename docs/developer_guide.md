# Rumoca Developer Guide

This guide provides an in-depth tour of the rumoca codebase, explaining its architecture, design patterns, and best practices for contributors.

## Table of Contents

1. [Overview](#overview)
2. [Project Structure](#project-structure)
3. [The Compilation Pipeline](#the-compilation-pipeline)
4. [Core Data Structures](#core-data-structures)
5. [The Visitor Pattern](#the-visitor-pattern)
6. [Module Deep Dives](#module-deep-dives)
7. [Testing](#testing)
8. [Best Practices](#best-practices)
9. [Common Tasks](#common-tasks)
10. [Future Development Guidelines](#future-development-guidelines)

---

## Overview

Rumoca is a Modelica compiler written in Rust. It transforms Modelica source code into a Differential-Algebraic Equation (DAE) representation suitable for numerical simulation. The project also provides:

- **Code formatter** (`rumoca_fmt`)
- **Linter** (`rumoca_lint`)
- **Language Server Protocol implementation** (`rumoca_lsp`)
- **Python bindings** (optional)
- **WebAssembly bindings** (optional)

The architecture emphasizes:
- **Separation of concerns** - Parsing, analysis, transformation, and code generation are cleanly separated
- **Visitor pattern** - Extensive use for AST traversal
- **Performance** - Multi-level caching and parallel processing
- **Extensibility** - Plugin-friendly design for lint rules and code generators

---

## Project Structure

```
src/
├── lib.rs                  # Library entry point, public API exports
├── main.rs                 # CLI entry point
├── bin/                    # Additional binary entry points
│   ├── rumoca_fmt.rs       # Code formatter CLI
│   ├── rumoca_lint.rs      # Linter CLI
│   └── rumoca_lsp.rs       # LSP server
├── compiler/               # High-level compilation API
├── dae/                    # DAE representation and utilities
├── fmt/                    # Code formatter
├── ir/                     # Intermediate Representation
│   ├── ast.rs              # Core AST definitions
│   ├── visitor.rs          # Visitor pattern implementation
│   ├── analysis/           # Read-only analysis passes
│   ├── transform/          # Mutable transformation passes
│   └── structural/         # DAE creation and causalization
├── lint/                   # Linting framework
├── lsp/                    # Language Server Protocol
├── modelica_grammar/       # Parser and grammar conversion
├── python/                 # Python bindings (feature-gated)
└── wasm/                   # WASM bindings (target-gated for wasm32)
```

### Key Files to Know

| File | Purpose |
|------|---------|
| `src/lib.rs` | Public API - start here to understand what's exported |
| `src/compiler/pipeline.rs` | The compilation pipeline - the heart of the compiler |
| `src/ir/ast.rs` | Core AST types - the data structures everything operates on |
| `src/ir/visitor.rs` | Visitor traits - the pattern used throughout the codebase |
| `src/dae/ast.rs` | DAE structure - the output of compilation |

---

## The Compilation Pipeline

The compiler transforms Modelica source through a series of stages. Understanding this pipeline is essential for working on the codebase.

### Pipeline Stages

```
┌─────────────────────────────────────────────────────────────────────────┐
│                          PARSING PHASE                                  │
├─────────────────────────────────────────────────────────────────────────┤
│  Source Code → Parol Parser → IR AST (StoredDefinition)                │
│                                                                         │
│  Location: src/modelica_grammar/                                        │
│  Output: ir::ast::StoredDefinition                                      │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                       TRANSFORMATION PHASE                              │
├─────────────────────────────────────────────────────────────────────────┤
│  1. Import Resolution     - Resolve imports and packages                │
│  2. Flattening            - Flatten class hierarchy                     │
│  3. Array Expansion       - Expand array comprehensions                 │
│  4. Constant Substitution - Inline constant values                      │
│  5. Enum Substitution     - Replace enum references                     │
│  6. Function Inlining     - Inline function calls                       │
│  7. Operator Expansion    - Expand complex operators                    │
│  8. Tuple Expansion       - Expand tuple equations                      │
│  9. Equation Expansion    - Expand for-loops in equations               │
│                                                                         │
│  Location: src/ir/transform/                                            │
│  Uses: MutVisitor pattern                                               │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                        ANALYSIS PHASE                                   │
├─────────────────────────────────────────────────────────────────────────┤
│  • Type checking          - Verify type compatibility                   │
│  • Binding validation     - Check component bindings                    │
│  • Assertion checking     - Validate assert() arguments                 │
│  • Cardinality checking   - Validate cardinality() usage                │
│  • Array bounds checking  - Detect out-of-bounds access                 │
│  • Member access checking - Validate class member access                │
│                                                                         │
│  Location: src/ir/analysis/                                             │
│  Uses: Visitor pattern (immutable)                                      │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                      STRUCTURAL ANALYSIS                                │
├─────────────────────────────────────────────────────────────────────────┤
│  • DAE Creation           - Classify variables and equations            │
│  • Balance Checking       - Verify equation/unknown balance             │
│  • BLT Decomposition      - Block Lower Triangular ordering             │
│  • Causalization          - Determine computation order                 │
│  • Pantelides Algorithm   - Index reduction for DAEs                    │
│                                                                         │
│  Location: src/ir/structural/                                           │
│  Output: dae::ast::Dae                                                  │
└─────────────────────────────────────────────────────────────────────────┘
```

### Pipeline Entry Point

The pipeline is orchestrated in `src/compiler/pipeline.rs`. The main function is:

```rust
pub fn compile_class(
    class: &ClassDefinition,
    class_dict: &ClassDict,
    functions: &IndexMap<String, ClassDefinition>,
    // ...
) -> Result<CompilationResult>
```

This function calls each transformation and analysis pass in order.

### Understanding the Pipeline Code

When reading `pipeline.rs`, look for the sequential calls to transformations:

```rust
// Example of pipeline flow (simplified)
let class = flatten(&class, &class_dict)?;          // Flatten hierarchy
let class = expand_comprehensions(class)?;           // Expand arrays
let class = substitute_constants(class, &params)?;   // Inline constants
let class = inline_functions(class, &functions)?;    // Inline functions
// ... more transformations

let result = check_semantic(&class, ...)?;           // Analysis

let dae = create_dae(&class)?;                       // Create DAE
let balance = dae.check_balance()?;                  // Check balance
```

---

## Core Data Structures

### The IR AST (`src/ir/ast.rs`)

The Intermediate Representation AST is the central data structure. Key types:

#### StoredDefinition
The top-level container for a parsed file:
```rust
pub struct StoredDefinition {
    pub classes: Vec<ClassDefinition>,
    pub within: Option<Token>,
    pub comments: Vec<ParsedComment>,
}
```

#### ClassDefinition
Represents a model, block, connector, package, or function:
```rust
pub struct ClassDefinition {
    pub class_type: ClassType,           // Model, Block, Connector, etc.
    pub name: String,
    pub components: IndexMap<String, Component>,
    pub equations: Vec<Equation>,
    pub algorithms: Vec<Vec<Statement>>,
    pub classes: IndexMap<String, ClassDefinition>,  // Nested classes
    pub extends: Vec<Extends>,
    // ...
}
```

#### Expression
The union of all expression types:
```rust
pub enum Expression {
    Terminal { terminal_type: TerminalType, token: Token },
    Binary { lhs: Box<Expression>, op: OpBinary, rhs: Box<Expression> },
    Unary { op: OpUnary, rhs: Box<Expression> },
    FunctionCall { comp: ComponentReference, args: Vec<Expression> },
    ComponentReference(ComponentReference),
    Array { elements: Vec<Expression> },
    If { branches: Vec<(Expression, Expression)>, else_branch: Box<Expression> },
    // ...
}
```

#### Equation
Represents equation section content:
```rust
pub enum Equation {
    Simple { lhs: Expression, rhs: Expression },
    Connect { lhs: ComponentReference, rhs: ComponentReference },
    For { indices: Vec<ForIndex>, equations: Vec<Equation> },
    If { cond_blocks: Vec<ConditionBlock<Equation>>, else_block: Option<Vec<Equation>> },
    When(Vec<WhenBlock<Equation>>),
    FunctionCall { comp: ComponentReference, args: Vec<Expression> },
    Empty,
}
```

### The DAE Structure (`src/dae/ast.rs`)

The output of compilation, following Modelica specification Appendix B:

```rust
pub struct Dae {
    // Variables (classified by type)
    pub p: IndexMap<String, Component>,    // Parameters
    pub cp: IndexMap<String, Component>,   // Constant parameters
    pub x: IndexMap<String, Component>,    // Continuous states
    pub y: IndexMap<String, Component>,    // Algebraic variables
    pub u: IndexMap<String, Component>,    // Inputs
    pub z: IndexMap<String, Component>,    // Discrete variables
    pub m: IndexMap<String, Component>,    // Mode variables
    pub c: IndexMap<String, Component>,    // Conditions

    // Pre-event values
    pub pre_x: IndexMap<String, Component>,
    pub pre_z: IndexMap<String, Component>,
    pub pre_m: IndexMap<String, Component>,

    // Equations
    pub fx: Vec<Equation>,                 // Continuous equations
    pub fx_init: Vec<Equation>,            // Initial equations
    pub fz: Vec<Equation>,                 // Event update equations
    pub fm: Vec<Equation>,                 // Mode update equations
    pub fc: IndexMap<String, Expression>,  // Condition expressions
    pub fr: IndexMap<String, Statement>,   // Reset expressions
}
```

---

## The Visitor Pattern

The visitor pattern is fundamental to rumoca. It provides a clean way to traverse and transform the AST without modifying the AST types themselves.

### Two Visitor Traits

#### Immutable Visitor (`Visitor`)
For read-only analysis - collecting information, checking properties:

```rust
pub trait Visitor {
    // Called before visiting children
    fn enter_expression(&mut self, _node: &Expression) {}
    fn enter_equation(&mut self, _node: &Equation) {}
    fn enter_statement(&mut self, _node: &Statement) {}
    fn enter_class_definition(&mut self, _node: &ClassDefinition) {}
    fn enter_component(&mut self, _name: &str, _node: &Component) {}

    // Called after visiting children
    fn exit_expression(&mut self, _node: &Expression) {}
    fn exit_equation(&mut self, _node: &Equation) {}
    // ...
}
```

#### Mutable Visitor (`MutVisitor`)
For transformations - modifying the AST:

```rust
pub trait MutVisitor {
    fn visit_expression(&mut self, _node: &mut Expression) {}
    fn visit_equation(&mut self, _node: &mut Equation) {}
    fn visit_statement(&mut self, _node: &mut Statement) {}
    fn visit_class_definition(&mut self, _node: &mut ClassDefinition) {}
    fn visit_component(&mut self, _name: &str, _node: &mut Component) {}
}
```

### Using the Visitor Pattern

#### Example: Collecting All Variable References

```rust
use crate::ir::ast::{Expression, ComponentReference};
use crate::ir::visitor::Visitor;

struct VariableCollector {
    variables: Vec<String>,
}

impl Visitor for VariableCollector {
    fn enter_expression(&mut self, node: &Expression) {
        if let Expression::ComponentReference(comp_ref) = node {
            self.variables.push(comp_ref.to_string());
        }
    }
}

// Usage:
fn collect_variables(expr: &Expression) -> Vec<String> {
    let mut collector = VariableCollector { variables: Vec::new() };
    expr.accept(&mut collector);  // Uses the Visitable trait
    collector.variables
}
```

#### Example: Transforming Expressions

```rust
use crate::ir::ast::Expression;
use crate::ir::visitor::MutVisitor;

struct ConstantFolder {
    params: HashMap<String, f64>,
}

impl MutVisitor for ConstantFolder {
    fn visit_expression(&mut self, node: &mut Expression) {
        // First, recursively visit children
        node.accept_mut(self);

        // Then, try to fold this node
        if let Expression::ComponentReference(comp_ref) = node {
            if let Some(&value) = self.params.get(&comp_ref.to_string()) {
                *node = Expression::Terminal {
                    terminal_type: TerminalType::UnsignedReal,
                    token: Token::new(&value.to_string()),
                };
            }
        }
    }
}
```

### When to Use Which Visitor

| Use Case | Visitor Type | Examples |
|----------|--------------|----------|
| Collecting information | `Visitor` | Symbol collection, type inference |
| Validation/checking | `Visitor` | Type checking, semantic analysis |
| Finding occurrences | `Visitor` | Reference finding, call hierarchy |
| Transforming AST | `MutVisitor` | Constant substitution, flattening |
| Renaming/replacing | `MutVisitor` | Scope resolution, import resolution |

### Visitor Best Practices

1. **Prefer visitors over manual recursion** - The visitor handles traversal order correctly
2. **Use `enter_*` for pre-order, `exit_*` for post-order** - Choose based on when you need to process
3. **Store state in the visitor struct** - Keep context like scope depth, collected items
4. **Call `accept`/`accept_mut` explicitly for children** in MutVisitor if you need control
5. **Compose visitors** - Create multiple focused visitors rather than one large one

---

## Module Deep Dives

### Compiler Module (`src/compiler/`)

The high-level API uses the builder pattern:

```rust
let result = Compiler::new()
    .model("Modelica.Blocks.Math.Gain")
    .include("/path/to/MSL")
    .verbose(true)
    .compile()?;
```

Key components:
- `mod.rs` - The `Compiler` struct with builder methods
- `pipeline.rs` - The actual compilation logic
- `cache.rs` - AST disk caching to `~/.cache/rumoca/ast/`
- `result.rs` - `CompilationResult` with timing and diagnostics
- `function_collector.rs` - Collects function definitions for inlining

### Flattening (`src/ir/transform/flatten/`)

Flattening converts hierarchical models into a single flat class. This is one of the most complex transformations:

```
src/ir/transform/flatten/
├── mod.rs           # Public API, main flatten() function
├── cache.rs         # Class definition caching
├── hash.rs          # File dependency tracking
├── class_dict.rs    # Class dictionary building
├── expansion.rs     # Component expansion logic
├── connections.rs   # Connect equation handling
├── imports.rs       # Import resolution
├── validation.rs    # Subscript/cardinality validation
├── helpers.rs       # Utility functions
└── tests.rs         # Unit tests
```

The flattening process:
1. Build a class dictionary from all parsed files
2. Resolve the target class (including extends)
3. Recursively expand nested components
4. Rename sub-component variables with dot notation
5. Expand connect equations into equality/flow equations

### Type Checking (`src/ir/analysis/semantic/`)

Type checking is split into focused modules:

```
src/ir/analysis/semantic/
├── mod.rs           # Public API, TypeError, TypeCheckResult
├── types.rs         # Type helper functions
├── equations.rs     # Equation/statement type checking
├── bindings.rs      # Component binding validation
├── assertions.rs    # Assert function argument checking
├── attributes.rs    # Builtin attribute validation
├── cardinality.rs   # Cardinality function validation
├── member_access.rs # Class member access checking
├── algorithms.rs    # Algorithm section checking
└── validation.rs    # General validation (bounds, subscripts)
```

### LSP Module (`src/lsp/`)

The Language Server Protocol implementation provides IDE features:

```
src/lsp/
├── mod.rs           # Main LSP server loop
├── workspace.rs     # Multi-file workspace state
├── handlers/        # LSP request handlers
│   ├── semantic_tokens.rs
│   ├── references.rs
│   ├── rename.rs
│   ├── goto_definition.rs
│   ├── hover.rs
│   ├── completion.rs
│   └── ...
├── features/        # Advanced features
│   ├── inlay_hints.rs
│   ├── code_action.rs
│   └── ...
├── analyze.rs       # Cross-file analysis
├── utils.rs         # Common utilities
└── data.rs          # Built-in function database
```

Many handlers use the visitor pattern to traverse the AST and collect information.

---

## Testing

### Test Organization

```
tests/
├── common/
│   └── mod.rs            # Shared test utilities
├── fixtures/             # Test .mo files
│   ├── integrator.mo
│   ├── bouncing_ball.mo
│   └── ...
├── parser_tests.rs       # Parsing tests
├── flatten_tests.rs      # Flattening tests
├── dae_tests.rs          # DAE creation tests
├── balance_tests.rs      # Balance checking tests
├── cache_tests.rs        # Caching tests
├── lsp_tests.rs          # LSP feature tests
└── msl_tests.rs          # MSL integration tests
```

### Writing Tests

Use the test utilities in `tests/common/mod.rs`:

```rust
use crate::common::{compile_fixture, compile_source};

#[test]
fn test_my_feature() {
    // Compile a fixture file
    let result = compile_fixture("my_model.mo", "MyModel").unwrap();
    assert!(result.dae.x.contains_key("x"));

    // Or compile inline source
    let result = compile_source(r#"
        model Test
            Real x;
        equation
            der(x) = 1;
        end Test;
    "#, "Test").unwrap();
}
```

### Running Tests

```bash
# Run all tests
cargo test

# Run specific test file
cargo test --test parser_tests

# Run with output
cargo test -- --nocapture

# Run ignored (slow) tests
cargo test --release -- --ignored

# Run MSL balance tests (requires MSL)
cargo test --release test_msl_balance_all -- --ignored --nocapture
```

---

## Best Practices

### 1. Use the Visitor Pattern

**Do this:**
```rust
struct MyAnalyzer { /* state */ }

impl Visitor for MyAnalyzer {
    fn enter_expression(&mut self, node: &Expression) {
        // Analyze expression
    }
}

fn analyze(class: &ClassDefinition) -> AnalysisResult {
    let mut analyzer = MyAnalyzer::new();
    class.accept(&mut analyzer);
    analyzer.result()
}
```

**Avoid this:**
```rust
fn analyze_expr(expr: &Expression) {
    match expr {
        Expression::Binary { lhs, rhs, .. } => {
            analyze_expr(lhs);  // Manual recursion
            analyze_expr(rhs);
        }
        // ... many more cases
    }
}
```

### 2. Prefer Focused Modules

Split large files into focused submodules. Aim for ~500 lines per file:

```
# Good
semantic/
├── mod.rs       (140 lines)
├── bindings.rs  (180 lines)
├── equations.rs (286 lines)
└── ...

# Avoid
semantic.rs  (2800 lines)
```

### 3. Clone Optimization

Format error messages before moving values:

```rust
// Good - format first, then move
let message = format!("Type mismatch: {} vs {}", expected, actual);
TypeError::new(location, expected, actual, message)

// Avoid - cloning to format
TypeError::new(
    location,
    expected.clone(),
    actual.clone(),
    format!("Type mismatch: {} vs {}", expected, actual),
)
```

### 4. Use IndexMap for Ordered Collections

When iteration order matters (which it usually does for determinism):

```rust
use indexmap::IndexMap;

// Good - preserves insertion order
let components: IndexMap<String, Component> = IndexMap::new();

// Avoid for ordered data
let components: HashMap<String, Component> = HashMap::new();
```

### 5. Error Handling

Use `anyhow` for error propagation with context:

```rust
use anyhow::{Context, Result};

fn parse_file(path: &Path) -> Result<StoredDefinition> {
    let source = std::fs::read_to_string(path)
        .with_context(|| format!("Failed to read {}", path.display()))?;
    parse_source(&source)
        .with_context(|| format!("Failed to parse {}", path.display()))
}
```

### 6. Feature Flags

Use feature flags for optional functionality:

```rust
// In Cargo.toml
[features]
default = []
lsp = []
python = ["pyo3"]
wasm = ["wasm-bindgen"]

// In code
#[cfg(feature = "lsp")]
pub mod lsp;
```

---

## Common Tasks

### Adding a New Lint Rule

1. Create the rule in `src/lint/rules/`:
```rust
pub fn check_my_rule(class: &ClassDefinition) -> Vec<LintWarning> {
    let mut warnings = Vec::new();
    // Use visitor pattern to find issues
    warnings
}
```

2. Register it in `src/lint/mod.rs`

### Adding a New Semantic Check

1. Create the check in `src/ir/analysis/semantic/`:
```rust
pub fn check_my_feature(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();
    // Use visitor pattern
    result
}
```

2. Export from `mod.rs`
3. Call from `src/ir/analysis/check_visitor.rs`

### Adding a New Transformation

1. Create in `src/ir/transform/`:
```rust
struct MyTransformer { /* state */ }

impl MutVisitor for MyTransformer {
    fn visit_expression(&mut self, node: &mut Expression) {
        node.accept_mut(self);  // Visit children first
        // Then transform this node
    }
}

pub fn my_transform(class: &mut ClassDefinition) {
    let mut transformer = MyTransformer::new();
    class.accept_mut(&mut transformer);
}
```

2. Add to pipeline in `src/compiler/pipeline.rs`

### Adding a New LSP Feature

1. Create handler in `src/lsp/handlers/` or `src/lsp/features/`
2. Use visitor pattern to collect information from AST
3. Register in `src/lsp/mod.rs`

---

## Future Development Guidelines

### Architectural Principles

1. **Keep analysis and transformation separate** - Analysis should not modify the AST
2. **Prefer composition over inheritance** - Use traits and composition
3. **Make illegal states unrepresentable** - Use enums and strong typing
4. **Fail fast with good errors** - Validate early, provide context

### Performance Considerations

1. **Use caching strategically** - AST cache, DAE cache, class definition cache
2. **Avoid unnecessary cloning** - Use references and Cow where possible
3. **Parallelize at the file level** - Use rayon for multi-file processing
4. **Profile before optimizing** - Use the timing instrumentation in pipeline

### Code Style

1. **Document public APIs** - Use `///` doc comments
2. **Keep functions focused** - Single responsibility
3. **Use meaningful names** - Prefer clarity over brevity
4. **Follow Rust idioms** - Use `?` for error handling, iterators over loops

### Adding New Features

Before implementing:
1. Check if a visitor-based approach would work
2. Consider impact on caching
3. Add tests alongside implementation
4. Update this documentation if needed

### Testing New Features

1. Write unit tests for the feature
2. Add integration tests with fixture files
3. Test with MSL models if applicable
4. Check performance impact with timing

---

## Quick Reference

### Important Traits

| Trait | Location | Purpose |
|-------|----------|---------|
| `Visitor` | `ir/visitor.rs` | Immutable AST traversal |
| `MutVisitor` | `ir/visitor.rs` | Mutable AST traversal |
| `Visitable` | `ir/visitor.rs` | Accept immutable visitor |
| `MutVisitable` | `ir/visitor.rs` | Accept mutable visitor |
| `SymbolInfo` | `ir/analysis/symbol_trait.rs` | Unified symbol interface |

### Important Functions

| Function | Location | Purpose |
|----------|----------|---------|
| `flatten()` | `ir/transform/flatten/mod.rs` | Flatten class hierarchy |
| `create_dae()` | `ir/structural/create_dae.rs` | Create DAE from flat class |
| `check_semantic()` | `ir/analysis/check_visitor.rs` | Run all semantic checks |
| `parse_source()` | `modelica_grammar/mod.rs` | Parse Modelica source |

### Directory Quick Reference

| Need to... | Look in... |
|------------|------------|
| Parse Modelica | `src/modelica_grammar/` |
| Add an analysis | `src/ir/analysis/` |
| Add a transformation | `src/ir/transform/` |
| Modify DAE creation | `src/ir/structural/` |
| Add a lint rule | `src/lint/rules/` |
| Add LSP feature | `src/lsp/handlers/` or `src/lsp/features/` |
| Modify compilation | `src/compiler/pipeline.rs` |

---

## Getting Help

- **GitHub Issues**: Report bugs and request features
- **Code Comments**: Many modules have detailed `//!` doc comments
- **Tests**: Look at tests for usage examples
- **This Guide**: Keep updated as the codebase evolves
