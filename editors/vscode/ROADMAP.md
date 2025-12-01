# Rumoca VS Code Extension Roadmap

This roadmap outlines the features planned to make the Rumoca Modelica extension a first-class IDE experience comparable to Python with Pylance.

## Current Status (v0.7.1)

### Implemented Features
- [x] Syntax highlighting (TextMate grammar)
- [x] Real-time diagnostics (parse errors, compilation errors)
- [x] Code completion (keywords, built-in functions, scoped variables)
- [x] Signature help (function parameter hints)
- [x] Hover information (type info, documentation)
- [x] Go to definition (variables, classes, functions)
- [x] Dot-completion (component members, type attributes)

---

## Phase 1: Foundation (v0.8.0)

Essential features that provide the backbone for advanced IDE functionality.

### 1.1 Document Symbols / File Outline
**Priority: High** | **Complexity: Low** | **Est: 4-6 hours**

Enable the file outline view in VS Code showing:
- Classes (model, block, connector, record, package, function)
- Components (variables, parameters, constants)
- Equations and algorithm sections
- Nested class hierarchy

```
MyModel
├── Parameters
│   ├── m: Real
│   └── g: Real
├── Variables
│   ├── x: Real
│   └── v: Real
├── Functions
│   └── helper()
└── Equations (3)
```

**LSP Method:** `textDocument/documentSymbol`

### 1.2 Semantic Tokens (Semantic Highlighting)
**Priority: High** | **Complexity: Medium** | **Est: 8-12 hours**

Provide rich syntax highlighting based on semantic analysis:

| Token Type | Example | Color (typical) |
|------------|---------|-----------------|
| `parameter` | `parameter Real m` | Blue |
| `variable` | `Real x` | Default |
| `state` | Variables with `der()` | Green |
| `input` | `input Real u` | Orange |
| `output` | `output Real y` | Purple |
| `constant` | `constant Real pi` | Cyan |
| `function` | `sin`, `cos`, user functions | Yellow |
| `type` | `Real`, `Integer`, user types | Teal |
| `class` | `model`, `block`, etc. | Bold |

**LSP Method:** `textDocument/semanticTokens/full`

### 1.3 Enhanced Diagnostics
**Priority: High** | **Complexity: Medium** | **Est: 10-15 hours**

Expand error detection beyond parse errors:

- [ ] Type mismatch in equations (`Real = Integer`)
- [ ] Array dimension mismatch (`Real[3] = Real[2]`)
- [ ] Undefined variable references
- [ ] Unused variable warnings
- [ ] Parameter without default value warnings
- [ ] Causality violations (input assigned in equation)
- [ ] Connection type mismatches
- [ ] Missing `end` statements

**Severity Levels:**
- Error: Type mismatches, undefined variables
- Warning: Unused variables, missing defaults
- Info: Style suggestions

---

## Phase 2: Navigation (v0.9.0)

Features that enable efficient code navigation across files.

### 2.1 Find All References
**Priority: High** | **Complexity: Medium** | **Est: 10-15 hours**

Find all usages of a symbol across the workspace:
- Variable references in equations
- Type usages in component declarations
- Function calls
- Class instantiations
- Import statements

**LSP Method:** `textDocument/references`

### 2.2 Go to Type Definition
**Priority: Medium** | **Complexity: Low** | **Est: 4-6 hours**

Navigate from a component to its type definition:
- `Motor motor` → jump to `model Motor`
- `MyConnector c` → jump to `connector MyConnector`
- Works across files with imports

**LSP Method:** `textDocument/typeDefinition`

### 2.3 Workspace Symbol Search
**Priority: High** | **Complexity: Medium** | **Est: 8-12 hours**

Global symbol search (`Ctrl+T` / `Cmd+T`):
- Search all models, functions, connectors in workspace
- Fuzzy matching support
- Show symbol kind and location
- Cross-package navigation

**LSP Method:** `workspace/symbol`

### 2.4 Call Hierarchy
**Priority: Medium** | **Complexity: Medium** | **Est: 8-10 hours**

View incoming/outgoing calls for functions:
- **Incoming:** Who calls this function?
- **Outgoing:** What does this function call?
- Useful for understanding function dependencies

**LSP Method:** `textDocument/prepareCallHierarchy`, `callHierarchy/incomingCalls`, `callHierarchy/outgoingCalls`

---

## Phase 3: Modelica-Specific Intelligence (v0.10.0)

Features unique to Modelica that leverage DAE analysis.

### 3.1 Code Lens - Balance Information
**Priority: High** | **Complexity: Low** | **Est: 4-6 hours**

Show equation/variable balance inline:

```modelica
model Motor  // [3 equations, 2 states, 1 algebraic - BALANCED]
  Real x;
  Real v;
equation
  der(x) = v;
  der(v) = -x;
  v = time;
end Motor;
```

Display:
- Number of equations
- Number of unknowns (states + algebraic)
- Balance status (balanced/over-determined/under-determined)
- Click to see detailed breakdown

**LSP Method:** `textDocument/codeLens`

### 3.2 Inlay Hints
**Priority: Medium** | **Complexity: Medium** | **Est: 6-8 hours**

Show inline type and dimension information:

```modelica
// Without hints:
Real x;
Real[3] pos;
y = sin(x);

// With hints:
Real x;                    // : Real (state)
Real[3] pos;               // : Real[3]
y = sin(x: Real): Real;    // parameter names in calls
```

Hint types:
- Variable type annotations
- Array dimensions
- Parameter names in function calls
- Inferred units (if available)

**LSP Method:** `textDocument/inlayHint`

### 3.3 Connection Diagram Support (Future)
**Priority: Low** | **Complexity: High** | **Est: 40+ hours**

Visual representation of `connect()` equations:
- Show component connections graphically
- Click to navigate to connect statement
- Integration with VS Code webview

*Note: This is a stretch goal requiring significant webview development.*

---

## Phase 4: Refactoring (v0.11.0)

Safe code transformation features.

### 4.1 Rename Symbol
**Priority: High** | **Complexity: High** | **Est: 15-20 hours**

Rename variables, classes, and functions across workspace:
- Preview changes before applying
- Update all references
- Handle qualified names (`Package.Model`)
- Validate new name doesn't conflict

**LSP Methods:** `textDocument/prepareRename`, `textDocument/rename`

### 4.2 Code Actions / Quick Fixes
**Priority: Medium** | **Complexity: Medium** | **Est: 12-15 hours**

Automated fixes for common issues:

| Diagnostic | Quick Fix |
|------------|-----------|
| Undefined variable | "Declare variable as Real" |
| Missing import | "Add import statement" |
| Unused variable | "Remove variable" / "Prefix with _" |
| Missing parameter default | "Add default value" |
| Unbalanced model | "Show balance details" |

**LSP Method:** `textDocument/codeAction`

### 4.3 Extract Function
**Priority: Low** | **Complexity: High** | **Est: 15-20 hours**

Extract selected equations into a new function:
- Identify inputs (used variables)
- Identify outputs (assigned variables)
- Generate function signature
- Replace original code with function call

---

## Phase 5: Polish (v1.0.0)

Final polish for production-ready experience.

### 5.1 Code Formatting
**Priority: Medium** | **Complexity: Medium** | **Est: 15-20 hours**

Auto-format Modelica code:
- Consistent indentation
- Alignment of declarations
- Spacing around operators
- Line length limits
- Format on save option

**LSP Methods:** `textDocument/formatting`, `textDocument/rangeFormatting`, `textDocument/onTypeFormatting`

### 5.2 Folding Ranges
**Priority: Low** | **Complexity: Low** | **Est: 3-4 hours**

Collapsible code regions:
- Class definitions
- Equation sections
- Algorithm sections
- Annotation blocks
- Comment blocks

**LSP Method:** `textDocument/foldingRange`

### 5.3 Document Highlighting
**Priority: Low** | **Complexity: Low** | **Est: 3-4 hours**

Highlight all occurrences of symbol under cursor:
- Read references (default highlight)
- Write references (bold highlight)
- Definition (underline)

**LSP Method:** `textDocument/documentHighlight`

### 5.4 Selection Range
**Priority: Low** | **Complexity: Low** | **Est: 2-3 hours**

Smart selection expansion (`Ctrl+Shift+→`):
- Select word → expression → statement → block → class

**LSP Method:** `textDocument/selectionRange`

### 5.5 Breadcrumbs
**Priority: Low** | **Complexity: Low** | **Est: 2-3 hours**

Navigation breadcrumbs showing current location:
```
Package > SubPackage > Model > equation
```

*Enabled automatically with Document Symbols implementation.*

---

## Implementation Priority Matrix

| Feature | Impact | Effort | Priority Score |
|---------|--------|--------|----------------|
| Document Symbols | High | Low | **10** |
| Code Lens (Balance) | High | Low | **10** |
| Semantic Tokens | High | Medium | **9** |
| Find References | High | Medium | **9** |
| Enhanced Diagnostics | High | Medium | **8** |
| Workspace Symbols | High | Medium | **8** |
| Rename Symbol | High | High | **7** |
| Inlay Hints | Medium | Medium | **6** |
| Go to Type Def | Medium | Low | **6** |
| Code Actions | Medium | Medium | **5** |
| Call Hierarchy | Medium | Medium | **5** |
| Formatting | Medium | Medium | **5** |
| Folding Ranges | Low | Low | **4** |
| Document Highlight | Low | Low | **4** |
| Selection Range | Low | Low | **3** |

---

## Timeline Estimate

| Phase | Features | Est. Time | Target Version |
|-------|----------|-----------|----------------|
| Phase 1 | Foundation | 3-4 weeks | v0.8.0 |
| Phase 2 | Navigation | 3-4 weeks | v0.9.0 |
| Phase 3 | Modelica Intelligence | 2-3 weeks | v0.10.0 |
| Phase 4 | Refactoring | 4-5 weeks | v0.11.0 |
| Phase 5 | Polish | 2-3 weeks | v1.0.0 |

**Total Estimated Time: 14-19 weeks**

---

## Technical Notes

### Architecture Considerations

1. **Incremental Parsing**: Cache ASTs to avoid re-parsing entire files
2. **Workspace Indexing**: Background indexer for cross-file features
3. **Visitor Pattern**: Leverage existing `Visitor` trait for semantic analysis
4. **Symbol Table**: Extend `SymbolTable` for reference tracking

### Dependencies

Current LSP dependencies in `Cargo.toml`:
```toml
lsp-server = "0.7"
lsp-types = "0.97"
```

These versions support all planned features.

### Testing Strategy

Each feature should include:
1. Unit tests for core logic
2. Integration tests with sample Modelica files
3. Manual VS Code testing

---

## Contributing

Contributions are welcome! To work on a feature:

1. Check this roadmap for unclaimed features
2. Open an issue to discuss implementation approach
3. Submit a PR with tests
4. Update this roadmap when complete

---

## Version History

- **v0.7.1** (Current): Basic LSP with completion, hover, go-to-definition, signature help
- **v0.8.0** (Planned): Document symbols, semantic tokens, enhanced diagnostics
- **v0.9.0** (Planned): Find references, workspace symbols, type definition
- **v0.10.0** (Planned): Code lens, inlay hints, Modelica-specific features
- **v0.11.0** (Planned): Refactoring support
- **v1.0.0** (Planned): Full polish, formatting, production-ready
