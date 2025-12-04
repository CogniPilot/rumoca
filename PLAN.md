# Plan: Fix Remaining 6 MSL Parsing Failures

## Problem Analysis

The 6 remaining MSL failures all use the syntax pattern:
```modelica
redeclare replaceable model extends BaseProperties(...)
  ...
end BaseProperties;
```

This combines:
- `redeclare replaceable` modifiers
- `model extends BaseProperties(...)` - an extends_class_specifier (long class definition)

### Current Grammar Limitation

The grammar at line 636-638 of `modelica.par`:
```
element_replaceable
    : replaceable ( short_class_definition | component_clause1 ) [ constraining_clause ]
    ;
```

Only allows `short_class_definition` (e.g., `model Foo = Bar;`), but NOT full `class_definition` which includes `extends_class_specifier` (e.g., `model extends Bar ... end Bar;`).

### Comparison with `element_replaceable_definition`

Line 484 shows `element_replaceable_definition` already supports full `class_definition`:
```
element_replaceable_definition
    : replaceable ( class_definition | component_clause ) [ constraining_clause description ]
    ;
```

## Implementation Plan

### Step 1: Modify the Grammar File

File: `src/modelica_grammar/modelica.par`

Change line 637 from:
```
element_replaceable
    : replaceable ( short_class_definition | component_clause1 ) [ constraining_clause ]
    ;
```

To:
```
element_replaceable
    : replaceable ( class_definition | short_class_definition | component_clause1 ) [ constraining_clause ]
    ;
```

Note: Keep `short_class_definition` for backward compatibility and simpler cases.

### Step 2: Regenerate the Parser

Run:
```bash
cargo build --features regen-parser
```

This will regenerate:
- `src/modelica_grammar/generated/modelica_grammar_trait.rs`
- `src/modelica_grammar/generated/modelica_parser.rs`

The generated `ElementReplaceableGroup` enum will gain a new variant:
```rust
pub enum ElementReplaceableGroup {
    ClassDefinition(ElementReplaceableGroupClassDefinition), // NEW
    ShortClassDefinition(ElementReplaceableGroupShortClassDefinition),
    ComponentClause1(ElementReplaceableGroupComponentClause1),
}
```

### Step 3: Update Conversion Code

File: `src/modelica_grammar/expressions.rs`

In the `ElementRedeclarationGroup::ElementReplaceable` match arm (around line 480-590), add handling for the new `ClassDefinition` variant:

```rust
modelica_grammar_trait::ElementReplaceableGroup::ClassDefinition(class_def) => {
    // Handle full class definition: redeclare replaceable model extends Foo ...
    // Convert to the appropriate expression representation
    let class_def = &class_def.class_definition;
    let name_ref = ir::ast::ComponentReference {
        local: false,
        parts: vec![ir::ast::ComponentRefPart {
            ident: class_def.name.clone(),
            subs: None,
        }],
    };
    Ok(ir::ast::Expression::ComponentReference(name_ref))
}
```

### Step 4: Run Tests

```bash
# Run regular tests
cargo test

# Run MSL tests
MSL_PATH=/home/jgoppert/ws_fixedwing/src/ModelicaStandardLibrary cargo test test_msl_full_compilation -- --ignored --nocapture
```

Expected outcome: Pass rate should improve from 99.8% (2548/2554) to 100% (2554/2554).

## Affected Files

1. `src/modelica_grammar/modelica.par` - Grammar modification
2. `src/modelica_grammar/generated/modelica_grammar_trait.rs` - Auto-regenerated
3. `src/modelica_grammar/generated/modelica_parser.rs` - Auto-regenerated
4. `src/modelica_grammar/expressions.rs` - Add handling for new variant

## Risk Assessment

- **Low Risk**: The change extends existing functionality without breaking current behavior
- **Backward Compatible**: Existing `short_class_definition` and `component_clause1` paths remain unchanged
- **Testing**: Full MSL test suite validates the change

## Failing Files (for reference)

1. `Modelica/Media/R134a.mo:248`
2. `Modelica/Media/IdealGases/Common/package.mo:315`
3. `Modelica/Media/Air/MoistAir.mo:36`
4. `Modelica/Media/Air/ReferenceMoistAir.mo:44`
5. `Modelica/Media/package.mo:3862`
6. `Modelica/Media/Water/package.mo:149`
