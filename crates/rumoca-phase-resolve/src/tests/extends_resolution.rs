//! Extends resolution (MLS section 7): base class binding, multiple extends,
//! and circular inheritance detection.

use super::*;

#[test]
fn test_simple_extends_resolution() {
    // Test that a simple extends clause resolves correctly
    let source = r#"
model Base
Real x;
end Base;

model Derived
extends Base;
Real y;
end Derived;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(result.is_ok(), "resolution should succeed");

    let tree = result.unwrap().into_inner();

    // Verify base class exists and has a DefId
    let base = tree
        .definitions
        .classes
        .get("Base")
        .expect("Base should exist");
    assert!(base.def_id.is_some(), "Base should have DefId");

    // Verify derived class exists and extends has base_def_id set
    let derived = tree
        .definitions
        .classes
        .get("Derived")
        .expect("Derived should exist");
    assert_eq!(derived.extends.len(), 1, "Derived should have one extends");

    let extend = &derived.extends[0];
    assert!(
        extend.base_def_id.is_some(),
        "Extends should have base_def_id set"
    );
    assert_eq!(
        extend.base_def_id, base.def_id,
        "base_def_id should match Base's DefId"
    );
}

#[test]
fn test_qualified_extends_resolution() {
    // Test that qualified extends (Package.Model) resolves correctly
    let source = r#"
package MyPkg
model Base
    Real x;
end Base;
end MyPkg;

model Derived
extends MyPkg.Base;
Real y;
end Derived;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(result.is_ok(), "resolution should succeed");

    let tree = result.unwrap().into_inner();

    // Get the base class's DefId
    let pkg = tree
        .definitions
        .classes
        .get("MyPkg")
        .expect("MyPkg should exist");
    let base = pkg.classes.get("Base").expect("Base should exist in MyPkg");
    assert!(base.def_id.is_some(), "Base should have DefId");

    // Verify derived class extends has correct base_def_id
    let derived = tree
        .definitions
        .classes
        .get("Derived")
        .expect("Derived should exist");
    assert_eq!(derived.extends.len(), 1);

    let extend = &derived.extends[0];
    assert!(
        extend.base_def_id.is_some(),
        "Extends should have base_def_id set"
    );
    assert_eq!(
        extend.base_def_id, base.def_id,
        "base_def_id should match MyPkg.Base's DefId"
    );
}

#[test]
fn test_base_class_not_found() {
    // Test that extending a non-existent class produces an error
    let source = r#"
model Derived
extends NonExistent;
Real y;
end Derived;
"#;
    let result = resolve_parsed_tree_source(source);

    // Resolution should fail with base class not found error
    assert!(result.is_err(), "resolution should fail");
    let diagnostics = result.unwrap_err();
    assert!(diagnostics.has_errors(), "should have error diagnostics");

    // Check that the error message contains "base class not found"
    let has_base_not_found = diagnostics
        .iter()
        .any(|d| d.message.contains("base class not found"));
    assert!(has_base_not_found, "should have base class not found error");
}

#[test]
fn test_circular_inheritance_direct() {
    // Test that direct self-reference (A extends A) is detected.
    // This produces "base class not found" because when we exclude the
    // current class from lookup (to support redeclare extends pattern),
    // we can't find any other class with that name.
    let source = r#"
model A
extends A;
Real x;
end A;
"#;
    let result = resolve_parsed_tree_source(source);

    // Resolution should fail with "base class not found" error
    assert!(result.is_err(), "resolution should fail");
    let diagnostics = result.unwrap_err();
    assert!(diagnostics.has_errors(), "should have error diagnostics");

    // Check that the error message indicates base not found
    let has_base_not_found = diagnostics
        .iter()
        .any(|d| d.message.contains("base class not found"));
    assert!(has_base_not_found, "should have base class not found error");
}

#[test]
fn test_multiple_extends() {
    // Test that multiple extends clauses all resolve correctly
    let source = r#"
model Base1
Real x;
end Base1;

model Base2
Real y;
end Base2;

model Derived
extends Base1;
extends Base2;
Real z;
end Derived;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(result.is_ok(), "resolution should succeed");

    let tree = result.unwrap().into_inner();
    let derived = tree
        .definitions
        .classes
        .get("Derived")
        .expect("Derived should exist");
    assert_eq!(derived.extends.len(), 2, "Derived should have two extends");

    // Both extends should have base_def_id set
    for extend in &derived.extends {
        assert!(
            extend.base_def_id.is_some(),
            "All extends should have base_def_id set"
        );
    }
}

#[test]
fn test_circular_inheritance_indirect() {
    // Test that indirect circular inheritance (A extends B, B extends A) is detected
    let source = r#"
model A
extends B;
Real x;
end A;

model B
extends A;
Real y;
end B;
"#;
    let result = resolve_parsed_tree_source(source);

    // Resolution should fail with circular inheritance error
    assert!(result.is_err(), "resolution should fail for indirect cycle");
    let diagnostics = result.unwrap_err();
    assert!(diagnostics.has_errors(), "should have error diagnostics");

    // Check that the error message contains "circular"
    let has_circular = diagnostics.iter().any(|d| d.message.contains("circular"));
    assert!(
        has_circular,
        "should have circular inheritance error for indirect cycle"
    );
}

#[test]
fn test_circular_inheritance_chain() {
    // Test that longer cycles (A extends B, B extends C, C extends A) are detected
    let source = r#"
model A
extends B;
end A;

model B
extends C;
end B;

model C
extends A;
end C;
"#;
    let result = resolve_parsed_tree_source(source);

    // Resolution should fail with circular inheritance error
    assert!(result.is_err(), "resolution should fail for chain cycle");
    let diagnostics = result.unwrap_err();
    assert!(diagnostics.has_errors(), "should have error diagnostics");

    // Check that the error message contains "circular"
    let has_circular = diagnostics.iter().any(|d| d.message.contains("circular"));
    assert!(
        has_circular,
        "should have circular inheritance error for chain cycle"
    );
}

#[test]
fn short_class_definition_modifier_value_binds_enclosing_component() {
    // MLS §4.5/§5.3.1: in `function g = f(mode = mode)` the modifier value names
    // `Holder.mode`, not the `mode` input of `f` that the modifier targets.
    // Binding it to the targeted input makes the constant self-referential and
    // made constant folding recurse forever.
    let source = r#"
type Mode = enumeration(Slow, Fast);

function f
input Mode mode;
input Real u;
output Real y;
algorithm
y := if mode == Mode.Fast then 2*u else u;
end f;

model Holder
parameter Mode mode = Mode.Fast;
replaceable function g = f(mode = mode);
end Holder;
"#;
    let tree = resolve_tree_source(source).into_inner();

    let holder = tree
        .definitions
        .classes
        .get("Holder")
        .expect("Holder should exist");
    let holder_mode = holder
        .components
        .get("mode")
        .expect("Holder.mode should exist")
        .def_id
        .expect("Holder.mode should have a DefId");

    let short_def = holder
        .classes
        .get("g")
        .expect("short class definition g should exist");
    assert_eq!(
        short_def.extends.len(),
        1,
        "g should desugar to one extends"
    );
    let ast::Expression::Modification { target, value, .. } =
        &short_def.extends[0].modifications[0].expr
    else {
        panic!("modifier should be a simple value modification");
    };
    let target_def_id = target
        .root_def_id()
        .expect("modifier target should resolve");
    let value_def_id = find_comp_ref_def_id(value).expect("modifier value should resolve");

    assert_ne!(
        target_def_id, value_def_id,
        "modifier target and value must not be the same declaration"
    );
    assert_eq!(
        value_def_id, holder_mode,
        "modifier value must bind the enclosing component, not the modified input"
    );
}
