//! Baseline component and type reference resolution: DefId assignment,
//! unresolved-reference diagnostics, and reserved-DefId invariants.

use super::*;

#[test]
fn test_empty_resolution() {
    let tree = ClassTree::new();
    let parsed = ParsedTree::new(tree);
    let result = resolve(parsed);
    assert!(result.is_ok());
}

#[test]
fn test_component_reference_resolution() {
    let source = r#"
model Test
Real x;
Real y;
equation
y = x + 1;
end Test;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(result.is_ok(), "resolution should succeed");

    let tree = result.unwrap().into_inner();
    let model = tree
        .definitions
        .classes
        .get("Test")
        .expect("Test should exist");

    // Components should have DefIds
    assert!(model.components.get("x").unwrap().def_id.is_some());
    assert!(model.components.get("y").unwrap().def_id.is_some());

    // Model should have a scope
    assert!(model.scope_id.is_some());
}
#[test]
fn test_unresolved_component_reference_is_error() {
    let source = r#"
model Test
Real y;
equation
y = x + 1;
end Test;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(result.is_err(), "resolution should fail");

    let diags = result.expect_err("expected resolve diagnostics");
    assert!(diags.iter().any(|d| {
        d.message.contains("unresolved component reference") && d.code.as_deref() == Some("ER002")
    }));
}

#[test]
fn test_def_id_zero_is_reserved_for_root_not_builtin() {
    let resolver = Resolver::new();
    let real_id = resolver
        .scope_tree
        .lookup(ScopeId::GLOBAL, &ComponentPath::from_flat_path("Real"))
        .expect("Real builtin should be registered globally");

    assert_ne!(
        real_id,
        DefId::new(0),
        "SPEC_0001 reserves DefId(0) for root/global scope"
    );
    assert!(
        resolver.is_builtin(real_id),
        "Real should remain classified as a builtin"
    );
    assert!(
        !resolver.is_builtin(DefId::new(0)),
        "root/global DefId must not be classified as a builtin"
    );
}
