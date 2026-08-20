//! Variability and structural-parameter analysis, including the variability
//! projected onto instanced components.

use super::*;

#[test]
fn test_structural_parameter_marking() {
    // Test that parameters used in dimensions are marked as structural (MLS §18.3)
    let source = r#"
        model Test
            parameter Integer n = 3;
            parameter Integer m = 5;
            parameter Real unused = 1.0;
            Real x[n];
            Real y[m, 2];
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let typed = typecheck(resolved).expect("typecheck should succeed");

    let tree = typed.into_inner();
    let test_class = tree
        .definitions
        .classes
        .get("Test")
        .expect("Test class should exist");

    // Check n is marked as structural (used in x[n])
    let n = test_class.components.get("n").expect("n should exist");
    assert!(n.is_structural, "n should be marked as structural");

    // Check m is marked as structural (used in y[m, 2])
    let m = test_class.components.get("m").expect("m should exist");
    assert!(m.is_structural, "m should be marked as structural");

    // Check unused is NOT marked as structural
    let unused = test_class
        .components
        .get("unused")
        .expect("unused should exist");
    assert!(
        !unused.is_structural,
        "unused should not be marked as structural"
    );
}

#[test]
fn test_variability_validation() {
    // Test that variability constraints are validated (MLS §3.8.4)
    // A parameter binding that references a continuous variable is caught at resolve time
    let source = r#"
        model Test
            Real x;
            parameter Real p = x;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed);
    assert!(
        resolved.is_err(),
        "resolve should reject parameter depending on continuous variable"
    );
}

#[test]
fn test_variability_validation_valid() {
    // Test that valid variability bindings don't produce warnings
    let source = r#"
        model Test
            constant Real c = 1.0;
            parameter Real p = c;
            Real x = p;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(result.is_ok(), "typecheck should succeed");
}

#[test]
fn type_and_function_references_are_constant_for_variability_checks() {
    let diagnostics = typecheck_diagnostics(
        r#"
        type Init = enumeration(NoInit, InitialState);
        model Test
            parameter Init initType = Init.InitialState;
            parameter Real table[:, :] = fill(0.0, 0, 2);
        end Test;
        "#,
    );

    assert!(
        diagnostics
            .iter()
            .all(|diagnostic| diagnostic.code.as_deref() != Some("ET004")),
        "type literals and pure builtin calls must not be treated as continuous: {diagnostics:?}"
    );
}

#[test]
fn instanced_variability_uses_projected_member_variability() {
    let source = r#"
        model SystemLike
            parameter Boolean allowFlowReversal = true;
        end SystemLike;
        model Test
            SystemLike system;
            parameter Boolean allowFlowReversal = system.allowFlowReversal;
        end Test;
        "#;
    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let tree = resolved.into_inner();
    let test = tree.definitions.classes.get("Test").expect("Test class");
    let system_like = tree
        .definitions
        .classes
        .get("SystemLike")
        .expect("SystemLike class");
    let mut overlay = InstanceOverlay::new();
    add_test_instance(
        &mut overlay,
        "system",
        test.components.get("system").expect("system declaration"),
        None,
    );
    let member = system_like
        .components
        .get("allowFlowReversal")
        .expect("member declaration");
    add_test_instance(
        &mut overlay,
        "system.allowFlowReversal",
        member,
        member.binding.clone(),
    );
    let forwarded = test
        .components
        .get("allowFlowReversal")
        .expect("forwarded declaration");
    add_test_instance(
        &mut overlay,
        "allowFlowReversal",
        forwarded,
        forwarded.binding.clone(),
    );

    typecheck_instanced(&tree, &mut overlay, "Test")
        .expect("parameter projected from a parameter member should typecheck");
}
