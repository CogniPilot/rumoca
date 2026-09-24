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

#[test]
fn instanced_outer_parameter_member_binding_does_not_become_continuous() {
    let source = r#"
        model SystemLike
            parameter Boolean allowFlowReversal = true;
            parameter Real T_ambient = 293.15;
        end SystemLike;

        model Child
            outer SystemLike system;
            parameter Boolean allowFlowReversal = system.allowFlowReversal;
            parameter Real T_ambient = system.T_ambient;
        end Child;

        model Test
            inner SystemLike system;
            Child child;
        end Test;
        "#;
    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let tree = resolved.into_inner();
    let mut overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(&tree, "Test")
    {
        rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
        other => panic!("instantiation should succeed: {other:?}"),
    };
    let outer_redirects = overlay
        .outer_prefix_to_inner
        .iter()
        .map(|(outer, inner)| (outer.to_flat_string(), inner.to_flat_string()))
        .collect::<Vec<_>>();
    assert!(
        outer_redirects
            .iter()
            .any(|(outer, inner)| outer == "child.system" && inner == "system"),
        "instantiate must preserve the outer-to-inner identity: {outer_redirects:?}"
    );
    let component_metadata = overlay
        .components
        .values()
        .filter(|data| {
            let name = data.qualified_name.to_flat_string();
            name == "system"
                || name == "system.allowFlowReversal"
                || name == "system.T_ambient"
                || name == "child.allowFlowReversal"
                || name == "child.T_ambient"
        })
        .map(|data| {
            (
                data.qualified_name.to_flat_string(),
                format!("{:?}", data.variability),
                data.owner_class_id,
            )
        })
        .collect::<Vec<_>>();
    let mut checker = TypeChecker::new();
    checker.check_instanced(&tree, &mut overlay, "Test");
    let diagnostics = checker.take_diagnostics();
    let variability_diagnostics = diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.code.as_deref() == Some("WT003"))
        .collect::<Vec<_>>();
    assert!(
        variability_diagnostics.is_empty(),
        "outer parameter members must retain parameter variability: \
         {variability_diagnostics:?}; redirects={outer_redirects:?}, \
         components={component_metadata:?}"
    );
}

#[test]
fn instanced_duplicate_outer_parameter_members_use_retained_identity() {
    let source = r#"
        model Sys
            parameter Boolean allowFlowReversal = true;
        end Sys;

        partial model TransportBase
            outer Sys system;
            parameter Boolean allowFlowReversal = system.allowFlowReversal;
        end TransportBase;

        partial model LumpedFlowBase
            outer Sys system;
            parameter Boolean allowFlowReversal = system.allowFlowReversal;
        end LumpedFlowBase;

        model Orifice
            extends TransportBase;
            extends LumpedFlowBase;
        end Orifice;

        model Test
            inner Sys system;
            Orifice child;
        end Test;
        "#;
    let tree = resolve(parse(source))
        .expect("resolve should succeed")
        .into_inner();
    let mut overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(&tree, "Test")
    {
        rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
        other => panic!("instantiation should succeed: {other:?}"),
    };
    let remaps = overlay
        .inherited_def_id_remaps
        .values()
        .flat_map(|remaps| remaps.iter())
        .collect::<Vec<_>>();
    assert!(
        !remaps.is_empty(),
        "duplicate inherited declarations need a remap"
    );

    let mut checker = TypeChecker::new();
    checker.check_instanced(&tree, &mut overlay, "Test");
    let diagnostics = checker.take_diagnostics();
    assert!(
        diagnostics
            .iter()
            .all(|diagnostic| diagnostic.code.as_deref() != Some("WT003")),
        "retained duplicate outer identity must preserve parameter variability: {diagnostics:?}"
    );
}

#[test]
fn instanced_opposite_order_diamond_preserves_outer_parameter_identity() {
    let source = r#"
        model Sys
            parameter Boolean allowFlowReversal = true;
        end Sys;

        partial model A
            outer Sys system;
            parameter Boolean fromA = system.allowFlowReversal;
        end A;

        partial model B
            outer Sys system;
            parameter Boolean fromB = system.allowFlowReversal;
        end B;

        partial model RightFirst
            extends B;
            extends A;
        end RightFirst;

        model Derived
            extends A;
            extends RightFirst;
        end Derived;

        model Test
            inner Sys system;
            Derived child;
        end Test;
        "#;
    let tree = resolve(parse(source))
        .expect("resolve should succeed")
        .into_inner();
    let mut overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(&tree, "Test")
    {
        rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
        other => panic!("instantiation should succeed: {other:?}"),
    };
    let a_system = tree
        .definitions
        .classes
        .get("A")
        .and_then(|class| class.components.get("system"))
        .and_then(|component| component.def_id)
        .expect("A system identity");
    let b_system = tree
        .definitions
        .classes
        .get("B")
        .and_then(|class| class.components.get("system"))
        .and_then(|component| component.def_id)
        .expect("B system identity");
    assert!(
        overlay
            .inherited_def_id_remaps
            .values()
            .any(|remaps| remaps.get(&b_system) == Some(&a_system)),
        "the opposite-order branch must map B's source declaration to A's retained declaration"
    );
    assert!(
        overlay
            .inherited_def_id_remaps
            .values()
            .all(|remaps| !remaps.contains_key(&a_system)),
        "the retained A declaration must not be remapped back into the branch"
    );

    let mut checker = TypeChecker::new();
    checker.check_instanced(&tree, &mut overlay, "Test");
    let diagnostics = checker.take_diagnostics();
    assert!(
        diagnostics
            .iter()
            .all(|diagnostic| diagnostic.code.as_deref() != Some("WT003")),
        "valid opposite-order diamond should retain parameter variability: {diagnostics:?}"
    );
}
