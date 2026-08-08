//! Expression semantics in instanced scopes: string operands, array
//! concatenation slices, inherited member references, and builtin/user
//! overload separation.

use super::*;

#[test]
fn instanced_string_concatenation_accepts_subscripted_string_operands() {
    let source = r#"
        model Test
            parameter String names[1] = {"x"};
            parameter Integer i = 1;
            String message = "name " + names[i] + " #" + String(i);
        end Test;
        "#;
    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let tree = resolved.into_inner();
    let test = tree.definitions.classes.get("Test").expect("Test class");
    let mut overlay = InstanceOverlay::new();
    for (name, component) in &test.components {
        add_test_instance(&mut overlay, name, component, component.binding.clone());
    }

    typecheck_instanced(&tree, &mut overlay, "Test")
        .expect("subscripted String operands should concatenate");
}

#[test]
fn instanced_cat_accepts_range_slices() {
    let source = r#"
        model Test
            parameter Integer n = 4;
            Real values[n];
            Real joined[n];
        equation
            joined = cat(1, {values[1]}, 0.5 * (values[2:n - 1] + values[2:n - 1]), {values[n]});
        end Test;
        "#;
    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let tree = resolved.into_inner();
    let test = tree.definitions.classes.get("Test").expect("Test class");
    let mut overlay = InstanceOverlay::new();
    for (name, component) in &test.components {
        add_test_instance(&mut overlay, name, component, component.binding.clone());
    }

    typecheck_instanced(&tree, &mut overlay, "Test")
        .expect("range subscripts retain array rank in cat arguments");
}

#[test]
fn instanced_component_references_find_inherited_members() {
    let source = r#"
        connector RealInput = input Real;
        connector RealOutput = output Real;
        partial block SISO
            RealInput u;
            RealOutput y;
        end SISO;
        block Filter
            extends SISO;
        equation
            y = u;
        end Filter;
        model Test
            Filter filter;
        equation
            filter.y = filter.u;
        end Test;
        "#;
    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let tree = resolved.into_inner();
    let test = tree.definitions.classes.get("Test").expect("Test class");
    let filter = test.components.get("filter").expect("filter component");
    let mut overlay = InstanceOverlay::new();
    add_test_instance(&mut overlay, "filter", filter, None);

    typecheck_instanced(&tree, &mut overlay, "Test")
        .expect("component references should resolve members inherited by their class");
}

#[test]
fn instanced_unknown_builtin_result_does_not_capture_user_overload() {
    let source = r#"
        record Complex
            Real re;
            Real im;
        end Complex;
        package ComplexMath
            function sum
                input Complex values[:];
                output Complex result;
            algorithm
                result := values[1];
            end sum;
        end ComplexMath;
        package WallFriction
            partial function PressureLossBase
                input Real value;
                output Real pressure;
            end PressureLossBase;
            function pressureLoss
                extends PressureLossBase;
            algorithm
                pressure := value;
            end pressureLoss;
        end WallFriction;
        model Test
            Real pressure = sum(WallFriction.pressureLoss({1.0, 2.0}));
        end Test;
        "#;
    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let tree = resolved.into_inner();
    let test = tree.definitions.classes.get("Test").expect("Test class");
    let pressure = test.components.get("pressure").expect("pressure component");
    let mut overlay = InstanceOverlay::new();
    add_test_instance(&mut overlay, "pressure", pressure, pressure.binding.clone());

    typecheck_instanced(&tree, &mut overlay, "Test")
        .expect("unknown builtin element types must not capture a same-name user overload");
}

#[test]
fn instanced_identity_has_integer_elements_and_requires_an_integer_extent() {
    let source = r#"
        model Accepted
            Integer values[2, 2] = identity(2);
        end Accepted;
        model Rejected
            Integer values[2, 2] = identity(2.0);
        end Rejected;
        "#;
    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let tree = resolved.into_inner();

    let accepted = tree
        .definitions
        .classes
        .get("Accepted")
        .expect("Accepted class");
    let mut accepted_overlay = InstanceOverlay::new();
    for (name, component) in &accepted.components {
        add_test_instance(
            &mut accepted_overlay,
            name,
            component,
            component.binding.clone(),
        );
    }
    typecheck_instanced(&tree, &mut accepted_overlay, "Accepted")
        .expect("identity returns an Integer matrix");

    let rejected = tree
        .definitions
        .classes
        .get("Rejected")
        .expect("Rejected class");
    let mut rejected_overlay = InstanceOverlay::new();
    for (name, component) in &rejected.components {
        add_test_instance(
            &mut rejected_overlay,
            name,
            component,
            component.binding.clone(),
        );
    }
    let diagnostics = typecheck_instanced(&tree, &mut rejected_overlay, "Rejected")
        .expect_err("a Real identity extent must be rejected");
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.message.contains("identity")),
        "identity extent rejection must remain named: {diagnostics:?}"
    );
}

fn typecheck_instanced_model(
    tree: &ClassTree,
    model_name: &str,
) -> Result<(), rumoca_core::Diagnostics> {
    let model = tree
        .definitions
        .classes
        .get(model_name)
        .expect("test model");
    let mut overlay = InstanceOverlay::new();
    for (name, component) in &model.components {
        add_test_instance(&mut overlay, name, component, component.binding.clone());
    }
    typecheck_instanced(tree, &mut overlay, model_name)
}

#[test]
fn instanced_cross_requires_two_numeric_three_vectors_and_has_exact_result_shape() {
    let source = r#"
        model Accepted
            Real result[3] = cross({1, 2, 3}, {4.0, 5.0, 6.0});
        end Accepted;
        model OneArgument
            Real result[3] = cross({1.0, 2.0, 3.0});
        end OneArgument;
        model ThreeArguments
            Real result[3] = cross(
                {1.0, 2.0, 3.0},
                {4.0, 5.0, 6.0},
                {7.0, 8.0, 9.0});
        end ThreeArguments;
        model BooleanArguments
            Boolean result[3] = cross(
                {true, false, true},
                {false, true, false});
        end BooleanArguments;
        model WrongResultShape
            Real result[2];
        equation
            result = cross({1.0, 2.0, 3.0}, {4.0, 5.0, 6.0});
        end WrongResultShape;
        "#;
    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let tree = resolved.into_inner();

    typecheck_instanced_model(&tree, "Accepted")
        .expect("cross accepts compatible numeric 3-vectors and returns a 3-vector");

    for model_name in ["OneArgument", "ThreeArguments"] {
        let diagnostics = typecheck_instanced_model(&tree, model_name)
            .expect_err("cross arity outside exactly two must be rejected");
        assert!(
            diagnostics
                .iter()
                .any(|diagnostic| { diagnostic.message.contains("cross() expects 2 argument(s)") }),
            "cross arity rejection must remain exact: {diagnostics:?}"
        );
    }

    let diagnostics = typecheck_instanced_model(&tree, "BooleanArguments")
        .expect_err("cross Boolean vectors must be rejected");
    assert!(
        diagnostics.iter().any(|diagnostic| {
            diagnostic.message.contains("cross() vector argument")
                && diagnostic.message.contains("Real or Integer")
        }),
        "cross element-type rejection must remain numeric: {diagnostics:?}"
    );

    let diagnostics = typecheck_instanced_model(&tree, "WrongResultShape")
        .expect_err("cross result must have exact shape [3]");
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.message.contains("array dimension mismatch")),
        "cross result shape rejection must remain exact: {diagnostics:?}"
    );
}
