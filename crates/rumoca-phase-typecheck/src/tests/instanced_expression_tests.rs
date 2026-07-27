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
