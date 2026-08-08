use super::*;

fn parsed_tree(source: &str) -> ClassTree {
    let parsed = parse(source);
    resolve(parsed)
        .expect("resolve should succeed")
        .into_inner()
}

fn add_model_components(
    tree: &ClassTree,
    overlay: &mut InstanceOverlay,
    model_name: &str,
    component_names: &[&str],
) {
    let model = tree
        .get_class_by_qualified_name(model_name)
        .expect("model class");
    for name in component_names {
        add_instanced_component(
            overlay,
            &format!("{model_name}.{name}"),
            model.components.get(*name).expect("model component"),
            true,
        );
    }
}

#[test]
fn clocked_two_argument_sample_preserves_sampled_value_type() {
    let source = r#"
        connector RealInput = input Real;
        connector RealOutput = output Real;
        connector ClockInput = input Clock;
        model Test
            RealInput u;
            RealOutput y;
            ClockInput clock;
        equation
            y = sample(u, clock);
        end Test;
    "#;
    let tree = parsed_tree(source);
    let mut overlay = InstanceOverlay::new();
    add_model_components(&tree, &mut overlay, "Test", &["u", "y", "clock"]);

    typecheck_instanced(&tree, &mut overlay, "Test")
        .expect("sample(value, clock) should have the sampled value type");
}

#[test]
fn function_call_named_output_projection_uses_output_type() {
    let source = r#"
        record Payload
            Real value;
        end Payload;
        package Functions
            function makePayload
                input Real value;
                output Payload result;
            algorithm
                result := Payload(value);
            end makePayload;
        end Functions;
        model Test
            Payload payload;
        equation
            payload = Functions.makePayload(1.0);
        end Test;
    "#;
    let tree = parsed_tree(source);
    let mut overlay = InstanceOverlay::new();
    add_model_components(&tree, &mut overlay, "Test", &["payload"]);

    typecheck_instanced(&tree, &mut overlay, "Test")
        .expect("a named function output is a valid call projection");
}

#[test]
fn function_call_type_uses_first_output_even_when_it_is_an_array() {
    let source = r#"
        function makeValues
            input Integer n;
            output Real values[n];
            output String scratch;
        algorithm
            values := fill(1.0, n);
            scratch := "";
        end makeValues;
        model Test
            Real values[2];
        equation
            values = makeValues(2);
        end Test;
    "#;
    let tree = parsed_tree(source);
    let mut overlay = InstanceOverlay::new();
    add_model_components(&tree, &mut overlay, "Test", &["values"]);
    overlay
        .components
        .values_mut()
        .find(|data| data.qualified_name.to_flat_string() == "Test.values")
        .expect("values instance")
        .dims = vec![2];

    typecheck_instanced(&tree, &mut overlay, "Test")
        .expect("the first shaped function output determines the call value type");
}

#[test]
fn resolved_user_function_can_overload_a_predefined_function_name() {
    let source = r#"
        record Pair
            Real left;
            Real right;
        end Pair;
        function abs
            input Pair value;
            output Real magnitude;
        algorithm
            magnitude := sqrt(value.left^2 + value.right^2);
        end abs;
        model Test
            Pair value;
            Real magnitude;
        equation
            magnitude = abs(value);
        end Test;
    "#;
    let tree = parsed_tree(source);
    let mut overlay = InstanceOverlay::new();
    add_model_components(&tree, &mut overlay, "Test", &["value", "magnitude"]);

    typecheck_instanced(&tree, &mut overlay, "Test")
        .expect("a resolved user function named abs must not use builtin argument rules");
}

#[test]
fn predefined_function_def_id_is_not_captured_by_an_import_alias_from_another_scope() {
    let source = r#"
        record Pair
            Real left;
            Real right;
        end Pair;
        package Helpers
            function sum
                input Real values[:];
                output Pair result;
            algorithm
                result := Pair(0.0, 0.0);
            end sum;
        end Helpers;
        model ImportsHelper
            import Helpers.sum;
        end ImportsHelper;
        model Test
            Real values[2];
            Real total;
        equation
            total = sum(values);
        end Test;
    "#;
    let tree = parsed_tree(source);
    let mut overlay = InstanceOverlay::new();
    add_model_components(&tree, &mut overlay, "Test", &["values", "total"]);
    overlay
        .components
        .values_mut()
        .find(|data| data.qualified_name.to_flat_string() == "Test.values")
        .expect("values instance")
        .dims = vec![2];

    typecheck_instanced(&tree, &mut overlay, "Test").expect(
        "the builtin sum DefId must remain authoritative across scope-local import aliases",
    );
}

#[test]
fn redeclared_function_signature_includes_inherited_inputs_before_local_inputs() {
    let source = r#"
        record State
            Real value;
        end State;
        partial function BaseProperty
            input State state;
            output Real result;
        end BaseProperty;
        function Property
            extends BaseProperty;
            input Integer method = 1;
        algorithm
            result := state.value + method;
        end Property;
        model Test
            State state;
            Real result;
        equation
            result = Property(state);
        end Test;
    "#;
    let tree = parsed_tree(source);
    let mut overlay = InstanceOverlay::new();
    add_model_components(&tree, &mut overlay, "Test", &["state", "result"]);

    typecheck_instanced(&tree, &mut overlay, "Test")
        .expect("a function extension retains inherited inputs ahead of local defaulted inputs");
}

#[test]
fn package_alias_specializes_an_inherited_function_result_type() {
    let source = r#"
        partial package PartialMedium
            replaceable record ThermodynamicState
                Real value;
            end ThermodynamicState;
            replaceable function setState
                input Real value;
                output ThermodynamicState state;
            algorithm
                state := ThermodynamicState(value);
            end setState;
        end PartialMedium;
        package ConcreteMedium
            extends PartialMedium;
            redeclare record ThermodynamicState
                Real value;
                Real extra;
            end ThermodynamicState;
        end ConcreteMedium;
        model Test
            package Medium = ConcreteMedium;
            Medium.ThermodynamicState state;
        equation
            state = Medium.setState(1.0);
        end Test;
    "#;
    let tree = parsed_tree(source);
    let mut overlay = InstanceOverlay::new();
    add_model_components(&tree, &mut overlay, "Test", &["state"]);

    typecheck_instanced(&tree, &mut overlay, "Test").expect(
        "an inherited function's virtual result type uses the package alias's declaration slot",
    );
}

#[test]
fn package_alias_specializes_a_result_type_redeclared_to_a_composite_name() {
    // MLS §5.3.2: `Common.BaseProps` in the extends modification is resolved
    // from the lexically enclosing scopes of `Lib.ConcreteMedium`, so it names
    // `Lib.Common.BaseProps`. Neither a root-relative lookup of the written
    // name nor the reference's own resolved `DefId` reaches it, and without
    // that resolution the redeclaration is not recorded and `setState` keeps
    // the constraining type as its result type.
    let source = r#"
        package Lib
            package Common
                record BaseProps
                    Real value;
                    Real extra;
                end BaseProps;
            end Common;
            partial package PartialMedium
                replaceable record ThermodynamicState
                    Real value;
                end ThermodynamicState;
                replaceable function setState
                    input Real value;
                    output ThermodynamicState state;
                algorithm
                    state := ThermodynamicState(value);
                end setState;
            end PartialMedium;
            package ConcreteMedium
                extends PartialMedium(
                    redeclare record ThermodynamicState = Common.BaseProps);
            end ConcreteMedium;
            model Test
                package Medium = ConcreteMedium;
                Common.BaseProps state;
            equation
                state = Medium.setState(1.0);
            end Test;
        end Lib;
    "#;
    let tree = parsed_tree(source);
    let mut overlay = InstanceOverlay::new();
    add_model_components(&tree, &mut overlay, "Lib.Test", &["state"]);

    typecheck_instanced(&tree, &mut overlay, "Lib.Test")
        .expect("a redeclaration written as a composite name specializes the function result type");
}

#[test]
fn enclosing_package_specializes_an_unqualified_inherited_function_result_type() {
    let source = r#"
        partial package PartialMedium
            replaceable record ThermodynamicState
                Real value;
            end ThermodynamicState;
            replaceable function setState
                input Real value;
                output ThermodynamicState state;
            algorithm
                state := ThermodynamicState(value);
            end setState;
        end PartialMedium;
        package ConcreteMedium
            extends PartialMedium;
            redeclare record extends ThermodynamicState
                Real extra;
            end ThermodynamicState;
            model Properties
                ThermodynamicState state;
            equation
                state = setState(1.0);
            end Properties;
        end ConcreteMedium;
    "#;
    let tree = parsed_tree(source);
    let mut overlay = InstanceOverlay::new();
    let properties = tree
        .get_class_by_qualified_name("ConcreteMedium.Properties")
        .expect("properties model");
    add_instanced_component(
        &mut overlay,
        "ConcreteMedium.Properties.state",
        properties.components.get("state").expect("state"),
        true,
    );

    typecheck_instanced(&tree, &mut overlay, "ConcreteMedium.Properties").expect(
        "an inherited function's virtual result type uses its enclosing package specialization",
    );
}

#[test]
fn fill_prepends_dimensions_to_the_filled_values_shape() {
    let source = r#"
        model Test
            parameter Integer n = 1;
            parameter Integer m = 0;
            Real values[n, m];
            Real row[m];
        equation
            values = fill(row, n);
        end Test;
    "#;
    let tree = parsed_tree(source);
    let mut overlay = InstanceOverlay::new();
    add_model_components(&tree, &mut overlay, "Test", &["n", "m", "values", "row"]);
    for (name, dims) in [("values", vec![1, 0]), ("row", vec![0])] {
        overlay
            .components
            .values_mut()
            .find(|data| data.qualified_name.to_flat_string() == format!("Test.{name}"))
            .expect("array instance")
            .dims = dims;
    }

    typecheck_instanced(&tree, &mut overlay, "Test")
        .expect("fill(array, dims...) prepends dimensions to the array shape");
}

#[test]
fn partial_function_application_has_the_concrete_function_type() {
    let source = r#"
        partial function ScalarFunction
            input Real u;
            output Real y;
        end ScalarFunction;
        function Affine
            extends ScalarFunction;
            input Real gain = 1.0;
        algorithm
            y := gain*u;
        end Affine;
        function integrate
            input ScalarFunction f;
            output Real result;
        algorithm
            result := f(1.0);
        end integrate;
        model Test
            Real result;
        equation
            result = integrate(function Affine(gain=2.0));
        end Test;
    "#;
    let tree = parsed_tree(source);
    let mut overlay = InstanceOverlay::new();
    add_model_components(&tree, &mut overlay, "Test", &["result"]);

    typecheck_instanced(&tree, &mut overlay, "Test")
        .expect("a partial application is a compatible function value, not its Real result");
}

#[test]
fn single_row_matrix_constructor_has_row_major_shape() {
    let source = r#"
        model Test
            Real row[1, 4];
        equation
            row = [1.0, 2.0, 3.0, 4.0];
        end Test;
    "#;
    let tree = parsed_tree(source);
    let mut overlay = InstanceOverlay::new();
    add_model_components(&tree, &mut overlay, "Test", &["row"]);
    overlay
        .components
        .values_mut()
        .find(|data| data.qualified_name.to_flat_string() == "Test.row")
        .expect("row instance")
        .dims = vec![1, 4];

    typecheck_instanced(&tree, &mut overlay, "Test")
        .expect("a single matrix row has shape [1, columns]");
}

#[test]
fn structural_size_does_not_inherit_array_value_variability() {
    let source = r#"
        model Test
            input Real values[3];
            parameter Integer n = size(values, 1);
        end Test;
    "#;
    let tree = parsed_tree(source);
    let mut overlay = InstanceOverlay::new();
    add_model_components(&tree, &mut overlay, "Test", &["values", "n"]);
    overlay
        .components
        .values_mut()
        .find(|data| data.qualified_name.to_flat_string() == "Test.values")
        .expect("values instance")
        .dims = vec![3];

    typecheck_instanced(&tree, &mut overlay, "Test")
        .expect("size(array, dimension) has parameter variability");
}

#[test]
fn subscripts_are_validated_on_the_component_prefix_that_owns_the_array() {
    let source = r#"
        connector Port
            Boolean occupied;
        end Port;
        model Test
            parameter Integer n = 2;
            Port inPort[n];
        equation
            for i in 1:n loop
                inPort[i].occupied = false;
            end for;
        end Test;
    "#;
    let tree = parsed_tree(source);
    let model = tree.get_class_by_qualified_name("Test").expect("model");
    let port = tree.get_class_by_qualified_name("Port").expect("port");
    let mut overlay = InstanceOverlay::new();
    add_instanced_component(
        &mut overlay,
        "Test.n",
        model.components.get("n").expect("n"),
        true,
    );
    add_instanced_component(
        &mut overlay,
        "Test.inPort",
        model.components.get("inPort").expect("inPort"),
        false,
    );
    add_instanced_component(
        &mut overlay,
        "Test.inPort.occupied",
        port.components.get("occupied").expect("occupied"),
        true,
    );
    typecheck_instanced(&tree, &mut overlay, "Test")
        .expect("member access after indexing an array component should typecheck");
}

#[test]
fn member_access_through_an_array_component_preserves_the_owner_shape() {
    let source = r#"
        connector Pin
            Real v;
        end Pin;
        connector Plug
            parameter Integer m = 3;
            Pin pin[m];
        end Plug;
        model Test
            Plug plug;
            Real values[3];
        equation
            values = plug.pin.v;
        end Test;
    "#;
    let tree = parsed_tree(source);
    let test = tree.get_class_by_qualified_name("Test").expect("Test");
    let plug = tree.get_class_by_qualified_name("Plug").expect("Plug");
    let pin = tree.get_class_by_qualified_name("Pin").expect("Pin");
    let mut overlay = InstanceOverlay::new();
    add_instanced_component(
        &mut overlay,
        "Test.plug",
        test.components.get("plug").expect("plug"),
        false,
    );
    add_instanced_component(
        &mut overlay,
        "Test.values",
        test.components.get("values").expect("values"),
        true,
    );
    overlay
        .components
        .values_mut()
        .find(|data| data.qualified_name.to_flat_string() == "Test.values")
        .expect("values instance")
        .dims = vec![3];
    for index in 1..=3 {
        add_instanced_component(
            &mut overlay,
            &format!("Test.plug.pin[{index}]"),
            plug.components.get("pin").expect("pin"),
            false,
        );
        add_instanced_component(
            &mut overlay,
            &format!("Test.plug.pin[{index}].v"),
            pin.components.get("v").expect("v"),
            true,
        );
    }
    typecheck_instanced(&tree, &mut overlay, "Test")
        .expect("an unsubscripted array component contributes its domain to member access");
}

/// MLS §10.4.1: `ports.c` has shape `size(ports)` ++ `size(c)`. The two
/// extents here are deliberately equal so a member row cannot be mistaken for
/// a row that already carries the owner's domain.
#[test]
fn member_access_shape_repeats_an_extent_that_matches_the_owner_domain() {
    let source = r#"
        connector Port
            Real c[2];
        end Port;
        model Test
            Port ports[2];
            Real values[2, 2];
        equation
            ports.c = values;
        end Test;
    "#;
    let tree = parsed_tree(source);
    let test = tree.get_class_by_qualified_name("Test").expect("Test");
    let port = tree.get_class_by_qualified_name("Port").expect("Port");
    let mut overlay = InstanceOverlay::new();
    add_instanced_component(
        &mut overlay,
        "Test.values",
        test.components.get("values").expect("values"),
        true,
    );
    for index in 1..=2 {
        add_instanced_component(
            &mut overlay,
            &format!("Test.ports[{index}]"),
            test.components.get("ports").expect("ports"),
            false,
        );
        add_instanced_component(
            &mut overlay,
            &format!("Test.ports[{index}].c"),
            port.components.get("c").expect("c"),
            true,
        );
    }
    typecheck_instanced(&tree, &mut overlay, "Test")
        .expect("every reference part contributes its own declared extents");
}

/// MLS §10.5.1 subscript arity is measured against the same composed shape
/// MLS §10.4.1 gives the reference. The owner and member extents are equal
/// here so a member row cannot be mistaken for an owner-inclusive row.
#[test]
fn subscript_arity_uses_the_composed_owner_and_member_extents() {
    let source = r#"
        connector Port
            Real c[2];
        end Port;
        model Test
            Port ports[2];
            Real value;
        equation
            value = ports.c[1, 2];
        end Test;
    "#;
    let resolved = resolve(parse(source)).expect("resolve should succeed");
    let mut instanced = rumoca_phase_instantiate::instantiate(resolved, "Test")
        .expect("instantiate should succeed");

    typecheck_instanced(&instanced.tree, &mut instanced.overlay, "Test")
        .expect("two subscripts address the composed two-dimensional reference shape");
}

#[test]
fn unknown_array_extent_is_not_treated_as_scalar_during_subscript_validation() {
    let source = r#"
        model Test
            input Real values[:];
            output Real first;
        equation
            first = values[1];
        end Test;
    "#;
    let tree = parsed_tree(source);
    let mut overlay = InstanceOverlay::new();
    add_model_components(&tree, &mut overlay, "Test", &["values", "first"]);

    typecheck_instanced(&tree, &mut overlay, "Test")
        .expect("an unresolved array extent still carries a non-scalar shape contract");
}

#[test]
fn colon_dimension_with_literal_suffix_is_not_treated_as_lower_rank() {
    let source = r#"
        model Test
            input Real lines[:, 2, 2];
            output Real first;
        equation
            first = lines[1, 2, 1];
        end Test;
    "#;
    let tree = parsed_tree(source);
    let mut overlay = InstanceOverlay::new();
    add_model_components(&tree, &mut overlay, "Test", &["lines", "first"]);

    typecheck_instanced(&tree, &mut overlay, "Test")
        .expect("a `[:, 2, 2]` declaration has unknown extent, not rank two");
}

#[test]
fn qualified_package_array_absent_from_instance_overlay_is_not_treated_as_scalar() {
    let source = r#"
        package Tables
            constant Integer values[2] = {1, 2};
        end Tables;
        model Test
            Integer first;
        equation
            first = Tables.values[1];
        end Test;
    "#;
    let tree = parsed_tree(source);
    let mut overlay = InstanceOverlay::new();
    add_model_components(&tree, &mut overlay, "Test", &["first"]);

    typecheck_instanced(&tree, &mut overlay, "Test")
        .expect("package constants outside the instance overlay retain their declared array rank");
}
