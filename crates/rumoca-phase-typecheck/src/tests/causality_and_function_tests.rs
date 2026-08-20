//! Causality validation on declarations and argument compatibility for
//! user-defined and inherited function calls.

use super::*;

#[test]
fn test_causality_validation_input_with_binding() {
    // MLS §4.8 requires bindings on non-connector model/block inputs.
    let source = r#"
        connector RealInput = input Real;
        model Test
            RealInput u = 1.0;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");

    let result = typecheck(resolved);
    assert!(result.is_ok(), "an input declaration binding is valid");
}

#[test]
fn test_causality_validation_output_valid() {
    // Test that output with binding is valid
    let source = r#"
        connector RealOutput = output Real;
        model Test
            RealOutput y = 1.0;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(result.is_ok(), "typecheck should succeed");
}

#[test]
fn user_function_accepts_record_subtype_argument() {
    let source = r#"
        record BaseState
            Real x;
        end BaseState;
        record ConcreteState
            extends BaseState;
            Real y;
        end ConcreteState;
        function value
            input BaseState state;
            output Real result;
        algorithm
            result := state.x;
        end value;
        model Test
            ConcreteState state;
            Real result = value(state);
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    typecheck(resolved).expect("a record subtype should satisfy its base function input");
}

#[test]
fn inherited_function_accepts_redeclared_replaceable_record_slot() {
    let source = r#"
        package Interface
            replaceable record State
                Real x;
            end State;
            function value
                input State state;
                output Real result;
            algorithm
                result := state.x;
            end value;
        end Interface;
        package Implementation
            extends Interface;
            redeclare record extends State
                Real y;
            end State;
        end Implementation;
        model Test
            Implementation.State state;
            Real result = Implementation.value(state);
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    typecheck(resolved)
        .expect("a redeclared record slot should satisfy its inherited function signature");
}

#[test]
fn instanced_function_accepts_redeclared_replaceable_record_slot() {
    let source = r#"
        package Interface
            replaceable record State
                Real x;
            end State;
            function value
                input State state;
                output Real result;
            algorithm
                result := state.x;
            end value;
        end Interface;
        package Implementation
            extends Interface;
            redeclare record extends State
                Real y;
            end State;
        end Implementation;
        model Test
            Implementation.State state;
            Real result = Implementation.value(state);
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let tree = resolved.into_inner();
    let test = tree
        .definitions
        .classes
        .get("Test")
        .expect("Test class should exist");
    let mut overlay = InstanceOverlay::new();
    for (name, component) in &test.components {
        add_test_instance(&mut overlay, name, component, component.binding.clone());
    }

    typecheck_instanced(&tree, &mut overlay, "Test")
        .expect("instanced checking must preserve replaceable record-slot compatibility");
}
