//! Modelica `min`/`max` builtin arity and scalar/array argument contracts.

use super::*;

fn instanced_diagnostics(source: &str, model_name: &str) -> Result<(), rumoca_core::Diagnostics> {
    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let tree = resolved.into_inner();
    let model = tree
        .definitions
        .classes
        .get(model_name)
        .expect("test model");
    let mut overlay = InstanceOverlay::new();
    for (name, component) in &model.components {
        add_test_instance(&mut overlay, name, component, component.binding.clone());
    }
    typecheck_instanced(&tree, &mut overlay, model_name)
}

#[test]
fn min_max_accept_two_scalars_and_one_array_of_multiple_ranks() {
    instanced_diagnostics(
        r#"
        model Accepted
            parameter Real scalar_min = min(1.0, 2.0);
            parameter Real scalar_max = max(1.0, 2.0);
            parameter Real vector[2] = {1.0, 2.0};
            parameter Real vector_min = min(vector);
            parameter Real matrix[2, 2] = [1.0, 2.0; 3.0, 4.0];
            parameter Real matrix_max = max(matrix);
            parameter Real tensor[2, 2, 2];
            parameter Real tensor_min = min(tensor);
            parameter Real reduction_min = min(vector[i] for i in 1:2);
            parameter Real reduction_max = max(abs(vector[i]) for i in 1:2);
            parameter Integer integer_min = min(1, 2);
            parameter Boolean boolean_max = max(true, false);
            type E = enumeration(a, b);
            parameter E enumeration_min = min(E.a, E.b);
        end Accepted;
        "#,
        "Accepted",
    )
    .expect("scalar pairs, arrays of any rank and reductions must typecheck");
}

#[test]
fn min_max_reject_invalid_arity_and_known_argument_shapes() {
    let diagnostics = instanced_diagnostics(
        r#"
        model Invalid
            Real scalar;
            Real vector[2];
            Real other_vector[3];
            Real matrix[2, 2];
            Real other_matrix[2, 3];
            Real min_zero = min();
            Real max_zero = max();
            Real min_three = min(1.0, 2.0, 3.0);
            Real max_three = max(1.0, 2.0, 3.0);
            Real min_scalar = min(scalar);
            Real max_scalar = max(scalar);
            Real min_arrays[2] = min(vector, other_vector);
            Real max_arrays[2, 2] = max(matrix, vector);
            Real min_matrix[2, 2] = min(matrix, other_matrix);
            Real max_vectors[3] = max(other_vector, vector);
        end Invalid;
        "#,
        "Invalid",
    )
    .expect_err("invalid builtin arities and shapes must produce diagnostics");

    assert_eq!(
        diagnostics
            .iter()
            .filter(|diagnostic| diagnostic.message.contains("expects 1..2 argument(s)"))
            .count(),
        4,
        "both builtins must reject zero and three arguments: {diagnostics:?}"
    );
    assert_eq!(
        diagnostics
            .iter()
            .filter(|diagnostic| diagnostic
                .message
                .contains("with one argument requires an array"))
            .count(),
        2,
        "both builtins must reject a known scalar in the one-argument form: {diagnostics:?}"
    );
    assert_eq!(
        diagnostics
            .iter()
            .filter(|diagnostic| diagnostic
                .message
                .contains("array argument shapes must match"))
            .count(),
        4,
        "both builtins must reject mismatched known array shapes: {diagnostics:?}"
    );
}

#[test]
fn min_max_vectorize_matching_arrays_and_broadcast_scalars() {
    instanced_diagnostics(
        r#"
        model Vectorized
            parameter Real left[2] = {1.0, 4.0};
            parameter Real right[2] = {2.0, 3.0};
            parameter Real pairs[2] = min(left, right);
            parameter Real scalar_left[2] = max(2.0, left);
            parameter Real scalar_right[2] = min(right, 2.0);
            parameter Real matrix[2, 2] = [1.0, 2.0; 3.0, 4.0];
            parameter Real matrix_pairs[2, 2] = max(matrix, matrix);
            parameter Real tensor[2, 2, 2];
            parameter Real tensor_scalar[2, 2, 2] = min(tensor, 2.0);
            parameter Real nested[2] = max(min(left, right), 2.0);
            parameter Real reduced = min(max(left, right));
            parameter Integer integers[2] = min({1, 4}, {2, 3});
            parameter Boolean booleans[2] = max({true, false}, false);
            type E = enumeration(a, b);
            parameter E enumerations[2] = min({E.a, E.b}, E.b);
        end Vectorized;
        "#,
        "Vectorized",
    )
    .expect("MLS 3.7 scalar-pair min/max vectorize over matching arrays and scalar operands");
}

#[test]
fn min_max_nested_scalar_result_is_not_an_array_reduction_argument() {
    let diagnostics = instanced_diagnostics(
        r#"
        model NestedScalar
            Real result = min(max(1.0, 2.0));
        end NestedScalar;
        "#,
        "NestedScalar",
    )
    .expect_err("a scalar-pair result remains scalar when passed to a one-argument reduction");
    assert!(diagnostics.iter().any(|diagnostic| {
        diagnostic
            .message
            .contains("with one argument requires an array")
    }));
}

#[test]
fn min_max_keep_existing_string_checks() {
    let string_diagnostics = instanced_diagnostics(
        r#"
        model StringArguments
            String min_value = min("a", "b");
            String max_value = max("a", "b");
        end StringArguments;
        "#,
        "StringArguments",
    )
    .expect_err("String arguments remain invalid");
    assert!(
        string_diagnostics.iter().any(|diagnostic| diagnostic
            .message
            .contains("min() is not defined for String values"))
            && string_diagnostics.iter().any(|diagnostic| diagnostic
                .message
                .contains("max() is not defined for String values")),
        "the existing String restrictions must remain for both builtins: {string_diagnostics:?}"
    );
}

#[test]
fn resolved_custom_min_and_qualified_max_keep_their_own_signatures() {
    instanced_diagnostics(
        r#"
        function min
            input Real first;
            input Real second;
            input Real third;
            output Real result;
        algorithm
            result := first + second + third;
        end min;
        package Custom
            function min
                input Real first;
                input Real second;
                input Real third;
                output Real result;
            algorithm
                result := first + second + third;
            end min;
            function max
                input Real first;
                input Real second;
                input Real third;
                output Real result;
            algorithm
                result := first + second + third;
            end max;
        end Custom;
        model CustomCalls
            Real shadowed = min(1.0, 2.0, 3.0);
            Real qualified_min = Custom.min(1.0, 2.0, 3.0);
            Real qualified = Custom.max(1.0, 2.0, 3.0);
        end CustomCalls;
        "#,
        "CustomCalls",
    )
    .expect("resolved same-name and qualified functions retain their signatures");
}

#[test]
fn min_max_do_not_guess_shapes_from_qualified_user_function_names() {
    instanced_diagnostics(
        r#"
        function sum
            input Real values[2];
            output Real result[2];
        algorithm
            result := values;
        end sum;
        package Custom
            function cross
                input Real left;
                input Real right;
                output Real result;
            algorithm
                result := left + right;
            end cross;
        end Custom;
        model UserFunctionShapes
            Real array_result = min(sum({1.0, 2.0}));
            Real scalar_result = max(Custom.cross(1.0, 2.0), 3.0);
        end UserFunctionShapes;
        "#,
        "UserFunctionShapes",
    )
    .expect("resolved user-function result shapes must not be guessed from their names");
}
