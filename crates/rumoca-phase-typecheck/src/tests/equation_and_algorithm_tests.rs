//! Equation and algorithm-section checking: assignment compatibility,
//! operator/member diagnostics, and user-defined equation compatibility.

use super::*;

#[test]
fn test_equation_typecheck() {
    let source = r#"
        model Test
            Real x;
            Real y;
        equation
            x = y + 1;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(result.is_ok());
}

#[test]
fn test_builtin_numeric_assignment_conversion_is_directional() {
    let accepted = typecheck_diagnostics(
        r#"
        model Accepted
            Real r;
            Integer i;
        algorithm
            r := i;
        end Accepted;
        "#,
    );
    assert!(
        accepted
            .iter()
            .all(|diagnostic| diagnostic.code.as_deref() != Some("ET002")),
        "Integer-to-Real assignment should be accepted: {accepted:?}"
    );

    let rejected = typecheck_diagnostics(
        r#"
        model Rejected
            Real r;
            Integer i;
        algorithm
            i := r;
        end Rejected;
        "#,
    );
    assert!(
        rejected
            .iter()
            .any(|diagnostic| diagnostic.code.as_deref() == Some("ET002")),
        "Real-to-Integer assignment must be rejected: {rejected:?}"
    );
}

#[test]
fn test_clock_is_not_assignment_compatible_with_real() {
    let diagnostics = typecheck_diagnostics(
        r#"
        model Test
            Clock c;
            Real r;
        equation
            c = r;
        end Test;
        "#,
    );

    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code.as_deref() == Some("ET002")),
        "Clock and Real must remain distinct builtin types: {diagnostics:?}"
    );
}

#[test]
fn test_algorithm_typecheck() {
    let source = r#"
        model Test
            Real x;
            Real y;
        algorithm
            x := y + 1;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(result.is_ok());
}

// Unknown dotted record/operator-record members never reach typecheck: resolve
// rejects them as `ER002` unresolved component references. Those cases are
// covered by `rumoca-phase-resolve`'s `tests::component_lookup`, which owns the
// `ER0xx` mnemonics (SPEC_0008 "Error Code Ranges").

#[test]
fn test_user_defined_equation_compatibility() {
    let source = r#"
        type Mode = enumeration(Off, On);
        record Payload
            Real x;
        end Payload;

        model Test
            Mode m1;
            Mode m2;
            Payload p1;
            Payload p2;
        equation
            m1 = m2;
            p1 = p2;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(
        result.is_ok(),
        "same enum/record types should be compatible"
    );
}

#[test]
fn test_equation_shape_mismatch_detection() {
    let source = r#"
        model Test
            Real lhs[2];
            Real rhs[3];
        equation
            lhs = rhs;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(
        result.is_err(),
        "same root type with different array dimensions should mismatch"
    );

    let diags = result.expect_err("expected diagnostics");
    assert!(
        diags.iter().any(|d| d.code.as_deref() == Some("ET002")
            && d.message.contains("array dimension mismatch")),
        "expected ET002 shape mismatch diagnostic, got: {:?}",
        diags
    );
}

#[test]
fn ragged_array_literal_is_invalid_instead_of_unknown() {
    let diagnostics = typecheck_diagnostics(
        r#"
        model Ragged
            Real lhs[2, 2];
        equation
            lhs = {{1.0, 2.0}, {3.0}};
        end Ragged;
        "#,
    );

    assert!(
        diagnostics.iter().any(|diagnostic| {
            diagnostic.code.as_deref() == Some("ET002")
                && diagnostic.message.contains("ragged array literal rows")
        }),
        "ragged known-invalid shape must produce ET002: {diagnostics:?}"
    );
}

#[test]
fn incompatible_matrix_product_is_invalid_instead_of_unknown() {
    let diagnostics = typecheck_diagnostics(
        r#"
        model BadProduct
            Real lhs[2, 2];
            Real a[2, 3];
            Real b[4, 2];
        equation
            lhs = a * b;
        end BadProduct;
        "#,
    );

    assert!(
        diagnostics.iter().any(|diagnostic| {
            diagnostic.code.as_deref() == Some("ET002")
                && diagnostic.message.contains("matrix product dimensions")
        }),
        "known-incompatible matrix product must produce ET002: {diagnostics:?}"
    );
}

#[test]
fn incompatible_binary_operands_are_invalid_instead_of_unknown() {
    let diagnostics = typecheck_diagnostics(
        r#"
        model BadBinary
            Real lhs[2];
            Real a[2];
            Real b[3];
        equation
            lhs = a + b;
        end BadBinary;
        "#,
    );

    assert!(
        diagnostics.iter().any(|diagnostic| {
            diagnostic.code.as_deref() == Some("ET002")
                && diagnostic.message.contains("binary operand dimensions")
        }),
        "known-incompatible binary operands must produce ET002: {diagnostics:?}"
    );
}

#[test]
fn addition_does_not_invent_scalar_broadcasting() {
    let diagnostics = typecheck_diagnostics(
        r#"
        model BadBroadcast
            Real lhs[2];
            Real rhs[2];
        equation
            lhs = 1.0 + rhs;
        end BadBroadcast;
        "#,
    );

    assert!(
        diagnostics.iter().any(|diagnostic| {
            diagnostic.code.as_deref() == Some("ET002")
                && diagnostic.message.contains("binary operand dimensions")
        }),
        "ARR-029 requires equal addition operand shapes: {diagnostics:?}"
    );
}

#[test]
fn algorithm_assignment_rejects_known_invalid_shape() {
    let diagnostics = typecheck_diagnostics(
        r#"
        model BadAlgorithmProduct
            Real lhs[2, 2];
            Real a[2, 3];
            Real b[4, 2];
        algorithm
            lhs := a * b;
        end BadAlgorithmProduct;
        "#,
    );

    assert!(
        diagnostics.iter().any(|diagnostic| {
            diagnostic.code.as_deref() == Some("ET002")
                && diagnostic.message.contains("matrix product dimensions")
        }),
        "algorithm assignment must reject a known-invalid product: {diagnostics:?}"
    );
}

#[test]
fn algorithm_assignment_rejects_known_dimension_mismatch() {
    let diagnostics = typecheck_diagnostics(
        r#"
        model BadAlgorithmAssignment
            Real lhs[2];
            Real rhs[3];
        algorithm
            lhs := rhs;
        end BadAlgorithmAssignment;
        "#,
    );

    assert!(
        diagnostics.iter().any(|diagnostic| {
            diagnostic.code.as_deref() == Some("ET002")
                && diagnostic.message.contains("expected `[2]`, found `[3]`")
        }),
        "algorithm assignment dimensions must be checked: {diagnostics:?}"
    );
}

#[test]
fn algorithm_condition_rejects_known_operand_shape_mismatch() {
    let diagnostics = typecheck_diagnostics(
        r#"
        model BadAlgorithmCondition
            Real lhs[2];
            Real rhs[3];
            Real value;
        algorithm
            if lhs == rhs then
                value := 1.0;
            end if;
        end BadAlgorithmCondition;
        "#,
    );

    assert!(
        diagnostics.iter().any(|diagnostic| {
            diagnostic.code.as_deref() == Some("ET002")
                && diagnostic.message.contains("binary operand dimensions")
        }),
        "a control-condition expression must not hide invalid shape: {diagnostics:?}"
    );
}

#[test]
fn user_function_call_rejects_known_input_shape_mismatch() {
    let diagnostics = typecheck_diagnostics(
        r#"
        function takeTwo
            input Real x[2];
            output Real y;
        algorithm
            y := x[1] + x[2];
        end takeTwo;

        model BadCall
            Real values[3];
            Real y;
        equation
            y = takeTwo(values);
        end BadCall;
        "#,
    );

    assert!(
        diagnostics.iter().any(|diagnostic| {
            diagnostic.code.as_deref() == Some("ET002")
                && diagnostic.message.contains("array dimension mismatch")
        }),
        "known function-input dimensions must be checked: {diagnostics:?}"
    );
}

#[test]
fn named_function_argument_cannot_hide_a_ragged_shape() {
    let diagnostics = typecheck_diagnostics(
        r#"
        function consumeMatrix
            input Real x[2, 2];
            output Real y;
        algorithm
            y := x[1, 1];
        end consumeMatrix;

        model BadNamedArgument
            Real y;
        equation
            y = consumeMatrix(x = {{1.0, 2.0}, {3.0}});
        end BadNamedArgument;
        "#,
    );

    assert!(
        diagnostics.iter().any(|diagnostic| {
            diagnostic.code.as_deref() == Some("ET002")
                && diagnostic.message.contains("ragged array literal rows")
        }),
        "a named-argument wrapper must preserve invalid shape: {diagnostics:?}"
    );
}

#[test]
fn array_index_cannot_hide_an_invalid_base_shape() {
    let diagnostics = typecheck_diagnostics(
        r#"
        model BadIndexedProduct
            Real lhs;
            Real a[2, 3];
            Real b[4, 2];
        equation
            lhs = (a * b)[1];
        end BadIndexedProduct;
        "#,
    );

    assert!(
        diagnostics.iter().any(|diagnostic| {
            diagnostic.code.as_deref() == Some("ET002")
                && diagnostic.message.contains("matrix product dimensions")
        }),
        "an array-index wrapper must preserve invalid base shape: {diagnostics:?}"
    );
}

#[test]
fn user_function_call_preserves_known_output_shape() {
    let diagnostics = typecheck_diagnostics(
        r#"
        function pair
            output Real y[2];
        algorithm
            y := {1.0, 2.0};
        end pair;

        model BadReturn
            Real lhs[3];
        equation
            lhs = pair();
        end BadReturn;
        "#,
    );

    assert!(
        diagnostics.iter().any(|diagnostic| {
            diagnostic.code.as_deref() == Some("ET002")
                && diagnostic.message.contains("expected `[3]`, found `[2]`")
        }),
        "known function-output dimensions must reach equation checking: {diagnostics:?}"
    );
}

#[test]
fn user_function_shape_is_not_captured_by_builtin_spelling() {
    let diagnostics = typecheck_diagnostics(
        r#"
        function sum
            input Real value;
            output Real result[2];
        algorithm
            result := {value, value};
        end sum;

        model UserSum
            Real lhs[2];
        equation
            lhs = sum(1.0);
        end UserSum;
        "#,
    );

    assert!(
        diagnostics
            .iter()
            .all(|diagnostic| diagnostic.code.as_deref() != Some("ET002")),
        "resolved user-function identity must outrank builtin spelling: {diagnostics:?}"
    );
}

#[test]
fn unknown_shape_does_not_fabricate_a_mismatch() {
    let diagnostics = typecheck_diagnostics(
        r#"
        model DeferredExtent
            parameter Integer n;
            Real deferred[n];
            Real known[2];
        equation
            deferred = known;
        end DeferredExtent;
        "#,
    );

    assert!(
        diagnostics
            .iter()
            .all(|diagnostic| diagnostic.code.as_deref() != Some("ET002")),
        "an unevaluated extent is unknown, not scalar or invalid: {diagnostics:?}"
    );
}

#[test]
fn scalar_promotion_and_matching_zero_extent_remain_valid() {
    let diagnostics = typecheck_diagnostics(
        r#"
        model ValidShapes
            Real zeroLeft[0];
            Real zeroRight[0];
            Real lhs[2];
            Real rhs[2];
        equation
            zeroLeft = zeroRight;
            lhs = 2.0 * rhs;
        end ValidShapes;
        "#,
    );

    assert!(
        diagnostics
            .iter()
            .all(|diagnostic| diagnostic.code.as_deref() != Some("ET002")),
        "valid scalar promotion and zero extents must be accepted: {diagnostics:?}"
    );
}

#[test]
fn scalar_function_vectorization_remains_deferred_to_its_owner() {
    let diagnostics = typecheck_diagnostics(
        r#"
        function twice
            input Real value;
            output Real result;
        algorithm
            result := 2.0 * value;
        end twice;

        model VectorizedCall
            Real inputValues[2];
            Real outputValues[2];
        equation
            outputValues = twice(inputValues);
        end VectorizedCall;
        "#,
    );

    assert!(
        diagnostics
            .iter()
            .all(|diagnostic| diagnostic.code.as_deref() != Some("ET002")),
        "typecheck must not reject legal automatic vectorization: {diagnostics:?}"
    );
}

#[test]
fn array_formal_vectorization_remains_deferred_to_its_owner() {
    let diagnostics = typecheck_diagnostics(
        r#"
        function copyPair
            input Real value[2];
            output Real result[2];
        algorithm
            result := value;
        end copyPair;

        model VectorizedArrayCall
            Real inputValues[3, 2];
            Real outputValues[3, 2];
        equation
            outputValues = copyPair(inputValues);
        end VectorizedArrayCall;
        "#,
    );

    assert!(
        diagnostics
            .iter()
            .all(|diagnostic| diagnostic.code.as_deref() != Some("ET002")),
        "leading dimensions may vectorize an array formal: {diagnostics:?}"
    );
}

#[test]
fn test_user_defined_equation_mismatch_detection() {
    let source = r#"
        type ModeA = enumeration(Off, On);
        type ModeB = enumeration(Off, On);
        record PayloadA
            Real x;
        end PayloadA;
        record PayloadB
            Real x;
        end PayloadB;

        model Test
            ModeA m1;
            ModeB m2;
            PayloadA p1;
            PayloadB p2;
        equation
            m1 = m2;
            p1 = p2;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(
        result.is_err(),
        "different enum/record types should mismatch"
    );

    let diags = result.expect_err("expected diagnostics");
    let et002_count = diags
        .iter()
        .filter(|d| d.code.as_deref() == Some("ET002"))
        .count();
    assert!(
        et002_count >= 2,
        "expected ET002 diagnostics for enum and record equation mismatch"
    );
}
