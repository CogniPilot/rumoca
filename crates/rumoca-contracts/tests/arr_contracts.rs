//! ARR (Array) contract tests - MLS §10
//!
//! Tests for the 40 array contracts defined in SPEC_0022.

use rumoca_contracts::test_support::{expect_balanced, expect_success};

// =============================================================================
// ARR-001: Fixed dimensionality
// "Number of dimensions is fixed and cannot be changed at run-time"
// =============================================================================

#[test]
fn arr_001_fixed_dimensionality() {
    expect_success(
        r#"
        model Test
            Real x[3];
        equation
            x = {1, 2, 3};
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// ARR-002: Rectangular arrays
// "All dimensions must be uniform (no ragged arrays)"
// =============================================================================

#[test]
fn arr_002_rectangular_2d() {
    expect_success(
        r#"
        model Test
            Real x[2, 3];
        equation
            x = {{1, 2, 3}, {4, 5, 6}};
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// ARR-003: Size matching
// "Binary operations require matching sizes or scalar operands"
// =============================================================================

#[test]
fn arr_003_matching_sizes() {
    expect_success(
        r#"
        model Test
            Real x[3];
            Real y[3];
            Real z[3];
        equation
            x = {1, 2, 3};
            y = {4, 5, 6};
            z = x + y;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// ARR-005: Index bounds
// "Array dimension indexed by Integer has lower bound 1, upper bound = size"
// =============================================================================

#[test]
fn arr_005_valid_index() {
    expect_success(
        r#"
        model Test
            Real x[5];
            Real y;
        equation
            x = {1, 2, 3, 4, 5};
            y = x[3];
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// ARR-009: Integer to Real coercion
// "Integer expression automatically converted to Real in Real context"
// =============================================================================

#[test]
fn arr_009_int_to_real_coercion() {
    expect_balanced(
        r#"
        model Test
            Real x;
        equation
            x = 1 + 2.0;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// ARR-019: linspace n >= 2
// "linspace(x1, x2, n): required that n >= 2"
// =============================================================================

#[test]
fn arr_019_linspace_valid() {
    expect_success(
        r#"
        model Test
            Real x[5];
        equation
            x = linspace(0, 1, 5);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// ARR-027: Equality same dimensions
// "Equality/assignment require both objects to have same number of dimensions and sizes"
// =============================================================================

#[test]
fn arr_027_same_dim_assignment() {
    expect_success(
        r#"
        model Test
            Real x[3];
            Real y[3];
        equation
            x = {1, 2, 3};
            y = x;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// ARR-029: Add/sub same size
// "Addition/subtraction require size(a) = size(b) and numeric type"
// =============================================================================

#[test]
fn arr_029_add_same_size() {
    expect_success(
        r#"
        model Test
            Real x[3];
            Real y[3];
            Real z[3];
        equation
            x = {1, 2, 3};
            y = {4, 5, 6};
            z = x + y;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// ARR-030: Division by scalar
// "Division a / s: a is numeric scalar/vector/matrix/array, s is numeric scalar"
// =============================================================================

#[test]
fn arr_030_divide_by_scalar() {
    expect_success(
        r#"
        model Test
            Real x[3];
            Real y[3];
        equation
            x = {2, 4, 6};
            y = x / 2.0;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// Array integration tests
// =============================================================================

#[test]
fn arr_for_loop_array() {
    expect_success(
        r#"
        model Test
            parameter Integer n = 5;
            Real x[n];
        equation
            for i in 1:n loop
                x[i] = i * 1.0;
            end for;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn arr_array_of_parameters() {
    expect_success(
        r#"
        model Test
            parameter Real p[3] = {1, 2, 3};
            Real x[3];
        equation
            x = p;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn arr_scalar_broadcast() {
    expect_success(
        r#"
        model Test
            Real x[3];
        equation
            x = {1, 1, 1} * 2.0;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn arr_zeros_ones() {
    expect_success(
        r#"
        model Test
            Real x[3];
            Real y[2, 2];
        equation
            x = zeros(3);
            y = ones(2, 2);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn arr_fill_function() {
    expect_success(
        r#"
        model Test
            Real x[3];
        equation
            x = fill(0.5, 3);
        end Test;
    "#,
        "Test",
    );
}
