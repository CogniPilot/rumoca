//! ARR (Array) contract tests - MLS §10
//!
//! Tests for the 40 array contracts defined in SPEC_0022.

use rumoca_contracts::test_support::{
    expect_balanced, expect_failure_in_phase, expect_failure_in_phase_with_code,
    expect_resolve_failure_with_code, expect_success,
};
use rumoca_session::FailedPhase;

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
// ARR TODO coverage
// =============================================================================

#[test]
#[ignore = "TODO(ARR-004): enforce MLS array contract"]
fn arr_004_slice_uniformity() {
    expect_failure_in_phase_with_code(
        r#"
        record R
            Real v[2];
        end R;

        model Test
            R r[2];
            Real y[2];
        equation
            y = r.v[1];
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

#[test]
fn arr_006_constructor_non_empty() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            Real x[1];
        equation
            x = array();
        end Test;
    "#,
        "Test",
        "ER002",
    );
}

#[test]
#[ignore = "TODO(ARR-007): enforce MLS array contract"]
fn arr_007_concatenation_dimensions() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real x[2,2];
        equation
            x = cat(1, {1,2}, {{3,4}});
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

#[test]
#[ignore = "TODO(ARR-008): enforce MLS array contract"]
fn arr_008_concatenation_size_match() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real x[2,3];
        equation
            x = cat(1, {{1,2}}, {{3,4,5}});
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

#[test]
fn arr_010_promote_n_ge_ndims() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            Real x[2];
            Real y[2];
        equation
            x = {1, 2};
            y = promote(x, 0);
        end Test;
    "#,
        "Test",
        "ER002",
    );
}

#[test]
fn arr_011_promote_n_constant() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            Real x[2];
            Real y[2];
        equation
            x = {1, 2};
            y = promote(x, integer(time));
        end Test;
    "#,
        "Test",
        "ER002",
    );
}

#[test]
#[ignore = "TODO(ARR-012): enforce MLS array contract"]
fn arr_012_size_i_bounds() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real x[2];
            Integer n;
        equation
            x = {1, 2};
            n = size(x, 3);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

#[test]
fn arr_013_size_expandable_decl() {
    expect_failure_in_phase(
        r#"
        expandable connector EC
            Real a[:];
        end EC;

        model Test
            EC e;
            Integer n;
        equation
            n = size(e.b);
        end Test;
    "#,
        "Test",
        FailedPhase::Typecheck,
    );
}

#[test]
#[ignore = "TODO(ARR-014): enforce MLS array contract"]
fn arr_014_scalar_all_size_1() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real x[2,2];
            Real y;
        equation
            x = {{1,2},{3,4}};
            y = scalar(x);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

#[test]
#[ignore = "TODO(ARR-015): enforce MLS array contract"]
fn arr_015_vector_at_most_one_gt_1() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real x[2,2];
            Real y[4];
        equation
            x = {{1,2},{3,4}};
            y = vector(x);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

#[test]
#[ignore = "TODO(ARR-016): enforce MLS array contract"]
fn arr_016_matrix_trailing_size_1() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real x[2,2,2];
            Real y[2,2];
        equation
            x = ones(2,2,2);
            y = matrix(x);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

#[test]
#[ignore = "TODO(ARR-017): enforce MLS array contract"]
fn arr_017_zeros_ones_non_empty() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real x[1];
        equation
            x = zeros();
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

#[test]
#[ignore = "TODO(ARR-018): enforce MLS array contract"]
fn arr_018_fill_at_least_two_args() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real x[1];
        equation
            x = fill(1.0);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

#[test]
fn arr_020_array_type_compatible() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            Real x[2];
        equation
            x = array(1, "a");
        end Test;
    "#,
        "Test",
        "ER002",
    );
}

#[test]
#[ignore = "TODO(ARR-021): enforce MLS array contract"]
fn arr_021_cat_k_existing_dim() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real x[2];
        equation
            x = cat(3, {1}, {2});
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

#[test]
#[ignore = "TODO(ARR-022): enforce MLS array contract"]
fn arr_022_cat_k_parameter() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real x[2];
        equation
            x = cat(integer(time), {1}, {2});
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

#[test]
#[ignore = "TODO(ARR-023): enforce MLS array contract"]
fn arr_023_concat_non_empty() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real x[1];
        equation
            x = cat(1);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

#[test]
#[ignore = "TODO(ARR-024): enforce MLS array contract"]
fn arr_024_index_bounds_assertion() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real x[2];
            Integer i;
            Real y;
        equation
            x = {1, 2};
            i = integer(time) + 1;
            y = x[i];
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

#[test]
#[ignore = "TODO(ARR-025): enforce MLS array contract"]
fn arr_025_index_type_match() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real x[2];
            Real y;
        equation
            x = {1, 2};
            y = x[true];
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

#[test]
#[ignore = "TODO(ARR-026): enforce MLS array contract"]
fn arr_026_slice_subscript_limit() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real x[2,2];
            Real y;
        equation
            x = {{1,2},{3,4}};
            y = x[1,1,1];
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

#[test]
#[ignore = "TODO(ARR-028): enforce MLS array contract"]
fn arr_028_equality_type_equivalent() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Integer x[2];
            Real y[2];
        equation
            y = {1.0, 2.0};
            x = y;
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

#[test]
#[ignore = "TODO(ARR-031): enforce MLS array contract"]
fn arr_031_power_scalar_real() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real x[2];
            Real y[2];
        equation
            x = {1, 2};
            y = x .^ x;
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

#[test]
#[ignore = "TODO(ARR-032): enforce MLS array contract"]
fn arr_032_power_exceptional_illegal() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real x;
        equation
            x = 0 ^ (-1);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

#[test]
#[ignore = "TODO(ARR-033): enforce MLS array contract"]
fn arr_033_matrix_power_square() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real a[2,3];
            Real b[2,3];
        equation
            a = ones(2,3);
            b = a ^ 2;
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

#[test]
#[ignore = "TODO(ARR-034): enforce MLS array contract"]
fn arr_034_reduction_iterator_vector() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real x;
        equation
            x = sum(i for i in 1);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

#[test]
fn arr_035_reduction_event_evaluable() {
    expect_failure_in_phase(
        r#"
        model Test
            Real x;
        equation
            x = sum(i for i in 1:integer(time));
        end Test;
    "#,
        "Test",
        FailedPhase::Flatten,
    );
}

#[test]
fn arr_036_empty_array_size_req() {
    expect_failure_in_phase(
        r#"
        model Test
            Real a[0];
            Real b[1];
        equation
            b = a + b;
        end Test;
    "#,
        "Test",
        FailedPhase::ToDae,
    );
}

#[test]
#[ignore = "TODO(ARR-037): enforce MLS array contract"]
fn arr_037_cross_skew_3vector() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real x[2];
            Real y[2];
            Real z[2];
        equation
            x = {1, 2};
            y = {3, 4};
            z = cross(x, y);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

#[test]
#[ignore = "TODO(ARR-038): enforce MLS array contract"]
fn arr_038_transpose_2d_minimum() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real x[2];
            Real y[2];
        equation
            x = {1, 2};
            y = transpose(x);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

#[test]
#[ignore = "TODO(ARR-039): enforce MLS array contract"]
fn arr_039_reduction_empty_defaults() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real x;
        equation
            x = min(fill(1.0, 0));
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

#[test]
#[ignore = "TODO(ARR-040): enforce MLS array contract"]
fn arr_040_min_max_type_restriction() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            String x;
        equation
            x = min("a", "b");
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
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
