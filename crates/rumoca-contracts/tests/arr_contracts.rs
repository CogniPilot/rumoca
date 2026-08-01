//! ARR (Array) contract tests - MLS §10
//!
//! Tests for the 40 array contracts defined in SPEC_0022.

use rumoca_compile::compile::FailedPhase;
use rumoca_contracts::test_support::{
    expect_balanced, expect_failure_in_phase_reporting_code, expect_failure_in_phase_with_code,
    expect_failure_in_phase_with_detail, expect_resolve_failure_with_code, expect_success,
};

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
// ARR-006: Constructor non-empty
// "array() or {} is not defined; there must be at least one argument"
// =============================================================================

#[test]
fn arr_006_empty_array_constructor_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            Real x[1];
        equation
            x = array();
        end Test;
    "#,
        "Test",
        "ER043",
    );
}

// =============================================================================
// ARR-017: zeros/ones non-empty
// "zeros/ones needs one or more arguments; zeros() is not legal"
// =============================================================================

#[test]
fn arr_017_zeros_or_ones_empty_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            Real x[1];
        equation
            x = zeros();
        end Test;
    "#,
        "Test",
        "ER044",
    );
}

// =============================================================================
// ARR-018: fill needs at least two args
// "fill(s, n1, n2, ...) needs two or more arguments; fill(s) is not legal"
// =============================================================================

#[test]
fn arr_018_fill_too_few_args_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            Real x[1];
        equation
            x = fill(1.0);
        end Test;
    "#,
        "Test",
        "ER045",
    );
}

// =============================================================================
// ARR-023: Concatenation non-empty
// "Concatenation syntax: there must be at least one argument ([] is not defined)"
// =============================================================================

#[test]
fn arr_023_cat_requires_arguments_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            Real x[1];
        equation
            x = cat();
        end Test;
    "#,
        "Test",
        "ER046",
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

// =============================================================================
// ARR-026: Slice subscript limit
// "Number of subscripts on m must not be greater than array dimension for m"
// =============================================================================

#[test]
fn arr_026_subscript_within_dimensions_accepted() {
    expect_success(
        r#"
        model Test
            Real a[2] = {1, 2};
            Real x(start = 0, fixed = true);
        equation
            der(x) = a[1];
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn arr_026_excess_subscripts_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real a[2] = {1, 2};
            Real x(start = 0);
        equation
            der(x) = a[1, 1];
        end Test;
    "#,
        "Test",
        FailedPhase::Typecheck,
        "ET009",
    );
}

// =============================================================================
// ARR-028: Equality type equivalent
// "The operands need to be type equivalent"
// =============================================================================

#[test]
fn arr_028_boolean_in_real_equation_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real x;
        equation
            x = true;
        end Test;
    "#,
        "Test",
        FailedPhase::Typecheck,
        "ET002",
    );
}

#[test]
fn arr_028_array_size_mismatch_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real x[2];
        equation
            x = {1, 2, 3};
        end Test;
    "#,
        "Test",
        FailedPhase::Typecheck,
        "ET002",
    );
}

// =============================================================================
// ARR-007 / ARR-008: Concatenation dimension/size requirements (MLS §10.4.2)
// =============================================================================

#[test]
fn arr_007_cat_dimension_count_mismatch_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real a[2] = {1, 2};
            Real b[2, 2] = [1, 2; 3, 4];
            Real c[4];
            Real x(start = 0);
        equation
            c = cat(1, a, b);
            der(x) = 1;
        end Test;
    "#,
        "Test",
        FailedPhase::Typecheck,
        "ET009",
    );
}

#[test]
fn arr_008_cat_size_mismatch_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real a[2, 2] = [1, 2; 3, 4];
            Real b[2, 3] = [1, 2, 3; 4, 5, 6];
            Real c[4, 2];
            Real x(start = 0);
        equation
            c = cat(1, a, b);
            der(x) = 1;
        end Test;
    "#,
        "Test",
        FailedPhase::Typecheck,
        "ET009",
    );
}

// =============================================================================
// ARR-012: size(A, i) bounds (MLS §10.3.1)
// =============================================================================

#[test]
fn arr_012_size_index_in_range_accepted() {
    expect_success(
        r#"
        model Test
            Real a[2] = {1, 2};
            Integer n = size(a, 1);
            Real x(start = 0, fixed = true);
        equation
            der(x) = n;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn arr_012_size_index_out_of_range_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real a[2] = {1, 2};
            Integer n;
            Real x(start = 0);
        equation
            n = size(a, 3);
            der(x) = n;
        end Test;
    "#,
        "Test",
        FailedPhase::Typecheck,
        "ET009",
    );
}

// =============================================================================
// ARR-014/015/016: scalar/vector/matrix conversion requirements (MLS §10.3.2)
// =============================================================================

#[test]
fn arr_014_scalar_of_non_unit_array_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real a[2] = {1, 2};
            Real s;
            Real x(start = 0);
        equation
            s = scalar(a);
            der(x) = s;
        end Test;
    "#,
        "Test",
        FailedPhase::Typecheck,
        "ET009",
    );
}

#[test]
fn arr_015_vector_of_full_matrix_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real a[2, 2] = [1, 2; 3, 4];
            Real v[4];
            Real x(start = 0);
        equation
            v = vector(a);
            der(x) = 1;
        end Test;
    "#,
        "Test",
        FailedPhase::Typecheck,
        "ET009",
    );
}

#[test]
fn arr_016_matrix_of_wide_3d_array_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real a[2, 2, 3] = fill(1, 2, 2, 3);
            Real m[2, 2];
            Real x(start = 0);
        equation
            m = matrix(a);
            der(x) = 1;
        end Test;
    "#,
        "Test",
        FailedPhase::Typecheck,
        "ET009",
    );
}

// =============================================================================
// ARR-021: cat dimension addresses an existing dimension (MLS §10.4.2)
// =============================================================================

#[test]
fn arr_021_cat_beyond_ndims_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real a[2] = {1, 2};
            Real b[2] = {3, 4};
            Real c[2];
            Real x(start = 0);
        equation
            c = cat(3, a, b);
            der(x) = 1;
        end Test;
    "#,
        "Test",
        FailedPhase::Typecheck,
        "ET009",
    );
}

// =============================================================================
// ARR-037: cross/skew require 3-vectors (MLS §10.4.7)
// =============================================================================

#[test]
fn arr_037_cross_of_3_vectors_accepted() {
    expect_success(
        r#"
        model Test
            Real a[3] = {1, 2, 3};
            Real b[3] = {4, 5, 6};
            Real c[3] = cross(a, b);
            Real x(start = 0, fixed = true);
        equation
            der(x) = c[1];
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn arr_037_cross_of_2_vectors_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real a[2] = {1, 2};
            Real b[2] = {3, 4};
            Real c[3];
            Real x(start = 0);
        equation
            c = cross(a, b);
            der(x) = 1;
        end Test;
    "#,
        "Test",
        FailedPhase::Typecheck,
        "ET009",
    );
}

// Regression: a column slice `m[:, i]` of a `[3, n]` matrix is a 3-vector, so
// `cross` must accept it. The shape walk previously dropped dimension 0 on the
// scalar subscript (yielding `[n]`) instead of dimension 1, spuriously firing
// ET009 for `n != 3`.
#[test]
fn arr_037_cross_of_matrix_column_slice_accepted() {
    expect_success(
        r#"
        model Test
            parameter Integer n = 4;
            Real m[3, n] = zeros(3, n);
            Real omega[3] = {0, 0, 1};
            Real v[3, n];
            Real x(start = 0, fixed = true);
        equation
            for i in 1:n loop
                v[:, i] = cross(omega, m[:, i]);
            end for;
            der(x) = v[1, 1];
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn arr_vector_constructor_values_reach_simulation() {
    let trace = rumoca_contracts::test_support::simulate_model(
        r#"
        model M
            Real line[3] = linspace(0.0, 1.0, 3);
            Real axis[3] = cross({1.0, 0.0, 0.0}, {0.0, 1.0, 0.0});
            Real t(start = 0, fixed = true);
        equation
            der(t) = 1;
        end M;
    "#,
        "M",
        0.1,
    );
    assert_eq!(trace.final_value("line[1]"), 0.0);
    assert_eq!(trace.final_value("line[2]"), 0.5);
    assert_eq!(trace.final_value("line[3]"), 1.0);
    assert_eq!(trace.final_value("axis[1]"), 0.0);
    assert_eq!(trace.final_value("axis[2]"), 0.0);
    assert_eq!(trace.final_value("axis[3]"), 1.0);
}

// =============================================================================
// ARR-038: transpose needs at least two dimensions (MLS §10.3.2)
// =============================================================================

#[test]
fn arr_038_transpose_of_vector_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real a[2] = {1, 2};
            Real b[2];
            Real x(start = 0);
        equation
            b = transpose(a);
            der(x) = 1;
        end Test;
    "#,
        "Test",
        FailedPhase::Typecheck,
        "ET009",
    );
}

// =============================================================================
// ARR-040: min/max type restriction (MLS §10.3.4)
// =============================================================================

#[test]
fn arr_040_min_of_strings_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            String s;
            Real x(start = 0);
        equation
            s = min("a", "b");
            der(x) = 1;
        end Test;
    "#,
        "Test",
        FailedPhase::Typecheck,
        "ET009",
    );
}

// =============================================================================
// ARR-020: All arguments of array() must be type compatible expressions
// =============================================================================

#[test]
fn arr_020_array_constructor_mixed_types_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            Real x[2];
        equation
            x = array(1.0, true);
        end M;
    "#,
        "M",
        FailedPhase::Typecheck,
        "ET009",
    );
}

// =============================================================================
// ARR-022: cat() k shall be a parameter expression of Integer type
// =============================================================================

#[test]
fn arr_022_cat_real_dimension_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            Real a[2] = {1, 2};
            Real c[4];
        equation
            c = cat(1.5, a, a);
        end M;
    "#,
        "M",
        FailedPhase::Typecheck,
        "ET009",
    );
}

// =============================================================================
// ARR-031: a .^ b requires Real or Integer operands
// =============================================================================

// Type-checking reports two distinct codes for this one construct: the generic
// operand check (`ET002`, MLS 3.7 §6.7 -- no implicit Boolean-to-Real
// conversion) and the array-operator check (`ET009`, MLS 3.7 §10.6.7). The
// summary `PhaseResult::error_code` is therefore the `ET000` multi-code
// sentinel, so this case asserts on the reported diagnostics.
#[test]
fn arr_031_elementwise_power_on_boolean_rejected() {
    expect_failure_in_phase_reporting_code(
        r#"
        model M
            Real x;
        equation
            x = true .^ 2;
        end M;
    "#,
        "M",
        FailedPhase::Typecheck,
        "ET009",
    );
}

// =============================================================================
// ARR-033: a ^ s requires square numeric matrix and scalar Integer s >= 0
// =============================================================================

#[test]
fn arr_033_matrix_power_non_square_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            Real a[2,3] = fill(1.0, 2, 3);
            Real b[2,3];
        equation
            b = a ^ 2;
        end M;
    "#,
        "M",
        FailedPhase::Typecheck,
        "ET009",
    );
}

// =============================================================================
// ARR-013: size(A) with expandable connector: component must be declared,
// must not use colon
// =============================================================================

// `b.sig` is neither declared in `Bus` nor added by any `connect`, so per MLS
// 3.7 §9.1.3 the expandable connector has no such member and the reference is
// an error; `size` then has no array to measure (MLS 3.7 §10.3.1). Type
// checking rejects the reference directly (`ET001` unknown member), so the
// model never reaches ToDae -- the earlier, named diagnostic supersedes the
// downstream `ED008` "unresolved reference" this case used to expect. The phase
// also emits a cascading `ET009`, making the summary code the `ET000`
// multi-code sentinel, so this case asserts on the reported diagnostics.
#[test]
fn arr_013_size_of_undeclared_expandable_member_rejected() {
    expect_failure_in_phase_reporting_code(
        r#"
        model M
            expandable connector Bus
            end Bus;
            Bus b;
            Integer n = size(b.sig, 1);
        end M;
    "#,
        "M",
        FailedPhase::Typecheck,
        "ET001",
    );
}

// =============================================================================
// ARR-034: Expressions in iterators shall be vector expressions
// =============================================================================

#[test]
fn arr_034_matrix_iterator_range_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            Real m[2,2] = [1, 2; 3, 4];
            Real s;
        equation
            s = sum(x for x in m);
        end M;
    "#,
        "M",
        FailedPhase::Typecheck,
        "ET009",
    );
}

// =============================================================================
// ARR-036: Size-requirements of operations must be fulfilled even if dimension
// is zero
// =============================================================================

#[test]
fn arr_036_zero_sized_array_operations_accepted() {
    expect_success(
        r#"
        model M
            Real z[0];
            Real w[0];
            Real s;
            Real t(start = 0, fixed = true);
        equation
            der(t) = 1;
            s = sum(z + w);
        end M;
    "#,
        "M",
    );
}

// =============================================================================
// ARR-039: Empty array: sum returns zeros, product returns 1
// =============================================================================

#[test]
fn arr_039_empty_array_reduction_identities() {
    let trace = rumoca_contracts::test_support::simulate_model(
        r#"
        model M
            Real z[0];
            Real s = sum(z);
            Real p = product(z);
            Real t(start = 0, fixed = true);
        equation
            der(t) = 1;
        end M;
    "#,
        "M",
        1.0,
    );
    assert_eq!(trace.final_value("s"), 0.0, "sum over empty array is 0");
    assert_eq!(trace.final_value("p"), 1.0, "product over empty array is 1");
}

// =============================================================================
// ARR-035: If event-generating, expressions inside iterators shall be evaluable
// =============================================================================

#[test]
fn arr_035_event_generating_iterator_noneval_range_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            Real v[3] = {1.0, 2.0, time};
            Real s;
        equation
            s = sum(if x > 0.5 then 1.0 else 0.0 for x in v);
        end M;
    "#,
        "M",
        "ER116",
    );
}

// =============================================================================
// ARR-010/011: promote() restrictions. The operator is not supported yet;
// every use (and therefore every violation) is rejected during resolve.
// =============================================================================

#[test]
fn arr_010_promote_below_ndims_rejected_as_unsupported() {
    expect_resolve_failure_with_code(
        r#"
        model M
            Real a[2, 3];
            Real b[2, 3] = promote(a, 1);
        end M;
    "#,
        "M",
        "ER002",
    );
}

#[test]
fn arr_011_promote_nonconstant_n_rejected_as_unsupported() {
    expect_resolve_failure_with_code(
        r#"
        model M
            Integer k(start = 2);
            Real a[2];
            Real b[2] = promote(a, k);
        end M;
    "#,
        "M",
        "ER002",
    );
}

// =============================================================================
// ARR-025: Type of index should correspond to type used for declaring
// dimension
// =============================================================================

#[test]
fn arr_025_enum_dimension_with_enum_index_accepted() {
    expect_success(
        r#"
        model M
            type E = enumeration(a, b);
            Real x[E] = {1, 2};
            Real y = x[E.a];
        end M;
    "#,
        "M",
    );
}

// =============================================================================
// ARR-026: the subscript budget of a component reference is the *declared*
// array dimension (MLS §10.5.1).
//
// A subscripted component array is not made of scalars as far as its
// declaration is concerned: `c[1]` subscripts `c`, which the model declared
// with one dimension, whatever the compiler chose to do about the elements
// underneath. Instantiation expands such an array element by element whenever a
// per-element rewrite fires — an array-valued modifier, an array binding, an
// indexed `start` — and compacts it otherwise (SPEC_0032 §1). That choice is an
// instantiation optimization and must not reach the diagnostic: every model
// below is legal Modelica (`omc checkModel` accepts each one), and each was
// rejected as `ET009 ... has 0 dimension(s)` while the declared extents were
// read off the compaction descriptor instead of off the expansion record.
// =============================================================================

#[test]
fn arr_026_element_expanded_array_is_subscriptable() {
    // `p = kk` is array-valued, so instantiation expands element by element.
    expect_success(
        r#"
        package P
            model Sub
                parameter Real p = 1;
                Real x;
            equation
                x = p*time;
            end Sub;
            model M
                parameter Real kk[3] = {1, 2, 3};
                Sub c[3](p = kk);
                Real y;
            equation
                y = c[1].x;
            end M;
        end P;
    "#,
        "P.M",
    );
}

#[test]
fn arr_026_element_expanded_array_with_parameter_extent_is_subscriptable() {
    expect_success(
        r#"
        package P
            model Sub
                parameter Real p = 1;
                Real x;
            equation
                x = p*time;
            end Sub;
            model M
                parameter Integer m = 3;
                parameter Real kk[m] = {1, 2, 3};
                Sub c[m](p = kk);
                Real y;
            equation
                y = c[m].x;
            end M;
        end P;
    "#,
        "P.M",
    );
}

#[test]
fn arr_026_element_expanded_array_member_carries_the_owner_domain() {
    // MLS §10.4.1: the shape of `c.x` is size(c) ++ size(x). An unsubscripted
    // reference must see the owner's declared extent, not a scalar element.
    expect_success(
        r#"
        package P
            model Sub
                parameter Real p = 1;
                Real x;
            equation
                x = p*time;
            end Sub;
            model M
                parameter Real kk[3] = {1, 2, 3};
                Sub c[3](p = kk);
                Real y[3];
            equation
                y = c.x;
            end M;
        end P;
    "#,
        "P.M",
    );
}

#[test]
fn arr_026_nested_member_array_under_an_element_expanded_owner_is_subscriptable() {
    // MLS §10.5: each part carries its own subscripts, so `c[1].x[2]` spends
    // one against the owner's declared extent and one against the member's.
    // The owner here is element-expanded; the member is not.
    expect_success(
        r#"
        package P
            model Inner
                parameter Real p = 1;
                parameter Integer n = 3;
                Real x[n];
            equation
                x = {p*time for k in 1:n};
            end Inner;
            model M
                parameter Integer m = 2;
                parameter Real kk[m] = {1, 2};
                Inner c[m](p = kk);
                Real y;
            equation
                y = c[1].x[2];
            end M;
        end P;
    "#,
        "P.M",
    );
}

#[test]
fn arr_026_element_expanded_array_reports_its_declared_rank_when_over_subscripted() {
    // The rejection is kept, and it names the rank the declaration has. Reading
    // the element's own (scalar) shape reported `0 dimension(s)` here, which is
    // a statement about the expansion rather than about the model.
    expect_failure_in_phase_with_detail(
        r#"
        package P
            model Sub
                parameter Real p = 1;
                Real x;
            equation
                x = p*time;
            end Sub;
            model M
                parameter Real kk[3] = {1, 2, 3};
                Sub c[3](p = kk);
                Real y;
            equation
                y = c[1, 2].x;
            end M;
        end P;
    "#,
        "P.M",
        FailedPhase::Typecheck,
        "ET009",
        "`c` has 1 dimension(s) but is subscripted with 2 subscript(s)",
    );
}

#[test]
fn arr_026_element_expanded_array_bounds_are_checked_against_the_declared_extent() {
    expect_failure_in_phase_with_detail(
        r#"
        package P
            model Sub
                parameter Real p = 1;
                Real x;
            equation
                x = p*time;
            end Sub;
            model M
                parameter Real kk[3] = {1, 2, 3};
                Sub c[3](p = kk);
                Real y;
            equation
                y = c[4].x;
            end M;
        end P;
    "#,
        "P.M",
        FailedPhase::Typecheck,
        "ET009",
        "subscript 4 for `c` is out of bounds for dimension of size 3",
    );
}

#[test]
fn arr_026_a_genuine_scalar_component_is_still_rejected_when_subscripted() {
    // The ablation for the four acceptances above: a component the model never
    // gave a dimension keeps reporting zero of them.
    expect_failure_in_phase_with_detail(
        r#"
        package P
            model Sub
                parameter Real p = 1;
                Real x;
            equation
                x = p*time;
            end Sub;
            model M
                Sub c(p = 2);
                Real y;
            equation
                y = c[1].x;
            end M;
        end P;
    "#,
        "P.M",
        FailedPhase::Typecheck,
        "ET009",
        "`c` has 0 dimension(s) but is subscripted with 1 subscript(s)",
    );
}
