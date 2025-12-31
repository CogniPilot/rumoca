//! MLS §10: Arrays - Detailed Conformance Tests
//!
//! Comprehensive tests covering normative statements from MLS §10 including:
//! - §10.3: Advanced array functions
//! - §10.5: Array slicing and indexing
//! - §10.6: Matrix operations
//!
//! Reference: https://specification.modelica.org/master/arrays.html

use crate::spec::{expect_failure, expect_parse_success, expect_success};

// ============================================================================
// §10.5 ARRAY INDEXING AND SLICING
// ============================================================================

/// MLS §10.5: Array indexing normative requirements
mod array_indexing {
    use super::*;

    /// Basic scalar indexing
    #[test]
    fn mls_10_5_scalar_indexing() {
        expect_success(
            r#"
            model Test
                Real x[5] = {1, 2, 3, 4, 5};
                Real a = x[1];
                Real b = x[3];
                Real c = x[5];
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Matrix element access
    #[test]
    fn mls_10_5_matrix_element() {
        expect_success(
            r#"
            model Test
                Real A[3, 3] = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}};
                Real corner = A[1, 1];
                Real center = A[2, 2];
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// 3D array indexing
    #[test]
    fn mls_10_5_3d_indexing() {
        expect_success(
            r#"
            model Test
                Real B[2, 2, 2];
                Real element;
            equation
                B = {{{1, 2}, {3, 4}}, {{5, 6}, {7, 8}}};
                element = B[1, 2, 1];
            end Test;
            "#,
            "Test",
        );
    }

    /// Index with expression
    #[test]
    fn mls_10_5_expression_index() {
        expect_success(
            r#"
            model Test
                Real x[5] = {1, 2, 3, 4, 5};
                parameter Integer i = 2;
                Real y = x[i + 1];
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Index must be within bounds"
    #[test]
    fn error_index_out_of_bounds() {
        expect_failure(
            r#"
            model Test
                Real x[3] = {1, 2, 3};
                Real y = x[4];
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Index must be Integer"
    #[test]
    fn error_non_integer_index() {
        expect_failure(
            r#"
            model Test
                Real x[3] = {1, 2, 3};
                Real y = x[1.5];
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

/// MLS §10.5: Array slicing
mod array_slicing {
    use super::*;

    /// Range indexing
    #[test]
    fn mls_10_5_range_slice() {
        expect_success(
            r#"
            model Test
                Real x[5] = {1, 2, 3, 4, 5};
                Real y[3] = x[2:4];
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Colon for all elements
    #[test]
    fn mls_10_5_colon_all() {
        expect_success(
            r#"
            model Test
                Real x[5] = {1, 2, 3, 4, 5};
                Real y[5] = x[:];
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Row extraction with colon
    #[test]
    fn mls_10_5_row_extraction() {
        expect_success(
            r#"
            model Test
                Real A[3, 4] = {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 10, 11, 12}};
                Real row2[4] = A[2, :];
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Column extraction with colon
    #[test]
    fn mls_10_5_column_extraction() {
        expect_success(
            r#"
            model Test
                Real A[3, 4] = {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 10, 11, 12}};
                Real col3[3] = A[:, 3];
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Submatrix extraction
    #[test]
    fn mls_10_5_submatrix() {
        expect_success(
            r#"
            model Test
                Real A[4, 4] = {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 10, 11, 12}, {13, 14, 15, 16}};
                Real B[2, 2] = A[2:3, 2:3];
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Range with step
    #[test]
    fn mls_10_5_range_with_step() {
        expect_success(
            r#"
            model Test
                Real x[6] = {1, 2, 3, 4, 5, 6};
                Real y[3] = x[1:2:5];
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Array indexing
    #[test]
    #[ignore = "Array-based indexing not yet supported"]
    fn mls_10_5_array_index() {
        expect_success(
            r#"
            model Test
                Real x[5] = {10, 20, 30, 40, 50};
                Integer idx[3] = {1, 3, 5};
                Real y[3] = x[idx];
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §10.6 MATRIX OPERATIONS
// ============================================================================

/// MLS §10.6: Matrix multiplication and operations
mod matrix_operations {
    use super::*;

    /// Matrix-matrix multiplication
    #[test]
    fn mls_10_6_matrix_multiply() {
        expect_success(
            r#"
            model Test
                Real A[2, 3] = {{1, 2, 3}, {4, 5, 6}};
                Real B[3, 2] = {{1, 2}, {3, 4}, {5, 6}};
                Real C[2, 2] = A * B;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Matrix-vector multiplication
    #[test]
    fn mls_10_6_matrix_vector_multiply() {
        expect_success(
            r#"
            model Test
                Real A[3, 3] = {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}};
                Real x[3] = {1, 2, 3};
                Real y[3] = A * x;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Vector-matrix multiplication
    #[test]
    fn mls_10_6_vector_matrix_multiply() {
        expect_success(
            r#"
            model Test
                Real x[3] = {1, 2, 3};
                Real A[3, 2] = {{1, 2}, {3, 4}, {5, 6}};
                Real y[2] = x * A;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Scalar-matrix multiplication
    #[test]
    fn mls_10_6_scalar_matrix_multiply() {
        expect_success(
            r#"
            model Test
                Real A[2, 2] = {{1, 2}, {3, 4}};
                Real B[2, 2] = 2 * A;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Matrix addition
    #[test]
    fn mls_10_6_matrix_addition() {
        expect_success(
            r#"
            model Test
                Real A[2, 2] = {{1, 2}, {3, 4}};
                Real B[2, 2] = {{5, 6}, {7, 8}};
                Real C[2, 2] = A + B;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Matrix subtraction
    #[test]
    fn mls_10_6_matrix_subtraction() {
        expect_success(
            r#"
            model Test
                Real A[2, 2] = {{5, 6}, {7, 8}};
                Real B[2, 2] = {{1, 2}, {3, 4}};
                Real C[2, 2] = A - B;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Matrix power
    #[test]
    fn mls_10_6_matrix_power() {
        expect_success(
            r#"
            model Test
                Real A[2, 2] = {{1, 1}, {1, 0}};
                Real A2[2, 2] = A ^ 2;
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §10.3 ADVANCED ARRAY FUNCTIONS
// ============================================================================

/// MLS §10.3: Advanced array functions
mod advanced_array_functions {
    use super::*;

    /// transpose function
    #[test]
    fn mls_10_3_transpose() {
        expect_success(
            r#"
            model Test
                Real A[2, 3] = {{1, 2, 3}, {4, 5, 6}};
                Real B[3, 2] = transpose(A);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// outerProduct function
    #[test]
    fn mls_10_3_outer_product() {
        expect_success(
            r#"
            model Test
                Real u[3] = {1, 2, 3};
                Real v[2] = {4, 5};
                Real A[3, 2] = outerProduct(u, v);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// cross function
    #[test]
    fn mls_10_3_cross() {
        expect_success(
            r#"
            model Test
                Real a[3] = {1, 0, 0};
                Real b[3] = {0, 1, 0};
                Real c[3] = cross(a, b);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// skew function
    #[test]
    #[ignore = "skew function not yet supported"]
    fn mls_10_3_skew() {
        expect_success(
            r#"
            model Test
                Real v[3] = {1, 2, 3};
                Real S[3, 3] = skew(v);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// cat function (concatenation)
    #[test]
    #[ignore = "cat function not yet supported"]
    fn mls_10_3_cat_1d() {
        expect_success(
            r#"
            model Test
                Real a[2] = {1, 2};
                Real b[3] = {3, 4, 5};
                Real c[5] = cat(1, a, b);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// cat function for 2D arrays
    #[test]
    #[ignore = "cat function for 2D arrays not yet supported"]
    fn mls_10_3_cat_2d_rows() {
        expect_success(
            r#"
            model Test
                Real A[2, 3] = {{1, 2, 3}, {4, 5, 6}};
                Real B[1, 3] = {{7, 8, 9}};
                Real C[3, 3] = cat(1, A, B);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// cat function for columns
    #[test]
    #[ignore = "cat function for columns not yet supported"]
    fn mls_10_3_cat_2d_cols() {
        expect_success(
            r#"
            model Test
                Real A[2, 2] = {{1, 2}, {3, 4}};
                Real B[2, 1] = {{5}, {6}};
                Real C[2, 3] = cat(2, A, B);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// promote function
    #[test]
    fn mls_10_3_promote() {
        expect_parse_success(
            r#"
            model Test
                Real x = 5;
                Real A[1, 1] = promote(x, 2);
            equation
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §10.5 BOOLEAN AND ENUMERATION ARRAY INDEXING
// ============================================================================

/// MLS §10.5: Boolean and enumeration array indexing
mod special_indexing {
    use super::*;

    /// Boolean array indexing
    #[test]
    #[ignore = "Boolean array indexing not yet supported"]
    fn mls_10_5_boolean_index() {
        expect_success(
            r#"
            model Test
                Real x[Boolean] = {1, 2};
                Real a = x[false];
                Real b = x[true];
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Enumeration array indexing
    #[test]
    fn mls_10_5_enum_index() {
        expect_parse_success(
            r#"
            type Color = enumeration(Red, Green, Blue);

            model Test
                Real x[Color] = {1, 2, 3};
                Real r = x[Color.Red];
                Real g = x[Color.Green];
                Real b = x[Color.Blue];
            equation
            end Test;
            "#,
        );
    }
}

// ============================================================================
// ARRAY SIZE AND DIMENSION FUNCTIONS
// ============================================================================

/// Size and dimension functions
mod size_dimension_functions {
    use super::*;

    /// size with single argument returns array
    #[test]
    fn mls_10_3_size_array() {
        expect_parse_success(
            r#"
            model Test
                Real A[3, 4, 5];
                Integer dims[3] = size(A);
            equation
            end Test;
            "#,
        );
    }

    /// size with dimension argument
    #[test]
    fn mls_10_3_size_dim() {
        expect_success(
            r#"
            model Test
                Real A[3, 4];
                Integer rows = size(A, 1);
                Integer cols = size(A, 2);
            equation
                A = fill(1, 3, 4);
            end Test;
            "#,
            "Test",
        );
    }

    /// ndims function
    #[test]
    fn mls_10_3_ndims() {
        expect_success(
            r#"
            model Test
                Real A[3, 4, 5];
                Integer n = ndims(A);
            equation
                A = fill(0, 3, 4, 5);
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// COMPLEX ARRAY SCENARIOS
// ============================================================================

/// Complex array scenarios
mod complex_scenarios {
    use super::*;

    /// Nested array comprehension
    #[test]
    fn complex_nested_comprehension() {
        expect_success(
            r#"
            model Test
                Real A[4, 4] = {{i * j for j in 1:4} for i in 1:4};
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Array in differential equation
    #[test]
    fn complex_array_ode() {
        expect_success(
            r#"
            model Test
                parameter Integer n = 5;
                Real x[n](each start = 0);
                Real A[n, n] = identity(n);
            equation
                der(x) = A * x;
            end Test;
            "#,
            "Test",
        );
    }

    /// Array with component references
    #[test]
    fn complex_array_component() {
        expect_success(
            r#"
            model Test
                parameter Integer n = 3;
                Real x[n];
                Real total = sum(x);
            equation
                for i in 1:n loop
                    x[i] = i;
                end for;
            end Test;
            "#,
            "Test",
        );
    }

    /// Multiple array operations
    #[test]
    fn complex_multiple_operations() {
        expect_success(
            r#"
            model Test
                Real a[3] = {1, 2, 3};
                Real b[3] = {4, 5, 6};
                Real sum_ab[3] = a + b;
                Real dot = sum(a .* b);
                Real scaled[3] = 2 * a;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Array constructor with functions
    #[test]
    fn complex_array_with_functions() {
        expect_success(
            r#"
            model Test
                Real angles[5] = {i * 0.2 for i in 1:5};
                Real sines[5] = {sin(angles[i]) for i in 1:5};
                Real cosines[5] = {cos(angles[i]) for i in 1:5};
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// ARRAY DIMENSION ERRORS
// ============================================================================

/// Array dimension error cases
mod dimension_errors {
    use super::*;

    /// Mismatched dimensions in addition
    #[test]
    fn error_dimension_mismatch_add() {
        expect_failure(
            r#"
            model Test
                Real a[3] = {1, 2, 3};
                Real b[4] = {1, 2, 3, 4};
                Real c[3] = a + b;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Mismatched dimensions in assignment
    #[test]
    fn error_dimension_mismatch_assign() {
        expect_failure(
            r#"
            model Test
                Real x[3];
            equation
                x = {1, 2, 3, 4};
            end Test;
            "#,
            "Test",
        );
    }

    /// Mismatched matrix dimensions
    #[test]
    fn error_matrix_dimension_mismatch() {
        expect_failure(
            r#"
            model Test
                Real A[2, 3] = {{1, 2, 3}, {4, 5, 6}};
                Real B[2, 2] = A;
            equation
            end Test;
            "#,
            "Test",
        );
    }
}
