//! MLS §10: High Priority Array Tests
//!
//! This module tests high priority normative requirements:
//! - §10.5: Array indexing bounds checking
//! - §10.3: Array concatenation and slicing
//! - §10.7: Empty array handling
//! - §10.1: Array dimension compatibility
//!
//! Reference: https://specification.modelica.org/master/arrays.html

use crate::spec::{expect_failure, expect_success};

// ============================================================================
// §10.5 ARRAY INDEXING BOUNDS
// ============================================================================

/// MLS §10.5: Array index bounds checking
mod array_bounds {
    use super::*;

    /// MLS: "Array index must be within bounds"
    #[test]
    fn mls_10_5_index_out_of_bounds_high() {
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

    /// MLS: "Array index must be within bounds"
    #[test]
    fn mls_10_5_index_out_of_bounds_zero() {
        expect_failure(
            r#"
            model Test
                Real x[3] = {1, 2, 3};
                Real y = x[0];
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Array index must be within bounds"
    #[test]
    fn mls_10_5_index_negative() {
        expect_failure(
            r#"
            model Test
                Real x[3] = {1, 2, 3};
                Real y = x[-1];
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "2D array index out of bounds"
    #[test]
    fn mls_10_5_2d_index_out_of_bounds() {
        expect_failure(
            r#"
            model Test
                Real A[2, 3] = {{1, 2, 3}, {4, 5, 6}};
                Real y = A[3, 1];
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Index within bounds
    #[test]
    fn mls_10_5_valid_indexing() {
        expect_success(
            r#"
            model Test
                Real x[3] = {1, 2, 3};
                Real a = x[1];
                Real b = x[2];
                Real c = x[3];
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: 2D indexing within bounds
    #[test]
    fn mls_10_5_valid_2d_indexing() {
        expect_success(
            r#"
            model Test
                Real A[2, 3] = {{1, 2, 3}, {4, 5, 6}};
                Real a = A[1, 1];
                Real b = A[2, 3];
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Dynamic indexing (bounds check at runtime)
    #[test]
    fn mls_10_5_dynamic_indexing() {
        expect_success(
            r#"
            model Test
                parameter Integer i = 2;
                Real x[3] = {1, 2, 3};
                Real y = x[i];
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §10.3 ARRAY SLICING
// ============================================================================

/// MLS §10.3: Array slicing operations
mod array_slicing {
    use super::*;

    /// MLS: "Colon selects entire dimension"
    #[test]
    #[ignore = "Array slicing with colon not yet fully supported"]
    fn mls_10_3_colon_slice() {
        expect_success(
            r#"
            model Test
                Real A[2, 3] = {{1, 2, 3}, {4, 5, 6}};
                Real row[3] = A[1, :];
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Range slice selects subset"
    #[test]
    #[ignore = "Array slicing with range not yet fully supported"]
    fn mls_10_3_range_slice() {
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

    /// MLS: "Column extraction"
    #[test]
    #[ignore = "Array column extraction not yet fully supported"]
    fn mls_10_3_column_slice() {
        expect_success(
            r#"
            model Test
                Real A[3, 2] = {{1, 2}, {3, 4}, {5, 6}};
                Real col[3] = A[:, 1];
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Submatrix extraction"
    #[test]
    #[ignore = "Submatrix extraction not yet fully supported"]
    fn mls_10_3_submatrix_slice() {
        expect_success(
            r#"
            model Test
                Real A[4, 4] = {{1,2,3,4},{5,6,7,8},{9,10,11,12},{13,14,15,16}};
                Real B[2, 2] = A[2:3, 2:3];
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Simple array access (not slicing)
    #[test]
    fn mls_10_3_simple_access() {
        expect_success(
            r#"
            model Test
                Real A[2, 3] = {{1, 2, 3}, {4, 5, 6}};
                Real x = A[1, 2];
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §10.7 EMPTY ARRAYS
// ============================================================================

/// MLS §10.7: Empty array handling
mod empty_arrays {
    use super::*;

    /// MLS: "Empty array with zeros(0)"
    #[test]
    fn mls_10_7_zeros_empty() {
        expect_success(
            r#"
            model Test
                Real x[0] = zeros(0);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Empty array with ones(0)"
    #[test]
    fn mls_10_7_ones_empty() {
        expect_success(
            r#"
            model Test
                Real x[0] = ones(0);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Sum of empty array is 0"
    #[test]
    fn mls_10_7_sum_empty() {
        expect_success(
            r#"
            model Test
                Real x[0];
                Real s = sum(x);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Product of empty array is 1"
    #[test]
    fn mls_10_7_product_empty() {
        expect_success(
            r#"
            model Test
                Real x[0];
                Real p = product(x);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "For-loop over empty array iterates zero times"
    #[test]
    fn mls_10_7_for_empty() {
        expect_success(
            r#"
            model Test
                Real x[0];
                Real s = 0;
            equation
                for i in 1:0 loop
                    s = s + 1;
                end for;
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Non-empty array operations
    #[test]
    fn mls_10_7_nonempty_operations() {
        expect_success(
            r#"
            model Test
                Real x[3] = {1, 2, 3};
                Real s = sum(x);
                Real p = product(x);
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §10.1 DIMENSION COMPATIBILITY
// ============================================================================

/// MLS §10.1: Array dimension compatibility
mod dimension_compatibility {
    use super::*;

    /// MLS: "Array assignment requires same dimensions"
    #[test]
    fn mls_10_1_dimension_mismatch_assignment() {
        expect_failure(
            r#"
            model Test
                Real x[3] = {1, 2, 3};
                Real y[4] = x;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Array addition requires same dimensions"
    #[test]
    #[ignore = "Array operation dimension check not yet implemented"]
    fn mls_10_1_dimension_mismatch_add() {
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

    /// MLS: "2D array dimension mismatch"
    #[test]
    fn mls_10_1_2d_dimension_mismatch() {
        expect_failure(
            r#"
            model Test
                Real A[2, 3] = {{1, 2, 3}, {4, 5, 6}};
                Real B[3, 2] = A;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Same dimension operations
    #[test]
    fn mls_10_1_same_dimensions() {
        expect_success(
            r#"
            model Test
                Real a[3] = {1, 2, 3};
                Real b[3] = {4, 5, 6};
                Real c[3] = a + b;
                Real d[3] = a .* b;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Scalar-array operations
    #[test]
    fn mls_10_1_scalar_array_ops() {
        expect_success(
            r#"
            model Test
                Real k = 2;
                Real a[3] = {1, 2, 3};
                Real b[3] = k * a;
                Real c[3] = a * k;
                Real d[3] = a + k;
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §10.4 ARRAY CONSTRUCTORS
// ============================================================================

/// MLS §10.4: Array constructor requirements
mod array_constructors {
    use super::*;

    /// MLS: "Array elements must have same type"
    #[test]
    #[ignore = "Array element type checking not yet implemented"]
    fn mls_10_4_mixed_types() {
        expect_failure(
            r#"
            model Test
                Real x[3] = {1, "hello", true};
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Nested arrays must have consistent dimensions"
    #[test]
    #[ignore = "Nested array dimension checking not yet implemented"]
    fn mls_10_4_inconsistent_nested() {
        expect_failure(
            r#"
            model Test
                Real A[2, 3] = {{1, 2, 3}, {4, 5}};
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Consistent array constructor
    #[test]
    fn mls_10_4_consistent_constructor() {
        expect_success(
            r#"
            model Test
                Real x[3] = {1, 2, 3};
                Real A[2, 3] = {{1, 2, 3}, {4, 5, 6}};
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Array comprehension
    #[test]
    fn mls_10_4_array_comprehension() {
        expect_success(
            r#"
            model Test
                Real x[5] = {i^2 for i in 1:5};
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: 2D comprehension
    #[test]
    fn mls_10_4_2d_comprehension() {
        expect_success(
            r#"
            model Test
                Real A[3, 3] = {i * j for i in 1:3, j in 1:3};
            equation
            end Test;
            "#,
            "Test",
        );
    }
}
