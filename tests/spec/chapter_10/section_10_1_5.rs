//! MLS §10.1-10.5: Array Declarations, Constructors, and Indexing
//!
//! Tests for:
//! - §10.1: Array declarations
//! - §10.4: Array constructor expressions
//! - §10.5: Array indexing
//!
//! Reference: https://specification.modelica.org/master/arrays.html

use crate::spec::{expect_parse_success, expect_success};

// ============================================================================
// §10.1 ARRAY DECLARATIONS
// ============================================================================

/// MLS §10.1: Array declarations
mod section_10_1_declarations {
    use super::*;

    #[test]
    fn mls_10_1_1d_array() {
        expect_success(
            "model Test Real x[5]; equation for i in 1:5 loop x[i]=i; end for; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_10_1_2d_array() {
        expect_success(
            r#"
            model Test
                Real A[3,4];
            equation
                for i in 1:3 loop
                    for j in 1:4 loop
                        A[i,j] = i + j;
                    end for;
                end for;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_10_1_3d_array() {
        expect_success(
            r#"
            model Test
                Real B[2,3,4];
            equation
                for i in 1:2 loop
                    for j in 1:3 loop
                        for k in 1:4 loop
                            B[i,j,k] = i + j + k;
                        end for;
                    end for;
                end for;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_10_1_param_size() {
        expect_success(
            r#"
            model Test
                parameter Integer n = 5;
                Real x[n];
            equation
                for i in 1:n loop
                    x[i] = i;
                end for;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_10_1_array_of_records() {
        expect_parse_success(
            r#"
            record Point
                Real x;
                Real y;
            end Point;

            model Test
                Point p[3];
            equation
            end Test;
            "#,
        );
    }

    #[test]
    fn mls_10_1_array_with_modification() {
        expect_success(
            r#"
            model Test
                Real x[3](each start = 0);
            equation
                for i in 1:3 loop der(x[i]) = i; end for;
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §10.4 ARRAY CONSTRUCTORS
// ============================================================================

/// MLS §10.4: Array constructor expressions
mod section_10_4_constructors {
    use super::*;

    #[test]
    fn mls_10_4_literal_1d() {
        expect_success(
            "model Test Real x[3] = {1, 2, 3}; equation end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_10_4_literal_2d() {
        expect_success(
            "model Test Real A[2,3] = {{1,2,3}, {4,5,6}}; equation end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_10_4_comprehension() {
        expect_success(
            "model Test Real x[5] = {i^2 for i in 1:5}; equation end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_10_4_comprehension_expression() {
        expect_success(
            "model Test Real x[5] = {sin(i*0.1) for i in 1:5}; equation end Test;",
            "Test",
        );
    }

    /// TODO: Array comprehension with 'if' filter clause not yet supported
    #[test]
    #[ignore = "Array comprehension with 'if' filter clause not yet supported"]
    fn mls_10_4_comprehension_filter() {
        expect_parse_success(
            "model Test Real x[:] = {i for i in 1:10 if mod(i,2)==0}; equation end Test;",
        );
    }

    #[test]
    fn mls_10_4_nested_comprehension() {
        expect_success(
            r#"
            model Test
                Real A[3,3] = {{i*j for j in 1:3} for i in 1:3};
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §10.5 INDEXING
// ============================================================================

/// MLS §10.5: Array indexing
mod section_10_5_indexing {
    use super::*;

    #[test]
    fn mls_10_5_scalar_index() {
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

    #[test]
    fn mls_10_5_matrix_index() {
        expect_success(
            r#"
            model Test
                Real A[3,3] = {{1,2,3}, {4,5,6}, {7,8,9}};
                Real a = A[1,1];
                Real b = A[2,2];
                Real c = A[3,3];
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Array slicing with range indices
    #[test]
    fn mls_10_5_range_index() {
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

    /// Array slicing with colon
    #[test]
    fn mls_10_5_colon_index() {
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

    /// Array slicing for row extraction
    #[test]
    fn mls_10_5_row_extraction() {
        expect_success(
            r#"
            model Test
                Real A[3,4] = {{1,2,3,4}, {5,6,7,8}, {9,10,11,12}};
                Real row[4] = A[2, :];
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Array slicing for column extraction
    #[test]
    fn mls_10_5_column_extraction() {
        expect_success(
            r#"
            model Test
                Real A[3,4] = {{1,2,3,4}, {5,6,7,8}, {9,10,11,12}};
                Real col[3] = A[:, 3];
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// ARRAY EDGE CASES
// ============================================================================

/// Edge cases for arrays
mod array_edge_cases {
    use super::*;

    #[test]
    fn edge_single_element() {
        expect_success("model Test Real x[1] = {42}; equation end Test;", "Test");
    }

    #[test]
    fn edge_large_array() {
        expect_success(
            r#"
            model Test
                parameter Integer n = 100;
                Real x[n];
            equation
                for i in 1:n loop
                    x[i] = i;
                end for;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn edge_nested_index() {
        expect_success(
            r#"
            model Test
                Real A[3,3] = {{1,2,3},{4,5,6},{7,8,9}};
                Integer idx[2] = {1, 3};
                Real a = A[idx[1], idx[2]];
            equation
            end Test;
            "#,
            "Test",
        );
    }
}
