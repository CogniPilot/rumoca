//! MLS §10.3, 10.6-10.7: Array Functions, Operators, and Empty Arrays
//!
//! Tests for:
//! - §10.3: Built-in array functions
//! - §10.6: Element-wise and matrix operators
//! - §10.7: Empty arrays
//!
//! Reference: https://specification.modelica.org/master/arrays.html

use crate::spec::{expect_parse_success, expect_success};

// ============================================================================
// §10.6 ARRAY OPERATORS
// ============================================================================

/// MLS §10.6: Element-wise operators
mod section_10_6_elementwise_operators {
    use super::*;

    #[test]
    fn mls_10_6_elementwise_add() {
        expect_success(
            "model Test Real a[3]={1,2,3}; Real b[3]={4,5,6}; Real c[3]=a.+b; equation end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_10_6_elementwise_sub() {
        expect_success(
            "model Test Real a[3]={4,5,6}; Real b[3]={1,2,3}; Real c[3]=a.-b; equation end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_10_6_elementwise_mul() {
        expect_success(
            "model Test Real a[3]={1,2,3}; Real b[3]={4,5,6}; Real c[3]=a.*b; equation end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_10_6_elementwise_div() {
        expect_success(
            "model Test Real a[3]={4,6,8}; Real b[3]={2,2,2}; Real c[3]=a./b; equation end Test;",
            "Test",
        );
    }

    /// TODO: Elementwise power dimension handling not yet fully supported
    #[test]
    #[ignore = "Elementwise power dimension handling not yet fully supported"]
    fn mls_10_6_elementwise_pow() {
        expect_success(
            "model Test Real a[3]={2,3,4}; Real b[3]={2,2,2}; Real c[3]=a.^b; equation end Test;",
            "Test",
        );
    }
}

/// MLS §10.6: Matrix operators
mod section_10_6_matrix_operators {
    use super::*;

    /// Matrix multiplication A[2,3] * B[3,2] = C[2,2]
    #[test]
    fn mls_10_6_matrix_mul() {
        expect_success(
            r#"
            model Test
                Real A[2,3] = {{1,2,3}, {4,5,6}};
                Real B[3,2] = {{1,2}, {3,4}, {5,6}};
                Real C[2,2] = A * B;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_10_6_matvec_mul() {
        expect_success(
            r#"
            model Test
                Real A[3,3] = {{1,0,0}, {0,1,0}, {0,0,1}};
                Real x[3] = {1, 2, 3};
                Real y[3] = A * x;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_10_6_scalar_array_mul() {
        expect_success(
            "model Test Real a[3]={1,2,3}; Real b[3]=2*a; equation end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_10_6_array_scalar_mul() {
        expect_success(
            "model Test Real a[3]={1,2,3}; Real b[3]=a*2; equation end Test;",
            "Test",
        );
    }
}

// ============================================================================
// §10.3 BUILT-IN ARRAY FUNCTIONS
// ============================================================================

/// MLS §10.3: Built-in array functions
mod section_10_3_array_functions {
    use super::*;

    // -------------------------------------------------------------------------
    // Size and dimension functions
    // -------------------------------------------------------------------------

    #[test]
    fn mls_10_3_size() {
        expect_success(
            r#"
            model Test
                Real x[5];
                Integer n = size(x, 1);
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
    fn mls_10_3_ndims() {
        expect_success(
            "model Test Real A[3,4]; Integer n=ndims(A); equation end Test;",
            "Test",
        );
    }

    // -------------------------------------------------------------------------
    // Array construction functions
    // -------------------------------------------------------------------------

    #[test]
    fn mls_10_3_zeros() {
        expect_success("model Test Real x[5]=zeros(5); equation end Test;", "Test");
    }

    #[test]
    fn mls_10_3_zeros_2d() {
        expect_success(
            "model Test Real A[3,4]=zeros(3,4); equation end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_10_3_ones() {
        expect_success("model Test Real x[3]=ones(3); equation end Test;", "Test");
    }

    #[test]
    fn mls_10_3_ones_2d() {
        expect_success(
            "model Test Real A[3,3]=ones(3,3); equation end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_10_3_fill() {
        expect_success(
            "model Test Real x[4]=fill(7, 4); equation end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_10_3_fill_2d() {
        expect_success(
            "model Test Real A[2,3]=fill(5, 2, 3); equation end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_10_3_identity() {
        expect_success(
            "model Test Real I[3,3]=identity(3); equation end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_10_3_diagonal() {
        expect_success(
            "model Test Real v[3]={1,2,3}; Real D[3,3]=diagonal(v); equation end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_10_3_linspace() {
        expect_success(
            "model Test Real x[5]=linspace(0,1,5); equation end Test;",
            "Test",
        );
    }

    // -------------------------------------------------------------------------
    // Transformation functions
    // -------------------------------------------------------------------------

    #[test]
    fn mls_10_3_transpose() {
        expect_success(
            "model Test Real A[2,3]={{1,2,3},{4,5,6}}; Real B[3,2]=transpose(A); equation end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_10_3_symmetric() {
        expect_success(
            "model Test Real A[3,3]={{1,2,3},{2,4,5},{3,5,6}}; Real S[3,3]=symmetric(A); equation end Test;",
            "Test",
        );
    }

    // -------------------------------------------------------------------------
    // Reduction functions
    // -------------------------------------------------------------------------

    #[test]
    fn mls_10_3_sum() {
        expect_success(
            "model Test Real x[5]={1,2,3,4,5}; Real s=sum(x); equation end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_10_3_product() {
        expect_success(
            "model Test Real x[4]={1,2,3,4}; Real p=product(x); equation end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_10_3_min_array() {
        expect_success(
            "model Test Real x[5] = {3, 1, 4, 1, 5}; Real m = min(x); equation end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_10_3_max_array() {
        expect_success(
            "model Test Real x[5] = {3, 1, 4, 1, 5}; Real m = max(x); equation end Test;",
            "Test",
        );
    }

    // -------------------------------------------------------------------------
    // Concatenation functions
    // -------------------------------------------------------------------------

    /// TODO: cat() function not yet fully supported
    #[test]
    #[ignore = "cat() function not yet fully supported"]
    fn mls_10_3_cat() {
        expect_success(
            "model Test Real a[2]={1,2}; Real b[3]={3,4,5}; Real c[5]=cat(1,a,b); equation end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_10_3_vector() {
        expect_success(
            "model Test Real A[2,2]={{1,2},{3,4}}; Real v[4]=vector(A); equation end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_10_3_matrix() {
        expect_success(
            "model Test Real v[6]={1,2,3,4,5,6}; Real A[2,3]=matrix(v); equation end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_10_3_scalar() {
        expect_success(
            "model Test Real A[1,1]={{5}}; Real s=scalar(A); equation end Test;",
            "Test",
        );
    }
}

// ============================================================================
// §10.7 EMPTY ARRAYS
// ============================================================================

/// MLS §10.7: Empty arrays
mod section_10_7_empty_arrays {
    use super::*;

    #[test]
    fn mls_10_7_empty_1d() {
        expect_parse_success("model Test Real x[0]; end Test;");
    }

    #[test]
    fn mls_10_7_empty_2d() {
        expect_parse_success("model Test Real A[0,3]; end Test;");
    }

    #[test]
    fn mls_10_7_zeros_empty() {
        expect_parse_success("model Test Real x[0] = zeros(0); end Test;");
    }
}
