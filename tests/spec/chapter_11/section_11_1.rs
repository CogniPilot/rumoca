//! MLS §11.1: Algorithm Sections
//!
//! Tests for:
//! - §11.1 Algorithm sections in models and functions
//! - §11.1 Initial algorithm sections
//!
//! Reference: https://specification.modelica.org/master/statements-and-algorithm-sections.html

use crate::spec::expect_parse_success;

// ============================================================================
// §11.1 ALGORITHM SECTIONS
// ============================================================================

/// MLS §11.1: Algorithm sections
mod section_11_1_algorithms {
    use super::*;

    #[test]
    fn mls_11_1_empty_algorithm() {
        expect_parse_success(
            r#"
            model Test
                Real x;
            algorithm
            equation
                x = 1;
            end Test;
            "#,
        );
    }

    #[test]
    fn mls_11_1_simple_algorithm() {
        expect_parse_success(
            r#"
            model Test
                Real x;
                Real y;
            algorithm
                x := 1;
                y := 2 * x;
            equation
            end Test;
            "#,
        );
    }

    #[test]
    fn mls_11_1_multiple_algorithm_sections() {
        expect_parse_success(
            r#"
            model Test
                Real x;
                Real y;
            algorithm
                x := 1;
            algorithm
                y := x + 1;
            equation
            end Test;
            "#,
        );
    }

    #[test]
    fn mls_11_1_initial_algorithm() {
        expect_parse_success(
            r#"
            model Test
                Real x(start = 0);
            initial algorithm
                x := 1;
            equation
                der(x) = -x;
            end Test;
            "#,
        );
    }

    #[test]
    fn mls_11_1_algorithm_with_multiple_statements() {
        expect_parse_success(
            r#"
            model Test
                Real a;
                Real b;
                Real c;
            algorithm
                a := 1;
                b := 2;
                c := a + b;
                a := c * 2;
            equation
            end Test;
            "#,
        );
    }

    #[test]
    fn mls_11_1_algorithm_before_equation() {
        expect_parse_success(
            r#"
            model Test
                Real x;
                Real y(start = 0);
            algorithm
                x := 5;
            equation
                der(y) = x;
            end Test;
            "#,
        );
    }
}
