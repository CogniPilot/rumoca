//! MLS §2.5 Operator Symbols
//!
//! Tests for:
//! - Arithmetic operators: +, -, *, /, ^
//! - Elementwise operators: .+, .-, .*, ./, .^
//! - Relational operators: <, <=, >, >=, ==, <>
//! - Logical operators: and, or, not
//! - Other operators: :=, (, ), [, ], {, }, ,, ;, :, .
//!
//! Reference: https://specification.modelica.org/master/lexicalstructure.html

use crate::spec::expect_parse_success;

// ============================================================================
// §2.5 OPERATOR SYMBOLS
// ============================================================================

/// MLS §2.5: Operator symbols used in Modelica
mod section_2_5_operator_symbols {
    use super::*;

    // -------------------------------------------------------------------------
    // Arithmetic operators
    // -------------------------------------------------------------------------

    #[test]
    fn mls_2_5_arithmetic_plus() {
        expect_parse_success("model Test Real x = 1 + 2; end Test;");
    }

    #[test]
    fn mls_2_5_arithmetic_minus() {
        expect_parse_success("model Test Real x = 3 - 1; end Test;");
    }

    #[test]
    fn mls_2_5_arithmetic_multiply() {
        expect_parse_success("model Test Real x = 2 * 3; end Test;");
    }

    #[test]
    fn mls_2_5_arithmetic_divide() {
        expect_parse_success("model Test Real x = 6 / 2; end Test;");
    }

    #[test]
    fn mls_2_5_arithmetic_power() {
        expect_parse_success("model Test Real x = 2 ^ 3; end Test;");
    }

    // -------------------------------------------------------------------------
    // Unary operators
    // -------------------------------------------------------------------------

    #[test]
    fn mls_2_5_unary_minus() {
        expect_parse_success("model Test Real x = -5; end Test;");
    }

    #[test]
    fn mls_2_5_unary_plus() {
        expect_parse_success("model Test Real x = +5; end Test;");
    }

    #[test]
    #[ignore = "Parser doesn't handle consecutive unary minus operators"]
    fn mls_2_5_unary_double_minus() {
        expect_parse_success("model Test Real x = --5; end Test;");
    }

    // -------------------------------------------------------------------------
    // Relational operators
    // -------------------------------------------------------------------------

    #[test]
    fn mls_2_5_relational_less() {
        expect_parse_success("model Test Boolean b = 1 < 2; end Test;");
    }

    #[test]
    fn mls_2_5_relational_less_equal() {
        expect_parse_success("model Test Boolean b = 1 <= 2; end Test;");
    }

    #[test]
    fn mls_2_5_relational_greater() {
        expect_parse_success("model Test Boolean b = 2 > 1; end Test;");
    }

    #[test]
    fn mls_2_5_relational_greater_equal() {
        expect_parse_success("model Test Boolean b = 2 >= 1; end Test;");
    }

    #[test]
    fn mls_2_5_relational_equal() {
        expect_parse_success("model Test Boolean b = 1 == 1; end Test;");
    }

    #[test]
    fn mls_2_5_relational_not_equal() {
        expect_parse_success("model Test Boolean b = 1 <> 2; end Test;");
    }

    // -------------------------------------------------------------------------
    // Logical operators
    // -------------------------------------------------------------------------

    #[test]
    fn mls_2_5_logical_and() {
        expect_parse_success("model Test Boolean b = true and false; end Test;");
    }

    #[test]
    fn mls_2_5_logical_or() {
        expect_parse_success("model Test Boolean b = true or false; end Test;");
    }

    #[test]
    fn mls_2_5_logical_not() {
        expect_parse_success("model Test Boolean b = not true; end Test;");
    }

    // -------------------------------------------------------------------------
    // Elementwise operators
    // -------------------------------------------------------------------------

    #[test]
    fn mls_2_5_elementwise_add() {
        expect_parse_success("model Test Real x[2] = {1,2} .+ {3,4}; end Test;");
    }

    #[test]
    fn mls_2_5_elementwise_sub() {
        expect_parse_success("model Test Real x[2] = {3,4} .- {1,2}; end Test;");
    }

    #[test]
    fn mls_2_5_elementwise_mul() {
        expect_parse_success("model Test Real x[2] = {1,2} .* {3,4}; end Test;");
    }

    #[test]
    fn mls_2_5_elementwise_div() {
        expect_parse_success("model Test Real x[2] = {4,6} ./ {2,3}; end Test;");
    }

    #[test]
    fn mls_2_5_elementwise_power() {
        expect_parse_success("model Test Real x[2] = {2,3} .^ {2,2}; end Test;");
    }

    // -------------------------------------------------------------------------
    // Assignment operator
    // -------------------------------------------------------------------------

    #[test]
    fn mls_2_5_assignment() {
        expect_parse_success(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                y := x;
            end F;
            "#,
        );
    }

    #[test]
    fn mls_2_5_assignment_expression() {
        expect_parse_success(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                y := x + 1;
            end F;
            "#,
        );
    }

    // -------------------------------------------------------------------------
    // Grouping operators
    // -------------------------------------------------------------------------

    #[test]
    fn mls_2_5_parentheses() {
        expect_parse_success("model Test Real x = (1 + 2) * 3; end Test;");
    }

    #[test]
    fn mls_2_5_nested_parentheses() {
        expect_parse_success("model Test Real x = ((1 + 2) * (3 + 4)); end Test;");
    }

    #[test]
    fn mls_2_5_brackets_subscript() {
        expect_parse_success("model Test Real x[3]; Real y = x[1]; end Test;");
    }

    #[test]
    fn mls_2_5_braces_array() {
        expect_parse_success("model Test Real x[3] = {1, 2, 3}; end Test;");
    }

    #[test]
    fn mls_2_5_braces_nested() {
        expect_parse_success("model Test Real x[2,2] = {{1,2},{3,4}}; end Test;");
    }

    // -------------------------------------------------------------------------
    // Separator operators
    // -------------------------------------------------------------------------

    #[test]
    fn mls_2_5_comma() {
        expect_parse_success("model Test Real x[3] = {1, 2, 3}; end Test;");
    }

    #[test]
    fn mls_2_5_semicolon() {
        expect_parse_success("model Test Real x; Real y; end Test;");
    }

    #[test]
    fn mls_2_5_colon_range() {
        expect_parse_success("model Test Real x[3] = {i for i in 1:3}; end Test;");
    }

    #[test]
    fn mls_2_5_colon_range_with_step() {
        expect_parse_success("model Test Real x[5] = {i for i in 1:2:9}; end Test;");
    }

    #[test]
    fn mls_2_5_dot_component_access() {
        expect_parse_success(
            r#"
            record R Real x; end R;
            model Test R r; Real y = r.x; end Test;
            "#,
        );
    }

    #[test]
    #[ignore = "Parser requires record definitions to be on separate lines"]
    fn mls_2_5_dot_nested_access() {
        expect_parse_success(
            r#"
            record Inner Real value; end Inner;
            record Outer Inner inner; end Outer;
            model Test Outer o; Real x = o.inner.value; end Test;
            "#,
        );
    }
}

// ============================================================================
// OPERATOR COMBINATIONS
// ============================================================================

/// Complex operator combinations
mod operator_combinations {
    use super::*;

    #[test]
    fn operators_mixed_arithmetic() {
        expect_parse_success("model Test Real x = 1 + 2 - 3 * 4 / 5; end Test;");
    }

    #[test]
    fn operators_with_parentheses() {
        expect_parse_success("model Test Real x = (1 + 2) * (3 - 4) / (5 + 6); end Test;");
    }

    #[test]
    fn operators_mixed_logical() {
        expect_parse_success(
            "model Test Boolean b = (1 < 2) and (3 > 2) or not (4 == 5); end Test;",
        );
    }

    #[test]
    fn operators_array_with_subscripts() {
        expect_parse_success(
            "model Test Real x[3] = {1,2,3}; Real y = x[1] + x[2] + x[3]; end Test;",
        );
    }

    #[test]
    fn operators_function_call_with_operators() {
        expect_parse_success("model Test Real x = sin(1.0 + 2.0) * cos(3.0 - 1.0); end Test;");
    }
}
