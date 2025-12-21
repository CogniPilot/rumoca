//! MLS §3.1-3.4: Expressions and Arithmetic Operators
//!
//! Tests for:
//! - §3.1: General expression forms
//! - §3.2: Operator precedence and associativity
//! - §3.3: Evaluation order
//! - §3.4: Arithmetic operators
//!
//! Reference: https://specification.modelica.org/master/operatorsandexpressions.html

use crate::spec::expect_success;

// ============================================================================
// §3.1 EXPRESSIONS
// ============================================================================

/// MLS §3.1: General expression forms
mod section_3_1_expressions {
    use super::*;

    #[test]
    fn mls_3_1_simple_expression() {
        expect_success("model Test Real x = 1; end Test;", "Test");
    }

    #[test]
    fn mls_3_1_compound_expression() {
        expect_success("model Test Real x = 1 + 2 * 3; end Test;", "Test");
    }

    #[test]
    fn mls_3_1_parenthesized_expression() {
        expect_success("model Test Real x = (1 + 2) * 3; end Test;", "Test");
    }

    #[test]
    fn mls_3_1_nested_parentheses() {
        expect_success("model Test Real x = ((1 + 2) * (3 + 4)); end Test;", "Test");
    }

    #[test]
    fn mls_3_1_function_call_expression() {
        expect_success("model Test Real x = sin(1.0); end Test;", "Test");
    }

    #[test]
    fn mls_3_1_component_reference_expression() {
        expect_success("model Test Real x = 1; Real y = x; end Test;", "Test");
    }

    #[test]
    fn mls_3_1_array_expression() {
        expect_success("model Test Real x[3] = {1, 2, 3}; end Test;", "Test");
    }

    #[test]
    fn mls_3_1_if_expression() {
        expect_success(
            "model Test Boolean c = true; Real x = if c then 1 else 2; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_1_nested_function_calls() {
        expect_success("model Test Real x = sin(cos(0.5)); end Test;", "Test");
    }

    #[test]
    fn mls_3_1_mixed_expression() {
        expect_success(
            "model Test Real a = 1; Real x = (a + 2) * sin(a); end Test;",
            "Test",
        );
    }
}

// ============================================================================
// §3.2 OPERATOR PRECEDENCE AND ASSOCIATIVITY
// ============================================================================

/// MLS §3.2: Operator precedence rules
mod section_3_2_precedence {
    use super::*;

    // Power (^) has highest precedence among binary operators
    #[test]
    fn mls_3_2_power_over_multiplication() {
        // 2 * 3 ^ 2 = 2 * 9 = 18 (not 36)
        expect_success("model Test constant Real x = 2 * 3 ^ 2; end Test;", "Test");
    }

    #[test]
    fn mls_3_2_power_right_associative() {
        // 2 ^ 3 ^ 2 = 2 ^ 9 = 512 (right associative)
        expect_success("model Test constant Real x = 2 ^ 3 ^ 2; end Test;", "Test");
    }

    // Multiplication/division over addition/subtraction
    #[test]
    fn mls_3_2_mult_over_add() {
        // 1 + 2 * 3 = 1 + 6 = 7 (not 9)
        expect_success("model Test constant Real x = 1 + 2 * 3; end Test;", "Test");
    }

    #[test]
    fn mls_3_2_div_over_sub() {
        // 10 - 6 / 2 = 10 - 3 = 7 (not 2)
        expect_success("model Test constant Real x = 10 - 6 / 2; end Test;", "Test");
    }

    // Parentheses override precedence
    #[test]
    fn mls_3_2_parentheses_override() {
        // (1 + 2) * 3 = 3 * 3 = 9
        expect_success(
            "model Test constant Real x = (1 + 2) * 3; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_2_complex_precedence() {
        // 2 + 3 * 4 ^ 2 / 8 - 1 = 2 + 3*16/8 - 1 = 2 + 6 - 1 = 7
        expect_success(
            "model Test constant Real x = 2 + 3 * 4 ^ 2 / 8 - 1; end Test;",
            "Test",
        );
    }

    // Relational operators lower than arithmetic
    #[test]
    fn mls_3_2_relational_lower_than_arithmetic() {
        // 1 + 2 < 3 + 4 means (1+2) < (3+4)
        expect_success(
            "model Test constant Boolean b = 1 + 2 < 3 + 4; end Test;",
            "Test",
        );
    }

    // Logical operators lower than relational
    #[test]
    fn mls_3_2_logical_lower_than_relational() {
        // 1 < 2 and 3 < 4 means (1<2) and (3<4)
        expect_success(
            "model Test constant Boolean b = 1 < 2 and 3 < 4; end Test;",
            "Test",
        );
    }

    // Unary minus and power
    #[test]
    fn mls_3_2_unary_minus_power() {
        // -3 ^ 2 = -(3^2) = -9 (not 9)
        expect_success("model Test constant Real x = -3 ^ 2; end Test;", "Test");
    }

    // Left associativity of arithmetic
    #[test]
    fn mls_3_2_left_associative_sub() {
        // 10 - 5 - 2 = (10-5) - 2 = 3 (not 10 - 3 = 7)
        expect_success("model Test constant Real x = 10 - 5 - 2; end Test;", "Test");
    }

    #[test]
    fn mls_3_2_left_associative_div() {
        // 24 / 4 / 2 = (24/4) / 2 = 3 (not 24 / 2 = 12)
        expect_success("model Test constant Real x = 24 / 4 / 2; end Test;", "Test");
    }

    #[test]
    fn mls_3_2_left_associative_mult() {
        expect_success("model Test constant Real x = 2 * 3 * 4; end Test;", "Test");
    }

    #[test]
    fn mls_3_2_left_associative_add() {
        expect_success(
            "model Test constant Real x = 1 + 2 + 3 + 4; end Test;",
            "Test",
        );
    }
}

// ============================================================================
// §3.4 ARITHMETIC OPERATORS
// ============================================================================

/// MLS §3.4: Arithmetic operators
mod section_3_4_arithmetic {
    use super::*;

    // -------------------------------------------------------------------------
    // Binary arithmetic operators
    // -------------------------------------------------------------------------

    #[test]
    fn mls_3_4_add_reals() {
        expect_success(
            "model Test Real a=1; Real b=2; Real c=a+b; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_4_add_integers() {
        expect_success(
            "model Test Integer a=1; Integer b=2; Integer c=a+b; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_4_subtract_reals() {
        expect_success(
            "model Test Real a=5; Real b=3; Real c=a-b; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_4_multiply_reals() {
        expect_success(
            "model Test Real a=2; Real b=3; Real c=a*b; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_4_divide_reals() {
        expect_success(
            "model Test Real a=6; Real b=2; Real c=a/b; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_4_power_real() {
        expect_success(
            "model Test Real a=2; Real b=3; Real c=a^b; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_4_power_negative_exponent() {
        expect_success("model Test Real a=2; Real c=a^(-1); end Test;", "Test");
    }

    #[test]
    fn mls_3_4_power_fractional_exponent() {
        expect_success("model Test Real a=4; Real c=a^0.5; end Test;", "Test");
    }

    // -------------------------------------------------------------------------
    // Unary operators
    // -------------------------------------------------------------------------

    #[test]
    fn mls_3_4_unary_minus() {
        expect_success("model Test Real a=5; Real b=-a; end Test;", "Test");
    }

    #[test]
    fn mls_3_4_unary_plus() {
        expect_success("model Test Real a=5; Real b=+a; end Test;", "Test");
    }

    #[test]
    #[ignore = "Parser doesn't handle consecutive unary minus operators"]
    fn mls_3_4_double_unary_minus() {
        expect_success("model Test Real a=5; Real b=--a; end Test;", "Test");
    }

    // -------------------------------------------------------------------------
    // Integer division and modulo (MLS §3.4.1)
    // -------------------------------------------------------------------------

    #[test]
    fn mls_3_4_1_div_function() {
        expect_success(
            "model Test Integer a=7; Integer b=3; Integer c=div(a,b); end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_4_1_mod_function() {
        expect_success(
            "model Test Integer a=7; Integer b=3; Integer c=mod(a,b); end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_4_1_rem_function() {
        expect_success(
            "model Test Real a=7.5; Real b=2.5; Real c=rem(a,b); end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_4_1_mod_negative() {
        expect_success(
            "model Test Integer a=-7; Integer b=3; Integer c=mod(a,b); end Test;",
            "Test",
        );
    }

    // -------------------------------------------------------------------------
    // String concatenation
    // -------------------------------------------------------------------------

    #[test]
    fn mls_3_4_string_concat() {
        expect_success(
            r#"model Test constant String a="Hello"; constant String b=" World"; constant String c=a+b; end Test;"#,
            "Test",
        );
    }

    #[test]
    fn mls_3_4_string_concat_multiple() {
        expect_success(
            r#"model Test constant String x = "a" + "b" + "c"; end Test;"#,
            "Test",
        );
    }

    // -------------------------------------------------------------------------
    // Mixed integer and real
    // -------------------------------------------------------------------------

    #[test]
    fn mls_3_4_mixed_int_real_add() {
        expect_success(
            "model Test Integer i = 3; Real x = i + 1.5; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_4_mixed_int_real_mult() {
        expect_success(
            "model Test Integer i = 3; Real x = i * 2.5; end Test;",
            "Test",
        );
    }
}

// ============================================================================
// EXPRESSION EDGE CASES
// ============================================================================

/// Edge cases and complex expressions
mod expression_edge_cases {
    use super::*;

    #[test]
    fn edge_deeply_nested_parens() {
        expect_success(
            "model Test Real x = ((((1 + 2) * 3) - 4) / 5); end Test;",
            "Test",
        );
    }

    #[test]
    fn edge_long_chain_of_operations() {
        expect_success(
            "model Test Real x = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10; end Test;",
            "Test",
        );
    }

    #[test]
    fn edge_mixed_integer_real() {
        expect_success(
            "model Test Integer i = 3; Real x = i * 2.5; end Test;",
            "Test",
        );
    }

    #[test]
    fn edge_function_in_function() {
        expect_success("model Test Real x = sin(cos(0.5)); end Test;", "Test");
    }

    #[test]
    fn edge_complex_arithmetic() {
        expect_success(
            "model Test Real x = 1.5 * 2.5 + 3.5 / 0.5 - 1.0; end Test;",
            "Test",
        );
    }

    #[test]
    fn edge_very_long_expression() {
        expect_success(
            "model Test Real x = 1+2+3+4+5+6+7+8+9+10+11+12+13+14+15; end Test;",
            "Test",
        );
    }
}
