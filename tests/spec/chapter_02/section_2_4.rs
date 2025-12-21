//! MLS §2.4 Literals
//!
//! Tests for:
//! - §2.4.1: Integer Literals
//! - §2.4.2: Real Literals
//! - §2.4.3: Boolean Literals
//! - §2.4.4: String Literals
//!
//! Reference: https://specification.modelica.org/master/lexicalstructure.html

use crate::spec::expect_success;

// ============================================================================
// §2.4.1 INTEGER LITERALS
// ============================================================================

/// MLS §2.4.1: Integer Literals
mod section_2_4_1_integer_literals {
    use super::*;

    #[test]
    fn mls_2_4_1_integer_zero() {
        expect_success("model Test constant Integer x = 0; end Test;", "Test");
    }

    #[test]
    fn mls_2_4_1_integer_positive() {
        expect_success("model Test constant Integer x = 42; end Test;", "Test");
    }

    #[test]
    fn mls_2_4_1_integer_large() {
        expect_success(
            "model Test constant Integer x = 2147483647; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_2_4_1_integer_multi_digit() {
        expect_success(
            "model Test constant Integer x = 123456789; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_2_4_1_integer_in_expression() {
        expect_success("model Test constant Integer x = 10 + 20; end Test;", "Test");
    }

    #[test]
    fn mls_2_4_1_integer_negative() {
        expect_success("model Test constant Integer x = -42; end Test;", "Test");
    }

    #[test]
    fn mls_2_4_1_integer_one() {
        expect_success("model Test constant Integer x = 1; end Test;", "Test");
    }

    #[test]
    fn mls_2_4_1_integer_leading_zeros_in_expr() {
        // Note: 007 is valid (parsed as 7)
        expect_success("model Test constant Integer x = 007; end Test;", "Test");
    }

    #[test]
    fn mls_2_4_1_integer_in_array() {
        expect_success(
            "model Test constant Integer x[3] = {1, 2, 3}; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_2_4_1_integer_arithmetic() {
        expect_success(
            "model Test constant Integer x = 10 * 5 + 3 - 2; end Test;",
            "Test",
        );
    }
}

// ============================================================================
// §2.4.2 REAL LITERALS
// ============================================================================

/// MLS §2.4.2: Real Literals
mod section_2_4_2_real_literals {
    use super::*;

    #[test]
    fn mls_2_4_2_real_decimal() {
        expect_success("model Test constant Real x = 3.14159; end Test;", "Test");
    }

    #[test]
    fn mls_2_4_2_real_no_integer_part() {
        expect_success("model Test constant Real x = .5; end Test;", "Test");
    }

    #[test]
    fn mls_2_4_2_real_no_fraction() {
        expect_success("model Test constant Real x = 5.; end Test;", "Test");
    }

    #[test]
    fn mls_2_4_2_real_exponent_lowercase() {
        expect_success("model Test constant Real x = 1e10; end Test;", "Test");
    }

    #[test]
    fn mls_2_4_2_real_exponent_uppercase() {
        expect_success("model Test constant Real x = 1E10; end Test;", "Test");
    }

    #[test]
    fn mls_2_4_2_real_exponent_positive() {
        expect_success("model Test constant Real x = 1.5e+3; end Test;", "Test");
    }

    #[test]
    fn mls_2_4_2_real_exponent_negative() {
        expect_success("model Test constant Real x = 1.5e-3; end Test;", "Test");
    }

    #[test]
    fn mls_2_4_2_real_exponent_only() {
        expect_success("model Test constant Real x = 2e5; end Test;", "Test");
    }

    #[test]
    fn mls_2_4_2_real_decimal_exponent() {
        expect_success("model Test constant Real x = 1.5e2; end Test;", "Test");
    }

    #[test]
    fn mls_2_4_2_real_large_exponent() {
        expect_success("model Test constant Real x = 1e308; end Test;", "Test");
    }

    #[test]
    fn mls_2_4_2_real_small_exponent() {
        expect_success("model Test constant Real x = 1e-308; end Test;", "Test");
    }

    #[test]
    fn mls_2_4_2_real_zero() {
        expect_success("model Test constant Real x = 0.0; end Test;", "Test");
    }

    #[test]
    fn mls_2_4_2_real_negative() {
        expect_success("model Test constant Real x = -3.14; end Test;", "Test");
    }

    #[test]
    fn mls_2_4_2_real_very_small() {
        expect_success("model Test constant Real x = 0.0001; end Test;", "Test");
    }

    #[test]
    fn mls_2_4_2_real_very_large() {
        expect_success(
            "model Test constant Real x = 9999999.999; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_2_4_2_real_negative_exponent_decimal() {
        expect_success("model Test constant Real x = 3.14e-2; end Test;", "Test");
    }

    #[test]
    fn mls_2_4_2_real_in_array() {
        expect_success(
            "model Test constant Real x[3] = {1.1, 2.2, 3.3}; end Test;",
            "Test",
        );
    }
}

// ============================================================================
// §2.4.3 BOOLEAN LITERALS
// ============================================================================

/// MLS §2.4.3: Boolean Literals
mod section_2_4_3_boolean_literals {
    use super::*;

    #[test]
    fn mls_2_4_3_boolean_true() {
        expect_success("model Test constant Boolean x = true; end Test;", "Test");
    }

    #[test]
    fn mls_2_4_3_boolean_false() {
        expect_success("model Test constant Boolean x = false; end Test;", "Test");
    }

    #[test]
    fn mls_2_4_3_boolean_in_expression() {
        expect_success(
            "model Test constant Boolean x = true and false; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_2_4_3_boolean_not() {
        expect_success(
            "model Test constant Boolean x = not true; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_2_4_3_boolean_or() {
        expect_success(
            "model Test constant Boolean x = true or false; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_2_4_3_boolean_complex() {
        expect_success(
            "model Test constant Boolean x = (true and false) or (not false); end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_2_4_3_boolean_in_array() {
        expect_success(
            "model Test constant Boolean x[3] = {true, false, true}; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_2_4_3_boolean_comparison() {
        expect_success("model Test constant Boolean x = 1 < 2; end Test;", "Test");
    }
}

// ============================================================================
// §2.4.4 STRING LITERALS
// ============================================================================

/// MLS §2.4.4: String Literals
mod section_2_4_4_string_literals {
    use super::*;

    #[test]
    fn mls_2_4_4_string_simple() {
        expect_success(
            r#"model Test constant String x = "hello"; end Test;"#,
            "Test",
        );
    }

    #[test]
    fn mls_2_4_4_string_empty() {
        expect_success(r#"model Test constant String x = ""; end Test;"#, "Test");
    }

    #[test]
    fn mls_2_4_4_string_with_spaces() {
        expect_success(
            r#"model Test constant String x = "hello world"; end Test;"#,
            "Test",
        );
    }

    #[test]
    fn mls_2_4_4_string_escape_newline() {
        expect_success(
            r#"model Test constant String x = "line1\nline2"; end Test;"#,
            "Test",
        );
    }

    #[test]
    fn mls_2_4_4_string_escape_tab() {
        expect_success(
            r#"model Test constant String x = "col1\tcol2"; end Test;"#,
            "Test",
        );
    }

    #[test]
    fn mls_2_4_4_string_escape_quote() {
        expect_success(
            r#"model Test constant String x = "say \"hello\""; end Test;"#,
            "Test",
        );
    }

    #[test]
    fn mls_2_4_4_string_escape_backslash() {
        expect_success(
            r#"model Test constant String x = "path\\to\\file"; end Test;"#,
            "Test",
        );
    }

    #[test]
    fn mls_2_4_4_string_escape_carriage_return() {
        expect_success(
            r#"model Test constant String x = "a\rb"; end Test;"#,
            "Test",
        );
    }

    #[test]
    fn mls_2_4_4_string_numbers() {
        expect_success(
            r#"model Test constant String x = "12345"; end Test;"#,
            "Test",
        );
    }

    #[test]
    fn mls_2_4_4_string_mixed_content() {
        expect_success(
            r#"model Test constant String x = "Value = 42.5 m/s"; end Test;"#,
            "Test",
        );
    }

    #[test]
    fn mls_2_4_4_string_concatenation() {
        expect_success(
            r#"model Test constant String a = "Hello"; constant String b = " World"; constant String c = a + b; end Test;"#,
            "Test",
        );
    }

    #[test]
    fn mls_2_4_4_string_in_array() {
        expect_success(
            r#"model Test constant String x[2] = {"hello", "world"}; end Test;"#,
            "Test",
        );
    }

    #[test]
    fn mls_2_4_4_string_special_chars() {
        expect_success(
            r#"model Test constant String x = "!@#$%^&*()"; end Test;"#,
            "Test",
        );
    }

    #[test]
    fn mls_2_4_4_string_unicode_escape() {
        expect_success(
            r#"model Test constant String x = "degree: \u00B0"; end Test;"#,
            "Test",
        );
    }

    #[test]
    fn mls_2_4_4_string_single_char() {
        expect_success(r#"model Test constant String x = "a"; end Test;"#, "Test");
    }

    #[test]
    fn mls_2_4_4_string_long() {
        expect_success(
            r#"model Test constant String x = "This is a very long string that contains many characters and tests the parser's ability to handle longer string literals correctly."; end Test;"#,
            "Test",
        );
    }
}
