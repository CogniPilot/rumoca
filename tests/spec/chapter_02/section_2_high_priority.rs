//! MLS §2: High Priority Lexical Structure Tests
//!
//! This module tests high priority normative requirements:
//! - §2.3.3: Missing keywords (elsewhen, expandable)
//! - §2.3.1: Q-IDENT escape sequences
//! - §2.4.4: String escape sequences
//! - §2.1: Unicode handling
//!
//! Reference: https://specification.modelica.org/master/lexicalstructure.html

use crate::spec::{expect_parse_failure, expect_parse_success, expect_success};

// ============================================================================
// §2.3.3 MISSING KEYWORDS
// ============================================================================

/// MLS §2.3.3: Additional keywords that cannot be used as identifiers
mod missing_keywords {
    use super::*;

    /// MLS: "elsewhen is a keyword"
    #[test]
    fn mls_2_3_3_keyword_elsewhen() {
        expect_parse_failure("model Test Real elsewhen; end Test;");
    }

    /// MLS: "expandable is a keyword"
    #[test]
    fn mls_2_3_3_keyword_expandable() {
        expect_parse_failure("model Test Real expandable; end Test;");
    }

    /// MLS: Valid use of elsewhen in when-equation
    #[test]
    fn mls_2_3_3_elsewhen_valid_usage() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                discrete Integer state(start = 0);
            equation
                der(x) = 1;
                when x > 1 then
                    state = 1;
                elsewhen x > 2 then
                    state = 2;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "operator is a keyword"
    #[test]
    fn mls_2_3_3_keyword_operator_used() {
        expect_parse_failure("model Test Real operator; end Test;");
    }
}

// ============================================================================
// §2.3.1 Q-IDENT ESCAPE SEQUENCES
// ============================================================================

/// MLS §2.3.1: Q-IDENT escape sequences
mod qident_escapes {
    use super::*;

    /// MLS: "Q-IDENT can contain backslash escape"
    #[test]
    #[ignore = "Q-IDENT backslash escape not yet supported"]
    fn mls_2_3_1_qident_backslash() {
        expect_parse_success("model Test Real 'path\\\\file'; end Test;");
    }

    /// MLS: "Q-IDENT can contain single quote escape"
    #[test]
    #[ignore = "Q-IDENT single quote escape not yet supported"]
    fn mls_2_3_1_qident_single_quote_escape() {
        expect_parse_success("model Test Real 'it\\'s'; end Test;");
    }

    /// MLS: Q-IDENT with newline escape
    #[test]
    #[ignore = "Q-IDENT newline escape not yet supported"]
    fn mls_2_3_1_qident_newline_escape() {
        expect_parse_success("model Test Real 'line1\\nline2'; end Test;");
    }

    /// Valid: Q-IDENT with special characters (no escapes needed)
    #[test]
    fn mls_2_3_1_qident_special_chars() {
        expect_parse_success("model Test Real 'a.b.c'; end Test;");
    }

    /// Valid: Q-IDENT starting with digit
    #[test]
    fn mls_2_3_1_qident_digit_start() {
        expect_parse_success("model Test Real '123var'; end Test;");
    }

    /// Valid: Q-IDENT with spaces
    #[test]
    fn mls_2_3_1_qident_spaces() {
        expect_parse_success("model Test Real 'my variable name'; end Test;");
    }

    /// Valid: Q-IDENT containing keyword
    #[test]
    fn mls_2_3_1_qident_keyword_inside() {
        expect_parse_success("model Test Real 'model value'; end Test;");
    }
}

// ============================================================================
// §2.4.4 STRING ESCAPE SEQUENCES
// ============================================================================

/// MLS §2.4.4: String escape sequences
mod string_escapes {
    use super::*;

    /// MLS: "\\a is alert (bell)"
    #[test]
    fn mls_2_4_4_string_escape_alert() {
        expect_success(
            r#"model Test constant String s = "bell\a"; end Test;"#,
            "Test",
        );
    }

    /// MLS: "\\b is backspace"
    #[test]
    fn mls_2_4_4_string_escape_backspace() {
        expect_success(
            r#"model Test constant String s = "back\bspace"; end Test;"#,
            "Test",
        );
    }

    /// MLS: "\\f is form feed"
    #[test]
    fn mls_2_4_4_string_escape_formfeed() {
        expect_success(
            r#"model Test constant String s = "form\ffeed"; end Test;"#,
            "Test",
        );
    }

    /// MLS: "\\v is vertical tab"
    #[test]
    fn mls_2_4_4_string_escape_vtab() {
        expect_success(
            r#"model Test constant String s = "vertical\vtab"; end Test;"#,
            "Test",
        );
    }

    /// MLS: "\\? is question mark"
    #[test]
    fn mls_2_4_4_string_escape_question() {
        expect_success(
            r#"model Test constant String s = "what\?"; end Test;"#,
            "Test",
        );
    }

    /// MLS: "\\xNN is hex character"
    #[test]
    fn mls_2_4_4_string_escape_hex() {
        expect_success(
            r#"model Test constant String s = "hex\x41"; end Test;"#,
            "Test",
        );
    }

    /// MLS: "\\OOO is octal character"
    #[test]
    fn mls_2_4_4_string_escape_octal() {
        expect_success(
            r#"model Test constant String s = "octal\101"; end Test;"#,
            "Test",
        );
    }

    /// Valid: Common escapes that should work
    #[test]
    fn mls_2_4_4_string_escape_common() {
        expect_success(
            r#"model Test
                constant String s1 = "line1\nline2";
                constant String s2 = "tab\there";
                constant String s3 = "quote\"here";
                constant String s4 = "back\\slash";
            end Test;"#,
            "Test",
        );
    }
}

// ============================================================================
// §2.1 UNICODE HANDLING
// ============================================================================

/// MLS §2.1: Unicode character handling
mod unicode_handling {
    use super::*;

    /// MLS: "Modelica source uses Unicode"
    #[test]
    #[ignore = "Unicode identifiers not yet supported"]
    fn mls_2_1_unicode_identifier() {
        expect_parse_success("model Test Real α; end Test;");
    }

    /// MLS: "Unicode in strings"
    #[test]
    fn mls_2_1_unicode_in_string() {
        expect_success(
            r#"model Test constant String s = "Temperature: 25°C"; end Test;"#,
            "Test",
        );
    }

    /// MLS: "Unicode Greek letters"
    #[test]
    #[ignore = "Unicode Greek letter identifiers not yet supported"]
    fn mls_2_1_unicode_greek() {
        expect_parse_success("model Test Real Δx; Real θ; end Test;");
    }

    /// MLS: "Unicode subscripts/superscripts"
    #[test]
    #[ignore = "Unicode subscript/superscript identifiers not yet supported"]
    fn mls_2_1_unicode_subscript() {
        expect_parse_success("model Test Real x₁; Real x₂; end Test;");
    }
}

// ============================================================================
// §2.2 COMMENT HANDLING
// ============================================================================

/// MLS §2.2: Additional comment tests
mod comment_handling {
    use super::*;

    /// MLS: "Comments cannot be nested"
    #[test]
    fn mls_2_2_nested_comment_rejected() {
        // This should fail because /* inside comment starts nothing,
        // so "still in comment */" becomes code
        expect_parse_failure(
            r#"
            model Test
                /* outer /* inner */ still in comment */
                Real x;
            end Test;
            "#,
        );
    }

    /// Valid: Comment containing comment-like syntax (as text)
    #[test]
    fn mls_2_2_comment_text_with_slashes() {
        expect_parse_success(
            r#"
            model Test
                // URL: http://example.com
                Real x;
            end Test;
            "#,
        );
    }

    /// Valid: Multiline comment with stars
    #[test]
    fn mls_2_2_multiline_with_stars() {
        expect_parse_success(
            r#"
            model Test
                /*********************
                 * Documentation block
                 *********************/
                Real x;
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §2.4 LITERAL EDGE CASES
// ============================================================================

/// MLS §2.4: Literal edge cases
mod literal_edge_cases {
    use super::*;

    /// MLS: Very large integer literal
    #[test]
    fn mls_2_4_large_integer() {
        expect_success(
            "model Test constant Integer n = 9223372036854775807; end Test;",
            "Test",
        );
    }

    /// MLS: Very small real literal
    #[test]
    fn mls_2_4_very_small_real() {
        expect_success("model Test constant Real x = 1e-308; end Test;", "Test");
    }

    /// MLS: Very large real literal
    #[test]
    fn mls_2_4_very_large_real() {
        expect_success("model Test constant Real x = 1e308; end Test;", "Test");
    }

    /// MLS: Real with only decimal point (no digits before)
    #[test]
    fn mls_2_4_real_dot_start() {
        expect_success("model Test constant Real x = .5; end Test;", "Test");
    }

    /// MLS: Real with only decimal point (no digits after)
    #[test]
    fn mls_2_4_real_dot_end() {
        expect_success("model Test constant Real x = 5.; end Test;", "Test");
    }

    /// MLS: Integer with leading zeros
    #[test]
    fn mls_2_4_integer_leading_zeros() {
        expect_success("model Test constant Integer n = 007; end Test;", "Test");
    }

    /// MLS: Empty string
    #[test]
    fn mls_2_4_empty_string() {
        expect_success(r#"model Test constant String s = ""; end Test;"#, "Test");
    }

    /// MLS: String with only whitespace
    #[test]
    fn mls_2_4_whitespace_string() {
        expect_success(r#"model Test constant String s = "   "; end Test;"#, "Test");
    }
}
