//! MLS §2: Low Priority Lexical Tests
//!
//! This module tests low priority normative requirements:
//! - §2.1: BOM handling
//! - §2.4: Octal escape sequences, additional escape handling
//!
//! Reference: https://specification.modelica.org/master/lexical-structure.html

use crate::spec::expect_parse_success;

// ============================================================================
// §2.1 BOM HANDLING
// ============================================================================

/// MLS §2.1: Byte Order Mark handling
mod bom_handling {
    use super::*;

    /// MLS: "UTF-8 BOM should be handled"
    #[test]
    fn mls_2_1_utf8_bom() {
        // UTF-8 BOM is EF BB BF at start of file
        // This test would need special handling to include actual BOM bytes
        expect_parse_success(
            r#"
            model Test
                Real x = 1;
            equation
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §2.4 ADDITIONAL ESCAPE SEQUENCES
// ============================================================================

/// MLS §2.4: Low priority escape sequences
mod additional_escapes {
    use super::*;

    /// MLS: "Octal escape sequence \\OOO"
    #[test]
    fn mls_2_4_octal_escape_basic() {
        expect_parse_success(
            r#"
            model Test
                String s = "\101";
            equation
            end Test;
            "#,
        );
    }

    /// MLS: "Octal escape for tab (\\011)"
    #[test]
    fn mls_2_4_octal_escape_tab() {
        expect_parse_success(
            r#"
            model Test
                String s = "\011";
            equation
            end Test;
            "#,
        );
    }

    /// MLS: "Octal escape for newline (\\012)"
    #[test]
    fn mls_2_4_octal_escape_newline() {
        expect_parse_success(
            r#"
            model Test
                String s = "\012";
            equation
            end Test;
            "#,
        );
    }

    /// MLS: "Octal escape sequence with 1 digit"
    #[test]
    fn mls_2_4_octal_escape_1_digit() {
        expect_parse_success(
            r#"
            model Test
                String s = "\0";
            equation
            end Test;
            "#,
        );
    }

    /// MLS: "Octal escape sequence with 2 digits"
    #[test]
    fn mls_2_4_octal_escape_2_digits() {
        expect_parse_success(
            r#"
            model Test
                String s = "\77";
            equation
            end Test;
            "#,
        );
    }

    /// MLS: "Octal escape sequence with 3 digits"
    #[test]
    fn mls_2_4_octal_escape_3_digits() {
        expect_parse_success(
            r#"
            model Test
                String s = "\377";
            equation
            end Test;
            "#,
        );
    }

    /// MLS: "Mixed octal and regular text"
    #[test]
    fn mls_2_4_octal_mixed() {
        expect_parse_success(
            r#"
            model Test
                String s = "A\101B";
            equation
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §2.4 STRING LINE CONTINUATION
// ============================================================================

/// MLS §2.4: String line continuation
mod string_continuation {
    use super::*;

    /// MLS: "String continuation with backslash at end of line"
    #[test]
    #[ignore = "String line continuation not yet supported"]
    fn mls_2_4_string_continuation() {
        expect_parse_success(
            r#"
            model Test
                String s = "This is a very long string that \
continues on the next line";
            equation
            end Test;
            "#,
        );
    }

    /// MLS: "Multiple line continuation"
    #[test]
    #[ignore = "String line continuation not yet supported"]
    fn mls_2_4_string_multi_continuation() {
        expect_parse_success(
            r#"
            model Test
                String s = "Line 1 \
Line 2 \
Line 3";
            equation
            end Test;
            "#,
        );
    }
}
