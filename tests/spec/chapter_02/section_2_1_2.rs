//! MLS §2.1 Character Set and §2.2 Comments
//!
//! Tests for:
//! - §2.1: Unicode character set, whitespace handling
//! - §2.2: Single-line comments (//), multi-line comments (/* */)
//!
//! Reference: https://specification.modelica.org/master/lexicalstructure.html

use crate::spec::{expect_parse_failure, expect_parse_success};

// ============================================================================
// §2.1 CHARACTER SET
// ============================================================================

/// MLS §2.1: Modelica source code uses Unicode character set
mod section_2_1_character_set {
    use super::*;

    #[test]
    fn mls_2_1_ascii_source() {
        expect_parse_success("model Test Real x; end Test;");
    }

    #[test]
    fn mls_2_1_whitespace_spaces() {
        expect_parse_success("model   Test   Real   x;   end   Test;");
    }

    #[test]
    fn mls_2_1_whitespace_tabs() {
        expect_parse_success("model\tTest\tReal\tx;\tend\tTest;");
    }

    #[test]
    fn mls_2_1_whitespace_newlines() {
        expect_parse_success(
            r#"
            model Test
                Real x;
            end Test;
            "#,
        );
    }

    #[test]
    fn mls_2_1_mixed_whitespace() {
        expect_parse_success("model Test\n\tReal x;\r\nend Test;");
    }

    #[test]
    fn mls_2_1_multiple_blank_lines() {
        expect_parse_success(
            r#"
            model Test


                Real x;


            end Test;
            "#,
        );
    }

    #[test]
    fn mls_2_1_leading_whitespace() {
        expect_parse_success("   \t\n  model Test Real x; end Test;");
    }

    #[test]
    fn mls_2_1_trailing_whitespace() {
        expect_parse_success("model Test Real x; end Test;   \t\n  ");
    }
}

// ============================================================================
// §2.2 COMMENTS
// ============================================================================

/// MLS §2.2: Single-line and multi-line comments
mod section_2_2_comments {
    use super::*;

    // -------------------------------------------------------------------------
    // Single-line comments (//)
    // -------------------------------------------------------------------------

    #[test]
    fn mls_2_2_single_line_comment_basic() {
        expect_parse_success(
            r#"
            model Test
                // This is a comment
                Real x;
            end Test;
            "#,
        );
    }

    #[test]
    fn mls_2_2_single_line_comment_at_end() {
        expect_parse_success("model Test Real x; end Test; // comment");
    }

    #[test]
    fn mls_2_2_single_line_comment_multiple() {
        expect_parse_success(
            r#"
            model Test
                // First comment
                // Second comment
                Real x; // Inline comment
            end Test;
            "#,
        );
    }

    #[test]
    fn mls_2_2_single_line_comment_with_special_chars() {
        expect_parse_success(
            r#"
            model Test
                // Special: /* */ // nested? @#$%^&*
                Real x;
            end Test;
            "#,
        );
    }

    #[test]
    fn mls_2_2_single_line_comment_empty() {
        expect_parse_success(
            r#"
            model Test
                //
                Real x;
            end Test;
            "#,
        );
    }

    #[test]
    fn mls_2_2_single_line_comment_with_keywords() {
        expect_parse_success(
            r#"
            model Test
                // model equation if then else
                Real x;
            end Test;
            "#,
        );
    }

    #[test]
    fn mls_2_2_single_line_comment_only_file() {
        expect_parse_success("// Just a comment");
    }

    // -------------------------------------------------------------------------
    // Multi-line comments (/* */)
    // -------------------------------------------------------------------------

    #[test]
    fn mls_2_2_multi_line_comment_basic() {
        expect_parse_success(
            r#"
            model Test
                /* This is a comment */
                Real x;
            end Test;
            "#,
        );
    }

    #[test]
    fn mls_2_2_multi_line_comment_spans_lines() {
        expect_parse_success(
            r#"
            model Test
                /* This is a
                   multi-line
                   comment */
                Real x;
            end Test;
            "#,
        );
    }

    #[test]
    fn mls_2_2_multi_line_comment_inline() {
        expect_parse_success("model Test /* comment */ Real x; end Test;");
    }

    #[test]
    fn mls_2_2_multi_line_comment_multiple() {
        expect_parse_success(
            r#"
            model Test
                /* First */ Real x; /* Second */
            end Test;
            "#,
        );
    }

    #[test]
    fn mls_2_2_multi_line_comment_empty() {
        expect_parse_success("model Test /**/ Real x; end Test;");
    }

    #[test]
    fn mls_2_2_multi_line_comment_with_stars() {
        expect_parse_success(
            r#"
            model Test
                /*******
                 * Star pattern
                 *******/
                Real x;
            end Test;
            "#,
        );
    }

    #[test]
    fn mls_2_2_multi_line_comment_with_slashes() {
        expect_parse_success(
            r#"
            model Test
                /* Contains // slashes */
                Real x;
            end Test;
            "#,
        );
    }

    #[test]
    fn mls_2_2_multi_line_comment_between_tokens() {
        expect_parse_success("model/* comment */Test/* another */Real x;end Test;");
    }

    /// MLS §2.2: "Comments are not allowed to be nested"
    /// This test is ignored because nested comment handling needs review
    #[test]
    fn mls_2_2_nested_comments_not_allowed() {
        // Per spec, /* inside a comment is just text, first */ ends the comment.
        // So "still outer */" would be treated as code which should cause an error.
        expect_parse_failure(
            r#"
            model Test
                /* outer /* inner */ still outer */
                Real x;
            end Test;
            "#,
        );
    }

    // -------------------------------------------------------------------------
    // Comment edge cases
    // -------------------------------------------------------------------------

    #[test]
    fn mls_2_2_comment_at_eof_no_newline() {
        expect_parse_success("model Test Real x; end Test; // no newline");
    }

    #[test]
    fn mls_2_2_comment_in_string_not_comment() {
        // Comments inside strings should be treated as string content
        expect_parse_success(
            r#"model Test constant String s = "/* not a comment */ // also not"; end Test;"#,
        );
    }
}
