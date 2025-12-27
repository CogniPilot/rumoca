//! MLS §2.3 Identifiers, Names, and Keywords
//!
//! Tests for:
//! - §2.3.1: Identifiers (IDENT and Q-IDENT)
//! - §2.3.2: Names (dot-separated identifiers)
//! - §2.3.3: Modelica Keywords
//!
//! Reference: https://specification.modelica.org/master/lexicalstructure.html

use crate::spec::{expect_parse_failure, expect_parse_success};

// ============================================================================
// §2.3.1 IDENTIFIERS
// ============================================================================

/// MLS §2.3.1: Identifiers
mod section_2_3_1_identifiers {
    use super::*;

    // -------------------------------------------------------------------------
    // Basic IDENT rules
    // -------------------------------------------------------------------------

    #[test]
    fn mls_2_3_1_ident_letter_start() {
        expect_parse_success("model Abc Real x; end Abc;");
    }

    #[test]
    fn mls_2_3_1_ident_lowercase_start() {
        expect_parse_success("model abc Real x; end abc;");
    }

    #[test]
    fn mls_2_3_1_ident_underscore_start() {
        expect_parse_success("model _Test Real _x; end _Test;");
    }

    #[test]
    fn mls_2_3_1_ident_with_digits() {
        expect_parse_success("model Test123 Real x1y2z3; end Test123;");
    }

    #[test]
    fn mls_2_3_1_ident_mixed_case() {
        expect_parse_success("model CamelCase Real mixedCase; end CamelCase;");
    }

    #[test]
    fn mls_2_3_1_ident_all_underscores() {
        expect_parse_success("model Test Real ___; end Test;");
    }

    #[test]
    fn mls_2_3_1_ident_underscore_middle() {
        expect_parse_success("model Test Real my_variable; end Test;");
    }

    #[test]
    fn mls_2_3_1_ident_underscore_end() {
        expect_parse_success("model Test Real x_; end Test;");
    }

    #[test]
    fn mls_2_3_1_ident_single_letter() {
        expect_parse_success("model T Real x; end T;");
    }

    #[test]
    fn mls_2_3_1_ident_single_underscore() {
        expect_parse_success("model Test Real _; end Test;");
    }

    #[test]
    fn mls_2_3_1_ident_long_name() {
        expect_parse_success(
            "model VeryLongModelNameThatIsStillValid Real anotherLongVariableName; end VeryLongModelNameThatIsStillValid;",
        );
    }

    #[test]
    fn mls_2_3_1_ident_all_letters() {
        expect_parse_success(
            "model ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz Real x; end ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz;",
        );
    }

    // Cannot start with digit
    #[test]
    fn mls_2_3_1_ident_digit_start_invalid() {
        expect_parse_failure("model 1Test Real x; end 1Test;");
    }

    #[test]
    fn mls_2_3_1_ident_digit_start_var_invalid() {
        expect_parse_failure("model Test Real 1x; end Test;");
    }

    // -------------------------------------------------------------------------
    // Q-IDENT (quoted identifiers)
    // -------------------------------------------------------------------------

    #[test]
    fn mls_2_3_1_qident_with_dot() {
        expect_parse_success("model Test Real 'a.b'; end Test;");
    }

    #[test]
    fn mls_2_3_1_qident_with_space() {
        expect_parse_success("model Test Real 'my variable'; end Test;");
    }

    #[test]
    fn mls_2_3_1_qident_with_dash() {
        expect_parse_success("model Test Real 'x-y'; end Test;");
    }

    #[test]
    fn mls_2_3_1_qident_with_plus() {
        expect_parse_success("model Test Real 'x+y'; end Test;");
    }

    #[test]
    fn mls_2_3_1_qident_with_slash() {
        expect_parse_success("model Test Real 'x/y'; end Test;");
    }

    #[test]
    fn mls_2_3_1_qident_with_colon() {
        expect_parse_success("model Test Real 'a:b'; end Test;");
    }

    #[test]
    fn mls_2_3_1_qident_starts_with_digit() {
        expect_parse_success("model Test Real '123abc'; end Test;");
    }

    #[test]
    fn mls_2_3_1_qident_keyword_inside() {
        expect_parse_success("model Test Real 'model'; end Test;");
    }

    #[test]
    fn mls_2_3_1_qident_special_chars() {
        expect_parse_success("model Test Real 'x@#$%'; end Test;");
    }

    #[test]
    fn mls_2_3_1_qident_parentheses() {
        expect_parse_success("model Test Real 'f(x)'; end Test;");
    }

    #[test]
    fn mls_2_3_1_qident_brackets() {
        expect_parse_success("model Test Real 'a[1]'; end Test;");
    }

    /// Unicode in quoted identifiers
    #[test]
    fn mls_2_3_1_qident_unicode() {
        expect_parse_success("model Test Real 'αβγ'; end Test;");
    }

    /// Unicode symbols in quoted identifiers
    #[test]
    fn mls_2_3_1_qident_unicode_symbols() {
        expect_parse_success("model Test Real 'Δx'; end Test;");
    }
}

// ============================================================================
// §2.3.2 NAMES
// ============================================================================

/// MLS §2.3.2: Names (dot-separated identifiers)
mod section_2_3_2_names {
    use super::*;

    #[test]
    fn mls_2_3_2_simple_name() {
        expect_parse_success("model Test Real x; end Test;");
    }

    #[test]
    fn mls_2_3_2_qualified_name_in_package() {
        expect_parse_success(
            r#"
            package Pkg
                model M Real x; end M;
            end Pkg;
            "#,
        );
    }

    #[test]
    fn mls_2_3_2_name_with_global_prefix() {
        expect_parse_success(
            r#"
            model Test
                .Modelica.Constants.pi x;
            end Test;
            "#,
        );
    }

    #[test]
    fn mls_2_3_2_nested_packages() {
        expect_parse_success(
            r#"
            package A
                package B
                    package C
                        model M Real x; end M;
                    end C;
                end B;
            end A;
            "#,
        );
    }

    #[test]
    fn mls_2_3_2_qualified_type_reference() {
        expect_parse_success(
            r#"
            package Pkg
                type MyReal = Real;
            end Pkg;
            model Test
                Pkg.MyReal x;
            equation
                x = 1;
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §2.3.3 KEYWORDS
// ============================================================================

/// MLS §2.3.3: Keywords cannot be used as identifiers
mod section_2_3_3_keywords {
    use super::*;

    // -------------------------------------------------------------------------
    // Class-related keywords
    // -------------------------------------------------------------------------

    #[test]
    fn mls_2_3_3_keyword_model() {
        expect_parse_failure("model Test Real model; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_class() {
        expect_parse_failure("model Test Real class; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_block() {
        expect_parse_failure("model Test Real block; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_connector() {
        expect_parse_failure("model Test Real connector; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_record() {
        expect_parse_failure("model Test Real record; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_type() {
        expect_parse_failure("model Test Real type; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_package() {
        expect_parse_failure("model Test Real package; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_function() {
        expect_parse_failure("model Test Real function; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_operator() {
        expect_parse_failure("model Test Real operator; end Test;");
    }

    // -------------------------------------------------------------------------
    // Section keywords
    // -------------------------------------------------------------------------

    #[test]
    fn mls_2_3_3_keyword_equation() {
        expect_parse_failure("model Test Real equation; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_algorithm() {
        expect_parse_failure("model Test Real algorithm; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_public() {
        expect_parse_failure("model Test Real public; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_protected() {
        expect_parse_failure("model Test Real protected; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_initial() {
        expect_parse_failure("model Test Real initial; end Test;");
    }

    // -------------------------------------------------------------------------
    // Variability keywords
    // -------------------------------------------------------------------------

    #[test]
    fn mls_2_3_3_keyword_parameter() {
        expect_parse_failure("model Test Real parameter; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_constant() {
        expect_parse_failure("model Test Real constant; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_discrete() {
        expect_parse_failure("model Test Real discrete; end Test;");
    }

    // -------------------------------------------------------------------------
    // Causality keywords
    // -------------------------------------------------------------------------

    #[test]
    fn mls_2_3_3_keyword_input() {
        expect_parse_failure("model Test Real input; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_output() {
        expect_parse_failure("model Test Real output; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_flow() {
        expect_parse_failure("model Test Real flow; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_stream() {
        expect_parse_failure("model Test Real stream; end Test;");
    }

    // -------------------------------------------------------------------------
    // Control flow keywords
    // -------------------------------------------------------------------------

    #[test]
    fn mls_2_3_3_keyword_if() {
        expect_parse_failure("model Test Real if; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_then() {
        expect_parse_failure("model Test Real then; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_else() {
        expect_parse_failure("model Test Real else; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_elseif() {
        expect_parse_failure("model Test Real elseif; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_for() {
        expect_parse_failure("model Test Real for; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_loop() {
        expect_parse_failure("model Test Real loop; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_while() {
        expect_parse_failure("model Test Real while; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_when() {
        expect_parse_failure("model Test Real when; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_break() {
        expect_parse_failure("model Test Real break; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_return() {
        expect_parse_failure("model Test Real return; end Test;");
    }

    // -------------------------------------------------------------------------
    // Modifier keywords
    // -------------------------------------------------------------------------

    #[test]
    fn mls_2_3_3_keyword_extends() {
        expect_parse_failure("model Test Real extends; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_partial() {
        expect_parse_failure("model Test Real partial; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_final() {
        expect_parse_failure("model Test Real final; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_replaceable() {
        expect_parse_failure("model Test Real replaceable; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_redeclare() {
        expect_parse_failure("model Test Real redeclare; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_constrainedby() {
        expect_parse_failure("model Test Real constrainedby; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_encapsulated() {
        expect_parse_failure("model Test Real encapsulated; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_each() {
        expect_parse_failure("model Test Real each; end Test;");
    }

    // -------------------------------------------------------------------------
    // Scope/hierarchy keywords
    // -------------------------------------------------------------------------

    #[test]
    fn mls_2_3_3_keyword_inner() {
        expect_parse_failure("model Test Real inner; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_outer() {
        expect_parse_failure("model Test Real outer; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_import() {
        expect_parse_failure("model Test Real import; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_within() {
        expect_parse_failure("model Test Real within; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_end() {
        expect_parse_failure("model Test Real end; end Test;");
    }

    // -------------------------------------------------------------------------
    // Logical keywords
    // -------------------------------------------------------------------------

    #[test]
    fn mls_2_3_3_keyword_and() {
        expect_parse_failure("model Test Real and; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_or() {
        expect_parse_failure("model Test Real or; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_not() {
        expect_parse_failure("model Test Real not; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_true() {
        expect_parse_failure("model Test Real true; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_false() {
        expect_parse_failure("model Test Real false; end Test;");
    }

    // -------------------------------------------------------------------------
    // Other keywords
    // -------------------------------------------------------------------------

    #[test]
    fn mls_2_3_3_keyword_connect() {
        expect_parse_failure("model Test Real connect; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_der() {
        expect_parse_failure("model Test Real der; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_external() {
        expect_parse_failure("model Test Real external; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_pure() {
        expect_parse_failure("model Test Real pure; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_impure() {
        expect_parse_failure("model Test Real impure; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_annotation() {
        expect_parse_failure("model Test Real annotation; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_in() {
        expect_parse_failure("model Test Real in; end Test;");
    }

    #[test]
    fn mls_2_3_3_keyword_enumeration() {
        expect_parse_failure("model Test Real enumeration; end Test;");
    }
}
