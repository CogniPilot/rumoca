//! MLS §3.5-3.8: Relational, Logical, Built-in, and Variability
//!
//! Tests for:
//! - §3.5: Equality, relational, and logical operators
//! - §3.6: Miscellaneous operators (if-expression)
//! - §3.7: Built-in operators and functions
//! - §3.8: Variability of expressions
//!
//! Reference: https://specification.modelica.org/master/operatorsandexpressions.html

use crate::spec::expect_success;

// ============================================================================
// §3.5 EQUALITY, RELATIONAL, AND LOGICAL OPERATORS
// ============================================================================

/// MLS §3.5: Relational operators
mod section_3_5_relational {
    use super::*;

    #[test]
    fn mls_3_5_less_than() {
        expect_success(
            "model Test Real a=1; Real b=2; Boolean c=a<b; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_5_less_equal() {
        expect_success(
            "model Test Real a=1; Real b=2; Boolean c=a<=b; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_5_greater_than() {
        expect_success(
            "model Test Real a=2; Real b=1; Boolean c=a>b; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_5_greater_equal() {
        expect_success(
            "model Test Real a=2; Real b=1; Boolean c=a>=b; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_5_equal() {
        expect_success(
            "model Test Real a=1; Real b=1; Boolean c=a==b; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_5_not_equal() {
        expect_success(
            "model Test Real a=1; Real b=2; Boolean c=a<>b; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_5_string_equal() {
        expect_success(
            r#"model Test String a="hi"; String b="hi"; Boolean c=a==b; end Test;"#,
            "Test",
        );
    }

    #[test]
    fn mls_3_5_string_not_equal() {
        expect_success(
            r#"model Test String a="hi"; String b="bye"; Boolean c=a<>b; end Test;"#,
            "Test",
        );
    }

    #[test]
    fn mls_3_5_boolean_equal() {
        expect_success(
            "model Test Boolean a=true; Boolean b=true; Boolean c=a==b; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_5_integer_comparison() {
        expect_success(
            "model Test Integer a=5; Integer b=3; Boolean c=a>b; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_5_chained_comparison() {
        // a < b and b < c (chained comparisons need explicit 'and')
        expect_success(
            "model Test Real a=1; Real b=2; Real c=3; Boolean d=a<b and b<c; end Test;",
            "Test",
        );
    }
}

/// MLS §3.5: Logical operators
mod section_3_5_logical {
    use super::*;

    #[test]
    fn mls_3_5_and() {
        expect_success(
            "model Test Boolean a=true; Boolean b=false; Boolean c=a and b; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_5_or() {
        expect_success(
            "model Test Boolean a=true; Boolean b=false; Boolean c=a or b; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_5_not() {
        expect_success(
            "model Test Boolean a=true; Boolean b=not a; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_5_compound_and_or() {
        expect_success(
            "model Test Boolean a=true; Boolean b=false; Boolean c=true; Boolean d=a and b or c; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_5_not_precedence() {
        expect_success(
            "model Test Boolean a=true; Boolean b=false; Boolean c=not a or b; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_5_complex_logical() {
        expect_success(
            "model Test Boolean a=true; Boolean b=false; Boolean c=not (a and b) or (a or b); end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_5_multiple_and() {
        expect_success(
            "model Test Boolean a=true; Boolean b=true; Boolean c=true; Boolean d=a and b and c; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_5_multiple_or() {
        expect_success(
            "model Test Boolean a=false; Boolean b=false; Boolean c=true; Boolean d=a or b or c; end Test;",
            "Test",
        );
    }
}

// ============================================================================
// §3.6 MISCELLANEOUS OPERATORS
// ============================================================================

/// MLS §3.6: If-expression
mod section_3_6_if_expression {
    use super::*;

    #[test]
    fn mls_3_6_if_simple() {
        expect_success(
            "model Test Boolean c=true; Real x=if c then 1 else 2; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_6_if_elseif() {
        expect_success(
            "model Test Integer n=1; Real x=if n==0 then 0 elseif n==1 then 1 else 2; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_6_if_multiple_elseif() {
        expect_success(
            r#"
            model Test
                Integer n = 2;
                Real x = if n==0 then 0
                         elseif n==1 then 1
                         elseif n==2 then 4
                         elseif n==3 then 9
                         else 100;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_3_6_if_nested() {
        expect_success(
            "model Test Boolean a=true; Boolean b=false; Real x=if a then (if b then 1 else 2) else 3; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_6_if_in_array() {
        expect_success(
            "model Test Boolean c=true; Real x[2]={if c then 1 else 0, if not c then 1 else 0}; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_6_if_with_expression() {
        expect_success(
            "model Test Real a=1; Real b=2; Real c=if a<b then a+b else a-b; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_6_if_boolean_result() {
        expect_success(
            "model Test Boolean a=true; Boolean b=if a then true else false; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_6_if_with_comparison() {
        expect_success(
            "model Test Real x = 5; Real y = if x > 0 then 1 else -1; end Test;",
            "Test",
        );
    }
}

// ============================================================================
// §3.7 BUILT-IN OPERATORS
// ============================================================================

/// MLS §3.7: Built-in operators (der, pre, etc.)
///
/// Note: Mathematical functions (sin, cos, exp, etc.) are tested in Chapter 12
/// (§12.5 Built-in Functions). This section focuses on operators that are
/// unique to expression evaluation in Chapter 3.
mod section_3_7_operators {
    use super::*;

    // -------------------------------------------------------------------------
    // Array reduction functions (unique to Ch3 - tests array context)
    // -------------------------------------------------------------------------

    #[test]
    fn mls_3_7_min_array() {
        expect_success(
            "model Test Real x[5] = {3,1,4,1,5}; Real m = min(x); end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_7_max_array() {
        expect_success(
            "model Test Real x[5] = {3,1,4,1,5}; Real m = max(x); end Test;",
            "Test",
        );
    }

    // -------------------------------------------------------------------------
    // der operator (differentiation - unique to Ch3)
    // -------------------------------------------------------------------------

    #[test]
    fn mls_3_7_der() {
        expect_success(
            "model Test Real x(start=1); equation der(x) = -x; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_7_der_in_expression() {
        expect_success(
            "model Test Real x(start=1); Real v; equation der(x) = v; v = -x; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_7_der_with_coefficient() {
        expect_success(
            "model Test parameter Real k = 2; Real x(start=1); equation der(x) = -k*x; end Test;",
            "Test",
        );
    }
}

// ============================================================================
// §3.8 VARIABILITY OF EXPRESSIONS
// ============================================================================

/// MLS §3.8: Expression variability
mod section_3_8_variability {
    use super::*;

    #[test]
    fn mls_3_8_constant_expression() {
        expect_success("model Test constant Real x = 2 + 3; end Test;", "Test");
    }

    #[test]
    fn mls_3_8_parameter_expression() {
        expect_success(
            "model Test parameter Real a = 1; parameter Real b = a + 1; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_8_continuous_expression() {
        expect_success(
            "model Test Real x(start=0); Real y; equation der(x)=1; y=x*2; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_8_discrete_expression() {
        expect_success(
            r#"
            model Test
                Real x(start=0);
                discrete Integer n(start=0);
            equation
                der(x) = 1;
                when x > 1 then
                    n = pre(n) + 1;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_3_8_mixed_variability() {
        expect_success(
            "model Test parameter Real k = 2; Real x(start=0); equation der(x) = k; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_8_constant_builtin() {
        expect_success(
            "model Test constant Real x = sin(0.5) + cos(0.5); end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_3_8_parameter_builtin() {
        expect_success(
            "model Test parameter Real a = 0.5; parameter Real x = sin(a); end Test;",
            "Test",
        );
    }
}

// ============================================================================
// COMPLEX BOOLEAN EXPRESSIONS
// ============================================================================

/// Complex boolean expressions
mod complex_boolean {
    use super::*;

    #[test]
    fn complex_boolean_expr() {
        expect_success(
            "model Test Boolean b = (1 < 2) and (3 > 2) or not (4 == 5); end Test;",
            "Test",
        );
    }

    #[test]
    fn complex_if_with_condition() {
        expect_success(
            "model Test Real a=1; Real b=2; Real c=3; Real x = if a<b and b<c then 1 else 0; end Test;",
            "Test",
        );
    }

    #[test]
    fn nested_boolean_expressions() {
        expect_success(
            "model Test Boolean a=true; Boolean b=false; Boolean c = (a or b) and not (a and b); end Test;",
            "Test",
        );
    }
}
