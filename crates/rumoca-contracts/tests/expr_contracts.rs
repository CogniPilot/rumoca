//! EXPR (Expression/Operator) contract tests - MLS §3
//!
//! Tests for the 40 expression contracts defined in SPEC_0022.

use rumoca_contracts::test_support::{
    expect_balanced, expect_failure_in_phase_with_code, expect_parse_err_with_code,
    expect_resolve_failure_with_code, expect_success,
};
use rumoca_compile::compile::FailedPhase;

// =============================================================================
// EXPR-001: Relational scalar only
// "Relational operators only defined for scalar operands of simple types"
// =============================================================================

#[test]
fn expr_001_relational_scalar_ok() {
    expect_balanced(
        r#"
        model Test
            Real x;
            Boolean b;
        equation
            x = 1.0;
            b = x > 0;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// EXPR-003: der continuity
// "Expression must be continuous and semi-differentiable"
// =============================================================================

#[test]
fn expr_003_der_of_continuous() {
    expect_balanced(
        r#"
        model Test
            Real x(start = 0);
        equation
            der(x) = -x;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// EXPR-005: Overflow undefined
// "If a numeric operation overflows the result is undefined"
// =============================================================================

#[test]
fn expr_005_normal_arithmetic() {
    expect_balanced(
        r#"
        model Test
            Real x;
        equation
            x = 2.0 * 3.0 + 1.0;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// EXPR-012: Variability assignment
// "Expression must not have higher variability than assigned component"
// =============================================================================

#[test]
fn expr_012_constant_to_parameter_ok() {
    expect_success(
        r#"
        model Test
            parameter Real p = 1.0;
            Real x;
        equation
            x = p;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn expr_012_variable_to_parameter_fails() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            Real x;
            parameter Real p = x;
        equation
            x = time;
        end Test;
    "#,
        "Test",
        "ER006",
    );
}

// =============================================================================
// EXPR-014: Non-associative chaining
// "Non-associative operators cannot be chained: 1 < 2 < 3 is invalid"
// =============================================================================

#[test]
fn expr_014_no_chained_relationals() {
    expect_parse_err_with_code(
        r#"
        model Test
            Boolean b;
        equation
            b = 1 < 2 < 3;
        end Test;
    "#,
        "EP001",
    );
}

// =============================================================================
// EXPR-016: If-expr Boolean condition
// "First expression of if-expression must be Boolean expression"
// =============================================================================

#[test]
fn expr_016_if_expr_boolean() {
    expect_balanced(
        r#"
        model Test
            Real x;
        equation
            x = if true then 1.0 else 0.0;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn expr_016_if_expr_non_boolean_fails() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            Real x;
        equation
            x = if 1 then 1.0 else 0.0;
        end Test;
    "#,
        "Test",
        "ER010",
    );
}

// =============================================================================
// EXPR-017: If-expr type compatible
// "The two branch expressions must be type compatible expressions"
// =============================================================================

#[test]
fn expr_017_if_expr_same_type() {
    expect_balanced(
        r#"
        model Test
            Real x;
        equation
            x = if true then 1.0 else 2.0;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn expr_output_primary_postfix_dot_ident_is_accepted() {
    expect_balanced(
        r#"
        record R
            Real re;
        end R;

        model Test
            R r;
            Real x;
        equation
            r.re = 1.0;
            x = (r).re;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn expr_output_primary_postfix_array_subscript_is_accepted() {
    expect_balanced(
        r#"
        model Test
            Real a[3];
            Real x;
        equation
            a[1] = 1.0;
            a[2] = 2.0;
            a[3] = 3.0;
            x = (a)[2];
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// EXPR-018: abs argument type
// "Argument v of abs(v) needs to be Integer or Real expression"
// =============================================================================

#[test]
fn expr_018_abs_real() {
    expect_balanced(
        r#"
        model Test
            Real x;
        equation
            x = abs(-3.14);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// EXPR-019: sign argument type
// "Argument v of sign(v) needs to be Integer or Real expression"
// =============================================================================

#[test]
fn expr_019_sign_real() {
    expect_balanced(
        r#"
        model Test
            Real x;
        equation
            x = sign(-3.14);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// EXPR-024: div/mod/rem types
// "Result and arguments shall have type Real or Integer"
// =============================================================================

#[test]
fn expr_024_div_integer() {
    expect_success(
        r#"
        model Test
            Integer x;
        equation
            x = div(7, 2);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn expr_024_mod_integer() {
    expect_success(
        r#"
        model Test
            Integer x;
        equation
            x = mod(7, 2);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// EXPR-025: ceil/floor argument
// "Result and argument shall have type Real"
// =============================================================================

#[test]
fn expr_025_floor_real() {
    expect_success(
        r#"
        model Test
            Real x;
        equation
            x = floor(3.7);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn expr_025_ceil_real() {
    expect_success(
        r#"
        model Test
            Real x;
        equation
            x = ceil(3.2);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// EXPR-006 / EXPR-029: delay parameter expressions
// =============================================================================

#[test]
fn expr_006_delaymax_parameter_expression_ok() {
    expect_success(
        r#"
        model Test
            parameter Real delayMax = 1.0;
            Real x(start = 0);
            Real y;
        equation
            der(x) = 1.0;
            y = delay(x, 0.1, delayMax);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn expr_006_delaymax_parameter_expression_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            Real x(start = 0);
            Real y;
        equation
            der(x) = 1.0;
            y = delay(x, 0.1, x);
        end Test;
    "#,
        "Test",
        "ER055",
    );
}

#[test]
fn expr_029_delaytime_parameter_expression_ok() {
    expect_success(
        r#"
        model Test
            parameter Real dt = 0.1;
            Real x(start = 0);
            Real y;
        equation
            der(x) = 1.0;
            y = delay(x, dt);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn expr_029_delaytime_non_parameter_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            Real x(start = 0);
            Real y;
        equation
            der(x) = 1.0;
            y = delay(x, x);
        end Test;
    "#,
        "Test",
        "ER055",
    );
}

// =============================================================================
// EXPR-008 / EXPR-033 / EXPR-035 / EXPR-036 / EXPR-037: function bans
// =============================================================================

#[test]
fn expr_008_delay_not_in_function() {
    expect_resolve_failure_with_code(
        r#"
        function F
            input Real x;
            output Real y;
        algorithm
            y := delay(x, 0.1);
        end F;
    "#,
        "F",
        "ER056",
    );
}

#[test]
fn expr_033_cardinality_not_in_function() {
    expect_resolve_failure_with_code(
        r#"
        function F
            input Real x;
            output Integer y;
        algorithm
            y := cardinality(x);
        end F;
    "#,
        "F",
        "ER056",
    );
}

#[test]
fn expr_035_instream_not_in_function() {
    expect_resolve_failure_with_code(
        r#"
        function F
            input Real x;
            output Real y;
        algorithm
            y := inStream(x);
        end F;
    "#,
        "F",
        "ER056",
    );
}

#[test]
fn expr_036_actualstream_not_in_function() {
    expect_resolve_failure_with_code(
        r#"
        function F
            input Real x;
            output Real y;
        algorithm
            y := actualStream(x);
        end F;
    "#,
        "F",
        "ER056",
    );
}

#[test]
fn expr_037_pre_not_in_function() {
    expect_resolve_failure_with_code(
        r#"
        function F
            input Real x;
            output Real y;
        algorithm
            y := pre(x);
        end F;
    "#,
        "F",
        "ER056",
    );
}

// =============================================================================
// EXPR-009: cardinality restrictions
// =============================================================================

#[test]
fn expr_009_cardinality_rejects_connector_arrays() {
    expect_resolve_failure_with_code(
        r#"
        connector Pin
            Real v;
            flow Real i;
        end Pin;

        model Test
            Pin p[2];
            Integer n;
        equation
            n = cardinality(p);
            p[1].v = 0.0;
            p[1].i = 0.0;
            p[2].v = 0.0;
            p[2].i = 0.0;
        end Test;
    "#,
        "Test",
        "ER057",
    );
}

#[test]
fn expr_009_cardinality_rejects_expandable_connectors() {
    expect_resolve_failure_with_code(
        r#"
        expandable connector Bus
            Real v;
        end Bus;

        model Test
            Bus bus;
            Integer n;
        equation
            n = cardinality(bus);
        end Test;
    "#,
        "Test",
        "ER057",
    );
}

// =============================================================================
// EXPR-026 / EXPR-027 / EXPR-028: builtin argument types
// =============================================================================

#[test]
fn expr_026_integer_accepts_real_argument() {
    expect_success(
        r#"
        model Test
            Real x;
            Integer n;
        equation
            x = 1.25;
            n = integer(x);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn expr_026_integer_rejects_boolean_argument() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Boolean b;
            Integer n;
        equation
            b = true;
            n = integer(b);
        end Test;
    "#,
        "Test",
        FailedPhase::Typecheck,
        "ET002",
    );
}

#[test]
fn expr_027_delay_rejects_string_value_argument() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            String s;
            String y;
        equation
            s = "hello";
            y = delay(s, 1.0);
        end Test;
    "#,
        "Test",
        FailedPhase::Typecheck,
        "ET002",
    );
}

#[test]
fn expr_028_delay_rejects_non_real_time_argument() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real x(start = 0);
            parameter Boolean b = true;
            Real y;
        equation
            der(x) = 1.0;
            y = delay(x, b);
        end Test;
    "#,
        "Test",
        FailedPhase::Typecheck,
        "ET002",
    );
}

// =============================================================================
// EXPR-039: noEvent event suppression
// "noEvent suppresses event generation for relational operators"
// =============================================================================

#[test]
fn expr_039_noevent_usage() {
    expect_success(
        r#"
        model Test
            Real x(start = 0);
        equation
            der(x) = if noEvent(x > 0) then -1 else 1;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// EXPR-040: Event triggering operators
// "div, ceil, floor, integer can only change values at events"
// =============================================================================

#[test]
fn expr_040_integer_event_trigger() {
    expect_success(
        r#"
        model Test
            Real x(start = 0);
            Integer n;
        equation
            der(x) = 1;
            n = integer(x);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// Basic expression tests (EXPR-002, EXPR-004, etc.)
// =============================================================================

#[test]
fn expr_002_no_real_equality() {
    // Real equality comparison should not be allowed outside functions
    expect_resolve_failure_with_code(
        r#"
        model Test
            Real x;
            Boolean b;
        equation
            x = 1.0;
            b = x == 1.0;
        end Test;
    "#,
        "Test",
        "ER029",
    );
}

#[test]
fn expr_004_no_der_in_functions() {
    expect_resolve_failure_with_code(
        r#"
        function F
            input Real x;
            output Real y;
        algorithm
            y := der(x);
        end F;
    "#,
        "F",
        "ER030",
    );
}

// =============================================================================
// EXPR-013: end only in subscripts
// =============================================================================

#[test]
fn expr_013_end_in_subscript() {
    expect_balanced(
        r#"
        model Test
            parameter Integer n = 5;
            Real x[n];
        equation
            x[end] = 1.0;
            for i in 1:n-1 loop
                x[i] = 0;
            end for;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn expr_013_end_outside_subscript_fails() {
    // "end" as standalone expression outside subscript should fail
    expect_resolve_failure_with_code(
        r#"
        model Test
            Real x;
        equation
            x = end;
        end Test;
    "#,
        "Test",
        "ER031",
    );
}

// =============================================================================
// EXPR-015: Unary additive position
// "Additive unary expressions only allowed in first term"
// =============================================================================

#[test]
fn expr_015_unary_minus_ok() {
    expect_balanced(
        r#"
        model Test
            Real x;
        equation
            x = -1.0;
        end Test;
    "#,
        "Test",
    );
}
