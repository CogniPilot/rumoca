//! CLK (Clock) contract tests - MLS §16
//!
//! Tests for clock-variable and clock-constructor restrictions.

use rumoca_contracts::test_support::expect_resolve_failure_with_code;

// =============================================================================
// CLK-001: Clock not in function
// "Clock constructors not allowed inside function classes"
// =============================================================================

#[test]
fn clk_001_clock_constructor_forbidden_in_function() {
    expect_resolve_failure_with_code(
        r#"
        function F
            output Integer y;
        algorithm
            y := integer(Clock(0.1));
        end F;
    "#,
        "F",
        "ER056",
    );
}

// =============================================================================
// CLK-013: No derivative on clock ops
// "Derivative on sample(), subSample(), superSample(), shiftSample(), backSample(), noClock() is forbidden"
// =============================================================================

#[test]
fn clk_013_derivative_on_clock_operator_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            Real u;
            Real x(start = 0);
        equation
            der(x) = der(sample(u, Clock(0.1)));
        end Test;
    "#,
        "Test",
        "ER069",
    );
}

#[test]
fn clk_013_derivative_on_clock_operator_in_binding_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            Real u;
            Real x = der(sample(u, Clock(0.1)));
        equation
            u = 1;
        end Test;
    "#,
        "Test",
        "ER069",
    );
}

// =============================================================================
// CLK-002: Clock prefix restrictions
// "Clock variables cannot have flow, stream, discrete, parameter, or constant prefix"
// =============================================================================

#[test]
fn clk_002_parameter_clock_prefix_rejected() {
    expect_resolve_failure_with_code(
        r#"
        type MyClock = Clock;

        model Test
            parameter MyClock clk;
        end Test;
    "#,
        "Test",
        "ER063",
    );
}

#[test]
fn clk_002_discrete_clock_prefix_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            discrete Clock clk;
        end Test;
    "#,
        "Test",
        "ER063",
    );
}

// =============================================================================
// CLK-020: Clock variable restrictions
// "Clock variables cannot have prefixes flow, stream, discrete, parameter, or constant"
// =============================================================================

#[test]
fn clk_020_flow_clock_prefix_rejected() {
    expect_resolve_failure_with_code(
        r#"
        connector C
            flow Clock clk;
        end C;

        model Test
            C c;
        end Test;
    "#,
        "Test",
        "ER063",
    );
}

#[test]
fn clk_020_stream_clock_prefix_rejected() {
    expect_resolve_failure_with_code(
        r#"
        connector C
            stream Clock clk;
        end C;

        model Test
            C c;
        end Test;
    "#,
        "Test",
        "ER063",
    );
}
