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

// =============================================================================
// CLK-014: Clocked when no nest
// "Clocked when-clauses cannot nest and have no elsewhen parts"
// =============================================================================

#[test]
fn clk_014_nested_clocked_when_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            Real x(start = 0);
            discrete Real d;
        equation
            der(x) = 1;
            when Clock(0.1) then
                when Clock(0.2) then
                    d = 1;
                end when;
            end when;
        end Test;
    "#,
        "Test",
        "ER017",
    );
}

// =============================================================================
// CLK-015: Clocked when-clauses cannot appear inside algorithms
// =============================================================================

#[test]
fn clk_015_clocked_when_in_algorithm_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            Real x(start = 0);
            Clock c = Clock(0.1);
        algorithm
            when c then
                x := pre(x) + 1;
            end when;
        end M;
    "#,
        "M",
        "ER097",
    );
}

// =============================================================================
// CLK-018: Clock(interval): interval must be strictly positive
// =============================================================================

#[test]
fn clk_018_zero_interval_clock_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            Clock c = Clock(0.0);
            Real x(start = 0);
        equation
            when c then
                x = pre(x) + 1;
            end when;
        end M;
    "#,
        "M",
        "ER082",
    );
}

// =============================================================================
// CLK-019: Clock(intervalCounter, resolution): intervalCounter must be > 0
// =============================================================================

#[test]
fn clk_019_negative_interval_counter_clock_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            Clock c = Clock(-1, 10);
            Real x(start = 0);
        equation
            when c then
                x = pre(x) + 1;
            end when;
        end M;
    "#,
        "M",
        "ER082",
    );
}

// =============================================================================
// CLK-008: hold() input must be a component expression or parameter expression
// =============================================================================

#[test]
fn clk_008_hold_with_general_expression_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            Clock c = Clock(0.1);
            Real xc(start = 0);
            Real y;
        equation
            when c then
                xc = previous(xc) + 1;
            end when;
            y = hold(xc + 1) * 2;
        end M;
    "#,
        "M",
        "ER109",
    );
}

// =============================================================================
// CLK-009: previous() input must be a component expression
// =============================================================================

#[test]
fn clk_009_previous_with_general_expression_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            Clock c = Clock(0.1);
            Real x(start = 0);
        equation
            when c then
                x = previous(x + 1) + 1;
            end when;
        end M;
    "#,
        "M",
        "ER109",
    );
}

// =============================================================================
// CLK-007: sample() input must be continuous-time with no variability
// restrictions (parameters are allowed)
// =============================================================================

#[test]
fn clk_007_sample_of_parameter_accepted() {
    rumoca_contracts::test_support::expect_success(
        r#"
        model M
            parameter Real p = 2;
            Clock c = Clock(0.1);
            Real xc(start = 0);
        equation
            when c then
                xc = sample(p, c) + previous(xc);
            end when;
        end M;
    "#,
        "M",
    );
}

// =============================================================================
// CLK-010: superSample() cannot be applied to event clocks to create faster
// clocks
// =============================================================================

#[test]
fn clk_010_supersample_on_event_clock_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            Boolean b = time > 0.5;
            Clock ec = Clock(b);
            Clock fast = superSample(ec, 2);
            Real x(start = 0);
        equation
            when fast then
                x = previous(x) + 1;
            end when;
        end M;
    "#,
        "M",
        "ER119",
    );
}

// =============================================================================
// CLK-011: shiftSample() on event clocks cannot have resolution other than 1
// =============================================================================

#[test]
fn clk_011_shiftsample_on_event_clock_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            Boolean b = time > 0.5;
            Clock ec = Clock(b);
            Clock shifted = shiftSample(ec, 1, 3);
            Real x(start = 0);
        equation
            when shifted then
                x = previous(x) + 1;
            end when;
        end M;
    "#,
        "M",
        "ER119",
    );
}

// =============================================================================
// CLK-012: backSample() cannot create clock ticks before the base-clock starts
// =============================================================================

#[test]
fn clk_012_backsample_on_event_clock_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            Boolean b = time > 0.5;
            Clock ec = Clock(b);
            Clock back = backSample(ec, 1, 3);
            Real x(start = 0);
        equation
            when back then
                x = previous(x) + 1;
            end when;
        end M;
    "#,
        "M",
        "ER119",
    );
}

// =============================================================================
// CLK-016: Partitions containing der(), delay(), or when-clauses with Boolean
// conditions are discretized. rumoca conservatively refuses der() inside
// clocked partitions instead of discretizing; the refusal is pinned here.
// =============================================================================

#[test]
fn clk_016_der_in_clocked_partition_conservatively_rejected() {
    rumoca_contracts::test_support::expect_failure_in_phase_with_code(
        r#"
        model M
            Clock c = Clock(0.1);
            Real x(start = 0);
            Real v(start = 0);
        equation
            when c then
                v = previous(v) + 0.1;
                der(x) = v;
            end when;
        end M;
    "#,
        "M",
        rumoca_compile::compile::FailedPhase::Flatten,
        "EF004",
    );
}

// =============================================================================
// CLK-017: Accumulated sub- and supersampling factors in range 1 to 2^63 must
// be supported
// =============================================================================

#[test]
fn clk_017_large_resampling_factor_chain_accepted() {
    rumoca_contracts::test_support::expect_success(
        r#"
        model M
            Clock c = Clock(0.001);
            Clock slow = subSample(c, 1000);
            Clock fast = superSample(slow, 1000);
            Real x(start = 0);
        equation
            when fast then
                x = previous(x) + 1;
            end when;
        end M;
    "#,
        "M",
    );
}

// =============================================================================
// CLK-003: Subscripts and conditions switching between clock variables must
// be evaluable expressions
// =============================================================================

#[test]
fn clk_003_noneval_clock_subscript_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            Clock c[2] = {Clock(0.1), Clock(0.2)};
            Integer i(start = 1);
            Real x(start = 0);
        equation
            i = if time > 1 then 1 else 2;
            when c[i] then
                x = previous(x) + 1;
            end when;
        end M;
    "#,
        "M",
        "ER128",
    );
}

// =============================================================================
// CLK-005: Every clocked variable associates uniquely with exactly one clock
// =============================================================================

#[test]
fn clk_005_variable_assigned_in_two_partitions_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            Clock c1 = Clock(0.1);
            Clock c2 = Clock(0.2);
            Real x(start = 0);
        equation
            when c1 then
                x = previous(x) + 1;
            end when;
            when c2 then
                x = previous(x) + 2;
            end when;
        end M;
    "#,
        "M",
        "ER126",
    );
}

// =============================================================================
// CLK-004: Systems of equations cannot involve different sub-clocks
// simultaneously
// =============================================================================

#[test]
fn clk_004_direct_cross_partition_read_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            Clock c1 = Clock(0.1);
            Clock c2 = Clock(0.2);
            Real a(start = 0);
            Real b(start = 0);
        equation
            when c1 then
                a = previous(a) + 1;
            end when;
            when c2 then
                b = a + 1;
            end when;
        end M;
    "#,
        "M",
        "ER126",
    );
}

// =============================================================================
// CLK-006: Clocked variables can only be directly accessed when their
// associated clock is active
// =============================================================================

#[test]
fn clk_006_clocked_variable_continuous_access_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            Clock c = Clock(0.1);
            Real xc(start = 0);
            Real y;
        equation
            when c then
                xc = previous(xc) + 1;
            end when;
            y = 2 * xc;
        end M;
    "#,
        "M",
        "ER127",
    );
}

#[test]
fn clk_006_hold_wrapped_access_accepted() {
    rumoca_contracts::test_support::expect_success(
        r#"
        model M
            Clock c = Clock(0.1);
            Real xc(start = 0);
            Real y;
        equation
            when c then
                xc = previous(xc) + 1;
            end when;
            y = 2 * hold(xc);
        end M;
    "#,
        "M",
    );
}
