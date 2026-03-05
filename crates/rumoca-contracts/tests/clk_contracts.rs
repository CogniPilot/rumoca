//! CLK (Clocked partitions) contract tests - MLS §16
//!
//! Tests for the 20 clock contracts defined in SPEC_0022.

use rumoca_contracts::test_support::expect_failure_in_phase_with_code;
use rumoca_session::FailedPhase;

fn expect_clk_todo(source: &str, model: &str) {
    expect_failure_in_phase_with_code(source, model, FailedPhase::Instantiate, "EI000");
}

// =============================================================================
// CLK-001: Clock not in function
// =============================================================================

#[test]
#[ignore = "TODO(CLK-001): reject clock constructors in function classes"]
fn clk_001_clock_not_in_function() {
    expect_clk_todo(
        r#"
        function F
            input Real x;
            output Real y;
        algorithm
            y := sample(x, Clock(0.1));
        end F;
    "#,
        "F",
    );
}

// =============================================================================
// CLK-002: Clock prefix restrictions
// =============================================================================

#[test]
#[ignore = "TODO(CLK-002): enforce invalid-prefix restrictions for Clock variables"]
fn clk_002_clock_prefix_restrictions() {
    expect_clk_todo(
        r#"
        model Test
            flow Clock c;
        equation
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// CLK-003: Clock switch evaluable
// =============================================================================

#[test]
#[ignore = "TODO(CLK-003): enforce evaluable switching conditions/subscripts for clock vars"]
fn clk_003_clock_switch_evaluable() {
    expect_clk_todo(
        r#"
        model Test
            Clock c[2];
            Integer i;
        equation
            i = integer(time);
            c[if time > 1 then 1 else 2] = Clock(0.1);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// CLK-004: No cross-subclock equations
// =============================================================================

#[test]
#[ignore = "TODO(CLK-004): reject equations simultaneously involving different sub-clocks"]
fn clk_004_no_cross_subclock_equations() {
    expect_clk_todo(
        r#"
        model Test
            Real x;
            Real y;
        equation
            x = sample(y, Clock(0.1));
            y = sample(x, Clock(0.2));
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// CLK-005: Unique clock association
// =============================================================================

#[test]
#[ignore = "TODO(CLK-005): enforce unique clock association per clocked variable"]
fn clk_005_unique_clock_association() {
    expect_clk_todo(
        r#"
        model Test
            Real x;
            Real y;
        equation
            y = sample(x, Clock(0.1));
            y = sample(x, Clock(0.2));
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// CLK-006: Clocked access restriction
// =============================================================================

#[test]
#[ignore = "TODO(CLK-006): enforce direct access of clocked vars only on active clock"]
fn clk_006_clocked_access_restriction() {
    expect_clk_todo(
        r#"
        model Test
            Real x;
            Real y;
        equation
            y = sample(x, Clock(0.1));
            x = y;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// CLK-007: sample() continuous input
// =============================================================================

#[test]
#[ignore = "TODO(CLK-007): enforce continuous-time input requirement for sample()"]
fn clk_007_sample_continuous_input() {
    expect_clk_todo(
        r#"
        model Test
            discrete Real x;
            Real y;
        equation
            y = sample(x, Clock(0.1));
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// CLK-008: hold() component input
// =============================================================================

#[test]
#[ignore = "TODO(CLK-008): enforce hold() argument restriction to component/parameter expressions"]
fn clk_008_hold_component_input() {
    expect_clk_todo(
        r#"
        model Test
            Real x;
            Real y;
        equation
            y = hold(x + 1.0);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// CLK-009: previous() component input
// =============================================================================

#[test]
#[ignore = "TODO(CLK-009): enforce previous() component-expression argument requirement"]
fn clk_009_previous_component_input() {
    expect_clk_todo(
        r#"
        model Test
            Real x;
            Real y;
        equation
            y = previous(x + 1.0);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// CLK-010: superSample event clock
// =============================================================================

#[test]
#[ignore = "TODO(CLK-010): reject superSample() use creating faster event clocks"]
fn clk_010_supersample_event_clock() {
    expect_clk_todo(
        r#"
        model Test
            Clock c = Clock(0.1);
            Clock c2;
        equation
            c2 = superSample(c, 2);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// CLK-011: shiftSample resolution
// =============================================================================

#[test]
#[ignore = "TODO(CLK-011): enforce shiftSample() resolution constraint for event clocks"]
fn clk_011_shiftsample_resolution() {
    expect_clk_todo(
        r#"
        model Test
            Clock c = Clock(0.1);
            Clock c2;
        equation
            c2 = shiftSample(c, 1, 2);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// CLK-012: backSample pre-start
// =============================================================================

#[test]
#[ignore = "TODO(CLK-012): reject backSample() creating ticks before base-clock start"]
fn clk_012_backsample_pre_start() {
    expect_clk_todo(
        r#"
        model Test
            Clock c = Clock(0.1);
            Clock c2;
        equation
            c2 = backSample(c, 1, 1);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// CLK-013: No derivative on clock ops
// =============================================================================

#[test]
#[ignore = "TODO(CLK-013): forbid derivatives of clock operators"]
fn clk_013_no_derivative_on_clock_ops() {
    expect_clk_todo(
        r#"
        model Test
            Real x;
            Real y;
        equation
            y = der(sample(x, Clock(0.1)));
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// CLK-014: Clocked when no nest
// =============================================================================

#[test]
#[ignore = "TODO(CLK-014): reject nested clocked when-clauses and elsewhen variants"]
fn clk_014_clocked_when_no_nest() {
    expect_clk_todo(
        r#"
        model Test
            Real x;
        equation
            when Clock(0.1) then
                when Clock(0.2) then
                    x = 1;
                end when;
            end when;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// CLK-015: Clocked when not in alg
// =============================================================================

#[test]
#[ignore = "TODO(CLK-015): reject clocked when-clauses inside algorithm sections"]
fn clk_015_clocked_when_not_in_algorithm() {
    expect_clk_todo(
        r#"
        model Test
            Real x;
        algorithm
            when Clock(0.1) then
                x := 1;
            end when;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// CLK-016: Discretized partition
// =============================================================================

#[test]
#[ignore = "TODO(CLK-016): enforce discretization of partitions containing der/delay/Boolean when"]
fn clk_016_discretized_partition() {
    expect_clk_todo(
        r#"
        model Test
            Real x;
            Real y;
        equation
            der(x) = -x;
            y = delay(x, 1.0);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// CLK-017: Sampling factor range
// =============================================================================

#[test]
#[ignore = "TODO(CLK-017): enforce supported accumulated sampling-factor range [1, 2^63]"]
fn clk_017_sampling_factor_range() {
    expect_clk_todo(
        r#"
        model Test
            Clock c = Clock(0.1);
            Clock c2;
        equation
            c2 = superSample(c, 9223372036854775807);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// CLK-018: Clock interval positive
// =============================================================================

#[test]
#[ignore = "TODO(CLK-018): enforce Clock(interval) positivity"]
fn clk_018_clock_interval_positive() {
    expect_clk_todo(
        r#"
        model Test
            Clock c = Clock(0.0);
        equation
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// CLK-019: intervalCounter positive
// =============================================================================

#[test]
#[ignore = "TODO(CLK-019): enforce intervalCounter > 0 in Clock(intervalCounter, resolution)"]
fn clk_019_intervalcounter_positive() {
    expect_clk_todo(
        r#"
        model Test
            Integer n = 0;
            Clock c = Clock(n, 10);
        equation
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// CLK-020: Clock variable restrictions
// =============================================================================

#[test]
#[ignore = "TODO(CLK-020): enforce forbidden prefixes on Clock variables"]
fn clk_020_clock_variable_restrictions() {
    expect_clk_todo(
        r#"
        model Test
            parameter Clock c = Clock(0.1);
        equation
        end Test;
    "#,
        "Test",
    );
}
