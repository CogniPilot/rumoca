//! ALG (Algorithm) contract tests - MLS §11
//!
//! Tests for the 17 algorithm contracts defined in SPEC_0022.

use rumoca_contracts::test_support::{
    expect_failure_in_phase_with_code, expect_parse_err_with_code,
    expect_resolve_failure_with_code, expect_success,
};
use rumoca_session::FailedPhase;

// =============================================================================
// ALG-001: No equation syntax
// "Equation equality = shall not be used in an algorithm section"
// =============================================================================

#[test]
fn alg_001_assignment_in_algorithm() {
    // := is correct in algorithm sections
    expect_success(
        r#"
        model Test
            Real x(start = 0);
            Real y;
        algorithm
            y := x + 1;
        equation
            der(x) = -y;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn alg_001_no_equality_in_algorithm() {
    expect_parse_err_with_code(
        r#"
        model Test
            Real x;
            Real y;
        algorithm
            y = x + 1;
        equation
            x = 1;
        end Test;
    "#,
        "EP001",
    );
}

// =============================================================================
// ALG-002: LHS component types
// "Only type, record, operator record, connector may appear as left-hand-side"
// =============================================================================

#[test]
fn alg_002_lhs_component_types() {
    expect_failure_in_phase_with_code(
        r#"
        model Cell
            Real x;
        end Cell;

        model Test
            Cell c;
        algorithm
            c := Cell(1);
        end Test;
    "#,
        "Test",
        FailedPhase::ToDae,
        "ED006",
    );
}

// =============================================================================
// ALG-003: For event evaluable
// "If for-statement contains event-generating expressions, index shall be evaluable"
// =============================================================================

#[test]
#[ignore = "TODO(ALG-003): enforce event-evaluable loop-index requirement"]
fn alg_003_for_event_evaluable() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Integer n(start = 3);
            Real y(start = 0);
        algorithm
            for i in 1:n loop
                y := if time > 0.5 then y + i else y;
            end for;
        equation
            der(y) = -y;
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "ER000",
    );
}

// =============================================================================
// ALG-004: For no array overwrite
// "No assignments to entire arrays subscripted with loop variable inside for"
// =============================================================================

#[test]
#[ignore = "TODO(ALG-004): reject whole-array overwrite patterns tied to loop indices"]
fn alg_004_for_no_array_overwrite() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real a[3];
        algorithm
            for i in 1:3 loop
                a := fill(i, 3);
            end for;
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "ER000",
    );
}

// =============================================================================
// ALG-005: While scalar Boolean
// "Expression of while-statement shall be a scalar Boolean expression"
// =============================================================================

#[test]
fn alg_005_while_boolean() {
    expect_success(
        r#"
        function F
            input Integer n;
            output Integer result;
        protected
            Integer i;
        algorithm
            result := 0;
            i := 0;
            while i < n loop
                result := result + i;
                i := i + 1;
            end while;
        end F;
        model Test
            Integer y;
        equation
            y = F(5);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// ALG-006: While no events
// "Event-generating expressions not allowed in while condition or body"
// =============================================================================

#[test]
#[ignore = "TODO(ALG-006): reject event-generating expressions in while loops"]
fn alg_006_while_no_events() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real x(start = 0);
        algorithm
            while time < 1.0 loop
                x := x + 1.0;
            end while;
        equation
            der(x) = -x;
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "ER000",
    );
}

// =============================================================================
// ALG-007: When not in function
// "When-statement shall not be used inside a function class"
// =============================================================================

#[test]
fn alg_007_no_when_in_function() {
    expect_resolve_failure_with_code(
        r#"
        function F
            input Real x;
            output Real y;
        algorithm
            when x > 0 then
                y := 1;
            end when;
        end F;
    "#,
        "F",
        "ER015",
    );
}

// =============================================================================
// ALG-008: When not in initial
// "When-statement shall not occur inside an initial algorithm"
// =============================================================================

#[test]
#[ignore = "TODO(ALG-008): reject when-statements in initial algorithm sections"]
fn alg_008_when_not_in_initial() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            discrete Real x(start = 0);
        initial algorithm
            when time > 0 then
                x := 1;
            end when;
        equation
            x = 1;
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "ER000",
    );
}

// =============================================================================
// ALG-009: When no nesting
// "When-statement cannot be nested inside another when-statement"
// =============================================================================

#[test]
fn alg_009_no_nested_when_statements() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            discrete Real x(start = 0);
            discrete Real y(start = 0);
        algorithm
            when time > 1 then
                when time > 2 then
                    x := 1;
                end when;
                y := 1;
            end when;
        end Test;
    "#,
        "Test",
        "ER016",
    );
}

// =============================================================================
// ALG-010: When not in control
// "When-statements shall not occur inside while/for/if in algorithms"
// =============================================================================

#[test]
#[ignore = "TODO(ALG-010): reject when-statements nested in algorithm control structures"]
fn alg_010_when_not_in_control() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            discrete Real x(start = 0);
        algorithm
            if time > 0 then
                when time > 1 then
                    x := 1;
                end when;
            end if;
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "ER000",
    );
}

// =============================================================================
// ALG-011: When discrete Boolean
// "Expression of when-statement shall be discrete-time Boolean"
// =============================================================================

#[test]
#[ignore = "TODO(ALG-011): enforce discrete-time boolean condition requirement for when"]
fn alg_011_when_discrete_boolean() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real x(start = 0);
        algorithm
            when x > 0 then
                x := 1;
            end when;
        equation
            der(x) = -x;
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "ER000",
    );
}

// =============================================================================
// ALG-012: break scope
// "break can only be used in while or for loop"
// =============================================================================

#[test]
fn alg_012_break_in_for() {
    expect_success(
        r#"
        function F
            input Integer n;
            output Integer result;
        algorithm
            result := 0;
            for i in 1:n loop
                if i > 5 then
                    break;
                end if;
                result := result + i;
            end for;
        end F;
        model Test
            Integer y;
        equation
            y = F(10);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// ALG-013: return scope
// "return can only be used inside functions"
// =============================================================================

#[test]
fn alg_013_return_in_function() {
    expect_success(
        r#"
        function F
            input Real x;
            output Real y;
        algorithm
            if x > 0 then
                y := x;
                return;
            end if;
            y := -x;
        end F;
        model Test
            Real y;
        equation
            y = F(2.0);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// ALG-014: terminate not in function
// "terminate-statement shall not be used in functions"
// =============================================================================

#[test]
#[ignore = "TODO(ALG-014): reject terminate-statements in function classes"]
fn alg_014_terminate_not_in_function() {
    expect_failure_in_phase_with_code(
        r#"
        function F
            input Real x;
            output Real y;
        algorithm
            if x < 0 then
                terminate("stop");
            end if;
            y := x;
        end F;

        model Test
            Real y;
        equation
            y = F(1.0);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "ER000",
    );
}

// =============================================================================
// ALG-015: Assert execution halt
// "A failed assert stops the execution of the current algorithm"
// =============================================================================

#[test]
fn alg_015_assert_execution_halt() {
    expect_success(
        r#"
        model Test
            Real x(start = 1);
        algorithm
            assert(x > 0, "x must stay positive");
        equation
            der(x) = -1;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// ALG-016: For range fixed
// "For-statement range expressions are evaluated once before entering loop"
// =============================================================================

#[test]
fn alg_016_for_range() {
    expect_success(
        r#"
        model Test
            Real x[5];
        algorithm
            for i in 1:5 loop
                x[i] := i * 1.0;
            end for;
        equation
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// ALG-017: LHS initialization
// "Variables on the left-hand side of := must be initialized when algorithm is invoked"
// =============================================================================

#[test]
fn alg_017_lhs_initialization() {
    expect_resolve_failure_with_code(
        r#"
        function F
            input Real x;
            output Real y;
        protected
            Real temp;
        algorithm
            y := temp + x;
        end F;

        model Test
            Real y;
        equation
            y = F(1.0);
        end Test;
    "#,
        "Test",
        "ER043",
    );
}

// =============================================================================
// Algorithm integration tests
// =============================================================================

#[test]
fn alg_basic_algorithm_section() {
    expect_success(
        r#"
        model Test
            Real x(start = 0);
            Real y;
        algorithm
            if x > 0 then
                y := 1;
            else
                y := -1;
            end if;
        equation
            der(x) = -y;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn alg_for_loop_algorithm() {
    expect_success(
        r#"
        model Test
            Real x[3];
            Real total;
        algorithm
            total := 0;
            for i in 1:3 loop
                x[i] := i * 2.0;
                total := total + x[i];
            end for;
        equation
        end Test;
    "#,
        "Test",
    );
}
