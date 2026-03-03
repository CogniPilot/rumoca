//! EQN (Equation) contract tests - MLS §8
//!
//! Tests for the 38 equation contracts defined in SPEC_0022.

use rumoca_contracts::test_support::{
    expect_balanced, expect_failure_in_phase_with_code, expect_parse_err_with_code,
    expect_resolve_failure_with_code, expect_success,
};
use rumoca_session::FailedPhase;

// =============================================================================
// EQN-001: Local balance
// "Local number of unknowns identical to local equation size"
// =============================================================================

#[test]
fn eqn_001_simple_balanced() {
    expect_balanced(
        r#"
        model Test
            Real x;
        equation
            x = 1;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn eqn_001_ode_balanced() {
    expect_balanced(
        r#"
        model Test
            Real x(start = 0);
            Real y(start = 1);
        equation
            der(x) = -x;
            der(y) = -y;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn eqn_001_underspecified() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real x;
            Real y;
        equation
            x = 1;
        end Test;
    "#,
        "Test",
        FailedPhase::ToDae,
        "ED001",
    );
}

// =============================================================================
// EQN-002: Input binding
// "All non-connector inputs of model/block must have binding equations"
// =============================================================================

#[test]
fn eqn_002_input_with_binding() {
    expect_success(
        r#"
        model Test
            input Real u = 1.0;
            Real x(start = 0);
        equation
            der(x) = u;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// EQN-003: Algorithm atomicity
// "Algorithm section with N LHS variables = atomic N-dimensional vector-equation"
// =============================================================================

#[test]
fn eqn_003_algorithm_section() {
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

// =============================================================================
// EQN-004: If-equation balance
// "Same number of equations in each branch"
// =============================================================================

#[test]
fn eqn_004_if_equation_balanced() {
    expect_success(
        r#"
        model Test
            parameter Boolean flag = true;
            Real x;
        equation
            if flag then
                x = 1;
            else
                x = 2;
            end if;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// EQN-005: When nesting
// "When-equations cannot be nested"
// =============================================================================

#[test]
fn eqn_005_no_nested_when() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            discrete Real x(start = 0);
            discrete Real y(start = 0);
        equation
            when time > 1 then
                when time > 2 then
                    x = 1;
                end when;
                y = 1;
            end when;
        end Test;
    "#,
        "Test",
        "ER017",
    );
}

// =============================================================================
// EQN-006: When location
// "When-equations shall not occur inside initial equations"
// =============================================================================

#[test]
fn eqn_006_no_when_in_initial() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            Real x(start = 0);
        initial equation
            when true then
                x = 1;
            end when;
        equation
            der(x) = -x;
        end Test;
    "#,
        "Test",
        "ER018",
    );
}

// =============================================================================
// EQN-008: For-equation vector
// "Expression of a for-equation shall be a vector expression"
// =============================================================================

#[test]
fn eqn_008_for_equation_vector() {
    expect_success(
        r#"
        model Test
            Real x[3];
        equation
            for i in 1:3 loop
                x[i] = i;
            end for;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// EQN-010: For-loop variable readonly
// "Loop-variable shall not be assigned to"
// =============================================================================

#[test]
fn eqn_010_for_variable_readonly() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            Real x[3];
        equation
            for i in 1:3 loop
                i = 1;
                x[i] = i;
            end for;
        end Test;
    "#,
        "Test",
        "ER019",
    );
}

// =============================================================================
// EQN-011: If-equation scalar Boolean
// "Expression of if-clause must be a scalar Boolean expression"
// =============================================================================

#[test]
fn eqn_011_if_boolean_ok() {
    expect_success(
        r#"
        model Test
            parameter Boolean flag = true;
            Real x;
        equation
            if flag then
                x = 1;
            else
                x = 0;
            end if;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// EQN-013: When branch variables
// "Different branches of when/elsewhen must have same set of left-hand side refs"
// =============================================================================

#[test]
fn eqn_013_when_elsewhen_same_lhs_set_ok() {
    expect_success(
        r#"
        model Test
            Real x(start = 0);
        equation
            der(x) = 1;
            when x > 0.2 then
                assert(x >= 0, "x should stay nonnegative");
            elsewhen x > 0.6 then
                assert(x >= 0, "x should stay nonnegative");
            end when;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn eqn_013_when_elsewhen_different_lhs_set_fails() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real x(start = 0);
            discrete Real y(start = 0);
            discrete Real z(start = 0);
        equation
            der(x) = 1;
            when x > 0.2 then
                y = 1;
            elsewhen x > 0.6 then
                z = 2;
            end when;
        end Test;
    "#,
        "Test",
        FailedPhase::Flatten,
        "EF004",
    );
}

// =============================================================================
// EQN-015: reinit in when only
// "reinit can only be used in the body of a when-equation"
// =============================================================================

#[test]
fn eqn_015_reinit_in_when() {
    expect_success(
        r#"
        model Test
            Real x(start = 0);
        equation
            der(x) = 1;
            when x > 5 then
                reinit(x, 0);
            end when;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn eqn_015_reinit_outside_when_fails() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            Real x(start = 0);
        equation
            der(x) = 1;
            reinit(x, 0);
        end Test;
    "#,
        "Test",
        "ER008",
    );
}

// =============================================================================
// EQN-016: reinit state variable
// "reinit x: x must be selected as a state"
// =============================================================================

#[test]
fn eqn_016_reinit_state_variable_ok() {
    expect_success(
        r#"
        model Test
            Real x(start = 0);
        equation
            der(x) = 1;
            when x > 0.5 then
                reinit(x, 0);
            end when;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn eqn_016_reinit_non_state_fails() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real x(start = 0);
            Real y;
        equation
            der(x) = 1;
            y = x;
            when x > 0.5 then
                reinit(y, 0);
            end when;
        end Test;
    "#,
        "Test",
        FailedPhase::ToDae,
        "ED004",
    );
}

// =============================================================================
// EQN-024: No statements in equations
// "No statements allowed in equation sections, including := operator"
// =============================================================================

#[test]
fn eqn_024_no_assignment_in_equations() {
    expect_parse_err_with_code(
        r#"
        model Test
            Real x;
        equation
            x := 1;
        end Test;
    "#,
        "EP001",
    );
}

// =============================================================================
// EQN-025: Equation type compatible
// "Types of left-hand-side and right-hand-side must be compatible"
// =============================================================================

#[test]
fn eqn_025_type_compatible() {
    expect_balanced(
        r#"
        model Test
            Real x;
        equation
            x = 1.0;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn eqn_025_user_defined_record_type_mismatch_fails() {
    expect_failure_in_phase_with_code(
        r#"
        record LeftPayload
            Real x;
        end LeftPayload;
        record RightPayload
            Real x;
        end RightPayload;
        model Test
            LeftPayload lhs;
            RightPayload rhs;
        equation
            lhs = rhs;
        end Test;
    "#,
        "Test",
        FailedPhase::Typecheck,
        "ET002",
    );
}

#[test]
fn eqn_025_record_constructor_type_mismatch_fails() {
    expect_failure_in_phase_with_code(
        r#"
        record LeftPayload
            Real x;
        end LeftPayload;
        record RightPayload
            Real x;
        end RightPayload;
        model Test
            LeftPayload lhs;
        equation
            lhs = RightPayload(x = 1.0);
        end Test;
    "#,
        "Test",
        FailedPhase::Typecheck,
        "ET002",
    );
}

#[test]
fn eqn_025_record_wrapper_constructor_compatible() {
    expect_success(
        r#"
        record BasePayload
            Real x;
        end BasePayload;
        record WrappedPayload = BasePayload;
        model Test
            WrappedPayload lhs;
        equation
            lhs = BasePayload(x = 1.0);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// EQN-029: When expr type
// "Expression shall be discrete-time Boolean scalar or vector expression"
// =============================================================================

#[test]
fn eqn_029_when_boolean_condition() {
    expect_success(
        r#"
        model Test
            Real x(start = 0);
        equation
            der(x) = 1;
            when x > 5 then
                reinit(x, 0);
            end when;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// EQN-033: Perfect matching
// "There must exist a perfect matching of variables to equations after flattening"
// =============================================================================

#[test]
fn eqn_033_perfect_matching() {
    expect_balanced(
        r#"
        model Test
            Real x;
            Real y;
        equation
            x + y = 1;
            x - y = 0;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// EQN-037: When not in initial eq
// "It is not allowed to use when-clauses in initial equation/algorithm sections"
// =============================================================================

#[test]
fn eqn_037_no_when_in_initial() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            Real x(start = 0);
        initial equation
            when true then
                x = 1;
            end when;
        equation
            der(x) = -x;
        end Test;
    "#,
        "Test",
        "ER018",
    );
}

// =============================================================================
// Additional basic equation tests
// =============================================================================

#[test]
fn eqn_multiple_equations() {
    expect_balanced(
        r#"
        model Test
            Real x(start = 1);
            Real y;
            Real z;
        equation
            der(x) = -x;
            y = x * 2;
            z = y + 1;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn eqn_for_loop_equations() {
    expect_success(
        r#"
        model Test
            parameter Integer n = 5;
            Real x[n];
        equation
            for i in 1:n loop
                x[i] = i * 1.0;
            end for;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn eqn_when_clause_basic() {
    expect_success(
        r#"
        model Test
            Real x(start = 0);
        equation
            der(x) = 1;
            when x > 10 then
                reinit(x, 0);
            end when;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn eqn_021_constants_by_declaration() {
    expect_balanced(
        r#"
        model Test
            constant Real g = 9.81;
            Real x;
        equation
            x = g;
        end Test;
    "#,
        "Test",
    );
}
