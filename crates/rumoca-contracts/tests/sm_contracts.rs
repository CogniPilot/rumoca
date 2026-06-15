//! SM (State Machine) contract tests - MLS §17
//!
//! Graph restrictions on transition()/initialState()/activeState(). State
//! machines are not lowered yet (uses also report ER073), but the structural
//! rules are validated with precise diagnostics during resolve.

use rumoca_contracts::test_support::expect_resolve_failure_with_code;

// =============================================================================
// SM-002: Different priorities
// "All transitions leaving one state must have different priorities"
// =============================================================================

#[test]
fn sm_002_duplicate_outgoing_transition_priority_rejected() {
    // Both transitions leave s1 with the default priority 1 (MLS §17.1).
    expect_resolve_failure_with_code(
        r#"
        model M
            block S
            end S;
            S s1;
            S s2;
            S s3;
        equation
            transition(s1, s2, time > 1);
            transition(s1, s3, time > 2);
            initialState(s1);
        end M;
    "#,
        "M",
        "ER077",
    );
}

// =============================================================================
// SM-003: Single initial state
// "One and only one instance in each state machine must be marked as initial"
// =============================================================================

#[test]
fn sm_003_missing_initial_state_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            block S
            end S;
            S s1;
            S s2;
        equation
            transition(s1, s2, time > 1);
        end M;
    "#,
        "M",
        "ER079",
    );
}

#[test]
fn sm_003_multiple_initial_states_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            block S
            end S;
            S s1;
            S s2;
        equation
            transition(s1, s2, time > 1);
            initialState(s1);
            initialState(s2);
        end M;
    "#,
        "M",
        "ER079",
    );
}

// =============================================================================
// SM-004: Priority minimum
// "Priority >= 1 required"
// =============================================================================

#[test]
fn sm_004_transition_priority_below_one_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            block S
            end S;
            S s1;
            S s2;
        equation
            transition(s1, s2, time > 1, priority = 0);
            initialState(s1);
        end M;
    "#,
        "M",
        "ER078",
    );
}

// =============================================================================
// SM-005: Not in functions
// "None of these operators allowed inside function classes"
// =============================================================================

#[test]
fn sm_005_state_machine_operator_forbidden_in_function() {
    expect_resolve_failure_with_code(
        r#"
        model M
            function F
                output Integer y;
            algorithm
                y := ticksInState();
            end F;
            Integer n = F();
        end M;
    "#,
        "M",
        "ER056",
    );
}

// =============================================================================
// SM-006: Equation context only
// "transition/initialState can only be used in equations, not in
//  non-parameter if-equations or when-equations"
// =============================================================================

#[test]
fn sm_006_transition_inside_when_equation_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            block S
            end S;
            S s1;
            S s2;
        equation
            when time > 0.5 then
                transition(s1, s2, time > 1);
            end when;
            initialState(s1);
        end M;
    "#,
        "M",
        "ER080",
    );
}

#[test]
fn sm_006_initial_state_inside_nonparameter_if_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            block S
            end S;
            S s1;
            S s2;
        equation
            transition(s1, s2, time > 1);
            if time > 0.5 then
                initialState(s1);
            end if;
        end M;
    "#,
        "M",
        "ER080",
    );
}

// =============================================================================
// SM-007: activeState instance check
// "Error if the instance is not a state of a state machine"
// =============================================================================

#[test]
fn sm_007_active_state_on_non_state_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            block S
            end S;
            S s1;
            S s2;
            S other;
            Real x;
        equation
            transition(s1, s2, time > 1);
            initialState(s1);
            x = if activeState(other) then 1.0 else 0.0;
        end M;
    "#,
        "M",
        "ER081",
    );
}

// =============================================================================
// SM-001: All state machine components must have the same clock
// SM-008: Error if parallel state machines assign to same variable at same
// clock tick
//
// State machines are not lowered yet: every model containing transition() is
// rejected with ER073, so every violation of these clock rules is rejected.
// The precise checks land with §17.3 elaboration into clocked equations.
// =============================================================================

#[test]
fn sm_001_mixed_clock_state_machine_rejected_as_unsupported() {
    expect_resolve_failure_with_code(
        r#"
        model M
            block S
            end S;
            S s1;
            S s2;
            Clock c1 = Clock(0.1);
        equation
            transition(s1, s2, time > 1);
            initialState(s1);
        end M;
    "#,
        "M",
        "ER073",
    );
}

#[test]
fn sm_008_parallel_machines_shared_assignment_rejected_as_unsupported() {
    expect_resolve_failure_with_code(
        r#"
        model M
            block S
                outer output Real shared;
            end S;
            S a1;
            S a2;
            S b1;
            S b2;
            inner Real shared(start = 0);
        equation
            transition(a1, a2, time > 1);
            initialState(a1);
            transition(b1, b2, time > 2);
            initialState(b1);
        end M;
    "#,
        "M",
        "ER073",
    );
}
