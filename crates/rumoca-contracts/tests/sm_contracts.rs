//! SM (State Machine) contract tests - MLS §17
//!
//! Tests for the 8 state-machine contracts defined in SPEC_0022.

use rumoca_contracts::test_support::expect_failure_in_phase_with_code;
use rumoca_session::FailedPhase;

// =============================================================================
// SM-001: Same clock
// "All state machine components must have the same clock"
// =============================================================================

#[test]
#[ignore = "TODO(SM-001): enforce same-clock constraint across state machine components"]
fn sm_001_same_clock() {
    expect_failure_in_phase_with_code(
        r#"
        model S1
            output Real y;
        equation
            y = 1;
        end S1;

        model S2
            output Real y;
        equation
            y = 2;
        end S2;

        model Test
            S1 s1;
            S2 s2;
        equation
            transition(s1, s2, condition=true, immediate=true, reset=true, synchronize=false, priority=1);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

// =============================================================================
// SM-002: Different priorities
// "All transitions leaving one state must have different priorities"
// =============================================================================

#[test]
#[ignore = "TODO(SM-002): enforce distinct transition priorities from a state"]
fn sm_002_different_priorities() {
    expect_failure_in_phase_with_code(
        r#"
        model S1
            output Real y;
        equation
            y = 1;
        end S1;

        model S2
            output Real y;
        equation
            y = 2;
        end S2;

        model S3
            output Real y;
        equation
            y = 3;
        end S3;

        model Test
            S1 s1;
            S2 s2;
            S3 s3;
        equation
            transition(s1, s2, condition=true, immediate=true, reset=true, synchronize=false, priority=1);
            transition(s1, s3, condition=true, immediate=true, reset=true, synchronize=false, priority=1);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

// =============================================================================
// SM-003: Single initial state
// "One and only one instance in each state machine must be marked as initial"
// =============================================================================

#[test]
#[ignore = "TODO(SM-003): enforce exactly one initial state per state machine"]
fn sm_003_single_initial_state() {
    expect_failure_in_phase_with_code(
        r#"
        model S1
            output Real y;
        equation
            y = 1;
        end S1;

        model S2
            output Real y;
        equation
            y = 2;
        end S2;

        model Test
            S1 s1;
            S2 s2;
        equation
            initialState(s1);
            initialState(s2);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

// =============================================================================
// SM-004: Priority minimum
// "Priority >= 1 required"
// =============================================================================

#[test]
#[ignore = "TODO(SM-004): enforce transition priority lower bound (>= 1)"]
fn sm_004_priority_minimum() {
    expect_failure_in_phase_with_code(
        r#"
        model S1
            output Real y;
        equation
            y = 1;
        end S1;

        model S2
            output Real y;
        equation
            y = 2;
        end S2;

        model Test
            S1 s1;
            S2 s2;
        equation
            transition(s1, s2, condition=true, immediate=true, reset=true, synchronize=false, priority=0);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

// =============================================================================
// SM-005: Not in functions
// "State-machine operators are not allowed inside function classes"
// =============================================================================

#[test]
#[ignore = "TODO(SM-005): reject state-machine operators inside functions"]
fn sm_005_not_in_functions() {
    expect_failure_in_phase_with_code(
        r#"
        function F
            input Real x;
            output Real y;
        algorithm
            y := if activeState(x) then 1 else 0;
        end F;

        model Test
            Real y;
        equation
            y = F(1.0);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

// =============================================================================
// SM-006: Equation context only
// "transition/initialState can only be used in equations"
// =============================================================================

#[test]
#[ignore = "TODO(SM-006): enforce equation-context restriction for transition/initialState"]
fn sm_006_equation_context_only() {
    expect_failure_in_phase_with_code(
        r#"
        model S1
            output Real y;
        equation
            y = 1;
        end S1;

        model S2
            output Real y;
        equation
            y = 2;
        end S2;

        model Test
            S1 s1;
            S2 s2;
        algorithm
            if true then
                // invalid context per MLS §17
                initialState(s1);
                transition(s1, s2, condition=true, immediate=true, reset=true, synchronize=false, priority=1);
            end if;
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

// =============================================================================
// SM-007: activeState instance check
// "Error if the instance is not a state of a state machine"
// =============================================================================

#[test]
#[ignore = "TODO(SM-007): validate activeState argument is a state-machine state instance"]
fn sm_007_activestate_instance_check() {
    expect_failure_in_phase_with_code(
        r#"
        model Plain
            Real x;
        equation
            x = 1;
        end Plain;

        model Test
            Plain p;
            Boolean b;
        equation
            b = activeState(p);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

// =============================================================================
// SM-008: Parallel assignment error
// "Error if parallel state machines assign to same variable at same clock tick"
// =============================================================================

#[test]
#[ignore = "TODO(SM-008): detect conflicting parallel state-machine assignments per clock tick"]
fn sm_008_parallel_assignment_error() {
    expect_failure_in_phase_with_code(
        r#"
        model S1
            output Real y;
        equation
            y = 1;
        end S1;

        model S2
            output Real y;
        equation
            y = 2;
        end S2;

        model Test
            S1 s1a;
            S2 s2a;
            S1 s1b;
            S2 s2b;
            Real x;
        equation
            x = if activeState(s1a) then 1 else 0;
            x = if activeState(s1b) then 2 else 0;
            transition(s1a, s2a, condition=true, immediate=true, reset=true, synchronize=false, priority=1);
            transition(s1b, s2b, condition=true, immediate=true, reset=true, synchronize=false, priority=1);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}
