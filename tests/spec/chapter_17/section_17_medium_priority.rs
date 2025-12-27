//! MLS §17: Medium Priority State Machine Tests
//!
//! This module tests medium priority normative requirements:
//! - §17.1: State machine operators (transition, initialState, activeState)
//! - §17.2: Transition properties (priority, immediate, reset, synchronize)
//! - §17.3: State machine execution rules
//!
//! Reference: https://specification.modelica.org/master/state-machines.html

use crate::spec::{expect_failure, expect_parse_success};

// ============================================================================
// §17.1 STATE MACHINE OPERATORS
// ============================================================================

/// MLS §17.1: Basic state machine operators
mod state_machine_operators {
    use super::*;

    /// MLS: "transition() defines state transition"
    #[test]
    fn mls_17_1_transition_basic() {
        expect_parse_success(
            r#"
            model StateMachine
                inner Integer state(start = 0);

                model State1
                    outer Integer state;
                equation
                    state = 1;
                end State1;

                model State2
                    outer Integer state;
                equation
                    state = 2;
                end State2;

                State1 state1;
                State2 state2;
            equation
                transition(state1, state2, time > 1);
            end StateMachine;
            "#,
        );
    }

    /// MLS: "initialState() marks initial state"
    #[test]
    fn mls_17_1_initial_state() {
        expect_parse_success(
            r#"
            model StateMachine
                model State1
                equation
                end State1;

                State1 state1;
            equation
                initialState(state1);
            end StateMachine;
            "#,
        );
    }

    /// MLS: "activeState(state) returns true if state is active"
    #[test]
    fn mls_17_1_active_state() {
        expect_parse_success(
            r#"
            model StateMachine
                model State1
                equation
                end State1;

                State1 state1;
                Boolean isState1Active;
            equation
                isState1Active = activeState(state1);
                initialState(state1);
            end StateMachine;
            "#,
        );
    }

    /// MLS: "ticksInState() returns ticks in current state"
    #[test]
    fn mls_17_1_ticks_in_state() {
        expect_parse_success(
            r#"
            model StateMachine
                model State1
                    Integer ticks;
                equation
                    ticks = ticksInState();
                end State1;

                State1 state1;
            equation
                initialState(state1);
            end StateMachine;
            "#,
        );
    }

    /// MLS: "timeInState() returns time in current state"
    #[test]
    fn mls_17_1_time_in_state() {
        expect_parse_success(
            r#"
            model StateMachine
                model State1
                    Real stateTime;
                equation
                    stateTime = timeInState();
                end State1;

                State1 state1;
            equation
                initialState(state1);
            end StateMachine;
            "#,
        );
    }
}

// ============================================================================
// §17.2 TRANSITION PROPERTIES
// ============================================================================

/// MLS §17.2: Transition property specifications
mod transition_properties {
    use super::*;

    /// MLS: "Transitions must have unique priorities"
    #[test]
    fn mls_17_2_transition_priority_unique() {
        expect_failure(
            r#"
            model StateMachine
                model State1 end State1;
                model State2 end State2;
                model State3 end State3;

                State1 s1;
                State2 s2;
                State3 s3;
            equation
                initialState(s1);
                transition(s1, s2, true, priority = 1);
                transition(s1, s3, true, priority = 1);
            end StateMachine;
            "#,
            "StateMachine",
        );
    }

    /// MLS: "immediate=true evaluates condition immediately"
    #[test]
    fn mls_17_2_transition_immediate() {
        expect_parse_success(
            r#"
            model StateMachine
                model State1 end State1;
                model State2 end State2;

                State1 s1;
                State2 s2;
            equation
                initialState(s1);
                transition(s1, s2, true, immediate = true);
            end StateMachine;
            "#,
        );
    }

    /// MLS: "reset=true reinitializes variables on entry"
    #[test]
    fn mls_17_2_transition_reset() {
        expect_parse_success(
            r#"
            model StateMachine
                model State1
                    Real x(start = 0);
                equation
                    der(x) = 1;
                end State1;

                model State2
                    Real y(start = 0);
                equation
                    der(y) = 2;
                end State2;

                State1 s1;
                State2 s2;
            equation
                initialState(s1);
                transition(s1, s2, s1.x > 5, reset = true);
            end StateMachine;
            "#,
        );
    }

    /// MLS: "synchronize=true waits for all parallel states"
    #[test]
    fn mls_17_2_transition_synchronize() {
        expect_parse_success(
            r#"
            model StateMachine
                model State1 end State1;
                model State2 end State2;

                State1 s1;
                State2 s2;
            equation
                initialState(s1);
                transition(s1, s2, true, synchronize = true);
            end StateMachine;
            "#,
        );
    }

    /// MLS: "Priority defaults to 1 if not specified"
    #[test]
    fn mls_17_2_transition_default_priority() {
        expect_parse_success(
            r#"
            model StateMachine
                model State1 end State1;
                model State2 end State2;

                State1 s1;
                State2 s2;
            equation
                initialState(s1);
                transition(s1, s2, time > 1);
            end StateMachine;
            "#,
        );
    }
}

// ============================================================================
// §17.3 STATE MACHINE EXECUTION
// ============================================================================

/// MLS §17.3: State machine execution rules
mod execution_rules {
    use super::*;

    /// MLS: "Only one state is active at a time (exclusive states)"
    #[test]
    fn mls_17_3_single_active_state() {
        expect_parse_success(
            r#"
            model StateMachine
                model State1
                    Real x(start = 0);
                equation
                    der(x) = 1;
                end State1;

                model State2
                    Real x(start = 0);
                equation
                    der(x) = -1;
                end State2;

                State1 s1;
                State2 s2;
            equation
                initialState(s1);
                transition(s1, s2, s1.x > 5);
                transition(s2, s1, s2.x < -5);
            end StateMachine;
            "#,
        );
    }

    /// MLS: "Variables persist in inactive states"
    #[test]
    fn mls_17_3_variable_persistence() {
        expect_parse_success(
            r#"
            model StateMachine
                model State1
                    discrete Real counter(start = 0);
                algorithm
                    when sample(0, 0.1) then
                        counter := counter + 1;
                    end when;
                end State1;

                model State2
                    discrete Real counter(start = 0);
                algorithm
                    when sample(0, 0.1) then
                        counter := counter + 2;
                    end when;
                end State2;

                State1 s1;
                State2 s2;
            equation
                initialState(s1);
                transition(s1, s2, s1.counter > 10);
                transition(s2, s1, s2.counter > 20);
            end StateMachine;
            "#,
        );
    }

    /// MLS: "Nested state machines (hierarchical states)"
    #[test]
    #[ignore = "nested state machines not yet implemented"]
    fn mls_17_3_nested_state_machine() {
        expect_parse_success(
            r#"
            model OuterStateMachine
                model InnerStateMachine
                    model SubState1 end SubState1;
                    model SubState2 end SubState2;

                    SubState1 ss1;
                    SubState2 ss2;
                equation
                    initialState(ss1);
                    transition(ss1, ss2, time > 1);
                end InnerStateMachine;

                InnerStateMachine inner;
            equation
                initialState(inner);
            end OuterStateMachine;
            "#,
        );
    }

    /// MLS: "Parallel state machines"
    #[test]
    fn mls_17_3_parallel_states() {
        expect_parse_success(
            r#"
            model ParallelStateMachines
                model StateMachine1
                    model S1 end S1;
                    model S2 end S2;
                    S1 s1;
                    S2 s2;
                equation
                    initialState(s1);
                    transition(s1, s2, time > 1);
                end StateMachine1;

                model StateMachine2
                    model S3 end S3;
                    model S4 end S4;
                    S3 s3;
                    S4 s4;
                equation
                    initialState(s3);
                    transition(s3, s4, time > 2);
                end StateMachine2;

                StateMachine1 sm1;
                StateMachine2 sm2;
            equation
            end ParallelStateMachines;
            "#,
        );
    }
}

// ============================================================================
// §17.1 STATE MACHINE RESTRICTIONS
// ============================================================================

/// MLS §17.1: State machine restrictions
mod state_restrictions {
    use super::*;

    /// MLS: "State must be a block or model"
    #[test]
    fn mls_17_1_state_must_be_block_or_model() {
        expect_failure(
            r#"
            model StateMachine
                record NotAState
                    Real x;
                end NotAState;

                NotAState s;
            equation
                initialState(s);
            end StateMachine;
            "#,
            "StateMachine",
        );
    }

    /// MLS: "initialState must be called exactly once"
    #[test]
    fn mls_17_1_single_initial_state() {
        expect_failure(
            r#"
            model StateMachine
                model State1 end State1;
                model State2 end State2;

                State1 s1;
                State2 s2;
            equation
                initialState(s1);
                initialState(s2);
            end StateMachine;
            "#,
            "StateMachine",
        );
    }

    /// MLS: "Transition must reference valid states"
    #[test]
    fn mls_17_1_transition_valid_states() {
        expect_failure(
            r#"
            model StateMachine
                model State1 end State1;
                State1 s1;
            equation
                initialState(s1);
                transition(s1, nonexistent, true);
            end StateMachine;
            "#,
            "StateMachine",
        );
    }
}
